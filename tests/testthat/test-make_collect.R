# collect_cran() ####

test_that("CRAN dependency fields are parsed correctly", {
  db <- data.frame(
    Package = c("a", "b", "c", "d"),
    Depends = c("R (>= 4.1.0), Matrix(>= 1.8-0)", NA, "R", NA),
    Imports = c("dplyr,\n  igraph , tibble", "a", NA, "d, e, e"),
    stringsAsFactors = FALSE)
  out <- .parse_cran_deps(db, c("Depends", "Imports"))
  # Version constraints are stripped whether or not they are preceded
  # by a space, so that neither dependency is dropped or malformed
  expect_true("Matrix" %in% out$to)
  expect_false(any(grepl("[(<>=]", out$to)))
  # Newlines and spaces around a name do not make it a separate node
  expect_equal(out$to, trimws(out$to))
  expect_true(all(c("dplyr", "igraph", "tibble") %in% out$to))
  # Base packages, and R itself, are not dependencies
  expect_false("R" %in% out$to)
  # Missing fields contribute nothing, and self-ties and
  # repeated declarations are dropped
  expect_false("c" %in% out$from)
  expect_equal(sum(out$from == "d"), 1)
  expect_equal(nrow(out), 6)
  # Ties are typed by the field that declared them
  expect_s3_class(out$type, "factor")
  expect_equal(levels(out$type), c("Depends", "Imports"))
})

test_that("CRAN nodes record which dependencies are on CRAN", {
  db <- data.frame(Package = c("a", "b"), Version = c("1.0", "2.0"),
                   NeedsCompilation = c("yes", "no"),
                   stringsAsFactors = FALSE)
  ties <- data.frame(from = "a", to = "elsewhere", stringsAsFactors = FALSE)
  out <- .cran_nodes(db, ties)
  expect_equal(out$name, c("a", "b", "elsewhere"))
  expect_equal(out$on_cran, c(TRUE, TRUE, FALSE))
  expect_equal(out$compiled, c(TRUE, FALSE, NA))
  # Absent columns do not error
  expect_true(all(is.na(out$license)))
})

test_that("collect_cran() collects readable dependency networks", {
  skip_on_cran()
  op <- options(repos = c(CRAN = "https://cloud.r-project.org"))
  on.exit(options(op), add = TRUE)
  # Skip where CRAN cannot be reached, rather than using
  # testthat::skip_if_offline(), which requires {curl}.
  # available.packages() warns and returns nothing where the download fails,
  # and caches the index for an hour, so this probe is almost free.
  reachable <- tryCatch(nrow(suppressWarnings(.cran_db())) > 0,
                        error = function(e) FALSE)
  skip_if_not(reachable, "CRAN could not be reached")
  out <- collect_cran("manynet")
  expect_true(is_manynet(out))
  expect_true("manynet" %in% node_labels(out))
  expect_true(all(grepl("^[A-Za-z][A-Za-z0-9.]*$", node_labels(out))))
  expect_true(all(node_attribute(out, "on_cran")))
  # Scoping by distance is cumulative
  near <- node_labels(collect_cran("manynet", max_dist = 1))
  far <- node_labels(collect_cran("manynet", max_dist = 2))
  expect_true(all(near %in% far))
  expect_lt(length(near), net_nodes(out))
  # Reverse dependencies are a different, smaller set here
  rev <- collect_cran("manynet", direction = "in", max_dist = 1)
  expect_false(setequal(node_labels(rev), near))
  # Requesting one field returns a network with one kind of tie
  expect_false(is_multiplex(collect_cran("manynet", dependencies = "Imports")))
  # Suggests are excluded by default, and greatly enlarge the network
  expect_gt(net_nodes(collect_cran("manynet",
                                   dependencies = c("Imports", "Suggests"))),
            net_nodes(out))
  # Unknown packages are reported rather than silently ignored
  expect_error(collect_cran("notapackage123"), "could not be found")
})

# collect_pkg() ####

# Writes a small package of R scripts to a temporary directory,
# rather than committing a fixture that deliberately fails to parse.
fixture_pkg <- function(broken = FALSE) {
  dir <- file.path(tempdir(), paste0("collectpkg", as.integer(broken)))
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "R"), recursive = TRUE)
  writeLines(c(
    "#' Roxygen prose mentioning baz() must not count",
    "# nor must to_ego() in a comment",
    "foo <- function(x) { bar(x); bar(x); baz(1); \"baz(2) in a string\" }",
    "bar = function(y) baz(y)",
    "baz <- \\(z) z + 1",
    "qux <-",
    "  function(a) foo(a)",
    "rec <- function(n) if (n > 0) rec(n - 1)",
    "outer <- function() { inner <- function() foo(1); inner() }",
    "nsq <- function() igraph::vcount(1)"),
    file.path(dir, "R", "a.R"))
  if (broken) writeLines("oops <- function( {", file.path(dir, "R", "b.R"))
  dir
}

test_that("collect_pkg() finds functions however they are defined", {
  out <- collect_pkg(fixture_pkg())
  expect_true(is_manynet(out))
  # `<-`, `=`, a lambda, and a definition split over two lines are all found,
  # as are functions nested inside another function
  expect_setequal(node_labels(out),
                  c("foo", "bar", "baz", "qux", "rec", "outer", "inner", "nsq"))
  expect_equal(node_attribute(out, "file"), rep("a.R", 8))
})

test_that("collect_pkg() counts calls exactly", {
  out <- collect_pkg(fixture_pkg())
  ties <- as_edgelist(out)
  ties$weight <- unname(tie_weights(out))
  called <- function(from, to) ties$weight[ties$from == from & ties$to == to]
  # Repeated calls are weighted, but calls in comments and strings are not
  expect_equal(called("foo", "bar"), 2)
  expect_equal(called("foo", "baz"), 1)
  # Calls are attributed to the innermost function enclosing them
  expect_equal(called("outer", "inner"), 1)
  expect_equal(called("inner", "foo"), 1)
  expect_length(called("outer", "foo"), 0)
  # Recursion is a self-tie
  expect_equal(called("rec", "rec"), 1)
  # Substrings of another function's name are not calls to it
  expect_length(called("qux", "foo"), 1)
  expect_equal(nrow(ties), 7)
})

test_that("collect_pkg() only includes external functions where asked", {
  expect_false("igraph::vcount" %in% node_labels(collect_pkg(fixture_pkg())))
  out <- collect_pkg(fixture_pkg(), external = TRUE)
  # Namespaced calls are qualified, so that they cannot collide with
  # a function of the same name defined here
  expect_true("igraph::vcount" %in% node_labels(out))
  expect_false(node_attribute(out, "internal")[
    which(node_labels(out) == "igraph::vcount")])
})

test_that("collect_pkg() reports scripts it cannot parse", {
  dir <- fixture_pkg(broken = TRUE)
  expect_warning(out <- collect_pkg(dir), "b.R")
  # The scripts that do parse are still collected
  expect_true("foo" %in% node_labels(out))
})

test_that("collect_pkg() errors informatively where there is nothing to find", {
  dir <- file.path(tempdir(), "collectpkgempty")
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  expect_error(collect_pkg(dir), "No R scripts")
  expect_error(collect_pkg(file.path(dir, "nowhere")), "does not exist")
  writeLines("x <- 1", file.path(dir, "a.R"))
  expect_error(collect_pkg(dir), "No function definitions")
})

# collect_emails() ####

# The messages of a small mailbox, each as the lines of one message.
# All of the addresses are fictional.
fixture_emails <- function() {
  list(
    # A folded field, a quoted name holding a comma, an encoded name,
    # and addresses that differ only in case
    c("Received: from somewhere",
      "\tby elsewhere",
      "Message-ID: <1@example.org>",
      "Date: Mon, 6 Jan 2025 09:15:00 +0100 (CET)",
      "From: Ada Lovelace <ada@example.org>",
      "To: \"Babbage, Charles\" <Charles@Example.org>,",
      "\t=?UTF-8?B?SsO8cmdlbg==?= <jurgen@example.org>,",
      " cy@example.org",
      "Subject: To: nobody@example.org",
      "",
      "Dear all,",
      "",
      "From here on, we go ahead.",
      "To: ghost@example.org",
      ">From ghost Mon Jan  6 09:15:00 2025"),
    # To, Cc, and Bcc, a date without seconds or a weekday,
    # and an address named in two fields
    c("Message-ID: <2@example.org>",
      "Date: 6 Jan 2025 10:02 GMT",
      "From: charles@example.org",
      "To: Ada <ada@example.org>",
      "Cc: =?ISO-8859-1?Q?Ren=E9_Descartes?= <rene@example.org>,",
      "  ada@example.org",
      "Bcc: cy@example.org",
      "",
      "Yes."),
    # The same message again
    c("Message-ID: <2@example.org>",
      "Date: 6 Jan 2025 10:02 GMT",
      "From: charles@example.org",
      "To: Ada <ada@example.org>",
      "",
      "Yes."),
    # No recipient
    c("Message-ID: <3@example.org>",
      "Date: Tue, 07 Jan 2025 23:30:00 -0500",
      "From: Ada <ada@example.org>",
      "To: undisclosed-recipients:;",
      "",
      "Nobody."),
    # No identifier, a group, and a negative time zone
    c("Date: Tue, 07 Jan 2025 23:30:00 -0500",
      "From: cy@example.org",
      "To: Team: ada@example.org, Rene <rene@example.org>;",
      "Delivered-To: ada@example.org",
      "",
      "No id.")
  )
}

fixture_mbox <- function(msgs = fixture_emails()) {
  path <- tempfile(fileext = ".mbox")
  writeLines(unlist(lapply(msgs, function(m) {
    c("From - Mon Jan  6 09:15:00 2025", m, "")
  })), path)
  path
}

fixture_eml <- function(msgs = fixture_emails()) {
  dir <- tempfile("eml")
  dir.create(dir)
  # Written as bytes, since a text connection on Windows would turn the "\n"
  # of each line end into "\r\n" again
  for (i in seq_along(msgs)) {
    con <- file(file.path(dir, paste0(i, ".eml")), open = "wb")
    writeLines(msgs[[i]], con, sep = "\r\n")
    close(con)
  }
  dir
}

test_that("email addresses are split from the names displayed with them", {
  out <- .email_addresses(c(
    "\"Doe, Jane\" <Jane@Example.org>, =?UTF-8?B?SsO8cmdlbg==?= <j@x.org>",
    "undisclosed-recipients:;",
    "Team: a@x.org, \"B: <c@d.org>\" <b@x.org>;",
    "'quoted@x.org'",
    ""))
  expect_equal(out$address[[1]], c("jane@example.org", "j@x.org"))
  expect_equal(out$display[[1]], c("Doe, Jane", "Jürgen"))
  expect_length(out$address[[2]], 0)
  # A group's name is not the name of its first address, and the characters
  # that structure a field do not do so within a quoted name
  expect_equal(out$address[[3]], c("a@x.org", "b@x.org"))
  expect_equal(out$display[[3]], c("", "B: <c@d.org>"))
  expect_equal(out$address[[4]], "quoted@x.org")
  expect_length(out$address[[5]], 0)
})

test_that("encoded names are decoded", {
  expect_equal(.email_decode(c("plain",
                               "=?UTF-8?B?QQ==?=", "=?UTF-8?B?QUI=?=",
                               "=?utf-8?b?QUJD?=",
                               "=?ISO-8859-1?Q?Ren=E9_Descartes?=",
                               "=?utf-8?Q?J=C3=BCrgen?= =?utf-8?Q?_M?=")),
               c("plain", "A", "AB", "ABC", "René Descartes",
                 "Jürgen M"))
  # A name that cannot be decoded is left as it was
  expect_equal(.email_decode("=?x-unknown?B?QUJD?="), "=?x-unknown?B?QUJD?=")
})

test_that("email dates are parsed to UTC whatever the locale", {
  out <- .email_date(c("Mon, 6 Jan 2025 09:15:00 +0100 (CET)",
                       "6 Jan 2025 10:02 GMT",
                       "Tue, 07 Jan 2025 23:30:00 -0500",
                       "Fri, 3 Dec 99 12:00:00 PST",
                       "rubbish", NA))
  expect_s3_class(out, "POSIXct")
  expect_equal(format(out, "%Y-%m-%d %H:%M", tz = "UTC"),
               c("2025-01-06 08:15", "2025-01-06 10:02", "2025-01-08 04:30",
                 "1999-12-03 20:00", NA, NA))
})

test_that("mbox files are read the same however they are chunked", {
  path <- fixture_mbox()
  whole <- .email_read_mbox(path)
  # Only the headers wanted are kept, together with the lines continuing them
  expect_false(any(grepl("^(Received|Subject|\tby)", whole$line)))
  expect_true(any(grepl("^\t=\\?UTF-8", whole$line)))
  # Nothing in the body of a message begins another or is read as a header
  expect_equal(max(whole$msg), 5)
  expect_false(any(grepl("ghost", whole$line)))
  for (chunk in 1:7)
    expect_equal(.email_read_mbox(path, chunk = chunk), whole)
})

test_that("collect_emails() collects a dynamic one-mode network", {
  out <- collect_emails(fixture_mbox())
  expect_s3_class(out, "stocnet")
  expect_silent(validate_stocnet(out))
  expect_equal(out$nodes$label,
               c("ada@example.org", "charles@example.org",
                 "jurgen@example.org", "cy@example.org", "rene@example.org"))
  expect_equal(out$nodes$display[1:3],
               c("Ada", "Babbage, Charles", "Jürgen"))
  # The repeated message is collected once, the message without a recipient
  # is dropped, and an address named twice in a message is tied once
  expect_equal(net_ties(out), 8)
  expect_equal(unique(out$ties$message),
               c("1@example.org", "2@example.org", "message4"))
  expect_equal(as.vector(table(out$ties$layer)[c("to", "cc", "bcc")]),
               c(6, 1, 1))
  expect_false(any(out$ties$from == out$ties$to))
  expect_true(is_directed(out))
  expect_true(is_multiplex(out))
  # Emails are events, not the waves of a panel
  expect_true(is_dynamic(out))
  expect_false(is_longitudinal(out))
  expect_equal(format(out$ties$time[1], "%H:%M", tz = "UTC"), "08:15")
  # The network survives coercion
  tg <- as_tidygraph(out)
  expect_equal(igraph::ecount(tg), 8)
  expect_true("time" %in% net_tie_attributes(tg))
  expect_equal(node_labels(as_igraph(out)), out$nodes$label)
})

test_that("collect_emails() collects a changing two-mode network", {
  out <- collect_emails(fixture_mbox(), twomode = TRUE)
  expect_s3_class(out, "stocnet")
  expect_silent(validate_stocnet(out))
  expect_true(is_twomode(out))
  expect_true(is_changing(out))
  expect_equal(sum(out$nodes$mode == "messages"), 3)
  expect_equal(sum(out$nodes$mode == "addresses"), 5)
  # Each message has one sender, and enters the network when it was sent
  expect_equal(sum(out$ties$layer == "from"), 3)
  expect_equal(nrow(out$changes), 3)
  expect_false(any(out$nodes$active[out$nodes$mode == "messages"]))
  expect_true(all(out$nodes$active[out$nodes$mode == "addresses"]))
  expect_true(is_twomode(as_igraph(out)))
})

test_that("collect_emails() reads folders of messages as it does mbox files", {
  mbox <- collect_emails(fixture_mbox())
  eml <- collect_emails(fixture_eml())
  expect_equal(eml$nodes, mbox$nodes)
  expect_equal(eml$ties, mbox$ties)
  # A single message, whatever its extension
  one <- tempfile(fileext = ".txt")
  writeLines(fixture_emails()[[1]], one)
  expect_equal(net_ties(collect_emails(one)), 3)
})

test_that("collect_emails() records an ego boundary only where there is an ego", {
  path <- fixture_mbox()
  out <- collect_emails(path)
  expect_equal(out$info$boundary, "ego")
  expect_equal(out$nodes$label[out$nodes$ego], "ada@example.org")
  # The ties are records of events and not ego's reports of them
  expect_false(is_egocentric(out))
  expect_false("by" %in% names(out$ties))
  # Ego may have several addresses, however they are capitalised
  out <- collect_emails(path, ego = c("Ada@example.org", "cy@example.org"))
  expect_equal(sum(out$nodes$ego), 2)
  expect_equal(out$info$boundary, "ego")
  expect_warning(collect_emails(path, ego = "nobody@example.org"),
                 "not party to")
  out <- collect_emails(path, ego = FALSE)
  expect_null(out$info$boundary)
  expect_false(any(out$nodes$ego))
  expect_error(collect_emails(path, ego = 1), "should be")
  # No address is party to half of the messages between three separate pairs
  pairs <- lapply(1:3, function(i) {
    c(paste0("Message-ID: <", i, "@example.org>"),
      paste0("From: a", i, "@example.org"),
      paste0("To: b", i, "@example.org"), "", "Hello.")
  })
  out <- collect_emails(fixture_mbox(pairs))
  expect_null(out$info$boundary)
  expect_false(any(out$nodes$ego))
  # With one kind of tie the network is not multiplex, and undated ties remain
  expect_false("layer" %in% names(out$ties))
  expect_true(all(is.na(out$ties$time)))
  # An address a message was delivered to is party to it, though unnamed
  listed <- lapply(pairs, c, "Delivered-To: owner@example.org")
  listed <- lapply(listed, function(m) m[order(m == "" | m == "Hello.")])
  expect_equal(collect_emails(fixture_mbox(listed))$info$boundary, "ego")
})

test_that("collect_emails() errors informatively where there is nothing to find", {
  expect_error(collect_emails(file.path(tempdir(), "nowhere.mbox")),
               "does not exist")
  expect_error(collect_emails(c("a", "b")), "single path")
  empty <- tempfile(fileext = ".mbox")
  writeLines("Nothing to see here.", empty)
  expect_error(collect_emails(empty), "No emails")
  alone <- fixture_mbox(fixture_emails()[4])
  expect_error(collect_emails(alone), "both a sender and a recipient")
})

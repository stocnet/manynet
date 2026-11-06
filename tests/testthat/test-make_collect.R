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

test_that("CRAN nodes record the minimum version of R", {
  db <- data.frame(Package = c("a", "b", "c", "d", "e"),
                   Depends = c("R (>= 4.1.0), Matrix(>= 1.8-0)",
                               "methods,\n  R(>=3.5)", "R", NA,
                               "Rcpp (>= 1.0.0)"),
                   stringsAsFactors = FALSE)
  ties <- data.frame(from = "a", to = "elsewhere", stringsAsFactors = FALSE)
  out <- .cran_nodes(db, ties)
  expect_equal(out$min_r, c("4.1.0", "3.5", NA, NA, NA, NA))
  # A package whose name starts with R is not taken for R itself
  expect_true(is.na(out$min_r[out$name == "e"]))
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

# collect_text() ####

fixture_speeches <- c(first = "Trade builds peace. Peace builds trade!",
                      second = "War ends trade.\n\nPeace needs work.")

fixture_play <- data.frame(speaker = c("Ann", "Bob", "Ann", NA),
                           addressee = c("Bob", "Ann", "Cy", "Ann"),
                           text = c("Have you seen Cy?", "Cy left with Dee.",
                                    "Dee is lovely, Cy is awful.",
                                    "Dee called."),
                           stringsAsFactors = FALSE)

fixture_cast <- list(Ann = "ann", Bob = "bob", Cy = "cy", Dee = "dee")

test_that("collect_text() collects a two-mode network of documents and terms", {
  out <- collect_text(fixture_speeches)
  expect_s3_class(out, "stocnet")
  expect_true(is_twomode(out))
  expect_equal(out$info$modes, c("documents", "terms"))
  expect_equal(out$nodes$label[1:2], c("first", "second"))
  terms <- out$nodes$label[out$nodes$mode == "terms"]
  expect_equal(terms, sort(terms, method = "radix"))
  # counts are weights, and each tie is stamped with its unit's position
  el <- as_edgelist(out)
  expect_equal(el$weight[el$from == "first" & el$to == "trade"], 2)
  expect_equal(sum(el$weight), 12)
  # documents need not be in order, so are not stamped with a time
  expect_false("time" %in% names(out$ties))
  expect_null(out$info$update)
  # projections take the mode names
  expect_equal(sort(node_labels(to_mode(out, "terms"))), sort(terms))
  expect_equal(node_labels(to_mode(out, "documents")), c("first", "second"))
})

test_that("collect_text() stamps no time on a text of one unit", {
  one <- "I think there is something in this thing. Why don't we find out."
  out <- collect_text(one)
  expect_false("time" %in% names(out$ties))
  expect_false(is_longitudinal(out))
  expect_true(is_longitudinal(collect_text(one, unit = "sentence")))
  speech <- collect_text(one, terms = "thing", from = "Ann")
  expect_false("time" %in% names(speech$ties))
})

test_that("collect_text() splits the text into units", {
  sent <- collect_text(fixture_speeches, unit = "sentences")
  expect_equal(sent$ties$time, sent$ties$from)
  expect_equal(sent$info$modes[1], "sentences")
  expect_equal(sent$nodes$label[sent$nodes$mode == "sentences"],
               c("first.1", "first.2", "second.1", "second.2"))
  expect_equal(sent$nodes$document[1:4], rep(c("first", "second"), each = 2))
  para <- collect_text(fixture_speeches, unit = "paragraph")
  expect_equal(sum(para$nodes$mode == "paragraphs"), 3)
  # a window of three words moves along each document
  win <- collect_text(fixture_speeches, unit = 3, ties = "tokens")
  expect_equal(sum(win$nodes$mode == "windows"), 4 + 4)
  expect_equal(nrow(win$ties), 8 * 3)
  expect_error(collect_text(fixture_speeches, unit = "chapter"), "unit")
  expect_error(collect_text(fixture_speeches, unit = 0), "positive integer")
})

test_that("collect_text() records tokens or counts", {
  tok <- collect_text(fixture_speeches, ties = "tokens")
  expect_false("weight" %in% names(tok$ties))
  expect_equal(nrow(tok$ties), 12)
  expect_equal(nrow(collect_text(fixture_speeches)$ties), 9)
})

test_that("collect_text() keeps the terms of a dictionary and drops stopwords", {
  out <- collect_text(fixture_speeches, terms = c("peace", "war", "Trade"))
  expect_equal(out$nodes$label[out$nodes$mode == "terms"],
               c("peace", "war", "Trade"))
  stop <- collect_text(fixture_speeches, stopwords = data.frame(word = "Peace"))
  expect_false("peace" %in% stop$nodes$label)
  expect_error(collect_text(fixture_speeches, stopwords = 1), "stopwords")
  # aliases of a name, with the longest match taken
  story <- c(one = "Harry Potter met Ron. Potter smiled at the Bank of England.")
  names <- list(Harry = c("harry", "potter", "harry potter"), Ron = "ron",
                Bank = "bank of england")
  out <- collect_text(story, terms = names, stopwords = "of")
  expect_equal(out$info$modes, c("documents", "characters"))
  el <- as_edgelist(out)
  expect_equal(el$weight, c(2, 1, 1))
  expect_error(collect_text(story, terms = list("harry")), "name each")
})

test_that("collect_text() reads data frames of texts and of tokens", {
  df <- data.frame(doc_id = c("a", "b"), year = c(2001, 2002),
                   text = unname(fixture_speeches))
  out <- collect_text(df)
  expect_equal(out$nodes$label[1:2], c("a", "b"))
  expect_equal(out$nodes$year[1:2], c(2001, 2002))
  # one token in each row, as tidytext or udpipe give
  tidy <- data.frame(document = rep(c("a", "b"), c(3, 2)),
                     word = c("trade", "builds", "peace", "war", "ends"))
  expect_equal(nrow(collect_text(tidy)$ties), 5)
  ud <- data.frame(doc_id = rep("a", 5), sentence_id = c(1, 1, 2, 2, 2),
                   token = c("Wars", "end", "Peace", "builds", "peace"),
                   lemma = c("war", "end", "peace", "build", "peace"),
                   upos = c("NOUN", "VERB", "NOUN", "VERB", "NOUN"))
  out <- collect_text(ud[ud$upos == "NOUN", ], unit = "sentence",
                      token = "lemma")
  expect_equal(out$nodes$label, c("a.1", "a.2", "peace", "war"))
  expect_equal(as_edgelist(out)$weight, c(1, 2))
  expect_error(collect_text(ud, token = "stem"), "not a column")
  expect_error(collect_text(ud[c("token", "upos")]), "document")
  expect_error(collect_text(ud[, -2], unit = "sentence"), "sentence_id")
  expect_error(collect_text(list("a")), "character vector or a data frame")
})

test_that("collect_text() collects mentions and gossip among speakers", {
  ment <- collect_text(fixture_play, terms = fixture_cast, from = "speaker")
  expect_false(is_twomode(ment))
  expect_true(is_directed(ment))
  el <- as_edgelist(ment)
  expect_equal(el$from, c("Ann", "Ann", "Bob", "Bob"))
  expect_equal(el$to, c("Cy", "Dee", "Cy", "Dee"))
  expect_equal(el$weight, c(2, 1, 1, 1))
  goss <- collect_text(fixture_play, terms = fixture_cast,
                       from = "speaker", to = "addressee")
  expect_true(is_gossip(goss))
  lab <- goss$nodes$label
  expect_equal(lab[goss$ties$from], c("Ann", "Ann", "Ann", "Bob", "Bob"))
  expect_equal(lab[goss$ties$to], c("Bob", "Cy", "Cy", "Ann", "Ann"))
  expect_equal(lab[goss$ties$about], c("Cy", "Cy", "Dee", "Cy", "Dee"))
  # sentences are stamped with their position
  lines <- collect_text(fixture_play, terms = fixture_cast, from = "speaker",
                        unit = "sentence")
  expect_equal(lines$ties$time, c(1, 2, 2, 3, 3))
  # speakers can be given as a vector as long as the text
  vec <- collect_text(fixture_play$text, terms = fixture_cast,
                      from = fixture_play$speaker)
  expect_equal(as_edgelist(vec)$to, el$to)
  expect_error(collect_text(fixture_play, to = "addressee"), "needs")
  expect_error(collect_text(fixture_play, from = c("Ann", "Bob")), "4 values")
})

test_that("collect_text() scores sentiment from a lexicon", {
  bing <- data.frame(word = c("lovely", "awful"),
                     sentiment = c("positive", "negative"))
  afinn <- data.frame(word = c("lovely", "awful"), value = c(3, -1))
  out <- collect_text(fixture_play, lexicon = afinn)
  expect_equal(out$nodes$sentiment[1:4], c(0, 0, 2, 0))
  out <- collect_text(fixture_play, lexicon = bing)
  expect_equal(out$nodes$sentiment[1:4], c(0, 0, 0, 0))
  goss <- collect_text(fixture_play, terms = fixture_cast, from = "speaker",
                       to = "addressee", lexicon = c(lovely = 1, awful = -2))
  expect_true(is_signed(goss))
  expect_equal(goss$ties$weight, c(1, -1, -1, 1, 1))
  tok <- collect_text(fixture_play, terms = fixture_cast, from = "speaker",
                      ties = "tokens", lexicon = c(awful = -2))
  expect_equal(tok$ties$weight, c(1, -1, -1, 1, 1))
  expect_error(collect_text(fixture_play, lexicon = data.frame(word = "a")),
               "value")
  expect_error(collect_text(fixture_play, lexicon = 1), "named numeric")
})

# collect_bibliography() ####

fixture_records <- data.frame(
  SR = c("SMITH J, 2001, SCIENTOMETRICS", "JONES K, 2003, SCIENTOMETRICS",
         "LI X, 2005, J INFORMETR"),
  AU = c("SMITH J;JONES K", "JONES K", "LI X;SMITH J;JONES K"),
  TI = c("Citations", "Coupling", "Co-words"),
  PY = c(2001, 2003, 2005),
  SO = c("SCIENTOMETRICS", "SCIENTOMETRICS", "J INFORMETR"),
  DE = c("CITATION; NETWORK", "COUPLING; NETWORK", "NETWORK"),
  ID = c("SCIENCE", NA, "SCIENCE; BIBLIOMETRICS"),
  DI = c("10.1000/R1", NA, NA),
  CR = c(paste("PRICE DJD, 1965, SCIENCE, V149, P510,",
               "DOI 10.1126/SCIENCE.149.3683.510"),
         # the last is another paper of the same key as the record citing it
         paste("SMITH J, 2001, SCIENTOMETRICS, V50, P1;",
               "PRICE DJD, 1965, SCIENCE;",
               "JONES K, 2003, SCIENTOMETRICS, V60, P5"),
         # the first matches a record by DOI, and the last holds a semicolon
         paste("ANON, 2001, SOMEWHERE, DOI 10.1000/R1;",
               "JONES K, 2003, SCIENTOMETRICS;",
               "PRICE DJD, 1965, SCIENCE, V149, P510;",
               "GARFIELD E, 1955, SCIENCE, V122, P108, DOI",
               "10.1002/(SICI)1097-4571(199401)45:1<12::AID-ASI2>3.0.CO;2-9")),
  stringsAsFactors = FALSE)

bib_weight <- function(x, a, b) as_matrix(x)[a, b]

test_that("collect_bibliography() collects a multimodal network of records", {
  out <- collect_bibliography(fixture_records)
  expect_s3_class(out, "stocnet")
  expect_equal(out$info$modes, c("works", "authors", "keywords", "sources"))
  expect_equal(out$info$layers, c("cites", "authored", "tagged", "published"))
  expect_equal(unname(out$info$directed), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(unname(out$info$sender), c("works", "authors", "works", "works"))
  expect_equal(unname(out$info$receiver),
               c("works", "works", "keywords", "sources"))
  expect_equal(out$info$update, "increment")
  expect_true(is_dynamic(out))
  works <- out$nodes[out$nodes$mode == "works", ]
  expect_equal(works$recorded, rep(c(TRUE, FALSE), each = 3))
  expect_equal(works$year, c(2001, 2003, 2005, 1965, 2003, 1955))
  expect_equal(works$title[1:3], fixture_records$TI)
  # keywords are lowercased and one for each spelling
  expect_setequal(out$nodes$label[out$nodes$mode == "keywords"],
                  c("citation", "network", "coupling"))
  # every tie has the year of its work, and a citation that of the citing work
  cites <- out$ties[out$ties$layer == "cites", ]
  expect_equal(cites$time, fixture_records$PY[cites$from])
  expect_equal(sum(out$ties$layer == "authored"), 6)
  expect_equal(sum(out$ties$layer == "published"), 3)
  # to_time() gives the literature up to a year
  expect_equal(sum(to_time(out, 2003)$ties$layer == "cites"), 4)
})

test_that("collect_bibliography() matches cited references to works", {
  out <- collect_bibliography(fixture_records)
  el <- as_edgelist(to_uniplex(out, "cites"))
  expect_equal(paste(el$from, el$to, sep = " > "),
               c("SMITH J, 2001, SCIENTOMETRICS > PRICE DJD, 1965, SCIENCE",
                 "JONES K, 2003, SCIENTOMETRICS > SMITH J, 2001, SCIENTOMETRICS",
                 "JONES K, 2003, SCIENTOMETRICS > PRICE DJD, 1965, SCIENCE",
                 paste("JONES K, 2003, SCIENTOMETRICS >",
                       "JONES K, 2003, SCIENTOMETRICS 1"),
                 "LI X, 2005, J INFORMETR > SMITH J, 2001, SCIENTOMETRICS",
                 "LI X, 2005, J INFORMETR > JONES K, 2003, SCIENTOMETRICS",
                 "LI X, 2005, J INFORMETR > PRICE DJD, 1965, SCIENCE",
                 "LI X, 2005, J INFORMETR > GARFIELD E, 1955, SCIENCE"))
  works <- out$nodes[out$nodes$mode == "works", ]
  # references without a DOI join the one DOI of their key
  expect_equal(works$doi[4], "10.1126/science.149.3683.510")
  # a DOI that holds a semicolon is kept whole
  expect_equal(works$doi[6],
               "10.1002/(sici)1097-4571(199401)45:1<12::aid-asi2>3.0.co;2-9")
  expect_equal(works$reference[5], "JONES K, 2003, SCIENTOMETRICS, V60, P5")
  # works of the same author, year, and source are one, whatever their DOIs
  two <- data.frame(SR = "LI X, 2005, J INFORMETR", AU = "LI X", PY = 2005,
                    CR = paste("PRICE DJD, 1965, SCIENCE, V149, P510, DOI 10.1126/A;",
                               "PRICE DJD, 1965, SCIENCE, V150, P1, DOI 10.1126/B"))
  out <- collect_bibliography(two)
  expect_equal(sum(out$nodes$mode == "works"), 2)
  expect_equal(out$nodes$doi[2], "10.1126/a")
})

test_that("collect_bibliography() records the position of each author", {
  out <- collect_bibliography(fixture_records)
  authored <- out$ties[out$ties$layer == "authored", ]
  expect_equal(authored$position, c(1, 2, 1, 1, 2, 3))
  expect_true(all(is.na(out$ties$position[out$ties$layer != "authored"])))
})

test_that("collect_bibliography() gives the classic networks by recipe", {
  out <- collect_bibliography(fixture_records)
  cites <- to_uniplex(out, "cites")
  cocit <- to_proximity(cites, across = "columns")
  expect_equal(bib_weight(cocit, "SMITH J, 2001, SCIENTOMETRICS",
                          "PRICE DJD, 1965, SCIENCE"), 2)
  expect_equal(bib_weight(cocit, "SMITH J, 2001, SCIENTOMETRICS",
                          "JONES K, 2003, SCIENTOMETRICS"), 1)
  expect_equal(bib_weight(cocit, "JONES K, 2003, SCIENTOMETRICS",
                          "JONES K, 2003, SCIENTOMETRICS 1"), 0)
  coup <- to_proximity(cites, across = "rows")
  expect_equal(bib_weight(coup, "JONES K, 2003, SCIENTOMETRICS",
                          "LI X, 2005, J INFORMETR"), 2)
  expect_equal(bib_weight(coup, "SMITH J, 2001, SCIENTOMETRICS",
                          "LI X, 2005, J INFORMETR"), 1)
  coauth <- to_mode(out, "authors", through = "works",
                    similarity = "fractional")
  expect_equal(bib_weight(coauth, "SMITH J", "JONES K"), 1.5)
  expect_equal(bib_weight(coauth, "LI X", "SMITH J"), 0.5)
  coword <- to_mode(out, "keywords", through = "works")
  expect_equal(bib_weight(coword, "network", "citation"), 1)
  expect_equal(bib_weight(coword, "citation", "coupling"), 0)
  topics <- to_twomode(out, through = c("works", "sources"))
  expect_equal(topics$info$modes, c("authors", "keywords"))
  expect_equal(bib_weight(topics, "JONES K", "network"), 3)
})

test_that("collect_bibliography() collects the keywords asked for", {
  indexed <- collect_bibliography(fixture_records, keywords = "indexed")
  expect_setequal(indexed$nodes$label[indexed$nodes$mode == "keywords"],
                  c("science", "bibliometrics"))
  both <- collect_bibliography(fixture_records, keywords = "both")
  expect_equal(sum(both$nodes$mode == "keywords"), 5)
  # a mode without nodes is left out
  none <- collect_bibliography(fixture_records[c("AU", "PY", "CR")])
  expect_equal(none$info$modes, c("works", "authors"))
  expect_equal(none$info$layers, c("cites", "authored"))
})

test_that("collect_bibliography() reads OpenAlex records", {
  oa <- data.frame(id = c("https://openalex.org/W1", "https://openalex.org/W2"),
                   display_name = c("First", "Second"),
                   publication_year = c(2010L, 2012L),
                   doi = c("https://doi.org/10.1000/A", NA),
                   source_display_name = c("J One", "J Two"))
  oa$authorships <- list(data.frame(display_name = c("Ann Lee", "Bo Chen")),
                         data.frame(display_name = "Ann Lee"))
  oa$referenced_works <- list("https://openalex.org/W9",
                              c("https://openalex.org/W1",
                                "https://openalex.org/W9"))
  oa$keywords <- list(data.frame(display_name = c("Networks", "Text")),
                      data.frame(display_name = "networks"))
  oa$concepts <- list(data.frame(display_name = "Sociology"), NULL)
  oa$cited_by_count <- c(5L, 0L)
  out <- collect_bibliography(oa)
  works <- out$nodes[out$nodes$mode == "works", ]
  expect_equal(works$openalex, c("W1", "W2", "W9"))
  # the global citation counts, which cited works that are not records lack
  expect_equal(works$citations, c(5L, 0L, NA))
  # where no record gives a count, there is no column of counts
  expect_false("citations" %in% names(collect_bibliography(fixture_records)$nodes))
  expect_equal(works$recorded, c(TRUE, TRUE, FALSE))
  expect_equal(works$doi[1], "10.1000/a")
  el <- as_edgelist(to_uniplex(out, "cites"))
  expect_equal(el$to, c("W9", "Ann Lee, 2010, J One", "W9"))
  expect_equal(sum(out$nodes$mode == "keywords"), 2)
  indexed <- collect_bibliography(oa, keywords = "indexed")
  expect_equal(indexed$nodes$label[indexed$nodes$mode == "keywords"],
               "sociology")
})

test_that("collect_bibliography() reads BibTeX files", {
  file <- tempfile(fileext = ".bib")
  on.exit(unlink(file))
  writeLines(c(
    "@comment{Exported from the Web of Science}",
    "@article{lee2010,",
    "  author = {Lee, Ann and Chen, Bo},",
    "  title = {First {N}etworks},",
    "  journal = \"J One\",",
    "  year = 2010,",
    "  doi = {10.1000/A},",
    "  keywords = {networks; text},",
    "  keywords-plus = {SOCIOLOGY},",
    "  cited-references = {PRICE DJD, 1965, SCIENCE, V149, P510",
    "   SMALL H, 1973, J AM SOC INFORM SCI, V24, P265}",
    "}",
    "@incollection{lee2012, author = {Lee, Ann}, title = {Second},",
    "  booktitle = {Book Two}, year = {2012},",
    "  cited-references = {LEE A, 2010, J ONE",
    "   PRICE DJD, 1965, SCIENCE}}"), file)
  out <- collect_bibliography(file)
  works <- out$nodes[out$nodes$mode == "works", ]
  expect_equal(works$label[1:2], c("lee2010", "lee2012"))
  expect_equal(works$title[1], "First Networks")
  expect_equal(works$recorded, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(out$nodes$label[out$nodes$mode == "sources"],
               c("J One", "Book Two"))
  el <- as_edgelist(to_uniplex(out, "cites"))
  # the second record cites the first by its author, year, and source
  expect_equal(el$to, c("PRICE DJD, 1965, SCIENCE",
                        "SMALL H, 1973, J AM SOC INFORM SCI",
                        "lee2010", "PRICE DJD, 1965, SCIENCE"))
  expect_equal(collect_bibliography(file, keywords = "indexed")$nodes$label[7],
               "sociology")
})

test_that("collect_bibliography() reads BibLaTeX files from Zotero", {
  file <- tempfile(fileext = ".bib")
  on.exit(unlink(file))
  writeLines(c(
    "@article{kellyDebunking2026,",
    "\ttitle = {Debunking {When} {Prophecy} {Fails}},",
    "\tabstract = {In 1954, a group,",
    "            predicted a flood.},",
    "\tjournaltitle = {Journal of the History of the Behavioral Sciences},",
    "\tauthor = {Kelly, Thomas},",
    "\tdate = {2026-01},",
    "}",
    "@book{heupelRule2016,",
    "\ttitle = {The {Rule} of {Law} in {Global} {Governance}},",
    "\tpublisher = {Palgrave Macmillan UK},",
    "\teditor = {Heupel, Monika and Reinold, Theresa},",
    "\tyear = {2016},",
    "}"), file)
  out <- collect_bibliography(file)
  expect_equal(out$info$layers, c("authored", "published"))
  expect_equal(out$nodes$year[1:2], c(2026, 2016))
  expect_equal(out$nodes$label[out$nodes$mode == "authors"],
               c("Kelly, Thomas", "Heupel, Monika", "Reinold, Theresa"))
  expect_equal(out$nodes$label[out$nodes$mode == "sources"],
               c("Journal of the History of the Behavioral Sciences",
                 "Palgrave Macmillan UK"))
})

test_that("collect_bibliography() errors informatively", {
  expect_error(collect_bibliography(1), "data frame or the path")
  expect_error(collect_bibliography(tempfile(fileext = ".bib")),
               "does not exist")
  expect_error(collect_bibliography(data.frame(x = 1)),
               "not a data frame of bibliographic records")
  expect_error(collect_bibliography(fixture_records[0, ]), "No records")
})

test_that("collect_bibliography() counts as bibliometrix does", {
  skip_if_not_installed("bibliometrix")
  skip_if_not_installed("bibliometrixData")
  records <- get(utils::data("scientometrics", package = "bibliometrixData",
                             envir = environment()))
  out <- collect_bibliography(records)
  cites <- to_uniplex(out, "cites")
  cocit <- as_matrix(to_proximity(cites, across = "columns"))
  coup <- as_matrix(to_proximity(cites, across = "rows"))
  theirs_cocit <- as.matrix(suppressWarnings(bibliometrix::biblioNetwork(
    records, "co-citation", "references", shortlabel = FALSE)))
  theirs_coup <- as.matrix(suppressWarnings(bibliometrix::biblioNetwork(
    records, "coupling", "references", shortlabel = FALSE)))
  # bibliometrix names a cited work by its reference up to its volume or DOI,
  # where manynet keeps works of different DOIs apart, so the counts are
  # compared for the references that both name in the same way
  recs <- .bib_records(records, "author")
  refs <- .bib_match(recs)$references
  string <- gsub("[,;]", " ", gsub("\\).*", ")", unlist(recs$refs)))
  cut <- regexpr("V[0-9]|DOI ", string)
  string[cut > 0] <- substr(string[cut > 0], 1, cut[cut > 0] - 1)
  string <- gsub("\\s+", " ", string)
  map <- unique(data.frame(string = string, work = refs$cited))
  same <- !map$string %in% map$string[duplicated(map$string)] &
    !map$work %in% map$work[duplicated(map$work)] &
    map$string %in% colnames(theirs_cocit)
  map <- map[same, ]
  expect_gt(nrow(map), 4000)
  labels <- out$nodes$label[map$work]
  ours <- cocit[labels, labels]
  theirs <- theirs_cocit[map$string, map$string]
  diag(ours) <- diag(theirs) <- 0
  expect_equal(unname(ours), unname(theirs))
  # coupling, for the records whose references are all named in the same way
  whole <- which(tapply(refs$cited %in% map$work, refs$citing,
                        all))
  expect_gt(length(whole), 20)
  ours <- coup[out$nodes$label[whole], out$nodes$label[whole]]
  theirs <- theirs_coup[whole, whole]
  diag(ours) <- diag(theirs) <- 0
  expect_equal(unname(ours), unname(theirs))
  # co-authorship and co-word counts are the same throughout
  theirs <- as.matrix(bibliometrix::biblioNetwork(records, "collaboration",
                                                   "authors"))
  ours <- as_matrix(to_mode(out, "authors", through = "works"))
  diag(theirs) <- 0
  expect_equal(unname(ours[rownames(theirs), colnames(theirs)]),
               unname(theirs))
  theirs <- as.matrix(bibliometrix::biblioNetwork(records, "co-occurrences",
                                                   "author_keywords"))
  ours <- as_matrix(to_mode(out, "keywords", through = "works"))
  diag(theirs) <- 0
  keywords <- tolower(rownames(theirs))
  expect_equal(unname(ours[keywords, keywords]), unname(theirs))
})

# collect_statements() ####

# Returns a function that gives the answers one at a time, then NA.
feed_answers <- function(x) {
  i <- 0
  function() {
    i <<- i + 1
    if (i > length(x)) NA_character_ else x[i]
  }
}

speeches <- data.frame(doc_id = c("a", "b"),
                       date = as.Date(c("2011-03-14", "2011-03-15")),
                       text = c("Merkel wants a moratorium. Greens agree.",
                                "Industry rejects any exit."))

# Answers are given as: concept, speaker, sign; "" at the concept moves on.
code_speeches <- function(answers, unit = "sentence", start = 1,
                          roster = NULL, .data = NULL) {
  units <- .statement_units(speeches, unit)
  coded <- .statement_coded(.data)
  roster <- .statement_roster(roster, coded)
  suppressMessages(.statement_code(units, seq(start, nrow(units)), roster,
                                   unit, feed_answers(answers)))
}

test_that("collect_statements() splits the text into dated units", {
  units <- .statement_units(speeches, "sentence")
  expect_equal(units$document, c("a", "a", "b"))
  expect_equal(units$sentence, c(1, 2, 1))
  expect_equal(units$time, as.Date(c("2011-03-14", "2011-03-14",
                                     "2011-03-15")))
  units <- .statement_units(speeches, "document")
  expect_equal(units$document, c("a", "b"))
  expect_null(units$sentence)
  expect_equal(nrow(.statement_units(c(x = "One. Two.\n\nThree."),
                                     "paragraph")), 2)
  expect_null(.statement_units(c(x = "One. Two."), "sentence")$time)
  expect_error(.statement_units("", "sentence"), "No text")
  expect_equal(.statement_unit("sentences"), "sentence")
  expect_error(.statement_unit(5), "windows")
})

test_that("collect_statements() reads the signs", {
  expect_equal(vapply(c("+", "agree", "Yes", "1", "-", "disagree", "n", "-1"),
                      .statement_sign, integer(1), USE.NAMES = FALSE),
               c(1L, 1L, 1L, 1L, -1L, -1L, -1L, -1L))
  expect_true(is.na(.statement_sign("maybe")))
})

test_that("collect_statements() offers a roster of each mode", {
  coded <- .statement_coded(NULL)
  expect_equal(.statement_roster("exit", coded)$concepts, "exit")
  expect_equal(.statement_roster(list(concepts = "exit", speakers = "SPD"),
                                 coded)$speakers, "SPD")
  expect_equal(.statement_roster(list("SPD", "exit"), coded)$concepts, "exit")
  expect_true("Angela Merkel" %in%
                .statement_roster(NULL, .statement_coded(irps_nuclear))$speakers)
  expect_error(.statement_roster(list("SPD"), coded), "two character")
  # the terms of collect_text() are offered as concepts
  terms <- suppressMessages(collect_text(speeches,
                                         terms = c("moratorium", "exit")))
  roster <- .statement_roster(terms, coded)
  expect_equal(roster$concepts, c("moratorium", "exit"))
  expect_length(roster$speakers, 0)
})

test_that("collect_statements() picks names from the roster", {
  roster <- c("Angela Merkel", "SPD", "Greens")
  expect_equal(.statement_pick("2", roster)$label, "SPD")
  expect_equal(.statement_pick("spd", roster)$label, "SPD")
  expect_equal(.statement_pick("RWE", roster)$label, "RWE")
  expect_equal(.statement_pick("merk", roster)$matches, 1)
  expect_equal(.statement_pick("merk", roster, again = TRUE)$label, "merk")
  expect_equal(.statement_pick("9", roster)$label, "9")
})

test_that("collect_statements() codes statements from the answers", {
  found <- code_speeches(c("nuclear exit", "Merkel", "+",
                           "1", "Greens", "maybe", "yes", "",
                           "", "nuc", "1", "Industry", "-", "q"))
  expect_true(found$stopped)
  st <- found$statements
  expect_equal(st$speaker, c("Merkel", "Greens", "Industry"))
  expect_equal(st$concept, rep("nuclear exit", 3))
  expect_equal(st$weight, c(1L, 1L, -1L))
  expect_equal(st$document, c("a", "a", "b"))
  expect_equal(st$sentence, c(1, 1, 1))
  # "q" stops at any prompt, and the statement is not kept
  found <- code_speeches(c("exit", "Greens", "q"))
  expect_true(found$stopped)
  expect_null(found$statements)
})

test_that("collect_statements() returns a network in the irps_nuclear form", {
  found <- code_speeches(c("nuclear exit", "Merkel", "+", "", "",
                           "1", "Industry", "-", ""))
  net <- suppressMessages(.statement_network(found, .statement_coded(NULL)))
  expect_s3_class(net, "stocnet")
  expect_true(is_twomode(net))
  expect_true(is_signed(net))
  expect_false(is_directed(net))
  expect_equal(net$info$modes, irps_nuclear$info$modes)
  expect_equal(net$info$update, "increment")
  expect_equal(node_labels(net), c("Merkel", "Industry", "nuclear exit"))
  expect_equal(net$ties$weight, c(1L, -1L))
  expect_s3_class(net$ties$time, "Date")
  expect_equal(net$ties$document, c("a", "b"))
  expect_error(.statement_network(list(statements = NULL),
                                  .statement_coded(NULL)), "No statements")
})

test_that("collect_statements() codes documents for their concepts", {
  found <- code_speeches(c("moratorium", "", "", "exit", "", "", "",
                           "exit", "", "", ""), unit = "document")
  expect_false(found$stopped)
  expect_true(all(is.na(found$statements$speaker)))
  net <- suppressMessages(.statement_network(found, .statement_coded(NULL)))
  expect_equal(net$info$modes, c("documents", "concepts"))
  expect_equal(node_labels(net), c("a", "b", "moratorium", "exit"))
  expect_false("weight" %in% names(net$ties))
  expect_false(is_signed(net))
  expect_equal(net$info$name, "Network of coded texts")
  cooc <- to_mode(net, "concepts")
  expect_equal(net_ties(cooc), 1)
  # documents and speakers both make statements: three modes
  found <- code_speeches(c("exit", "", "", "exit", "Merkel", "+", ""),
                         unit = "document")
  expect_warning(net <- suppressMessages(
    .statement_network(found, .statement_coded(NULL))), "no sign")
  expect_equal(net$info$modes, c("documents", "speakers", "concepts"))
  expect_equal(net$ties$weight, c(NA, 1L))
})

test_that("collect_statements() continues a network coded in part", {
  first <- code_speeches(c("nuclear exit", "Merkel", "+", "q"))
  net <- suppressMessages(.statement_network(first, .statement_coded(NULL)))
  units <- .statement_units(speeches, "sentence")
  coded <- .statement_coded(net)
  expect_equal(.statement_start(units, coded, "sentence"), 2)
  more <- code_speeches(c("", "nuclear", "1", "Industry", "-", ""),
                        start = 2, .data = net)
  net <- suppressMessages(.statement_network(more, coded))
  expect_equal(net_ties(net), 2)
  expect_equal(node_labels(net), c("Merkel", "Industry", "nuclear exit"))
  expect_equal(.statement_start(units, .statement_coded(net),
                                "sentence"), 4)
  # a network of documents continues by document
  docs <- suppressMessages(.statement_network(
    code_speeches(c("exit", "", "", "q"), unit = "document"),
    .statement_coded(NULL)))
  coded <- .statement_coded(docs)
  expect_true(all(is.na(coded$statements$speaker)))
  expect_equal(.statement_start(.statement_units(speeches, "document"),
                                coded, "document"), 2)
  # irps_nuclear keeps its nodes and their attributes
  coded <- .statement_coded(irps_nuclear)
  expect_equal(.statement_start(units, coded, "sentence"), 1)
  more <- code_speeches(c("atomausstieg", "Angela Merkel", "+", "q"),
                        .data = irps_nuclear)
  net <- suppressMessages(.statement_network(more, coded))
  expect_equal(net_ties(net), net_ties(irps_nuclear) + 1)
  expect_equal(net_nodes(net), net_nodes(irps_nuclear) + 1)
  expect_equal(net$info$modes, irps_nuclear$info$modes)
  expect_equal(net$nodes$party[1:5], irps_nuclear$nodes$party[1:5])
  expect_equal(tail(net$ties$weight, 1), 1L)
  expect_error(.statement_coded(ison_adolescents), "two-mode")
})

test_that("collect_statements() shows unknown signs again", {
  found <- code_speeches(c("exit", "Merkel", "?", "", "",
                           "exit", "Industry", "-", ""))
  expect_equal(found$statements$unknown, c(TRUE, FALSE))
  net <- suppressMessages(.statement_network(found, .statement_coded(NULL)))
  expect_true(is_signed(net))
  expect_equal(net$ties$weight, c(NA, -1L))
  expect_false("unknown" %in% names(net$ties))
  coded <- .statement_coded(net)
  expect_equal(coded$statements$unknown, c(TRUE, FALSE))
  units <- .statement_units(speeches, "sentence")
  # [Enter] leaves the sign unknown, as "?" does
  again <- suppressMessages(.statement_revisit(units, coded, "sentence",
                                               feed_answers("")))
  expect_false(again$stopped)
  expect_equal(again$coded$statements$unknown, c(TRUE, FALSE))
  again <- suppressMessages(.statement_revisit(units, coded, "sentence",
                                               feed_answers("+")))
  expect_equal(again$coded$statements$weight, c(1L, -1L))
  net <- suppressMessages(.statement_network(list(statements = NULL),
                                             again$coded))
  expect_equal(net$ties$weight, c(1L, -1L))
  expect_equal(.statement_revisit(units, again$coded, "sentence",
                                  feed_answers(character(0)))$coded,
               again$coded)
  # "q" stops the second pass
  expect_true(suppressMessages(.statement_revisit(units, coded, "sentence",
                                                  feed_answers("q")))$stopped)
})

code_session <- function(answers, unit = "sentence", .data = NULL) {
  units <- .statement_units(speeches, unit)
  coded <- .statement_coded(.data)
  roster <- .statement_roster(NULL, coded)
  suppressMessages(.statement_session(units, coded, roster, unit,
                                      feed_answers(answers)))
}

test_that("collect_statements() comes back to units skipped with '?'", {
  session <- code_session(c("moratorium", "Merkel", "+", "?",
                            "?",
                            "exit", "Industry", "-", "",
                            # the skipped units are shown again
                            "", "?"))
  expect_false(session$stopped)
  expect_equal(session$found$statements$concept, c("moratorium", "exit"))
  expect_equal(session$uncoded, data.frame(document = "a", sentence = 2))
  net <- suppressMessages(.statement_network(session$found, session$coded,
                                             session$uncoded))
  expect_equal(net$info$uncoded$sentence, 2)
  printed <- utils::capture.output(invisible(utils::capture.output(print(net))),
                                   type = "message")
  expect_true(any(grepl("1 unit of text still to code", printed)))
  # the next session starts with the unit still to code, and then clears it
  coded <- .statement_coded(net)
  expect_equal(.statement_start(.statement_units(speeches, "sentence"),
                                coded, "sentence"), 4)
  session <- code_session(c("moratorium", "Greens", "+", ""), .data = net)
  expect_equal(nrow(session$uncoded), 0)
  net <- suppressMessages(.statement_network(session$found, session$coded,
                                             session$uncoded))
  expect_null(net$info$uncoded)
  expect_equal(net_ties(net), 3)
  expect_equal(node_labels(net),
               c("Merkel", "Industry", "Greens", "moratorium", "exit"))
})

test_that("collect_statements() keeps the units skipped where coding stops", {
  # "?" at the speaker skips the unit too
  session <- code_session(c("exit", "?", "q"))
  expect_true(session$stopped)
  expect_null(session$found$statements)
  expect_equal(session$uncoded, data.frame(document = "a", sentence = 1))
  # units still to code but not reached stay to code
  net <- suppressMessages(.statement_network(
    code_session(c("", "", "exit", "Industry", "-", ""))$found,
    .statement_coded(NULL),
    data.frame(document = c("a", "a"), sentence = c(1, 2))))
  session <- code_session(c("exit", "Merkel", "+", "q"), .data = net)
  expect_equal(session$uncoded, data.frame(document = "a", sentence = 1:2))
  session <- code_session(c("exit", "Merkel", "+", "", "q"), .data = net)
  expect_equal(session$uncoded, data.frame(document = "a", sentence = 2L))
  expect_equal(session$found$statements$speaker, "Merkel")
  # units still to code that are not in the text are dropped
  net$info$uncoded <- dplyr::tibble(document = "z", sentence = 1)
  expect_warning(code_session("", .data = net), "not in")
  expect_error(make_stocnet(info = list(uncoded = "a"),
                            nodes = data.frame(label = "a")), "uncoded")
})

test_that("statements project onto coalitions in moving windows", {
  dates <- seq(as.Date("2011-03-15"), as.Date("2011-06-15"), by = "month")
  windows <- to_times(irps_nuclear, dates, window = 30)
  coalitions <- lapply(windows, to_mode, "speakers", similarity = "agreement")
  expect_length(coalitions, length(dates))
  expect_true(all(vapply(coalitions, is_weighted, logical(1))))
  expect_false(any(vapply(coalitions, is_twomode, logical(1))))
})

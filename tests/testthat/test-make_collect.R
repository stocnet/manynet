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
  op <- options(snet_verbosity = "verbose")
  on.exit(options(op), add = TRUE)
  expect_message(out <- collect_pkg(dir), "b.R")
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

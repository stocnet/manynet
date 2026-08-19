# Test missing

missTest <- ison_adolescents %>%
  add_tie_attribute("weight", c(1,NA,NA,1,1,1,NA,NA,1,1)) %>%
  as_matrix

missTest2 <- ison_adolescents %>%
  mutate_ties(weight = c(1:8, NA, NA))

test_that("missing values are imputed correctly",{
  expect_false(anyNA(impute_ties(missTest)))
  expect_false(anyNA(impute_ties(missTest, "mean")))
  expect_false(anyNA(impute_ties(missTest2)))
  expect_false(anyNA(impute_ties(missTest2, "mean")))
  expect_s3_class(impute_ties(missTest2), "tbl_graph")
  expect_s3_class(impute_ties(missTest2, "mean"), "tbl_graph")
})

test_that("every rule leaves nothing the network records as unobserved", {
  for(rule in c("zero", "density", "reciprocity", "indegree")){
    set.seed(1)
    expect_equal(net_tie_missing(impute_ties(ison_classmates, rule)), 0,
                 label = paste0("net_tie_missing() after the ", rule, " rule"))
  }
  expect_equal(net_tie_incomplete(impute_ties(missTest2, "mean")), 0)
})

# Which state is imputed ####

test_that("the two parts of a tie's missingness are imputed separately", {
  # a weight of NA marks a tie that is there and whose value is not known, so
  # asking for the missing ties only must leave it alone
  untouched <- impute_ties(missTest2, "zero", which = "nonresponse")
  expect_true(anyNA(tie_weights(untouched)))
  expect_equal(igraph::ecount(untouched), igraph::ecount(missTest2))
  # and asking for the incomplete ties only must leave the missing ties alone
  kept <- impute_ties(ison_classmates, "mean", which = "incomplete")
  expect_equal(net_tie_missing(kept), net_tie_missing(ison_classmates))
})

test_that("a rule that suits none of the selected states is refused", {
  expect_error(impute_ties(ison_classmates, "mean", which = "nonresponse"),
               "does not apply")
  expect_error(impute_ties(missTest2, "density", which = "incomplete"),
               "does not apply")
})

test_that("a rule that suits some of the selected states imputes those", {
  # "density" says whether a tie exists, so with the default `which` it treats
  # the missing ties and reports that it has left the incomplete ones alone
  set.seed(1)
  expect_equal(net_tie_missing(impute_ties(ison_classmates, "density")), 0)
})

test_that("reciprocity needs a directed network", {
  expect_error(impute_ties(to_undirected(ison_classmates), "reciprocity"),
               "directed")
})

test_that("reciprocity reduces to density where the two proportions agree", {
  # where a tie is as likely to be returned as it is to be sent unprompted,
  # conditioning on what the other node reported tells you nothing that the
  # density does not, so the two rules must draw at the same probability
  pres <- dplyr::tibble(from = c(1L, 2L, 3L, 4L), to = c(2L, 1L, 4L, 3L))
  miss <- dplyr::tibble(from = c(1L, 3L), to = c(3L, 1L))
  probs <- manynet:::.probs_reciprocity(pres, miss, dyads = 20, density = 0.2)
  expect_length(probs, 2)
  expect_true(all(probs >= 0 & probs <= 1))
  # with no observed tie either way between 1 and 3, both draws fall back to
  # the network's own density
  expect_equal(probs, c(0.2, 0.2))
})

test_that("dyads involving a node that was not in the network are passed over", {
  x <- ison_classmates
  # the nodes that never enter the network hold no ties that could have been
  # observed, so imputing must neither add ties for them nor count them
  expect_equal(net_tie_missing(impute_ties(x, "zero")), 0)
  expect_equal(nrow(impute_ties(x, "zero")$ties), nrow(x$ties))
})

# Nodes ####

incomplete_lotr <- fict_lotr |>
  mutate_nodes(Age = c(NA, seq_len(net_nodes(fict_lotr) - 1L) + 100L))

test_that("impute_nodes fills every attribute that holds a missing value", {
  expect_gt(net_node_incomplete(incomplete_lotr), 0)
  for(rule in c("modal", "mean", "median", "neighbourhood")){
    out <- impute_nodes(incomplete_lotr, rule)
    expect_equal(net_node_incomplete(out), 0,
                 label = paste0("net_node_incomplete() after the ", rule, " rule"))
  }
})

test_that("impute_nodes takes the modal value for a categorical attribute", {
  x <- incomplete_lotr |>
    mutate_nodes(Race = replace(node_attribute(incomplete_lotr, "Race"), 2, NA))
  out <- impute_nodes(x, "mean")
  expect_false(anyNA(node_attribute(out, "Race")))
  expect_true(node_attribute(out, "Race")[2] %in%
                node_attribute(x, "Race"))
})

test_that("impute_nodes keeps a factor a factor", {
  x <- incomplete_lotr |>
    mutate_nodes(Race = factor(replace(node_attribute(incomplete_lotr, "Race"),
                                       2, NA)))
  out <- impute_nodes(x, "modal", "Race")
  expect_s3_class(node_attribute(out, "Race"), "factor")
  expect_false(anyNA(node_attribute(out, "Race")))
})

test_that("impute_nodes names an attribute the network does not hold", {
  expect_error(impute_nodes(fict_lotr, "modal", "Nonesuch"), "no node attribute")
})

test_that("impute_nodes leaves a complete network alone", {
  expect_equal(as_matrix(impute_nodes(fict_lotr, "modal")), as_matrix(fict_lotr))
  expect_equal(node_attribute(impute_nodes(fict_lotr, "modal"), "Race"),
               node_attribute(fict_lotr, "Race"))
})

# to_imputed() ####

test_that("to_imputed matches the two calls made by hand", {
  # every rule is worked out from the network as observed, so neither call
  # shifts the ground under the other and the order they run in is immaterial
  set.seed(1)
  together <- to_imputed(incomplete_lotr, ties = "zero", nodes = "mean")
  set.seed(1)
  apart <- impute_nodes(impute_ties(incomplete_lotr, "zero"), "mean")
  set.seed(1)
  reversed <- impute_ties(impute_nodes(incomplete_lotr, "mean"), "zero")
  expect_equal(as_matrix(together), as_matrix(apart))
  expect_equal(node_attribute(together, "Age"), node_attribute(apart, "Age"))
  expect_equal(node_attribute(together, "Age"), node_attribute(reversed, "Age"))
})

test_that("to_imputed leaves nothing unobserved or incomplete", {
  set.seed(1)
  out <- to_imputed(ison_classmates)
  expect_equal(net_tie_missing(out), 0)
  # 'alcohol' is the one attribute no pupil was observed to hold, so there is
  # nothing to impute it from and it is left as it is
  expect_false(anyNA(node_attribute(out, "religion")))
  expect_false(anyNA(node_attribute(out, "ethnicity")))
  expect_true(all(is.na(node_attribute(out, "alcohol"))))
})

# Measures ####

test_that("the incompleteness measures count values and not ties", {
  expect_equal(net_tie_incomplete(missTest2), 0.2)
  expect_equal(sum(tie_incomplete(missTest2)), 2)
  expect_equal(net_tie_incomplete(ison_adolescents), 0)
  expect_length(node_incomplete(fict_lotr), net_nodes(fict_lotr))
  expect_equal(net_node_incomplete(fict_lotr), 0)
})

# Regression tests for the bugs the family carried ####

test_that("the zero rule sets missing weights to zero instead of deleting ties", {
  # it used to filter out NA-weighted ties, which contradicted both its
  # documentation and its own matrix and edgelist methods
  # counted over the ties the network holds, since `net_ties()` counts only
  # those it does not record as missing, and imputing them changes that
  expect_equal(igraph::ecount(impute_ties(missTest2)), igraph::ecount(missTest2))
  expect_equal(sum(tie_weights(impute_ties(missTest2)) == 0),
               sum(is.na(tie_weights(missTest2))))
})

test_that("imputing a stocnet's missing ties clears its records", {
  x <- ison_classmates
  zeroed <- impute_ties(x, "zero")
  # a stocnet holds the absence of a tie as the absence of a row, so treating
  # the missing ties as absent adds nothing and only clears the record
  expect_null(as_missinglist(zeroed))
  expect_equal(nrow(zeroed$ties), nrow(x$ties))
  expect_false(any(zeroed$changes$var == "na"))
  set.seed(1)
  drawn <- impute_ties(x, "density")
  expect_null(as_missinglist(drawn))
  # each of the 73 missing nominations is drawn at the density of the layer
  # and wave it was missing from, which is about 0.18, and not at the density
  # of a matrix summing four waves and two layers, which is about 0.9
  expect_lt(nrow(drawn$ties) - nrow(x$ties), 30)
  expect_gt(nrow(drawn$ties), nrow(x$ties))
})

test_that("the zero rule leaves an unweighted network alone", {
  # it used to raise "`..1` must be of size 10 or 1, not size 0" on any
  # network without a weight column, since there was nothing to test for NA
  expect_no_error(impute_ties(ison_adolescents))
  expect_equal(as_matrix(impute_ties(ison_adolescents)),
               as_matrix(ison_adolescents))
})

test_that("the mean rule works on the example in its own documentation", {
  # any(tie_weights(.data) > 1) is NA when a weight is NA, so the if() raised
  # "missing value where TRUE/FALSE needed" whenever every weight was <= 1
  expect_no_error(impute_ties(missTest, "mean"))
  expect_no_error(impute_ties(missTest2, "mean"))
})

test_that("binary networks are imputed rather than passed over", {
  # the tbl_graph branch iterated over seq_len() of the weight vector and
  # tested is.na() on the index, so it never imputed anything
  # a weight column holding only 0, 1, and NA records which ties are present
  # and not their values, so an NA there marks a missing tie
  binary <- ison_adolescents |>
    mutate_ties(weight = c(1, NA, 1, NA, 1, 1, 1, 1, 1, 1))
  set.seed(1234)
  out <- impute_ties(binary, "density")
  expect_false(anyNA(tie_weights(out)))
  expect_true(all(tie_weights(out) %in% c(0, 1)))
})

test_that("the drawn rules are reproducible given a seed", {
  binary <- ison_adolescents |>
    mutate_ties(weight = c(1, NA, 1, NA, 1, 1, 1, 1, 1, 1))
  set.seed(1234)
  first <- tie_weights(impute_ties(binary, "density"))
  set.seed(1234)
  expect_equal(tie_weights(impute_ties(binary, "density")), first)
})

test_that("the statistics exclude the diagonal", {
  # a node's tie to itself is not a tie that could have been observed, so
  # counting the n structural zeros biased the density down by (n-1)/n
  n <- nrow(missTest)
  offdiag <- missTest
  diag(offdiag) <- NA
  expect_equal(manynet:::.miss_average(missTest), mean(offdiag, na.rm = TRUE))
  expect_false(isTRUE(all.equal(manynet:::.miss_average(missTest),
                                mean(missTest, na.rm = TRUE))))
  expect_equal(manynet:::.miss_statistic(missTest, "median"),
               stats::median(offdiag, na.rm = TRUE))
})

test_that("imputation leaves a network without missing values alone", {
  expect_equal(impute_ties(as_matrix(ison_adolescents)),
               as_matrix(ison_adolescents))
  expect_equal(impute_ties(as_matrix(ison_adolescents), "mean"),
               as_matrix(ison_adolescents))
})

# The deprecated names ####

test_that("the old names still work and point at the new ones", {
  expect_warning(out <- na_to_zero(missTest), "deprecated")
  expect_false(anyNA(out))
  expect_warning(out <- na_to_mean(missTest2), "deprecated")
  expect_false(anyNA(tie_weights(out)))
})

# What is recorded ####

test_that("imputation records the method and the amount, per GRAND item 4.6", {
  set.seed(1)
  out <- impute_ties(ison_classmates, "reciprocity")
  expect_equal(as_infolist(out)$transformations$imputation,
               "reciprocity (73 missing ties)")
  # the element accumulates, so a network imputed in more than one step
  # reports each of them in the order they were applied
  both <- as_infolist(to_imputed(ison_classmates))$transformations$imputation
  expect_equal(both[[1]], "zero (73 missing ties)")
  expect_true(any(grepl("^modal \\([0-9]+ incomplete 'religion' values\\)$", both)))
  # one entry per attribute, so a reader can tell which hold manufactured
  # values and which were observed throughout
  expect_false(any(grepl("'sex'", both)))
})

test_that("imputation sits beside the other transformations, not among them", {
  # GRAND section 4 covers symmetrising (4.1) and imputing (4.6) alike, so each
  # takes its own name and a reader can ask after either one on its own
  set.seed(1)
  out <- as_infolist(impute_ties(to_undirected(ison_classmates),
                                         "density"))$transformations
  expect_match(out$symmetrisation,
               "^collapse \\([0-9]+% of connected dyads non-reciprocal\\)$")
  expect_match(out$imputation, "^density \\([0-9]+ missing ties\\)$")
  # the names appear in the order the transformations were applied
  expect_equal(names(out), c("symmetrisation", "imputation"))
})

test_that("the transformations survive a round trip through another class", {
  set.seed(1)
  out <- impute_ties(ison_classmates, "density")
  expect_equal(as_infolist(as_stocnet(as_tidygraph(out)))$transformations,
               as_infolist(out)$transformations)
})

test_that("the recorded count is of what was imputed, not of what was added", {
  w <- ison_adolescents |> mutate_ties(weight = c(1:9, NA))
  out <- impute_ties(w, "mean")
  expect_equal(as_infolist(out)$transformations$imputation,
               "mean (1 incomplete tie value)")
})

test_that("nothing is recorded where there was nothing to impute", {
  expect_length(as_infolist(impute_ties(ison_adolescents))$transformations, 0)
  expect_length(as_infolist(impute_nodes(fict_lotr, "modal"))$transformations, 0)
  expect_length(as_infolist(impute_ties(ison_classmates, "zero",
                                                which = "incomplete"))$transformations, 0)
})

test_that("a matrix is returned as a matrix, having nowhere to record", {
  out <- impute_ties(as_matrix(ison_classmates), "zero")
  expect_true(is.matrix(out))
  expect_false(anyNA(out))
})

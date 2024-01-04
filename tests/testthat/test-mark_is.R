test_that("is_ tests return correct values", {
  expect_true(is_twomode(ison_southern_women))
  expect_false(is_directed(ison_southern_women))
  expect_false(is_weighted(ison_southern_women))
  expect_false(is_weighted(ison_southern_women))
  expect_true(is_labelled(ison_southern_women))
  expect_false(is_complex(ison_southern_women))
  expect_true(is_graph(ison_southern_women))
  expect_true(is_manynet(ison_southern_women))
  expect_true(is_graph(ison_southern_women))
  expect_false(is_directed(as_network(ison_southern_women)))
  expect_true(is_connected(ison_southern_women))
  expect_false(is_perfect_matching(ison_southern_women))
  expect_false(is_eulerian(ison_southern_women))
  expect_false(is_acyclic(ison_southern_women))
  expect_false(is_aperiodic(ison_southern_women))
})

manyis <- ls(getNamespace("manynet"), pattern = "^is_")
manyis <- manyis[!grepl("\\.", manyis)] # don't test on each method
manyds <- data(package = "manynet")$results[,"Item"]
manyds <- manyds[!sapply(manyds, function(s) is_list(get(s)))] # just while setting up the tests
manyds <- manyds[1:3] # just while setting up the tests
for (f in manyis) {
  for (d in manyds){
    testthat::test_that(paste(f, "works for", d), {
      testthat::expect_type(get(f)(get(d)), "logical")
    })
  }
}

      
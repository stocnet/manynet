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

collect_functions <- function(pattern, package = "manynet"){
  funnies <- ls(getNamespace(package), pattern = pattern)
  funnies <- funnies[!grepl("\\.", funnies)] # don't test on each method
  funnies
}
# collect_functions("^generate_")

manyis <- collect_functions("^is_")
manyds <- pkg_data() |> 
  dplyr::distinct(dplyr::across(dplyr::where(is.logical)), .keep_all = TRUE) |> 
  dplyr::select(dataset) |> unlist() |> unname()

for (f in manyis) {
  for (d in manyds){
    testthat::test_that(paste(f, "works for", d), {
      testthat::expect_type(get(f)(get(d)), "logical")
    })
  }
}

      
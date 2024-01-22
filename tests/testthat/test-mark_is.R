test_that("is_ tests return correct values", {
  expect_true(is_twomode(ison_southern_women))
  expect_false(is_directed(ison_southern_women))
  expect_false(is_weighted(ison_southern_women))
  expect_false(is_weighted(ison_southern_women))
  expect_true(is_labelled(ison_southern_women))
  expect_false(is_complex(ison_southern_women))
  expect_true(is_graph(ison_southern_women))
  expect_true(is_manynet(ison_southern_women))
  expect_false(is_directed(as_network(ison_southern_women)))
  expect_true(is_connected(ison_southern_women))
  expect_false(is_perfect_matching(ison_southern_women))
  expect_false(is_eulerian(ison_southern_women))
  expect_false(is_acyclic(ison_southern_women))
  expect_false(is_aperiodic(ison_southern_women))
})

  # manyis <- collect_functions("^is_")
  # manyds <- pkg_data() |> 
  #   dplyr::distinct(directed, weighted, twomode, multiplex, .keep_all = TRUE) |>
  #   dplyr::select(dataset) |> unlist() |> unname()
  # 
  # for (f in manyis) {
  #   for (d in manyds){
  #     testthat::test_that(paste(f, "works for", d), {
  #       skip_on_cran()
  #       testthat::expect_type(get(f)(get(d)), "logical")
  #     })
  #   }
  # }

  # dplyr::distinct(dplyr::across(dplyr::where(is.logical)), .keep_all = TRUE) |>

# benchmark <- function(functions, coercions, data){
#   if(missing(coercions)) coercions <- c("as_edgelist","as_igraph","as_matrix","as_network","as_tidygraph")
#   if(missing(data)) data <- "ison_adolescents"
#   isas <- expand.grid(functions, coercions, stringsAsFactors = FALSE)
#   isas <- paste0(isas$Var1, "(", isas$Var2, "(", data, "))")
#   out <- microbenchmark::microbenchmark(list = sapply(isas, function(x) x = eval(parse(text = x))), times = 1000)
#   out
# }
# 
# drill <- function(object, ..., which_summ = c("functions","coercions","data")){
#   which_summ <- match.arg(which_summ)
#   expr <- NULL
#   object |> tibble::as_tibble() |> 
#     tidyr::separate_wider_delim(expr, delim = "(", names = c("functions", "coercions", "data")) |> 
#     dplyr::mutate(data = stringr::str_remove(data, "\\)\\)")) |> 
#     dplyr::group_by(!!!rlang::parse_expr(which_summ)) |> 
#     dplyr::summarise(mean = mean(time), min = min(time), max = max(time)) |> 
#     dplyr::arrange(-mean)
# }
# test <- benchmark(manyis)
# drill(test)
# ggplot2::autoplot(test)
  
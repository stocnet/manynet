is_funs <- funs_objs[grepl("^is_", names(funs_objs)) &
                           !grepl("bloop", names(funs_objs))]

for(fn in collect_functions("^is_")) {
  if(grepl("twomode|attributed|igraph|connected|labelled|graph|manynet|uniplex", fn)) {
    test_that(paste(fn, "returns TRUE for ison_southern_women"), {
      expect_true(is_funs[[fn]](ison_southern_women))
      if(!grepl("igraph", fn)){
        expect_true(is_funs[[fn]](as_stocnet(ison_southern_women)))
      }
    })
  } else {
    test_that(paste(fn, "returns FALSE for ison_southern_women"), {
      expect_false(is_funs[[fn]](ison_southern_women))
      expect_false(is_funs[[fn]](as_stocnet(ison_southern_women)))
    })
  }
}


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
#   object |> dplyr::as_tibble() |> 
#     tidyr::separate_wider_delim(expr, delim = "(", names = c("functions", "coercions", "data")) |> 
#     dplyr::mutate(data = stringr::str_remove(data, "\\)\\)")) |> 
#     dplyr::group_by(!!!rlang::parse_expr(which_summ)) |> 
#     dplyr::summarise(mean = mean(time), min = min(time), max = max(time)) |> 
#     dplyr::arrange(-mean)
# }
# test <- benchmark(manyis)
# drill(test)
# ggplot2::autoplot(test)
  
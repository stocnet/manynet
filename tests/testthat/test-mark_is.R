is_funs <- funs_objs[grepl("^is_", names(funs_objs)) &
                           !grepl("bloop", names(funs_objs))]

for(fn in collect_functions("^is_")) {
  test_that(paste(fn, "has a default method"), {
    expect_true(any(grepl(paste0("^", fn, "\\.default$"), utils::methods(fn))))
  })
  
  if(grepl("twomode|attributed|igraph|connected|labelled|(?<!hyper)graph|manynet|uniplex", fn, perl = TRUE)) {
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

test_that("is_multilevel distinguishes interlocking from plain two-mode networks", {
  # fict_marvel interlocks a signed one-mode layer among its characters with a
  # two-mode layer of affiliations to their teams; fict_actually does the same
  # with a smaller one-mode layer. ison_southern_women is two-mode but has no
  # ties within either mode, so it is not multilevel.
  expect_true(is_multilevel(fict_marvel))
  expect_true(is_multilevel(fict_actually))
  expect_false(is_multilevel(ison_southern_women))
  expect_false(is_multilevel(irps_revere))
  # One-mode networks are never multilevel.
  expect_false(is_multilevel(ison_adolescents))
  # to_multilevel() records levels in 'lvl' and deletes 'type', so the result
  # is no longer two-mode and has to be recognised by its levels instead.
  expect_false(is_twomode(to_multilevel(fict_marvel)))
  expect_true(is_multilevel(to_multilevel(fict_marvel)))
  # A two-mode network without any ties cannot have ties within a mode.
  expect_false(is_multilevel(create_empty(c(3,3))))
})

test_that("is_multilevel and to_multilevel keep a stocnet's modes", {
  marvel <- as_stocnet(fict_marvel)
  women <- as_stocnet(ison_southern_women)
  expect_true(is_multilevel(marvel))
  expect_false(is_multilevel(women))
  expect_false(is_multilevel(as_stocnet(ison_adolescents)))
  expect_false(is_multilevel(as_stocnet(create_empty(c(3,3)))))
  # A 'stocnet' holds its levels in 'mode' and its ties table already allows
  # ties within a mode, so `to_multilevel()` has nothing to reformat.
  expect_identical(to_multilevel(marvel), marvel)
  expect_true(is_twomode(to_multilevel(marvel)))
  expect_equal(net_modes(to_multilevel(marvel)), 2)
  # A 'mode' variable can name more than two levels, unlike an igraph 'type'.
  three <- marvel
  three$nodes$mode[1:5] <- "third"
  expect_equal(net_modes(three), 3)
  expect_true(is_multilevel(three))
  # as_stocnet() maps the 'lvl' attribute that to_multilevel.igraph() writes
  # back onto 'mode', naming the levels from the network's info where it can,
  # so that a round trip through an igraph loses neither of the two modes.
  levelled <- as_stocnet(to_multilevel(as_igraph(fict_marvel)))
  expect_false("lvl" %in% names(levelled$nodes))
  expect_equal(net_modes(levelled), 2)
  expect_setequal(levelled$nodes$mode, marvel$nodes$mode)
  expect_true(is_multilevel(levelled))
})

test_that("is_connected respects connectivity", {
  # fict_starwars is weakly but not strongly connected
  expect_false(is_connected(fict_starwars))
  expect_false(is_connected(fict_starwars, connectivity = "strong"))
  expect_true(is_connected(fict_starwars, connectivity = "weak"))
  # the two notions coincide for undirected networks
  expect_true(is_connected(ison_adolescents))
  expect_true(is_connected(ison_adolescents, connectivity = "weak"))
  expect_true(is_connected(ison_adolescents, connectivity = "strong"))
  expect_error(is_connected(ison_adolescents, connectivity = "bloop"))
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
  
# Test split functions

egos <- ison_adolescents %>%
    tidygraph::activate(edges)

test_that("to_ and from_ egos works", {
  expect_length(to_egos(ison_brandes), length(ison_brandes))
  expect_equal(length(to_egos(ison_brandes)), length(to_egos(ison_brandes, 2)))
  expect_equal(length(egos), length(from_egos(to_egos(egos))))
  expect_s3_class(to_egos(egos)[[1]], "tbl_graph")
  expect_s3_class(from_egos(to_egos(egos)), "tbl_graph")
})

unicorn <- ison_adolescents %>%
    tidygraph::activate(nodes) %>%
    mutate(unicorn = sample(c("yes", "no"), 8,
    replace = TRUE))

test_that("to_ and from_ subgraphs works", {
  expect_length(to_subgraphs(unicorn, "unicorn"), 2)
  expect_equal(length(from_subgraphs(to_subgraphs(unicorn, "unicorn"))),
               length(unicorn))
  expect_s3_class(to_subgraphs(unicorn, "unicorn")[[1]],
                  "tbl_graph")
  expect_s3_class(from_subgraphs(to_subgraphs(unicorn, "unicorn")),
                  "tbl_graph")
})

test_that("to_components works", {
  expect_length(to_components(ison_marvel_relationships), 4)
  expect_length(to_components(ison_adolescents), 1)
  expect_s3_class(to_components(ison_adolescents)[[1]], "tbl_graph")
})

set.seed(1234)
wave <- ison_adolescents %>%
    tidygraph::activate(edges) %>%
    mutate(wave = sample(1995:1998, 10, replace = TRUE))

test_that("to_waves works", {
  expect_equal(class(to_waves(wave)), "list")
  expect_length(to_waves(wave), 4)
  expect_length(to_waves(wave, panels = c(1995, 1996)), 2)
  expect_length(from_waves(to_waves(wave)), 8)
  expect_s3_class(to_waves(wave)[[1]], "tbl_graph")
  expect_s3_class(from_waves(to_waves(wave)), "tbl_graph")
})  

test_that("to_waves works for diff_model objects", {
  skip_on_cran()
  skip_on_ci()
  wave_diff <- migraph::play_diffusion(ison_brandes)
  expect_length(to_waves(wave_diff), length(wave_diff$t))
  expect_equal(network_nodes(to_waves(wave_diff)[[1]]),
               network_nodes(to_waves(wave_diff)[[12]]))
  expect_equal(network_ties(to_waves(wave_diff)[[1]]),
               network_ties(to_waves(wave_diff)[[12]]))
  expect_equal(network_nodes(to_waves(wave_diff)[[1]]),
               network_nodes(ison_brandes))
  expect_true(node_attribute(to_waves(wave_diff)[[1]], "Infected")[1])
  expect_false(node_attribute(to_waves(wave_diff)[[7]], "Exposed")[1])
  expect_false(node_attribute(to_waves(wave_diff)[[10]], "Recovered")[1])
})

slice <- ison_adolescents %>%
    mutate_ties(time = 1:10, increment = 1) %>%
    add_ties(c(1,2), list(time = 3, increment = -1))

test_that("to_ and from_ slices works", {
  expect_length(to_slices(slice, slice = 7), length(ison_adolescents))
  expect_true(length(igraph::edge_attr(to_slices(slice, slice = 7), "weight"))
              < 7)
  expect_length(to_slices(slice, slice = c(5, 7)), 2)
  expect_s3_class(to_slices(slice, slice = 7), "tbl_graph")
})

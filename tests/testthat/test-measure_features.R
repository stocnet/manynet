set.seed(123)

test_that("small-world metrics for two mode networks are calculated and displayed correctly", {
  expect_s3_class(net_smallworld(ison_southern_women), "network_measure")
  expect_equal(as.numeric(net_smallworld(ison_southern_women)), -0.94, tolerance = 0.02)
  expect_equal(c(net_smallworld(ison_adolescents, method = "SWI")), -0.25, tolerance = 0.05)
})

# test_that("net_balance works", {
#   out <- net_balance(ison_marvel_relationships)
#   expect_s3_class(out, "network_measure")
#   expect_equal(as.numeric(out), 0.668, tolerance = 0.01)
#   expect_length(out, 1)
#   expect_error(net_balance(ison_adolescents))
# })

test_that("net_modularity works for two mode networks", {
  out <- net_modularity(ison_southern_women,
                 node_in_partition(ison_southern_women))
  expect_s3_class(out, "network_measure")
  expect_length(out, 1)
})

test_that("net_core works", {
  out <- net_core(ison_adolescents)
  expect_s3_class(out, "network_measure")
  expect_length(out, 1)
  expect_values(out, -0.133)
  expect_values(net_core(ison_adolescents, method = "ident"), 6.481)
  expect_values(net_core(ison_adolescents, method = "diff"), 6.094)
})

test_that("net_richclub works", {
  out <- net_richclub(ison_adolescents)
  expect_s3_class(out, "network_measure")
  expect_length(out, 1)
  expect_values(out, 0.833)
})

test_that("net_factions works", {
  out <- net_factions(ison_adolescents)
  expect_s3_class(out, "network_measure")
  expect_length(out,1)
})

test_that("net_scalefree works", {
  out <- net_scalefree(ison_adolescents)
  expect_s3_class(out, "network_measure")
  expect_values(out,3.689)
})

test_that("net_balance works", {
  out <- net_balance(irps_wwi)
  expect_s3_class(out, "network_measure")
  expect_values(out,1)
})


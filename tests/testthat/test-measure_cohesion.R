test_that("network components works", {
  expect_s3_class(net_components(ison_adolescents), "network_measure")
  expect_equal(as.numeric(net_components(ison_adolescents)), 1)
})

test_that("network cohesion works", {
  expect_s3_class(net_cohesion(ison_southern_women), "network_measure")
  expect_equal(as.numeric(net_cohesion(ison_southern_women)), 2)
})

test_that("network adhesion works", {
  expect_s3_class(net_adhesion(ison_southern_women), "network_measure")
  expect_equal(as.numeric(net_adhesion(ison_southern_women)), 2)
})

test_that("network diameter works", {
  expect_s3_class(net_diameter(ison_southern_women), "network_measure")
  expect_equal(as.numeric(net_diameter(ison_southern_women)), 4)
})

test_that("network length works", {
  expect_s3_class(net_length(ison_southern_women), "network_measure")
  expect_equal(as.numeric(net_length(ison_southern_women)), 2.306, 
               tolerance = 0.001)
})

test_that("net_independence works", {
  expect_s3_class(net_independence(ison_adolescents), "network_measure")
  expect_values(net_independence(ison_adolescents), 4)
})

test_that("net_strength works", {
  expect_s3_class(net_strength(ison_adolescents), "network_measure")
  expect_values(net_strength(ison_adolescents), 0.5)
})

test_that("net_toughness works", {
  expect_s3_class(net_toughness(ison_adolescents), "network_measure")
  expect_values(net_toughness(ison_adolescents), 0.5)
})
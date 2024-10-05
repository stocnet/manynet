# # Equivalence clustering tests

test_that("equivalence clustering returns the right class", {
  expect_s3_class(node_in_structural(ison_adolescents, "strict", "hier"), "node_member")
  expect_s3_class(node_in_regular(ison_adolescents), "node_member")
  expect_s3_class(node_in_automorphic(ison_adolescents), "node_member")
  testthat::skip_if_not_installed("sna")
  expect_s3_class(node_in_structural(ison_adolescents, "elbow", "hier"), "node_member")
  expect_s3_class(node_in_structural(ison_adolescents, "elbow", "concor"), "node_member")
})

test_that("equivalence clustering works", {
  expect_equal(node_in_structural(ison_adolescents, "silhouette", "hier"), node_in_structural(ison_adolescents))
  expect_equal(node_in_regular(ison_adolescents), node_in_regular(ison_adolescents, "silhouette", "hier"))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_structural(ison_adolescents, "silhouette", "concor")))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_structural(ison_adolescents, k = 3, "hier")))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_structural(ison_adolescents, "strict", "concor")))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_regular(ison_adolescents, cluster = "concor")))
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_regular(ison_adolescents, "strict")))
  expect_equal(c(net_nodes(ison_southern_women)), length(node_in_automorphic(ison_southern_women, "strict", distance = "binary")))
  expect_equal(c(net_nodes(ison_southern_women)), length(node_in_automorphic(ison_southern_women, distance = "maximum")))
  expect_true("C" %in% node_in_structural(ison_adolescents, k = 3, "concor"))
  expect_true("B" %in% node_in_regular(ison_adolescents, 2))
  expect_true("D" %in% node_in_automorphic(ison_southern_women, 4))
  testthat::skip_if_not_installed("sna")
  expect_equal(c(net_nodes(ison_adolescents)), length(node_in_regular(ison_adolescents, "elbow")))
})

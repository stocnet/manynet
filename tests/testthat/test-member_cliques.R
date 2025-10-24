test_that("node_in_roulette works", {
  res <- node_in_roulette(ison_adolescents, num_groups = 3)
  expect_s3_class(res, "node_member")
  expect_length(res, net_nodes(ison_adolescents))
  expect_false(res[1] == res[2])
})

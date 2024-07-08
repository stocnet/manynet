test_that("node_kernighanlin algorithm works", {
  expect_s3_class(node_in_partition(ison_adolescents), "node_member")
  expect_length(node_in_partition(ison_adolescents), 
                net_nodes(ison_adolescents))
  expect_false(any(node_in_partition(ison_adolescents) > "B"))
})

test_that("node_edge_betweenness algorithm works", {
  expect_s3_class(node_in_betweenness(ison_adolescents), "node_member")
  expect_length(node_in_betweenness(ison_adolescents), 
                net_nodes(ison_adolescents))
})

test_that("node_fast_greedy algorithm works", {
  expect_s3_class(node_in_greedy(ison_southern_women), "node_member")
  expect_length(node_in_greedy(ison_southern_women), 
                net_nodes(ison_southern_women))
})

test_that("node_walktrap algorithm works", {
  expect_s3_class(node_in_walktrap(ison_southern_women), "node_member")
  expect_length(node_in_walktrap(ison_southern_women), 
                net_nodes(ison_southern_women))
})

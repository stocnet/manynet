test_tbl <- as_tidygraph(ison_southern_women)
test_igr <- ison_southern_women
test_mat <- as_matrix(ison_southern_women)

test_that("one mode degree centrality calculated correctly",{
  expect_equal(top5(node_degree(ison_adolescents, normalized = FALSE)), c(1,4,4,2,3))
})

test_that("one mode strength centrality calculated correctly",{
  expect_equal(top5(node_degree(to_unweighted(ison_networkers), direction = "in", normalized = FALSE)), 
               c(29, 24, 11, 18, 8))
  expect_equal(top5(node_degree(ison_networkers, direction = "in", normalized = FALSE, alpha = 1)), 
               c(2495, 1212, 101, 322, 89))
})

test_that("two mode degree centrality calculated correctly",{
  expect_equal(top5(node_degree(test_mat, normalized = FALSE)), c(8,7,8,7,4))
  expect_equal(top5(node_degree(test_igr, normalized = FALSE)), c(8,7,8,7,4))
  expect_equal(top5(with_graph(test_tbl, node_degree(normalized = FALSE))), c(8,7,8,7,4))
  expect_equal(bot3(node_degree(test_mat, normalized = FALSE)), c(6,3,3))
  expect_equal(bot3(node_degree(test_igr, normalized = FALSE)), c(6,3,3))
  expect_equal(bot3(with_graph(test_tbl, node_degree(normalized = FALSE))), c(6,3,3))
  expect_equal(top5(node_degree(test_mat, normalized = TRUE)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(top5(node_degree(test_igr, normalized = TRUE)), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(top5(with_graph(test_tbl, node_degree(normalized = TRUE))), c(0.5714, .5, .5714, .5, .2857))
  expect_equal(bot3(node_degree(test_mat, normalized = TRUE)), c(.3333, .1667, .1667))
  expect_equal(bot3(node_degree(test_igr, normalized = TRUE)), c(.3333, .1667, .1667))
  expect_equal(bot3(with_graph(test_tbl, node_degree(normalized = TRUE))), c(.3333, .1667, .1667))
})

test_that("one mode closeness centrality calculated correctly",{
  expect_equal(top3(node_closeness(ison_adolescents, normalized = FALSE)), c(0.059, 0.091, 0.091), tolerance = 0.01)
})

test_that("two mode closeness centrality calculated correctly",{
  expect_equal(top5(node_closeness(test_mat, normalized = FALSE)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(top5(node_closeness(test_igr, normalized = FALSE)), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(top5(with_graph(test_tbl, node_closeness(normalized = FALSE))), c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(bot3(node_closeness(test_mat, normalized = FALSE)), c(0.0128, 0.0119, 0.0119))
  expect_equal(bot3(node_closeness(test_igr, normalized = FALSE)), c(0.0128, 0.0119, 0.0119))
  expect_equal(bot3(with_graph(test_tbl, node_closeness(normalized = FALSE))), c(0.0128, 0.0119, 0.0119))
  expect_equal(top5(node_closeness(test_mat, normalized = TRUE)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(top5(node_closeness(test_igr, normalized = TRUE)), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(top5(with_graph(test_tbl, node_closeness(normalized = TRUE))), c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(bot3(node_closeness(test_mat, normalized = TRUE)), c(0.5641, 0.5238, 0.5238))
  expect_equal(bot3(node_closeness(test_igr, normalized = TRUE)), c(0.5641, 0.5238, 0.5238))
  expect_equal(bot3(with_graph(test_tbl, node_closeness(normalized = TRUE))), c(0.5641, 0.5238, 0.5238))
})

test_that("one mode betweenness centrality calculated correctly",{
  expect_equal(top3(node_betweenness(ison_adolescents, normalized = FALSE)), c(0, 7.5, 5.5), tolerance = 0.001)
})

test_that("two mode betweenness centrality calculated correctly",{
  expect_equal(top5(node_betweenness(test_mat, normalized = FALSE)), c(42.9802, 22.8541, 38.9796, 22.0215, 4.7153))
  expect_equal(top5(node_betweenness(test_igr, normalized = FALSE)), c(42.9802, 22.8541, 38.9796, 22.0215, 4.7153))
  expect_equal(top5(with_graph(test_tbl, node_betweenness(normalized = FALSE))), c(42.9802, 22.8541, 38.9796, 22.0215, 4.7153))
  expect_equal(bot3(node_betweenness(test_mat, normalized = FALSE)), c(8.1786, 1.0128, 1.0128))
  expect_equal(bot3(node_betweenness(test_igr, normalized = FALSE)), c(8.1786, 1.0128, 1.0128))
  expect_equal(bot3(with_graph(test_tbl, node_betweenness(normalized = FALSE))), c(8.1786, 1.0128, 1.0128))
  expect_equal(top3(node_betweenness(test_mat, normalized = TRUE),4), c(0.0972, 0.0517, 0.0882))
  expect_equal(top3(node_betweenness(test_igr, normalized = TRUE),4), c(0.0972, 0.0517, 0.0882))
  expect_equal(top3(with_graph(test_tbl, node_betweenness(normalized = TRUE)),4), c(0.0972, 0.0517, 0.0882))
  expect_equal(bot3(node_betweenness(test_mat, normalized = TRUE),4), c(0.0181, 0.0022, 0.0022))
  expect_equal(bot3(node_betweenness(test_igr, normalized = TRUE),4), c(0.0181, 0.0022, 0.0022))
  expect_equal(bot3(with_graph(test_tbl, node_betweenness(normalized = TRUE)),4), c(0.0181, 0.0022, 0.0022))
})

test_that("one mode eigenvector centrality calculated correctly",{
  expect_equal(top3(node_eigenvector(ison_adolescents, normalized = FALSE)), c(0.16, 0.491, 0.529), tolerance = 0.001)
  expect_equal(top3(node_eigenvector(ison_adolescents, normalized = TRUE)), c(0.227, 0.694, 0.748), tolerance = 0.001)
})

test_that("two mode eigenvector centrality calculated correctly",{
  expect_equal(top3(node_eigenvector(test_mat, normalized = FALSE)), c(0.3185, 0.3004, 0.3536))
  expect_equal(top3(node_eigenvector(test_igr, normalized = FALSE)), c(0.3185, 0.3004, 0.3536))
  expect_equal(bot3(node_eigenvector(test_mat, normalized = FALSE)), c(0.2156, 0.1316, 0.1316))
  expect_equal(bot3(node_eigenvector(test_igr, normalized = FALSE)), c(0.2156, 0.1316, 0.1316))
  expect_equal(top3(node_eigenvector(test_igr, normalized = TRUE)), c(0.4505, 0.4248, 0.5000))
})

test_that("node measure class works", {
  expect_s3_class(node_degree(ison_adolescents), "node_measure")
  expect_s3_class(node_betweenness(ison_adolescents), "node_measure")
  expect_s3_class(node_closeness(ison_adolescents), "node_measure")
  expect_s3_class(node_eigenvector(ison_adolescents), "node_measure")
  expect_s3_class(node_reach(ison_adolescents), "node_measure")
  testplot <- plot(node_degree(ison_adolescents))
  expect_equal(testplot$data$Score, unname(node_degree(ison_adolescents)))
  # expect_equal(testplot$labels$y, "Frequency")
})

# ####### Centralization

test_that("one-mode centralisation is calculated correctly", {
  expect_equal(as.numeric(net_degree(ison_adolescents)), 0.2142, tolerance = 0.001)
  expect_equal(as.numeric(net_closeness(ison_adolescents)), 0.3195, tolerance = 0.001)
  expect_equal(as.numeric(net_betweenness(ison_adolescents)), 0.3401, tolerance = 0.001)
  expect_equal(as.numeric(net_eigenvector(ison_adolescents)), 0.5479, tolerance = 0.001)
})

test_that("two mode degree centralisation calculated correctly", {
  expect_equal(as.numeric(net_degree(ison_southern_women, normalized = FALSE)), c(0.2021, 0.5253), tolerance = 0.001)
  expect_equal(as.numeric(net_degree(ison_southern_women, direction = "in")), c(0.249, 0.484), tolerance = 0.001)
  expect_equal(as.numeric(net_degree(ison_southern_women, normalized = TRUE)), c(0.245, 0.493), tolerance = 0.001)
})

test_that("two mode closeness centralisation calculated correctly", {
  expect_equal(as.numeric(net_closeness(ison_southern_women, normalized = TRUE)), c(0.293, 0.452), tolerance = 0.001)
  expect_equal(as.numeric(net_closeness(ison_southern_women, direction = "in")), c(0.224, 0.537), tolerance = 0.001)
})

test_that("two mode betweenness centralisation calculated correctly", {
  expect_equal(as.numeric(net_betweenness(ison_southern_women, normalized = FALSE)), c(0.0733, 0.2113), tolerance = 0.001)
  expect_equal(as.numeric(net_betweenness(ison_southern_women, direction = "in")), c(0.082, 0.202), tolerance = 0.001)
  expect_equal(as.numeric(net_betweenness(ison_southern_women, normalized = TRUE)), c(0.0739, 0.2113), tolerance = 0.001)
})

test_that("net_measure class works", {
  expect_s3_class(net_degree(ison_algebra), "network_measure")
  expect_s3_class(net_betweenness(ison_southern_women), "network_measure")
  expect_s3_class(net_closeness(ison_southern_women), "network_measure")
  expect_output(print(net_degree(ison_algebra)))
})

# ####### Edge centrality
test_that("tie_degree works", {
  expect_s3_class(tie_degree(ison_adolescents),
                  "tie_measure")
  expect_length(tie_degree(ison_adolescents),
                manynet::net_ties(ison_adolescents))
})

test_that("tie_betweenness works", {
  expect_s3_class(tie_betweenness(ison_adolescents),
                  "tie_measure")
  expect_length(tie_betweenness(ison_adolescents),
                manynet::net_ties(ison_adolescents))
  expect_equal(unname(tie_betweenness(ison_adolescents)[1:3]),
               c(7,3,5), tolerance = 0.001)
})

test_that("tie_closeness works", {
  expect_s3_class(tie_closeness(ison_adolescents),
                  "tie_measure")
  expect_length(tie_closeness(ison_adolescents),
                manynet::net_ties(ison_adolescents))
  expect_equal(unname(tie_closeness(ison_adolescents)[1:3]),
               c(0.562,0.692,0.600), tolerance = 0.001)
})

test_that("tie_eigenvector works", {
  expect_s3_class(tie_eigenvector(ison_southern_women),
                  "tie_measure")
  expect_length(tie_eigenvector(ison_southern_women),
                manynet::net_ties(ison_southern_women))
})


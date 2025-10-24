test_that("cluster_cosine works", {
  expect_s3_class(cluster_cosine(node_by_triad(ison_monks), distance = "euclidean"), "hclust")
})

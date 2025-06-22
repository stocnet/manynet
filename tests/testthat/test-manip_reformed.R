test_that("to_no_missing.tbl_graph removes nodes with missing values", {
  # Create a tbl_graph with some missing values
  graph <- tbl_graph(
    nodes = tibble::tibble(name = c("A", "B", NA, "D")),
    edges = tibble::tibble(from = c(1, 2, 3), to = c(2, 3, 4))
  )
  
  # Apply the function
  cleaned_graph <- to_no_missing(graph)
  
  # Check that nodes with missing values are removed
  expect_equal(nrow(as_tibble(cleaned_graph, active = "nodes")), 3)
  expect_true(all(complete.cases(as_tibble(cleaned_graph, active = "nodes"))))
})

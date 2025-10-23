# Test hierarchy measures

test_that("net_connectedness works correctly", {
  connect_judo <- net_connectedness(ison_judo_moves)
  # Basic functionality tests
  expect_s3_class(connect_judo, "network_measure") # undirected

  # Return type and range tests
  expect_true(is.numeric(as.numeric(connect_judo)))
  expect_true(as.numeric(connect_judo) >= 0)
  expect_true(as.numeric(connect_judo) <= 1)
  
  # Test with complete graph (should be 1)
  expect_equal(round(as.numeric(net_connectedness(create_filled(5))), 4), 1)
  expect_equal(round(as.numeric(net_connectedness(create_empty(5))), 4), 0)
  
  # Test edge case: single node
  expect_false(is.finite(as.numeric(net_connectedness(create_empty(1)))))
})

test_that("net_efficiency works correctly", {
  # Basic functionality tests
  effic_judo <- net_efficiency(ison_judo_moves)
  expect_s3_class(effic_judo, "network_measure")

  # Return type tests
  expect_true(is.numeric(as.numeric(effic_judo)))
  expect_true(as.numeric(effic_judo) > 0)
})

test_that("net_upperbound works correctly", {
  # Basic functionality tests
  upper_judo <- net_efficiency(ison_judo_moves)
  expect_s3_class(upper_judo, "network_measure")

  # Return type and range tests
  expect_true(is.numeric(as.numeric(upper_judo)))
  expect_true(as.numeric(upper_judo) >= 0)
  expect_true(as.numeric(upper_judo) <= 1)
  
  # Test with perfect hierarchy (should approach 1)
  # Create a tournament-like structure
  perfect_hierarchy <- create_tree(5)
  expect_equal(as.numeric(net_upperbound(perfect_hierarchy)), 1)
})

test_that("net_by_hierarchy works correctly", {
  result <- net_by_hierarchy(ison_judo_moves)
  # Basic functionality tests
  expect_s3_class(result, "network_motif")

  # Check that it returns a data frame with correct columns
  expect_true(is.data.frame(result))
  expect_named(result, c("Connectedness", "InvReciprocity", "Efficiency", "LeastUpperBound"))
  
  # Check that all values are numeric and within expected ranges
  expect_true(all(sapply(result, is.numeric)))
  expect_true(result$Connectedness >= 0 && result$Connectedness <= 1)
  expect_true(result$InvReciprocity >= 0 && result$InvReciprocity <= 1)
  expect_true(result$Efficiency > 0)  # Should be positive (can be > 1)
  expect_true(result$LeastUpperBound >= 0 && result$LeastUpperBound <= 1)
})

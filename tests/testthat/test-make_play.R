test_that("play_learning works", {
  test <- play_learning(ison_networkers,
                rbinom(net_nodes(ison_networkers),1,prob = 0.25))
  expect_s3_class(test, "learn_model")
})

test_that("play_segregation works", {
    startValues <- rbinom(100,1,prob = 0.5)
    startValues[sample(seq_len(100), round(100*0.2))] <- NA
    latticeEg <- create_lattice(100)
    latticeEg <- add_node_attribute(latticeEg, "startValues", startValues)
    expect_s3_class(play_segregation(latticeEg, "startValues", 0.5), "mnet")
})

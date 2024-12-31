test <- play_diffusion(create_tree(12), steps = 2)

test_that("infection total works", {
  expect_values(net_infection_total(test), 0.583)
})

test_that("infection complete works", {
  expect_values(net_infection_complete(test), Inf)
})


test_that("play_diffusion works for named networks", {
  expect_warning(named_SI <- play_diffusion(ison_adolescents, old_version = TRUE))
  expect_equal(named_SI$S + named_SI$I, named_SI$n)
  expect_equal(summary(named_SI)$t[1], 0)
  expect_equal(summary(named_SI)$nodes[1:4], c(1,2,3,5))
})

test_that("play_diffusion works for named networks", {
  expect_warning(named_SEI <- play_diffusion(ison_adolescents, latency = 1, old_version = TRUE))
  expect_equal(named_SEI$S + named_SEI$E + named_SEI$I, named_SEI$n)
  expect_equal(summary(named_SEI)$t[1], 0)
  expect_equal(summary(named_SEI)$nodes[1:4], c(1,2,NA,NA))
})

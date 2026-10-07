
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

test_that("play_diffusion plays on a stocnet", {
  played <- play_diffusion(irps_tribes, seeds = 1)
  expect_s3_class(played, "stocnet")
  report <- as_diffusion(played)
  expect_equal(report$t[1], 0)
  expect_equal(report$I[1], 1)
})

test_that("as_diffusion reports every step, also where nothing spreads", {
  # nothing can spread from an isolate, so only its seeding is recorded
  alone <- as_diffusion(play_diffusion(create_empty(4), seeds = 1))
  expect_equal(nrow(alone), 1)
  expect_equal(alone$I, 1)
  expect_equal(alone$S, 3)
  # the last step is reported, by which the whole ring is infected
  ring <- as_diffusion(play_diffusion(create_ring(8), seeds = 1))
  expect_equal(ring$I[nrow(ring)], 8)
  expect_equal(sum(ring$I_new), 8)
})

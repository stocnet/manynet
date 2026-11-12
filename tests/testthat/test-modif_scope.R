# Moving time windows in to_time()

test_that("to_time with a window keeps the ties stamped in the days up to it", {
  d <- as.Date("2011-04-01")
  offset <- as.numeric(tie_attribute(irps_nuclear, "time") - d)
  # A window of 30 days holds the 30 days up to and including the day,
  # whatever `info$update` declares, and nothing after it
  out <- to_time(irps_nuclear, d, window = 30)
  expect_s3_class(out, "stocnet")
  expect_equal(as.numeric(net_ties(out)), sum(offset > -30 & offset <= 0))
  expect_false("time" %in% net_tie_attributes(out))
  # A uniform window leaves the stances as they were
  expect_setequal(unique(tie_attribute(out, "weight")), c(-1, 1))
  # A difftime and a date given as text are read the same way
  expect_equal(net_ties(to_time(irps_nuclear, d,
                                window = as.difftime(30, units = "days"))),
               net_ties(out))
  expect_equal(net_ties(to_time(irps_nuclear, "2011-04-01", window = 30)),
               net_ties(out))
})

test_that("to_time with two widths reaches past the moment as well", {
  d <- as.Date("2011-04-01")
  offset <- as.numeric(tie_attribute(irps_nuclear, "time") - d)
  out <- to_time(irps_nuclear, d, window = c(15, 15))
  expect_equal(as.numeric(net_ties(out)), sum(offset > -15 & offset <= 15))
  # a width of zero after the moment is the window that looks back only
  expect_equal(to_time(irps_nuclear, d, window = c(30, 0))$ties,
               to_time(irps_nuclear, d, window = 30)$ties)
})

test_that("to_time with an infinite window holds every tie up to the moment", {
  d <- as.Date("2011-04-01")
  stamps <- tie_attribute(irps_nuclear, "time")
  expect_equal(as.numeric(net_ties(to_time(irps_nuclear, d, window = Inf))),
               sum(stamps <= d))
  # in a panel, every wave up to the one asked for, as cumulative waves are
  expect_equal(as.numeric(net_ties(to_time(ison_monks, 2, window = Inf))),
               sum(tie_attribute(ison_monks, "time") <= 2))
})

test_that("to_time with a window keeps each row, and does not accumulate", {
  net <- ison_adolescents |>
    mutate_ties(time = 1:10, increment = 1) |>
    add_ties(c(1, 2), list(time = 3, increment = -1))
  # the accumulated state at 5 nets the two rows of the first tie to zero
  expect_equal(as.numeric(net_ties(to_time(net, 5))), 4)
  expect_equal(as.numeric(net_ties(to_time(net, 5, window = Inf))), 6)
})

test_that("to_time with a window is not reverted to the last moment", {
  last <- max(tie_attribute(irps_nuclear, "time"))
  expect_equal(as.numeric(net_ties(to_time(irps_nuclear, last + 100,
                                           window = 30))), 0)
})

test_that("to_time with a window of one wave is the wave", {
  expect_equal(to_time(ison_monks, 2, window = 1)$ties,
               to_time(ison_monks, 2)$ties)
  # a window of two waves holds the wave before too, and not the one after
  expect_equal(as.numeric(net_ties(to_time(ison_monks, 2, window = 2))),
               sum(tie_attribute(ison_monks, "time") %in% 1:2))
})

test_that("to_time with a window keeps the interval ties that overlap it", {
  begin <- tie_attribute(irps_wwi, "begin")
  end <- tie_attribute(irps_wwi, "end")
  # the ten years up to 1901: those after 1891 and up to 1901
  out <- to_time(irps_wwi, 1901, window = 10)
  expect_equal(as.numeric(net_ties(out)), sum(begin <= 1901 & end > 1891))
  expect_gte(as.numeric(net_ties(out)),
             as.numeric(net_ties(to_time(irps_wwi, 1901))))
  # A tie active at the moment is at no distance from it
  tri <- to_time(irps_wwi, 1901, window = 10, kernel = "triangular")
  active <- begin <= 1901 & end > 1901
  kept <- begin <= 1901 & end > 1891
  expect_equal(as.vector(tie_attribute(tri, "weight"))[active[kept]],
               as.vector(tie_attribute(irps_wwi, "weight"))[active])
})

test_that("to_time weights the ties in a window by a kernel", {
  net <- add_tie_attribute(create_ring(5), "time", c(0, 1, 2, 3, 4))
  weigh <- function(kernel, window = 4)
    as.vector(tie_attribute(to_time(net, 4, window = window, kernel = kernel),
                            "weight"))
  expect_false(is_weighted(to_time(net, 4, window = 4)))
  # the window of 4 up to 4 holds the moments 1 to 4, at 3/4 to 0 widths back
  dist <- c(3, 2, 1, 0) / 4
  expect_equal(weigh("triangular"), 1 - dist)
  expect_equal(weigh("epanechnikov"), 1 - dist^2)
  expect_equal(weigh("gaussian"), exp(-2 * dist^2))
  # each side of a window with two widths is read against its own width
  both <- as.vector(tie_attribute(to_time(net, 2, window = c(2, 4),
                                          kernel = "triangular"), "weight"))
  expect_equal(both, c(1 - 1/2, 1, 1 - 1/4, 1 - 2/4))
  # an infinite window weights every tie fully
  expect_equal(weigh("triangular", Inf), rep(1, 5))
  # a weighted tie has its weight multiplied by the kernel
  weighted <- add_tie_attribute(net, "weight", c(2, 2, -2, 2, 2))
  expect_equal(as.vector(tie_attribute(to_time(weighted, 4, window = 4,
                                               kernel = "triangular"),
                                       "weight")),
               c(2, -2, 2, 2) * (1 - dist))
})

test_that("to_time with a window returns the class it was given", {
  d <- as.Date("2011-04-01")
  expect_s3_class(to_time(as_igraph(irps_nuclear), d, window = 30,
                          kernel = "epanechnikov"), "igraph")
  expect_s3_class(to_time(as_tidygraph(irps_nuclear), d, window = 30),
                  "tbl_graph")
})

test_that("to_time checks the window and the kernel", {
  expect_error(to_time(ison_monks, 2, window = -1), "positive")
  expect_error(to_time(ison_monks, 2, window = c(0, 2)), "positive")
  expect_error(to_time(ison_monks, 2, window = 1:3), "positive")
  expect_error(to_time(ison_monks, 2, window = 1, kernel = "box"), "kernel")
  expect_warning(to_time(ison_monks, 2, kernel = "gaussian"), "window")
})

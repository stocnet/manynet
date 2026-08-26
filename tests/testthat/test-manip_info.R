test_that("a stocnet completed through the prompt path validates", {
  net <- make_stocnet(
    info = list(name = "Prompted Network",
                modes = "Person",
                layers = c("Friendship", "Advice")),
    nodes = data.frame(label = c("A", "B", "C")),
    ties = data.frame(from = c("A", "B"),
                      to = c("B", "C"),
                      weight = c(1, 2),
                      layer = c("Friendship", "Advice")))

  # The menu answers, in the order that .check_info() asks for them.
  answers <- c(1, 2,  # directed: Friendship, Advice
               1,     # source: Empirical
               1,     # method: Survey
               2,     # boundary: Roster
               2, 1,  # observation: Panel, Cross-sectional
               1, 2,  # update: Increment, Replacement
               2)     # focal: Advice
  i <- 0
  testthat::local_mocked_bindings(
    snet_menu = function(choices, title) {
      i <<- i + 1
      answers[i]
    },
    snet_readline = function(prompt) ""
  )

  out <- add_info(net, optional = TRUE)

  expect_no_error(validate_stocnet(out))
  expect_equal(out$info$source, "empirical")
  expect_equal(out$info$method, "survey")
  expect_equal(out$info$boundary, "roster")
  expect_equal(unname(out$info$observation), c("panel", "cross-sectional"))
  expect_equal(unname(out$info$update), c("increment", "replace"))
  expect_equal(out$info$focal, "Advice")
})

##### test themes
test_that("themes graph correctly", {
  test_iheid <- autographr(to_mentoring(ison_brandes)) + 
    labs(title = "Who leads and who follows?",
         subtitle = "A mentoring network",
         caption = "ison_brandes network") + 
    theme_iheid()
  test_ethz <- autographr(to_mentoring(ison_brandes)) + 
    labs(title = "Who leads and who follows?",
         subtitle = "A mentoring network",
         caption = "ison_brandes network") + 
    theme_ethz()
  test_uzh <- autographr(to_mentoring(ison_brandes)) + 
    labs(title = "Who leads and who follows?",
         subtitle = "A mentoring network",
         caption = "ison_brandes network") + 
    theme_uzh()
  expect_equal(names(test_iheid[["theme"]][["title"]][["colour"]]), "IHEIDRed")
  expect_null(names(test_ethz[["theme"]][["title"]][["colour"]]))
  expect_equal(names(test_uzh[["theme"]][["title"]][["colour"]]), "UZH_Orange")
  expect_equal(names(test_iheid[["theme"]][["plot.subtitle"]][["colour"]]),
               "IHEIDGrey")
  expect_null(names(test_ethz[["theme"]][["plot.subtitle"]][["colour"]]))
  expect_equal(names(test_uzh[["theme"]][["plot.subtitle"]][["colour"]]),
               "UZH_Blue")
  expect_equal(test_iheid[["theme"]][["plot.caption"]][["family"]], "serif")
  expect_equal(test_ethz[["theme"]][["plot.caption"]][["family"]], "sans")
  expect_equal(test_uzh[["theme"]][["plot.caption"]][["family"]], "sans")
})

##### test scales
test_that("scales graph correctly", {
  test_sdg <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3,4,5), 2), 1)) %>%
    autographr(node_color = color) +
    scale_color_sdgs()
  test_iheid <- ison_brandes %>%
    mutate(color = c(rep(c(1,2), 5), 3)) %>%
    autographr(node_color = color) +
    scale_color_iheid()
  test_ethz <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3), 3), 4, 5)) %>%
    autographr(node_color = color) +
    scale_color_ethz()
  test_uzh <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3), 3), 1, 2)) %>%
    autographr(node_color = color) +
    scale_color_uzh()
  test_rug <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3), 3), 1, 2)) %>%
    autographr(node_color = color) +
    scale_color_rug()
  expect_equal(test_sdg[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "SDGs")
  expect_equal(test_sdg[["scales"]][["scales"]][[1]][["call"]][["palette"]][["palette"]], "SDGs")
  expect_equal(test_iheid[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "IHEID")
  expect_equal(test_ethz[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "ETHZ")
  expect_equal(test_uzh[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "UZH")
  expect_equal(test_rug[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "RUG")
})

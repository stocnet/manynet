##### test themes
test_that("themes graph correctly", {
  test_iheid <- autographr(to_mentoring(ison_brandes)) + 
    labs(title = "Who leads and who follows?",
         subtitle = "A mentoring network",
         caption = "ison_brandes network") + 
    theme_iheid()
  test_eth <- autographr(to_mentoring(ison_brandes)) + 
    labs(title = "Who leads and who follows?",
         subtitle = "A mentoring network",
         caption = "ison_brandes network") + 
    theme_eth()
  test_uzh <- autographr(to_mentoring(ison_brandes)) + 
    labs(title = "Who leads and who follows?",
         subtitle = "A mentoring network",
         caption = "ison_brandes network") + 
    theme_uzh()
  expect_equal(names(test_iheid[["theme"]][["title"]][["colour"]]), "IHEIDRed")
  expect_equal(names(test_eth[["theme"]][["title"]][["colour"]]), "ETH_Green")
  expect_equal(names(test_uzh[["theme"]][["title"]][["colour"]]), "UZH_Orange")
  expect_equal(names(test_iheid[["theme"]][["plot.subtitle"]][["colour"]]),
               "IHEIDGrey")
  expect_equal(names(test_eth[["theme"]][["plot.subtitle"]][["colour"]]),
               "ETH_Bronze")
  expect_equal(names(test_uzh[["theme"]][["plot.subtitle"]][["colour"]]),
               "UZH_Blue")
  expect_equal(test_iheid[["theme"]][["plot.caption"]][["family"]], "Times")
  expect_equal(test_eth[["theme"]][["plot.caption"]][["family"]], "serif")
  expect_equal(test_uzh[["theme"]][["plot.caption"]][["family"]], "sans")
  expect_equal(test_iheid[["theme"]][["plot.caption"]][["face"]], "italic")
  expect_equal(test_eth[["theme"]][["plot.caption"]][["face"]], "italic")
  expect_equal(test_uzh[["theme"]][["plot.caption"]][["face"]], "italic")
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
  test_eth <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3), 3), 4, 5)) %>%
    autographr(node_color = color) +
    scale_color_eth()
  test_uzh <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3), 3), 1, 2)) %>%
    autographr(node_color = color) +
    scale_color_uzh()
  test_ug <- ison_brandes %>%
    mutate(color = c(rep(c(1,2,3), 3), 1, 2)) %>%
    autographr(node_color = color) +
    scale_color_ug()
  expect_equal(test_sdg[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "SDGs")
  expect_equal(test_sdg[["scales"]][["scales"]][[1]][["call"]][["palette"]][["palette"]], "SDGs")
  expect_equal(test_iheid[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "IHEID")
  expect_equal(test_eth[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "ETH")
  expect_equal(test_uzh[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "UZH")
  expect_equal(test_ug[["scales"]][["scales"]][[1]][["call"]][["scale_name"]], "UG")
})

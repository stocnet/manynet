# Test partition layouts
set.seed(1000)
test_that("railway layout works", {
  y <- layout_tbl_graph_railway(ison_southern_women)$y
  expect_equal(y[1:18], rep(0, 18))
  expect_equal(y[19:32], rep(1, 14))
  expect_length(layout_tbl_graph_railway(ison_southern_women), 2)
  expect_equal(length(layout_tbl_graph_railway(ison_southern_women)),
               length(layout_tbl_graph_railway(ison_adolescents)))
  expect_equal(min(layout_tbl_graph_railway(ison_adolescents)$y),
               min(layout_tbl_graph_railway(ison_algebra)$y))
  expect_equal(max(layout_tbl_graph_railway(ison_adolescents)$y),
               max(layout_tbl_graph_railway(ison_algebra)$y))
})

test_that("concentric layout works", {
  x <- round(layout_tbl_graph_concentric(ison_southern_women)$x, 3)
  y <- round(layout_tbl_graph_concentric(ison_southern_women)$y, 3)
  expect_equal(length(x)/4, length(unique(abs(x))))
  expect_equal(length(y)/4, length(unique(abs(y))))
  expect_length(layout_tbl_graph_concentric(ison_southern_women), 2)
  expect_length(layout_tbl_graph_concentric(ison_adolescents, 
                                            membership = c(rep(1,4),rep(2,4))), 2)
})

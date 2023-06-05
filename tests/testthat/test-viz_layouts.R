# Test layouts

# Test grid layouts
set.seed(1000)
x <- layout_tbl_graph_frgrid(ison_adolescents)$x
y <- layout_tbl_graph_frgrid(ison_adolescents)$y
test_that("FR grid layout works",{
  expect_true(all.equal(x, round(x)))
  expect_true(all.equal(y, round(y)))
  expect_length(layout_tbl_graph_frgrid(ison_adolescents), 2)
  expect_true(class(attributes(layout_tbl_graph_frgrid(ison_adolescents))$graph)[1]
              == "tbl_graph")
})

x <- layout_tbl_graph_kkgrid(ison_adolescents)$x
y <- layout_tbl_graph_kkgrid(ison_adolescents)$y
test_that("KK grid layout works",{
  expect_true(all.equal(x, round(x)))
  expect_true(all.equal(y, round(y)))
  expect_length(layout_tbl_graph_kkgrid(ison_adolescents), 2)
  expect_true(class(attributes(layout_tbl_graph_kkgrid(ison_adolescents))$graph)[1]
              == "tbl_graph")
})

x <- layout_tbl_graph_gogrid(ison_adolescents)$x
y <- layout_tbl_graph_gogrid(ison_adolescents)$y
test_that("GO grid layout works",{
  expect_true(all.equal(x, round(x)))
  expect_true(all.equal(y, round(y)))
  expect_length(layout_tbl_graph_gogrid(ison_adolescents), 2)
  expect_true(class(attributes(layout_tbl_graph_gogrid(ison_adolescents))$graph)[1]
              == "tbl_graph")
})

# Testing the helper functions (quite simple for now)
test_that("helpers work",{
  expect_equal(class(localmin(layout_tbl_graph_gogrid(ison_brandes), ison_brandes)), "data.frame")
  expect_equal(class(get_vacant_points(layout_tbl_graph_gogrid(ison_brandes))), "data.frame")
  expect_equal(class(get_vacant_points(layout_tbl_graph_gogrid(ison_brandes))), "data.frame")
  expect_equal(class(cost_function(layout_tbl_graph_gogrid(ison_brandes), ison_brandes)), c("matrix", "array"))
})

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
  expect_length(layout_tbl_graph_concentric(ison_adolescents), 2)
})

test_that("node_is_universal works", {
  expect_true(any(node_is_universal(create_star(11))))
})

test_that("node_kcoreness works", {
  expect_equal(top3(node_kcoreness(ison_adolescents)), c(1,2,2))
})

test_that("node_in_core works", {
  expect_equal(top3(node_in_core(ison_adolescents)), c("Periphery","Core","Core"))
  expect_output(print(node_in_core(ison_adolescents, groups = 5)), "Semi-periphery-1")
})

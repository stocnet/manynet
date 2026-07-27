test_that("glossary works", {
  expect_match(gloss("star"),"A star network")
  expect_match(gloss("bloop","star"),"A star network")
  expect_match(gloss("bloop","star"),"bloop")
  expect_match(print_glossary(),"A star network")
  expect_no_match(print_glossary(),"bloop")
})

test_that("glossary returns italicised text for unknown terms", {
  expect_equal(gloss("bloop"), "<em>bloop</em>")
  expect_equal(gloss("bloop", "blap"), "<em>bloop</em>")
  expect_no_match(print_glossary(), "bloop")
})

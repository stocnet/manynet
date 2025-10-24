test_that("glossary works", {
  expect_match(gloss("star"),"A star network")
  expect_match(gloss("bloop","star"),"A star network")
  expect_match(gloss("bloop","star"),"bloop")
  expect_match(print_glossary(),"A star network")
  expect_no_match(print_glossary(),"bloop")
})

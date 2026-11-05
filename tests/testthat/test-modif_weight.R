# Term frequency-inverse document frequency

terms <- matrix(c(2, 1, 0, 0,
                  1, 0, 0, 0,
                  0, 1, 0, 3), 3, byrow = TRUE,
                dimnames = list(c("d1","d2","d3"), c("a","b","c","e")))

test_that("to_normalised weights terms by tfidf", {
  out <- to_normalised(terms, "tfidf")
  # a is two thirds of d1's terms, and is in two of the three documents
  expect_equal(out["d1", "a"], 2/3 * log(3/2))
  expect_equal(out["d3", "e"], 0.75 * log(3))
  # a term that no document uses stays 0
  expect_true(all(out[, "c"] == 0))
})

test_that("tfidf weighs a term that every document uses at 0", {
  everywhere <- cbind(terms, f = 1)
  expect_true(all(to_normalised(everywhere, "tfidf")[, "f"] == 0))
})

test_that("tfidf takes the documents from the columns where asked", {
  expect_equal(to_normalised(terms, "tfidf", across = "columns"),
               t(to_normalised(t(terms), "tfidf")))
})

test_that("tfidf keeps the class and records the transformation", {
  out <- to_normalised(as_tidygraph(terms), "tfidf")
  expect_s3_class(out, "tbl_graph")
  expect_equal(as_matrix(out), to_normalised(terms, "tfidf"))
  sn <- to_normalised(as_stocnet(terms), "tfidf")
  expect_s3_class(sn, "stocnet")
  expect_match(describe_transformations(sn), "tfidf")
})

test_that("tfidf needs a two-mode network", {
  expect_error(to_normalised(ison_adolescents, "tfidf"), "two-mode")
})

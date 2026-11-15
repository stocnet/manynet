test_that("to_twomode returns a two-mode network as it is", {
  expect_equal(to_twomode(ison_southern_women), ison_southern_women)
  g <- as_igraph(ison_southern_women)
  expect_identical(to_twomode(g), g)
  expect_equal(to_twomode(as_matrix(ison_southern_women)),
               as_matrix(ison_southern_women))
})

# Projecting a network of three modes onto two

works <- make_stocnet(
  nodes = data.frame(label = c("a1","a2","a3","w1","w2","k1","k2"),
                     mode = c(rep("authors", 3), rep("works", 2),
                              rep("keywords", 2))),
  ties = data.frame(from = c("a1","a2","a2","a3","w1","w2","w2","a1"),
                    to = c("w1","w1","w2","w2","k1","k1","k2","a2"),
                    layer = c(rep("authored", 4), rep("tagged", 3), "knows")),
  info = list(modes = c("authors", "works", "keywords")))

test_that("to_twomode ties authors to the keywords of their works", {
  out <- to_twomode(works, through = "works")
  expect_true(is_twomode(out))
  expect_equal(mode_names(out), c("authors", "keywords"))
  # a2 wrote w1 (k1) and w2 (k1 and k2), so k1 counts twice
  expect_equal(as_matrix(out),
               matrix(c(1, 2, 1, 0, 1, 1), 3,
                      dimnames = list(c("a1","a2","a3"), c("k1","k2"))))
  # the tie between two authors and the layers are not kept
  expect_null(out$info$layers)
  expect_equal(to_twomode(works, through = 2), out)
})

test_that("to_twomode multiplies the weights of the two ties", {
  valued <- works
  valued$ties$weight <- c(2, 1, 1, 1, 3, 1, 1, 1)
  out <- as_matrix(to_twomode(valued, through = "works"))
  # a1 to w1 (2) times w1 to k1 (3); a2 adds 1 x 3 and 1 x 1
  expect_equal(out[, "k1"], c(a1 = 6, a2 = 4, a3 = 1))
})

test_that("to_twomode through keeps the class it was given", {
  expect_s3_class(to_twomode(as_igraph(works), through = "works"), "igraph")
  expect_s3_class(to_twomode(as_tidygraph(works), through = "works"),
                  "tbl_graph")
  expect_equal(as_matrix(to_twomode(as_igraph(works), through = "works")),
               as_matrix(to_twomode(works, through = "works")))
})

test_that("to_twomode through must leave two modes", {
  expect_error(to_twomode(works, through = c("works", "keywords")),
               "leave two modes")
  expect_error(to_twomode(ison_southern_women, through = "events"),
               "leave two modes")
})

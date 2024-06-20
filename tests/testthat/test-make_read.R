# Test read family of functions

test_that("read_edgelist works", {
  expect_equal(read_edgelist(testthat::test_path("sheets", "test.xlsx")),
               dplyr::tibble(From = c(1, 2, 2),
                             To = c(2, 1, 3),
                             Weight = c(1, 2, 3)))
  expect_equal(read_edgelist(testthat::test_path("sheets", "testCSVComma.csv"),
                             sv = "comma"),
               data.frame(From = c(1, 2, 2),
                             To = c(2, 1, 3),
                             Weight = c(1, 2, 3)))
  expect_equal(read_edgelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
                             sv = "semi-colon"),
               data.frame(From = c(1, 2, 2),
                             To = c(2, 1, 3),
                             Weight = c(1, 2, 3)))
  expect_error(read_edgelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
                             sv = "SomethingWrong"))
})

test_that("write_edgelist works", {
  file <-  tempfile() # Create file
  file2 <- tempfile() # Create file
  edgelisttest <- as_igraph(dplyr::tibble(from = c(1, 2, 2),
                                          to = c(2, 1, 3),
                                          weight = c(1, 2, 3)))
  write_edgelist(edgelisttest,
                 filename = file)
  expect_equal(read.csv(file),
               data.frame(from = c(1, 2, 2),
                             to = c(2, 1, 3),
                             weight = c(1, 2, 3)))
  write_edgelist(filename = file2)
  expect_equal(read.csv(file2), data.frame(from = c("A", "B", "C"),
                                           to = c("B", "C", "A"),
                                           weight = c(1.1, 11, 110)))
  on.exit(unlink(file)) # Unlink file
  on.exit(unlink(file2)) #Unlink file
})

test_that("read_nodelist works", {
  expect_equal(read_nodelist(testthat::test_path("sheets", "test.xlsx")),
               dplyr::tibble(From = c(1, 2, 2),
                             To = c(2, 1, 3),
                             Weight = c(1, 2, 3)))
  expect_equal(read_nodelist(testthat::test_path("sheets", "testCSVComma.csv"),
                             sv = "comma"),
               data.frame(From = c(1, 2, 2),
                          To = c(2, 1, 3),
                          Weight = c(1, 2, 3)))
  expect_equal(read_nodelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
                             sv = "semi-colon"),
               data.frame(From = c(1, 2, 2),
                          To = c(2, 1, 3),
                          Weight = c(1, 2, 3)))
  expect_error(read_nodelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
                             sv = "SomethingWrong"))
})

test_that("write_nodelist works", {
  file <-  tempfile() # Create file
  file2 <- tempfile() # Create file
  nodelisttest <- data.frame(data.frame(from = c("A", "B", "C"),
                                        to = c("B", "A", "A")))
  nodelisttest <- igraph::set_vertex_attr(as_igraph(nodelisttest),
                                          name = "type",
                                          value =c(FALSE, FALSE, TRUE) )
  write_nodelist(nodelisttest,
                 filename = file)
  expect_equal(read.csv(file),
               data.frame(name = c("A", "B", "C"),
                          type = c(FALSE, FALSE, TRUE)))
  write_nodelist(filename = file2)
  expect_equal(read.csv(file2),
               data.frame(type = c(FALSE, FALSE, TRUE),
                          name = c("A", "B", "C")))
  on.exit(unlink(file)) # Unlink file
  on.exit(unlink(file2)) # Unlink file
})

test_that("read_pajek and write_pajek works", {
  testpaj <- read_pajek(testthat::test_path("sheets", "SouthernWomen.paj"))
  expect_true(is.tbl_graph(testpaj))
  edgetest <- as_edgelist(testpaj)
  expect_equal(head(edgetest$from),
               head(as_edgelist(testpaj)$from))
  file <-  tempfile() # Create file
  write_pajek(as_igraph(testpaj), file)
  testpaj2 <- read_pajek(file)
  expect_true(is.tbl_graph(testpaj2))
  edgetest2 <- as_edgelist(testpaj2)
  # Note, the igraph::write.graph forgets names.
  expect_equal(head(edgetest2$from), as.character(rep(1, 6)))
  on.exit(unlink(file)) # Unlink file
})

test_that("read_ucinet and write_ucinet works", {
  testuci <- read_ucinet(testthat::test_path("sheets", "ucinettest.##h"))
  expect_true(is.tbl_graph(testuci))
  expect_equal(nrow(as_edgelist(testuci)), 78)
  expect_equal(ncol(as_edgelist(testuci)), 2)
  expect_equal(igraph::vertex_attr(as_igraph(testuci), "name"), NULL)
  expect_error(read_ucinet(testthat::test_path("sheets", "ucinettest")))
  expect_error(read_ucinet(testthat::test_path("sheets", "ucinettest1.##h")))
  file <-  tempfile() # Create file
  write_ucinet(as_tidygraph(testuci), file)
  testuci2 <- read_ucinet(paste0(file, ".##h"))
  expect_true(is.tbl_graph(testuci2))
  edgetest2 <- as_edgelist(testuci2)
  # Note, the write ucinet function forgets certain attributes
  expect_equal(length(edgetest2$from), length(as_edgelist(testuci2)$from))
  on.exit(unlink(file)) # Unlink file
})

test_that("read_graphml and write_graphml works", {
  testgml <- read_graphml(testthat::test_path("sheets", "nassau.graphml"))
  expect_true(is.tbl_graph(testgml))
  expect_equal(nrow(as_edgelist(testgml)), 15598)
  expect_equal(ncol(as_edgelist(testgml)), 17)
  expect_length(net_node_attributes(testgml), 8)
  expect_length(net_tie_attributes(testgml), 15)
  expect_error(read_graphml(testthat::test_path("sheets", "nassau")))
  path <-  tempfile() # Create file
  write_graphml(testgml, path)
  testgml2 <- suppressWarnings(read_graphml(paste0(path)))
  expect_true(is.tbl_graph(testgml2))
  edgegml <- as_edgelist(testgml2)
  expect_length(edgegml$from, length(as_edgelist(testgml)$from))
  on.exit(unlink(path)) # Unlink file
})

test_that("read_ and write_ matrix works", {
  Abruzzo <- Campania <- Calabria <- Puglia <- NULL
  Abruzzo <- c(1, 0.76, 0.8, 0.90)
  Campania <- c(0.76, 1, 0.62, 0.69)
  Calabria <- c(0.80, 0.62, 1, 0.85)
  Puglia <- c(0.90, 0.69, 0.85, 1)
  out <-data.frame(Abruzzo, Campania, Calabria, Puglia)
  row.names(out) <- c('Abruzzo','Campania', 'Calabria', 'Puglia')
  expect_equal(as_matrix(read_matrix(testthat::test_path("sheets", "test_matrix.csv"))),
               as.matrix(out))
  # file <-  tempfile() # Create file
  # write_matrix(out, filename = file)
  # expect_equal(as_matrix(read_matrix(file)),
  #              as.matrix(out))
  # on.exit(unlink(file)) # Unlink file
})

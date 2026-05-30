# Tests for arrange_nodes and other new symmetric manip functions

test_that("arrange_nodes.stocnet reorders nodes and updates tie indices", {
  # Create a stocnet with known structure
  snet <- make_stocnet(
    nodes = data.frame(label = c("C", "A", "B")),
    ties = data.frame(from = c("C", "A", "B"), 
                      to = c("A", "B", "C"))
  )
  # Before arrange: nodes are C(1), A(2), B(3)
  # Tie from=1->2, from=2->3, from=3->1
  expect_equal(snet$nodes$label, c("C", "A", "B"))
  expect_equal(snet$ties$from, c(1L, 2L, 3L))
  expect_equal(snet$ties$to, c(2L, 3L, 1L))
  

  # Arrange by label: A(1), B(2), C(3)
  result <- arrange_nodes(snet, label)
  expect_equal(result$nodes$label, c("A", "B", "C"))
  # Original C(1)->A(2) becomes C(3)->A(1)
  # Original A(2)->B(3) becomes A(1)->B(2)
  # Original B(3)->C(1) becomes B(2)->C(3)
  expect_true(all(c(1L, 2L, 3L) %in% result$ties$from))
  expect_true(all(c(1L, 2L, 3L) %in% result$ties$to))
  # Check specific mapping
  # Old node 1 (C) -> new position 3

  # Old node 2 (A) -> new position 1
  # Old node 3 (B) -> new position 2
  # So tie (1,2) -> (3,1), tie (2,3) -> (1,2), tie (3,1) -> (2,3)
  expect_equal(sort(paste(result$ties$from, result$ties$to)),
               sort(c("3 1", "1 2", "2 3")))
})

test_that("arrange_nodes.stocnet updates change indices", {
  snet <- make_stocnet(
    nodes = data.frame(label = c("C", "A", "B")),
    ties = data.frame(from = c("C", "A"), to = c("A", "B")),
    changes = data.frame(time = c(1, 2), 
                         node = c("C", "B"),
                         var = c("label", "label"),
                         value = c("C2", "B2"))
  )
  # Before: node "C" is index 1, "B" is index 3
  expect_equal(snet$changes$node, c(1L, 3L))
  
  # After arrange by label: A(1), B(2), C(3)
  result <- arrange_nodes(snet, label)
  # C was index 1, now index 3; B was index 3, now index 2
  expect_equal(result$changes$node, c(3L, 2L))
})

test_that("arrange_nodes.stocnet handles NULL ties and changes", {
  snet <- make_stocnet(
    nodes = data.frame(label = c("C", "A", "B"))
  )
  result <- arrange_nodes(snet, label)
  expect_equal(result$nodes$label, c("A", "B", "C"))
  expect_null(result$ties)
  expect_null(result$changes)
})

test_that("arrange_nodes.tbl_graph reorders nodes", {
  g <- tidygraph::tbl_graph(
    nodes = data.frame(name = c("C", "A", "B")),
    edges = data.frame(from = c(1, 2, 3), to = c(2, 3, 1))
  )
  result <- arrange_nodes(g, name)
  node_names <- as.data.frame(tidygraph::activate(result, nodes))$name
  expect_equal(node_names, c("A", "B", "C"))
})

# Tests for arrange_changes
test_that("arrange_changes.stocnet reorders changes", {
  snet <- make_stocnet(
    nodes = data.frame(label = c("A", "B", "C")),
    changes = data.frame(time = c(3, 1, 2),
                         node = c("A", "B", "C"),
                         var = c("label", "label", "label"),
                         value = c("A2", "B2", "C2"))
  )
  result <- arrange_changes(snet, time)
  expect_equal(result$changes$time, c(1, 2, 3))
})

# Tests for rename_changes
test_that("rename_changes.stocnet renames columns", {
  snet <- make_stocnet(
    nodes = data.frame(label = c("A", "B")),
    changes = data.frame(time = c(1, 2),
                         node = c("A", "B"),
                         var = c("label", "label"),
                         value = c("A2", "B2"))
  )
  result <- rename_changes(snet, wave = time)
  expect_true("wave" %in% names(result$changes))
  expect_false("time" %in% names(result$changes))
})

# Tests for select_info
test_that("select_info.stocnet selects info attributes", {
  snet <- make_stocnet(
    info = list(name = "Test", year = 2020, doi = "10.1234"),
    nodes = data.frame(label = c("A", "B"))
  )

  result <- select_info(snet, "name", "year")
  expect_equal(names(result$info), c("name", "year"))
  expect_null(result$info$doi)
})

# Tests for filter_info
test_that("filter_info.stocnet filters info attributes", {
  snet <- make_stocnet(
    info = list(name = "Test", year = 2020, doi = "10.1234"),
    nodes = data.frame(label = c("A", "B"))
  )
  result <- filter_info(snet, "name", "doi")
  expect_equal(sort(names(result$info)), c("doi", "name"))
  expect_null(result$info$year)
})

# Tests for rename_info
test_that("rename_info.stocnet renames info attributes", {
  snet <- make_stocnet(
    info = list(name = "Test", year = 2020),
    nodes = data.frame(label = c("A", "B"))
  )
  result <- rename_info(snet, title = "name")
  expect_true("title" %in% names(result$info))
  expect_false("name" %in% names(result$info))
  expect_equal(result$info$title, "Test")
})

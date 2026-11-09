# That play_mobility() runs and returns what it should
# is tested for the family in test-functional_makes.R. What it cannot test is
# a network in which some nodes start with no value, and the arguments below.
schelling_lattice <- function(){
  set.seed(1234)
  start <- stats::rbinom(100, 1, prob = 0.5)
  start[sample(seq_len(100), 20)] <- NA
  add_node_attribute(create_lattice(100), "start", start)
}

test_that("play_mobility runs where some nodes have no starting value", {
  lattice <- schelling_lattice()
  set.seed(1234)
  out <- play_mobility(lattice, "start", 0.5)
  expect_true(is_manynet(out))
  expect_equal(as.numeric(net_nodes(out)), 100)
  expect_equal(sum(is.na(node_attribute(out, "start"))), 20)
})

test_that("play_mobility moves the other attributes with the occupant", {
  lattice <- schelling_lattice()
  start <- node_attribute(lattice, "start")
  lattice <- add_node_attribute(lattice, "id", seq_len(100))
  lattice <- add_node_attribute(lattice, "colour",
                                ifelse(is.na(start), NA, paste0("c", seq_len(100))))
  set.seed(1234)
  out <- play_mobility(lattice, "start", 0.5)
  id <- node_attribute(out, "id")
  expect_false(identical(id, seq_len(100)))
  expect_identical(as.numeric(node_attribute(out, "start")),
                   as.numeric(start)[id])
  expect_identical(node_attribute(out, "colour"),
                   node_attribute(lattice, "colour")[id])
})

test_that("play_mobility keeps the labels of the places", {
  places <- add_node_attribute(to_labelled(create_ring(8)), "person",
                               c(LETTERS[1:7], NA))
  out <- play_mobility(places, "person", preference = function(.data, node) -1,
                       choice_function = "optimising", steps = 3)
  expect_identical(node_labels(out), node_labels(places))
  expect_false(identical(node_attribute(out, "person"),
                         node_attribute(places, "person")))
})

test_that("play_segregation is deprecated but gives the same result", {
  lattice <- schelling_lattice()
  set.seed(1234)
  expect_warning(out <- play_segregation(lattice, "start", 0.5), "play_mobility")
  # The result of play_segregation() in manynet 2.4.x for this seed
  expect_identical(which(is.na(node_attribute(out, "start"))),
                   c(1L, 17L, 26L, 28L, 34L, 36L, 41L, 42L, 51L, 57L, 61L, 68L,
                     71L, 76L, 81L, 84L, 86L, 92L, 96L, 98L))
  expect_identical(which(node_attribute(out, "start") == 1),
                   c(2L, 3L, 4L, 5L, 6L, 9L, 10L, 11L, 12L, 14L, 16L, 19L, 20L,
                     29L, 30L, 39L, 40L, 44L, 46L, 47L, 48L, 50L, 53L, 54L, 56L,
                     58L, 60L, 63L, 65L, 66L, 70L, 74L, 80L, 90L, 100L))
})

test_that("heterophily is a deprecated alias of preference", {
  lattice <- schelling_lattice()
  set.seed(1)
  expect_warning(old <- play_mobility(lattice, "start", heterophily = 0.5),
                 "preference")
  set.seed(1)
  new <- play_mobility(lattice, "start", preference = 0.5)
  expect_identical(node_attribute(old, "start"), node_attribute(new, "start"))
  expect_error(play_mobility(lattice, "start", heterophilly = 0.5),
               "heterophilly")
})

test_that("play_mobility takes a function as preference", {
  # No occupant is ever dissatisfied, so nobody moves
  lattice <- schelling_lattice()
  out <- play_mobility(lattice, "start", preference = function(.data, node) 1)
  expect_identical(node_attribute(out, "start"), node_attribute(lattice, "start"))
})

test_that("play_mobility moves occupants in the order of a function's scores", {
  skip_if_not_installed("netrics")
  # Node 5 is the hub, then nodes 3 and 4 have two ties each
  places <- as_stocnet(data.frame(from = c(5, 5, 5, 5, 5, 4),
                                  to = c(1, 2, 3, 4, 6, 3)))
  places <- add_node_attribute(places, "person", c(LETTERS[1:5], NA))
  unhappy <- function(.data, node) -1
  first <- play_mobility(places, "person", preference = unhappy,
                         who_moves = netrics::node_by_degree,
                         choice_function = "optimising", steps = 1)
  expect_identical(node_attribute(first, "person"),
                   c("A", "B", "C", "D", NA, "E"))
  second <- play_mobility(places, "person", preference = unhappy,
                          who_moves = netrics::node_by_degree,
                          choice_function = "optimising", steps = 2)
  expect_identical(node_attribute(second, "person"),
                   c("A", "B", NA, "D", "C", "E"))
  # Without the function, the first node moves first
  ordered <- play_mobility(places, "person", preference = unhappy,
                           choice_function = "optimising", steps = 1)
  expect_identical(node_attribute(ordered, "person"),
                   c(NA, "B", "C", "D", "E", "A"))
})

test_that("play_mobility checks the scores of a function in who_moves", {
  lattice <- schelling_lattice()
  expect_error(play_mobility(lattice, "start", who_moves = function(.data) 1),
               "one score for each node")
})

test_that("play_mobility takes a function as choice_function", {
  places <- add_node_attribute(create_ring(6), "person", c("A", "B", NA, "D", NA, NA))
  out <- play_mobility(places, "person", preference = function(.data, node) -1,
                       choice_function = function(.data) seq_len(6), steps = 1)
  expect_identical(node_attribute(out, "person"), c(NA, "B", NA, "D", NA, "A"))
})

test_that("play_mobility with max_dist = 1 makes a vacancy chain", {
  ladder <- delete_ties(create_ring(10, directed = TRUE), 10)
  ladder <- add_node_attribute(ladder, "person", c(LETTERS[1:9], NA))
  out <- play_mobility(ladder, "person", preference = function(.data, node) -1,
                       choice_function = "optimising", max_dist = 1)
  # Each occupant moves up to the next position, once
  expect_identical(node_attribute(out, "person"), c(NA, LETTERS[1:9]))
  # Without a limit, the first occupant jumps to the empty place
  free <- play_mobility(ladder, "person", preference = function(.data, node) -1,
                        choice_function = "optimising", steps = 1)
  expect_identical(node_attribute(free, "person"), c(NA, LETTERS[2:9], "A"))
  expect_error(play_mobility(ladder, "person", max_dist = 0), "max_dist")
})

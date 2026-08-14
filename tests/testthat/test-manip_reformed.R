match_net <- to_matching(ison_southern_women)

test_that("to_matching works with two-mode networks", {
  expect_values(net_ties(match_net), min(net_dims(ison_southern_women)))
})

test_that("to_matching is idempotent", {
  match_twice <- to_matching(match_net)
  expect_equal(as_edgelist(match_net), 
               as_edgelist(match_twice))
})


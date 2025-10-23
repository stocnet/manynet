test_that("node_in_component works", {
  comp <- ison_monks %>% to_uniplex("esteem") %>%
    node_in_component()
  expect_s3_class(comp, "node_member")
  expect_equal(length(unique(comp)), 
               c(net_components(to_uniplex(ison_monks, "esteem"))))
  expect_equal(length(unique(comp)), 
               length(unique(node_in_strong(to_uniplex(ison_monks, "esteem")))))
  comp <- ison_monks %>% to_uniplex("esteem") %>%
    to_undirected() %>% 
    node_in_component()
  expect_equal(length(unique(comp)), 
               length(unique(node_in_weak(to_uniplex(ison_monks, "esteem")))))
})

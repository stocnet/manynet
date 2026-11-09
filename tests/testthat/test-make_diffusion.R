
test_that("play_diffusion works for named networks", {
  expect_warning(named_SI <- play_diffusion(ison_adolescents, old_version = TRUE))
  expect_equal(named_SI$S + named_SI$I, named_SI$n)
  expect_equal(summary(named_SI)$t[1], 0)
  expect_equal(summary(named_SI)$nodes[1:4], c(1,2,3,5))
})

test_that("play_diffusion works for named networks with latency", {
  expect_warning(named_SEI <- play_diffusion(ison_adolescents, latency = 1, old_version = TRUE))
  expect_equal(named_SEI$S + named_SEI$E + named_SEI$I, named_SEI$n)
  expect_equal(summary(named_SEI)$t[1], 0)
  expect_equal(summary(named_SEI)$nodes[1:4], c(1,2,NA,NA))
})

test_that("play_diffusion plays on a stocnet", {
  played <- play_diffusion(irps_tribes, seeds = 1)
  expect_s3_class(played, "stocnet")
  report <- as_diffusion(played)
  expect_equal(report$time[1], 0)
  expect_equal(report$I[1], 1)
})

test_that("as_diffusion reports every step, also where nothing spreads", {
  # nothing can spread from an isolate, so only its seeding is recorded
  alone <- as_diffusion(play_diffusion(create_empty(4), seeds = 1))
  expect_equal(nrow(alone), 1)
  expect_equal(alone$I, 1)
  expect_equal(alone$S, 3)
  # the last step is reported, by which the whole ring is infected
  ring <- as_diffusion(play_diffusion(create_ring(8), seeds = 1))
  expect_equal(ring$I[nrow(ring)], 8)
  expect_equal(sum(ring$I_new), 8)
})

test_that("as_diffusion reports a diffusion alike whatever it was played on", {
  ring <- create_ring(8)
  set.seed(1234)
  graph <- as_diffusion(play_diffusion(ring, seeds = 1, recovery = 0.2))
  set.seed(1234)
  stoc <- as_diffusion(play_diffusion(as_stocnet(ring), seeds = 1,
                                      recovery = 0.2))
  expect_named(stoc, names(graph))
  expect_true(all(c("time", "S_new") %in% names(stoc)))
  expect_equal(as.data.frame(stoc), as.data.frame(graph), ignore_attr = TRUE)
  # other changes that a network records are not read as a diffusion
  expect_true("time" %in% names(as_diffusion(play_diffusion(create_filled(5),
                                                            seeds = 1))))
})

# Start state ####

test_that("play_diffusion starts from seeds where there is no state attribute", {
  set.seed(1)
  played <- play_diffusion(ison_adolescents, seeds = c("Betty", "Sue"),
                           steps = 2, immune = 5)
  expect_true(is.character(node_attribute(played, "diffusion")))
  expect_equal(node_attribute(played, "diffusion")[5], "R")
  report <- as_diffusion(played)
  expect_equal(report$R[1], 1)
})

test_that("play_diffusion reads seeds as labels or a logical attribute", {
  net <- ison_adolescents |> mutate_nodes(first = c(TRUE, rep(FALSE, 7)))
  set.seed(2); by_attr <- play_diffusion(net, seeds = "first")
  set.seed(2); by_index <- play_diffusion(net, seeds = 1)
  expect_equal(as_changelist(by_attr), as_changelist(by_index))
  set.seed(2); by_label <- play_diffusion(net, seeds = node_labels(net)[1])
  expect_equal(as_changelist(by_label), as_changelist(by_index))
  expect_error(play_diffusion(net, seeds = "Nobody"), "Nobody")
})

test_that("play_diffusion starts from an attribute on the nodes", {
  net <- create_ring(8) |> mutate_nodes(aware = c("yes", rep("no", 7)))
  played <- play_diffusion(net, "aware", steps = 3,
                           compartments = c(S = "no", I = "yes"))
  changes <- as_changelist(played)
  # no start rows: the attribute already holds the start
  expect_true(all(changes$time > 0))
  expect_true(all(changes$var == "aware"))
  expect_true(all(changes$value == "yes"))
  expect_equal(played$info$compartments, list(S = "no", I = "yes"))
  # the same diffusion as in S and I, for a seed
  set.seed(3); own <- play_diffusion(net, "aware", steps = 3, recovery = 0.3,
                                     compartments = c(S = "no", I = "yes", R = "over"))
  set.seed(3); seir <- play_diffusion(create_ring(8), seeds = 1, steps = 3,
                                      recovery = 0.3)
  seir_changes <- as_changelist(seir)
  seir_changes <- seir_changes[seir_changes$time > 0, ]
  expect_equal(as_changelist(own)$node, seir_changes$node)
  expect_equal(as_changelist(own)$value,
               c(yes = "yes", over = "over")[c(I = "yes", R = "over")[seir_changes$value]],
               ignore_attr = TRUE)
})

test_that("play_diffusion reads a logical attribute as I and S", {
  net <- create_ring(6) |> mutate_nodes(adopted = c(TRUE, rep(FALSE, 5)))
  played <- play_diffusion(net, "adopted", steps = 2)
  expect_true(is.logical(unlist(as_changelist(played)$value)))
  expect_true(all(unlist(as_changelist(played)$value)))
})

test_that("play_diffusion continues a history from its last time", {
  net <- bind_changes(create_ring(8),
                      data.frame(time = c(1, 2), node = c(1, 2),
                                 var = "diffusion", value = "I"))
  played <- play_diffusion(net, steps = 2)
  changes <- as_changelist(played)
  expect_equal(changes$time[1:2], c(1, 2))
  expect_equal(max(changes$time), 4)
  expect_false(any(duplicated(changes$node[changes$value == "I"])))
})

test_that("play_diffusion gives an error for a state it cannot start from", {
  net <- create_ring(4) |> mutate_nodes(status = c("a", "b", "a", "a"))
  expect_error(play_diffusion(net, "status"), "status")
  expect_error(play_diffusion(net, "status", seeds = 1,
                              compartments = c(S = "a", I = "b")), "seeds")
  expect_error(play_diffusion(net, "status", recovery = 0.2,
                              compartments = c(S = "a", I = "b")), "R")
  expect_error(play_diffusion(net, "status", compartments = c(X = "a")), "X")
})

test_that("play_diffusion maps missing states to S", {
  net <- create_ring(5) |> mutate_nodes(diffusion = c("I", NA, "S", "S", "S"))
  played <- play_diffusion(net, steps = 1)
  expect_true(2 %in% as_changelist(played)$node)
})

test_that("play_diffusion runs an SI model with only S and I", {
  played <- play_diffusion(create_ring(6), steps = 3,
                           compartments = c(S = "unaware", I = "adopter"))
  expect_true(all(as_changelist(played)$value == "adopter"))
  report <- as_diffusion(played)
  expect_true(all(c("unaware", "adopter") %in% names(report)))
  expect_equal(report$adopter[1], 1)
  expect_gt(report$adopter[nrow(report)], 1)
})

test_that("play_diffusion starts at the first time of the network", {
  expect_equal(min(as_changelist(play_diffusion(create_ring(5), steps = 2))$time), 0)
  classes <- delete_changes(ison_classmates)
  expect_warning(played <- play_diffusion(classes, steps = 2), "time of the ties")
  expect_equal(min(as_changelist(played)$time), 1)
  expect_equal(as_diffusion(played)$time[1], 1)
  potter <- suppressWarnings(play_diffusion(fict_potter, steps = 2))
  expect_equal(min(as_changelist(potter)$time), 1)
})

# Interface ####

test_that("play_diffusion names the argument a misspelt one was meant to be", {
  expect_error(play_diffusion(ison_adolescents, trans = 0.3), "transmissibility")
  expect_error(play_diffusion(ison_adolescents, transmisibility = 0.3), 
               "transmissibility")
  expect_error(play_diffusion(ison_adolescents, seed = 1), "seeds")
  expect_error(play_diffusion(ison_adolescents, trans = 0.3),
               "is not an argument of", class = "rlang_error")
})

test_that("play_diffusion moves an old positional seeds with a warning", {
  set.seed(5); expect_warning(old <- play_diffusion(ison_adolescents, 5), "seeds")
  set.seed(5); new <- play_diffusion(ison_adolescents, seeds = 5)
  expect_equal(as_changelist(old), as_changelist(new))
})

test_that("play_diffusion does not yet take transitions", {
  expect_error(play_diffusion(ison_adolescents, transitions = data.frame()),
               "not yet available")
})

test_that("play_diffusion warns for the old version with snet_warn", {
  expect_warning(play_diffusion(create_ring(4), old_version = TRUE), "new version")
})

# Fixes ####

test_that("play_diffusion does not let the dead infect", {
  set.seed(6)
  played <- play_diffusion(create_ring(10), fatality = 1, steps = 5)
  changes <- as_changelist(played)
  expect_equal(sum(changes$var == "diffusion"), 1)
  expect_equal(sum(changes$var == "active"), 1)
})

test_that("play_diffusion runs on a list of waves, also past the last wave", {
  waves <- to_waves(ison_monks)
  played <- play_diffusion(waves, steps = length(waves) + 3)
  expect_true(is.list(played))
  expect_length(played, length(waves))
  expect_s3_class(as_diffusion(played[[1]]), "diffusionModel")
  # each wave is in force for one step: no ties in the first, all in the second
  played <- play_diffusion(list(create_empty(4), create_filled(4)), steps = 2)
  expect_equal(as_changelist(played[[1]])$time, c(0, 2, 2, 2))
})

test_that("play_diffusion adds background adoption only where asked", {
  set.seed(7); none <- play_diffusion(create_empty(10), steps = 3)
  expect_equal(nrow(as_changelist(none)), 1)
  set.seed(7); some <- play_diffusion(create_empty(10), background = 0.5, steps = 3)
  expect_gt(nrow(as_changelist(some)), 1)
})

# Records ####

test_that("as_diffusion counts each value of the attribute", {
  net <- ison_lawfirm
  played <- play_diffusion(net, "status", steps = 2,
                           compartments = c(S = "associate", I = "partner"))
  report <- as_diffusion(played, attribute = "status")
  expect_s3_class(report, "diffusionModel")
  expect_s3_class(report, "diff_model")
  expect_true(all(c("associate", "partner") %in% names(report)))
  expect_equal(report$associate + report$partner, report$n)
  expect_equal(report$partner[1], sum(node_attribute(net, "status") == "partner"))
  # the report keeps the roles of its values, for a plot to read
  expect_equal(attr(report, "attribute"), "status")
  expect_equal(attr(report, "compartments"), list(S = "associate", I = "partner"))
  expect_equal(attr(as_diffusion(play_diffusion(create_ring(4))), "compartments"),
               list(S = "S", E = "E", I = "I", R = "R"))
})

test_that("as_diffusion counts a missing start state as susceptible", {
  net <- create_ring(6) |>
    mutate_nodes(leaning = c("Liberal", "Conservative", "Moderate", NA,
                             "Moderate", "Conservative"))
  played <- play_diffusion(net, "leaning", steps = 3,
                           compartments = list(S = c("Conservative", "Moderate"),
                                               I = "Liberal"))
  report <- as_diffusion(played, "leaning")
  expect_equal(report$Conservative + report$Moderate + report$Liberal, report$n)
})

test_that("bind_changes makes the start of any state variable", {
  net <- bind_changes(create_filled(4),
                      data.frame(time = 1, node = 1, var = "status", value = "I"))
  expect_equal(node_attribute(net, "status"), rep("S", 4))
  net <- bind_changes(create_filled(4),
                      data.frame(time = 1, node = 1, var = "status", value = "yes"),
                      compartments = c(S = "no", I = "yes"))
  expect_equal(node_attribute(net, "status"), rep("no", 4))
  expect_equal(net$info$compartments, list(S = "no", I = "yes"))
  # play_diffusion() reads the compartments that the network keeps
  played <- play_diffusion(net, "status", steps = 1)
  expect_true(all(as_changelist(played)$value == "yes"))
})

# Exposure ####

test_that("the exposure kernel agrees with the old exposure", {
  old_exposure <- function(net, mark){
    net <- as_tidygraph(net)
    if(is_weighted(net) || is_signed(net)){
      mk <- rep(FALSE, net_nodes(net)); mk[mark] <- TRUE
      out <- as_matrix(net)
      return(unname(colSums(out * matrix(mk, nrow(out), ncol(out)) *
                              matrix(!mk, nrow(out), ncol(out), byrow = TRUE))))
    }
    contacts <- unlist(lapply(igraph::neighborhood(net, nodes = mark, mode = "out"),
                              function(x) setdiff(x, mark)))
    out <- rep(0, net_nodes(net))
    tab <- table(contacts)
    out[as.numeric(names(tab))] <- unname(tab)
    out
  }
  set.seed(8)
  nets <- list(undirected = generate_random(12, 0.3),
               directed = generate_random(12, 0.3, directed = TRUE),
               weighted = mutate_ties(create_ring(8), weight = 1:8),
               signed = irps_wwi)
  for(net in nets){
    mark <- c(1, 3)
    expect_equal(c(manynet:::.node_exposure(net, mark)), old_exposure(net, mark),
                 ignore_attr = TRUE)
  }
  two <- ison_southern_women
  expect_equal(c(manynet:::.node_exposure(two, 1)),
               c(manynet:::.node_exposure_twomode(as_tidygraph(two), 1)))
})

test_that("exposure follows the direction of the ties", {
  chain <- create_explicit(A -+ B, B -+ C)
  mat <- as_matrix(chain)
  expect_equal(manynet:::.exposure_kernel(mat, 2, "in"), c(0, 0, 1))
  expect_equal(manynet:::.exposure_kernel(mat, 2, "out"), c(1, 0, 0))
  expect_equal(manynet:::.exposure_kernel(mat, 2, "all"), c(1, 0, 1))
  # only the nodes that an infected node sends ties to are exposed
  expect_equal(c(manynet:::.node_is_exposed(chain, 2)), c(FALSE, FALSE, TRUE),
               ignore_attr = TRUE)
})

test_that("a cognitive social structure exposes as its aggregated structure", {
  css <- ison_hightech
  agg <- to_aggregated(css, over = "by", rule = "min", reporters = "both")
  expect_equal(manynet:::.exposure_matrix(css), manynet:::.exposure_matrix(agg))
  expect_equal(as.data.frame(as_diffusion(css)), as.data.frame(as_diffusion(agg)))
  set.seed(9); on_css <- play_diffusion(css, steps = 3)
  set.seed(9); on_agg <- play_diffusion(agg, steps = 3)
  expect_equal(as_changelist(on_css), as_changelist(on_agg))
})

test_that("missing ties count as absent for exposure", {
  mat <- matrix(c(0, NA, 1, NA, 0, 0, 1, 0, 0), 3)
  net <- as_tidygraph(mat)
  expect_false(anyNA(manynet:::.exposure_matrix(net)))
})

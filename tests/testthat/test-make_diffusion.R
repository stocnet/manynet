
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
  # the diffusion follows the waves, so it no longer warns
  expect_no_warning(played <- play_diffusion(classes, steps = 2))
  expect_equal(min(as_changelist(played)$time), 1)
  expect_equal(as_diffusion(played)$time[1], 1)
  potter <- play_diffusion(fict_potter, steps = 2)
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

test_that("play_diffusion checks the columns of transitions", {
  expect_error(play_diffusion(ison_adolescents, transitions = data.frame()),
               "from")
  expect_error(play_diffusion(ison_adolescents,
                              transitions = data.frame(from = "S", to = "I",
                                                       rate = 1, speed = 2)),
               "speed")
})

test_that("play_diffusion warns for the old version with snet_warn", {
  expect_warning(play_diffusion(create_ring(4), old_version = TRUE), "new version")
})

# Fixes ####

test_that("play_diffusion does not let the dead infect", {
  # the seed dies in the first step, before it can infect anyone
  set.seed(6)
  played <- play_diffusion(create_ring(10), fatality = 1, steps = 5)
  changes <- as_changelist(played)
  expect_equal(sum(changes$var == "diffusion"), 1)
  expect_equal(sum(changes$var == "active"), 1)
  # the report counts the dead seed in D, and not in I
  report <- as_diffusion(played)
  expect_equal(report$D[nrow(report)], 1)
  expect_equal(report$I[nrow(report)], 0)
  expect_equal(sum(report$D_new), 1)
})

test_that("as_diffusion counts the nodes that die as D", {
  set.seed(1)
  played <- play_diffusion(ison_networkers, seeds = 1:3, recovery = 0.1,
                           fatality = 0.1, steps = 30)
  report <- as_diffusion(played)
  expect_true(all(c("D", "D_new") %in% names(report)))
  values <- c("S", "E", "I", "R", "D")
  expect_true(all(rowSums(as.data.frame(report)[, values]) == report$n))
  expect_equal(sum(report$D_new), report$D[nrow(report)])
  expect_equal(report$D[nrow(report)],
               sum(as_changelist(played)$var == "active"))
  expect_equal(attr(report, "compartments")$D, "D")
  # the same on an igraph
  expect_equal(as_diffusion(as_igraph(played))$D, report$D)
  # a value of its own for D
  set.seed(2)
  played <- play_diffusion(ison_networkers, seeds = 1:3, recovery = 0.1,
                           fatality = 0.1, steps = 30,
                           compartments = c(S = "healthy", I = "sick",
                                            R = "immune", D = "deceased"))
  report <- as_diffusion(played)
  expect_gt(max(report$deceased), 0)
  expect_equal(report$healthy + report$sick + report$immune + report$deceased,
               report$n)
  # without fatality, a node that becomes inactive has not died
  set.seed(3)
  net <- as_stocnet(create_ring(6))
  net <- mutate_nodes(net, active = TRUE)
  net <- bind_changes(net, data.frame(time = 1, node = 4L, var = "active",
                                      value = FALSE))
  report <- as_diffusion(play_diffusion(net, steps = 3))
  expect_false("D" %in% names(report))
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
})

test_that("two-mode exposure goes through co-affiliation or direct ties", {
  two <- ison_southern_women
  mat <- manynet:::.exposure_matrix(two)
  n1 <- nrow(mat)
  # marks in one mode expose the nodes of that mode that share an event
  shared <- c(mat[, mat[1, ] != 0, drop = FALSE] %*% rep(1, sum(mat[1, ] != 0))) > 0
  shared[1] <- FALSE
  expo <- c(manynet:::.node_exposure(two, 1))
  expect_equal(expo[seq_len(n1)] > 0, shared, ignore_attr = TRUE)
  expect_true(all(expo[-seq_len(n1)] == 0))
  # marks in both modes expose the nodes that they are tied to
  both <- c(manynet:::.node_exposure(two, c(1, n1 + 1)))
  expect_equal(both[n1 + 2], as.numeric(mat[1, 2] != 0), ignore_attr = TRUE)
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

# Time and composition (phase 3) ####

# A chain whose ties are each observed in one wave, at times 0, 4 and 8.
# As an event stream, each tie is an increment at its time.
timed_chain <- function(stream = FALSE){
  net <- mutate_ties(create_explicit(A-B, B-C, C-D), time = c(0, 4, 8))
  if(stream) net <- mutate_ties(net, increment = 1)
  net
}

diffusion_times <- function(net){
  ch <- as_changelist(net)
  ch <- ch[ch$var == "diffusion", , drop = FALSE]
  stats::setNames(as.numeric(ch$time), node_labels(net)[ch$node])
}

test_that("play_diffusion uses the ties of each wave of a panel", {
  net <- timed_chain()
  expect_true(is_longitudinal(net))
  played <- play_diffusion(net, seeds = "A", steps = 11)
  # B is reached in the wave at 0, C only once the wave at 4 holds B-C,
  # and D only once the wave at 8 holds C-D
  expect_equal(diffusion_times(played), c(A = 0, B = 1, C = 5, D = 9))
})

test_that("waves at times 0, 4 and 8 give four steps for each wave", {
  timeline <- manynet:::.diffusion_contacts(timed_chain(), 0, 11)
  expect_equal(timeline$times, c(0, 4, 8))
  expect_length(timeline$contacts, 3)
  waves <- vapply(0:11, function(t)
    manynet:::.diffusion_wave(timeline, t, 0), numeric(1))
  expect_equal(waves, rep(1:3, each = 4))
  # after the last wave, the last wave stays in force
  expect_equal(manynet:::.diffusion_wave(timeline, 20, 0), 3)
})

test_that("a list of networks is a panel only with the same nodes", {
  expect_error(play_diffusion(list(create_ring(4), create_ring(5)), steps = 2),
               "same nodes")
})

test_that("an event stream uses only the ties that have happened", {
  net <- timed_chain(stream = TRUE)
  expect_true(is_dynamic(net))
  early <- play_diffusion(net, seeds = "A", steps = 3)
  expect_equal(diffusion_times(early), c(A = 0, B = 1))
  late <- play_diffusion(net, seeds = "A", steps = 11)
  expect_equal(diffusion_times(late), c(A = 0, B = 1, C = 5, D = 9))
})

test_that("an interval tie transmits only while it is active", {
  net <- mutate_ties(create_explicit(A-B, A-C), begin = c(0, 3), end = c(10, 4))
  played <- play_diffusion(net, seeds = "A", steps = 8)
  times <- diffusion_times(played)
  expect_equal(unname(times["B"]), 1)
  expect_gt(unname(times["C"]), 3)
})

test_that("an inactive node never changes state and does not transmit", {
  set.seed(11)
  net <- mutate_nodes(create_filled(6), active = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))
  played <- play_diffusion(net, steps = 5)
  ch <- as_changelist(played)
  expect_false(3 %in% ch$node[ch$var == "diffusion"])
  # the centre of a star is the only path, so an inactive centre stops it
  star <- mutate_nodes(create_star(5), active = c(FALSE, rep(TRUE, 4)))
  ch <- as_changelist(play_diffusion(star, seeds = 2, steps = 5))
  expect_equal(ch$node[ch$var == "diffusion"], 2)
})

test_that("a node that becomes active later takes part from then on", {
  net <- as_stocnet(create_star(3))
  net <- mutate_nodes(net, active = c(TRUE, TRUE, FALSE))
  net <- bind_changes(net, data.frame(time = 3, node = 3L, var = "active",
                                      value = TRUE))
  played <- play_diffusion(net, steps = 6)
  times <- diffusion_times(played)
  expect_equal(unname(times[2]), 1)
  expect_equal(unname(times[3]), 4)
})

test_that("a stocnet keeps the transitions of its diffusion", {
  played <- play_diffusion(as_stocnet(create_ring(5)), steps = 2, recovery = 0.5)
  expect_s3_class(played$info$transitions, "diffusionTransitions")
  expect_true(all(c("I", "R") %in% played$info$transitions$to))
})

test_that("a two-mode diffusion of one mode goes through co-affiliation", {
  set.seed(12)
  played <- play_diffusion(ison_southern_women, steps = 10)
  ch <- as_changelist(played)
  ch <- ch[ch$var == "diffusion", , drop = FALSE]
  expect_s3_class(ch$time, "Date")
  # no node is infected twice, and only women are infected
  expect_false(anyDuplicated(ch$node) > 0)
  expect_true(all(ch$node <= 18))
  expect_true(all(is.na(node_attribute(played, "diffusion")[19:32])))
  # a woman is infected only once an event that she shares with Evelyn
  # has happened (the first, event 23, on 25 February)
  expect_true(all(ch$time[ch$node != 1] > as.Date("1936-02-25")))
  # the report counts only the women, keeps the dates, and prints
  report <- as_diffusion(played)
  expect_equal(unique(report$S + report$I), 18)
  expect_s3_class(report$time, "Date")
  expect_output(print(report), "1936")
})

test_that("a state on the second mode diffuses on that mode", {
  sw <- mutate_nodes(ison_southern_women,
                     flu = c(rep(NA, 18), "I", rep("S", 13)))
  ch <- as_changelist(play_diffusion(sw, "flu"))
  ch <- ch[ch$var == "flu", , drop = FALSE]
  expect_gt(nrow(ch), 0)
  expect_true(all(ch$node > 18))
})

test_that("a state on both modes diffuses along their ties", {
  played <- play_diffusion(ison_southern_women, seeds = c(1, 19), steps = 3)
  ch <- as_changelist(played)
  ch <- ch[ch$var == "diffusion" & !ch$node %in% c(1, 19), , drop = FALSE]
  # Evelyn infects an event she attends, once it has happened
  expect_true(all(ch$node > 18))
})

test_that("data that change in time run for their whole period", {
  # waves at 0, 4 and 8: the last wave gets a step
  played <- play_diffusion(timed_chain(), seeds = "A")
  expect_equal(max(as.numeric(as_changelist(played)$time)), 9)
  # a list of three waves gives three steps
  three <- list(contacts = list(1, 2, 3), times = NULL)
  expect_equal(manynet:::.diffusion_period(three, NULL, create_ring(3),
                                           "diffusion", 0, 3), 3)
  # the dates of the changes of ison_southern_women set the period
  ch <- as_changelist(play_diffusion(ison_southern_women))
  expect_equal(max(ch$time), as.Date("1936-11-21"))
})

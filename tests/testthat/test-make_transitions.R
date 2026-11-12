# The engine of states, transitions and hazards (phase 2)

# Mean final size and peak time over many runs ####

# The targets come from the engine before phase 2, over 800 runs on
# ison_dolphins with 15 steps (calculated on 9 November 2026).
expect_like_before <- function(args, final, peak, sd_final, sd_peak, reps = 100){
  set.seed(42)
  runs <- replicate(reps, {
    d <- suppressMessages(as_diffusion(do.call(play_diffusion,
                                               c(list(ison_dolphins, steps = 15), args))))
    c(final = sum(d$I_new), peak = d$time[which.max(d$I)])
  })
  expect_lt(abs(mean(runs["final", ]) - final), 4 * sd_final / sqrt(reps))
  expect_lt(abs(mean(runs["peak", ]) - peak), 4 * sd_peak / sqrt(reps))
}

test_that("old-argument calls give the old behaviour statistically", {
  skip_on_cran()
  expect_like_before(list(transmissibility = 0.3), 59.02, 14.46, 2.41, 0.80)
  expect_like_before(list(transmissibility = 0.1, background = 0.05),
                     52.97, 14.70, 3.05, 0.58)
  expect_like_before(list(seeds = 1:6, transmissibility = 0.4, thresholds = 2),
                     46.67, 14.65, 4.47, 0.69)
  expect_like_before(list(transmissibility = 0.3, recovery = 0.2),
                     33.80, 5.91, 21.45, 4.27)
  expect_like_before(list(transmissibility = 0.4, recovery = 0.2, fatality = 0.1),
                     31.91, 5.40, 22.77, 4.41)
})

test_that("a node that recovers in a step does not infect in that step", {
  # with recovery = 1, the seed recovers before it can infect anyone
  played <- play_diffusion(create_ring(8), recovery = 1, steps = 3)
  changes <- as_changelist(played)
  expect_equal(unlist(changes$value), c("I", "R"))
})

test_that("a node stays infected for 1/recovery steps on average", {
  set.seed(1)
  net <- create_empty(2000) |> mutate_nodes(diffusion = "I")
  played <- play_diffusion(net, recovery = 0.25, steps = 60)
  changes <- as_changelist(played)
  expect_equal(mean(changes$time), 4, tolerance = 0.05)
})

# Competing risks ####

test_that("competing risks give destination shares in proportion to the hazards", {
  set.seed(2)
  net <- create_empty(5000) |> mutate_nodes(state = "A")
  compete <- data.frame(from = c("A", "A", "B", "C"), to = c("B", "C", "B", "C"),
                        rate = c(0.2, 0.6, 0, 0))
  played <- play_diffusion(net, "state", transitions = compete, steps = 1)
  moved <- unlist(as_changelist(played)$value)
  expect_equal(length(moved) / 5000, 1 - exp(-0.8), tolerance = 0.03)
  expect_equal(mean(moved == "B"), 0.25, tolerance = 0.05)
})

test_that("rows with the same from and to add their hazards", {
  set.seed(3)
  net <- create_empty(5000) |> mutate_nodes(state = "A")
  twice <- data.frame(from = c("A", "A"), to = c("B", "B"), rate = c(0.1, 0.3))
  played <- play_diffusion(net, "state", transitions = twice, steps = 1)
  expect_equal(nrow(as_changelist(played)) / 5000, 1 - exp(-0.4),
               tolerance = 0.03)
})

# The table ####

test_that("the shortcuts compile into the default table", {
  sir <- make_transitions(transmissibility = 0.3, recovery = 0.1)
  expect_s3_class(sir, "diffusionTransitions")
  expect_equal(sir$from, c("S", "I"))
  expect_equal(sir$to, c("I", "R"))
  expect_equal(unlist(sir$rate), -log(1 - c(0.3, 0.1)))
  expect_equal(sir$source, list("I", NULL))
  seir <- make_transitions(transmissibility = 1, latency = 0.5, background = 0.1)
  expect_equal(seir$to, c("E", "E", "I"))
  expect_equal(seir$rate[[1]], Inf)
  expect_output(print(seir), "threshold")
})

test_that("the trial: an SIR from the shortcuts or from its table", {
  set.seed(1)
  short <- play_diffusion(ison_adolescents, seeds = 1, steps = 10,
                          transmissibility = 0.3, recovery = 0.1)
  set.seed(1)
  table <- play_diffusion(ison_adolescents, seeds = 1, steps = 10,
                          transitions = make_transitions(transmissibility = 0.3,
                                                         recovery = 0.1))
  expect_equal(as_changelist(short), as_changelist(table))
})

test_that("the trial: the voter model on irps_blogs", {
  voter <- data.frame(from = c("Liberal", "Conservative"),
                      to = c("Conservative", "Liberal"), rate = 1,
                      source = c("Conservative", "Liberal"), exposure = "voter")
  set.seed(4)
  blogs <- play_diffusion(irps_blogs, "Leaning", transitions = voter, steps = 2,
                          direction = "out")
  report <- as_diffusion(blogs, "Leaning")
  expect_true(all(c("Liberal", "Conservative") %in% names(report)))
  expect_true(all(report$Liberal + report$Conservative == 1490))
  expect_true(nrow(as_changelist(blogs)) > 0)
})

test_that("the trial: a spontaneous transition, by a number or an attribute", {
  set.seed(5)
  promote <- data.frame(from = "associate", to = "partner", rate = 0.1, source = NA)
  law <- play_diffusion(ison_lawfirm, "status", transitions = promote, steps = 10)
  report <- as_diffusion(law, "status")
  expect_true(all(diff(report$associate) <= 0))
  expect_equal(unique(unlist(as_changelist(law)$value)), "partner")
  # the rate as the name of a node attribute: only those with a rate move
  lawfirm <- mutate_nodes(ison_lawfirm, promotion = ifelse(seniority > 4, Inf, 0))
  law2 <- play_diffusion(lawfirm, "status", steps = 1,
                         transitions = data.frame(from = "associate", to = "partner",
                                                  rate = "promotion"))
  moved <- as_changelist(law2)$node
  expect_true(all(node_attribute(lawfirm, "seniority")[moved] > 4))
  expect_setequal(moved, which(node_attribute(lawfirm, "status") == "associate" &
                                 node_attribute(lawfirm, "seniority") > 4))
})

test_that("the trial: a joint source as text or as a list", {
  seir <- data.frame(from = c("S", "E", "I"), to = c("E", "I", "R"),
                     rate = c(0.4, 0.5, 0.2), source = c("E+I", NA, NA))
  expect_equal(make_transitions(seir)$source[[1]], c("E", "I"))
  expect_output(print(make_transitions(seir)), "E\\+I")
  listed <- seir
  listed$source <- list(c("E", "I"), NA, NA)
  expect_equal(make_transitions(listed)$source, make_transitions(seir)$source)
  # only the exposed infect: E alone is a source too
  net <- create_ring(6) |> mutate_nodes(diffusion = c("E", rep("S", 5)))
  played <- play_diffusion(net, transitions = data.frame(from = c("S", "E", "I"),
                                                         to = c("E", "I", "R"),
                                                         rate = c(Inf, 0, 0),
                                                         source = c("E+I", NA, NA)),
                           steps = 1)
  expect_setequal(as_changelist(played)$node, c(2, 6))
  # a value "E+I" and also values E and I is an error
  odd <- data.frame(from = c("S", "E+I", "E", "I"), to = c("E", "I", "I", "R"),
                    rate = 0.1, source = c("E+I", NA, NA, NA))
  expect_error(make_transitions(odd), "list")
})

test_that("the trial: the three exposure models", {
  # node 1 has three infected contacts, node 5 one infected contact of two
  adj <- matrix(0, 6, 6)
  adj[cbind(c(2, 3, 4, 4, 6), c(1, 1, 1, 5, 5))] <- 1
  net <- as_tidygraph(adj + t(adj)) |>
    mutate_nodes(diffusion = c("S", "I", "I", "I", "S", "S"))
  hazard <- function(model, threshold = 1){
    tab <- make_transitions(data.frame(from = c("S", "I"), to = c("I", "I"),
                                       rate = c(0.5, 0), source = c("I", NA),
                                       exposure = c(model, NA),
                                       threshold = c(threshold, NA)))
    begin <- list(dead_value = NULL, template = "S")
    mat <- .exposure_matrix(net)
    engine <- .transitions_engine(tab, net, 6, list(mat), "all", FALSE)
    set.seed(1)
    replicate(4000, {
      step <- .transitions_step(engine, node_attribute(net, "diffusion"), 1, 0, begin)
      c(1, 5) %in% step$moves
    }) |> rowMeans()
  }
  expect_equal(hazard("threshold"), rep(1 - exp(-0.5), 2), tolerance = 0.05)
  expect_equal(hazard("cascade"), 1 - exp(-0.5 * c(3, 1)), tolerance = 0.05)
  expect_equal(hazard("voter"), 1 - exp(-0.5 * c(3/3, 1/2)), tolerance = 0.05)
  # a threshold of two contacts gates node 5 out
  expect_equal(hazard("threshold", 2), c(1 - exp(-0.5), 0), tolerance = 0.05)
})

test_that("the trial: background adds to the contagion", {
  tab <- make_transitions(transmissibility = 0.2, background = 0.05)
  expect_equal(tab$from, c("S", "S"))
  expect_equal(tab$to, c("I", "I"))
  expect_null(tab$source[[2]])
  set.seed(6)
  played <- play_diffusion(create_empty(4000), seeds = 1, steps = 1,
                           transmissibility = 0.2, background = 0.05)
  expect_equal((nrow(as_changelist(played)) - 1) / 3999, 0.05, tolerance = 0.1)
})

test_that("prevalence lowers the threshold", {
  # nodes 7 and 10 have one infected contact, and need two
  net <- create_ring(10)
  none <- play_diffusion(net, seeds = 1:6, thresholds = 2, steps = 1)
  expect_equal(nrow(as_changelist(none)), 6)
  # with 60% infected, the threshold falls to 2 * (1 - 0.6) = 0.8
  some <- play_diffusion(net, seeds = 1:6, thresholds = 2, prevalence = 1,
                         steps = 1)
  changes <- as_changelist(some)
  expect_setequal(changes$node[changes$time == 1], c(7, 10))
})

test_that("rate can be a function of the state at each step", {
  calls <- 0
  rising <- function(state, exposure, time, .data){
    calls <<- calls + 1
    ifelse(exposure > 0, time, 0)
  }
  tab <- data.frame(from = c("S", "I"), to = c("I", "I"), rate = c(NA, 0),
                    source = c("I", NA))
  tab$rate <- list(rising, 0)
  expect_output(print(make_transitions(tab)), "<function>")
  played <- play_diffusion(create_ring(8), transitions = tab, steps = 4)
  # at time 0 the hazard is 0, so nobody moves in the first step
  changes <- as_changelist(played)
  expect_true(all(changes$time[changes$node != 1] >= 2))
  expect_equal(calls, 4)
  bad <- tab
  bad$rate <- list(function(...) -1, 0)
  expect_error(play_diffusion(create_ring(8), transitions = bad, steps = 2),
               "hazard")
})

test_that("a table gives errors for values that it does not use", {
  wrong <- data.frame(from = "Gryffindor", to = "Slytherin", rate = 0.1,
                      source = "Slytherin")
  expect_error(play_diffusion(fict_potter, "house", transitions = wrong),
               "rate = 0")
  expect_warning(play_diffusion(ison_adolescents, recovery = 0.5, steps = 2,
                                transitions = make_transitions(transmissibility = 0.5)),
                 "recovery")
  expect_error(suppressWarnings(play_diffusion(ison_adolescents, old_version = TRUE,
                                               transitions = make_transitions())),
               "old_version")
  expect_error(make_transitions(data.frame(from = "S", to = "I", rate = -1)),
               "hazard")
  expect_error(make_transitions(data.frame(from = "S", to = "I", rate = 1,
                                           source = "I", exposure = "simple")),
               "simple")
})

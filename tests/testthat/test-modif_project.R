# Concept lattices

# Every intent is the intersection of some subset of the rows, the empty subset
# giving the set of all attributes, so listing every subset finds them all.
brute_intents <- function(X){
  n <- nrow(X)
  out <- list(rep(TRUE, ncol(X)))
  for(s in seq_len(2^n - 1)){
    rows <- which(bitwAnd(s, 2^(seq_len(n) - 1)) > 0)
    out[[length(out) + 1]] <- apply(X[rows, , drop = FALSE], 2, all)
  }
  unique(do.call(rbind, out))
}
intent_keys <- function(I) sort(apply(I * 1, 1, paste, collapse = ""))

test_that("to_concepts() finds every concept", {
  set.seed(1)
  for(r in 1:20){
    X <- matrix(stats::runif(8 * 6) < 0.4, 8, 6)
    expect_identical(intent_keys(.concept_intents(X)),
                     intent_keys(brute_intents(X)))
  }
})

test_that("to_concepts() finds the ties directly beneath each concept", {
  # every pair of three objects shares one attribute, which gives the
  # lattice of all subsets of three: 8 concepts and 12 ties
  pairs <- matrix(c(1,1,0, 0,1,1, 1,0,1), 3, 3, byrow = TRUE,
                  dimnames = list(c("x","y","z"), c("a","b","c")))
  out <- to_concepts(pairs)
  expect_equal(dim(out), c(8, 8))
  expect_equal(sum(out), 12)
  # three objects with one attribute each give a diamond: 5 concepts, 6 ties
  diamond <- diag(3)
  dimnames(diamond) <- list(c("x","y","z"), c("a","b","c"))
  out <- to_concepts(diamond)
  expect_equal(dim(out), c(5, 5))
  expect_equal(sum(out), 6)
  # the ties agree with comparing every pair of extents
  set.seed(2)
  for(r in 1:10){
    X <- matrix(stats::runif(12 * 7) < 0.4, 12, 7)
    I <- .concept_intents(X)
    E <- (X * 1) %*% t(I * 1) ==
      matrix(rowSums(I), nrow(X), nrow(I), byrow = TRUE)
    below <- crossprod(E * 1) ==
      matrix(colSums(E), ncol(E), ncol(E), byrow = TRUE)
    diag(below) <- FALSE
    expected <- below & !((below * 1) %*% (below * 1) > 0)
    found <- matrix(FALSE, ncol(E), ncol(E))
    found[as.matrix(.concept_covers(X, E, I))] <- TRUE
    expect_identical(found, expected)
  }
})

test_that("to_concepts() labels each node and affiliation once", {
  out <- to_concepts(as_tidygraph(ison_southern_women))
  expect_true(is_directed(out))
  expect_true(is_acyclic(out))
  nodes <- node_attribute(out, "name")
  expect_false(any(duplicated(nodes)))
  labels <- unlist(strsplit(gsub("[{}]", "", nodes), ", | "))
  labels <- labels[!grepl("^C[0-9]+$", labels)]
  expect_setequal(labels, c(node_names(ison_southern_women)))
  expect_false(any(duplicated(labels)))
  # the top concept holds every woman
  expect_length(node_attribute(out, "extent")[[1]], 18)
  expect_equal(unname(node_attribute(out, "extent_size")[1]), 18)
  expect_equal(net_name(out), "Concept lattice of Southern Women Data")
})

test_that("to_concepts() returns the class it was given", {
  expect_s3_class(to_concepts(ison_southern_women), "stocnet")
  expect_s3_class(to_concepts(as_igraph(ison_southern_women)), "igraph")
  expect_s3_class(to_concepts(as_network(ison_southern_women)), "network")
  expect_true(is.matrix(to_concepts(as_matrix(ison_southern_women))))
})

test_that("to_concepts() dichotomises valued data with a warning", {
  valued <- matrix(c(2,1,0, 0,3,1), 2, 3, byrow = TRUE,
                   dimnames = list(c("x","y"), c("a","b","c")))
  expect_warning(out <- to_concepts(valued), "dichotomised")
  expect_equal(out, to_concepts((valued > 0) * 1))
})

test_that("to_concepts() treats a one-mode network by its out-neighbours", {
  out <- to_concepts(ison_adolescents)
  expect_true(is_directed(out))
  expect_true(is_acyclic(out))
  expect_false(any(duplicated(node_names(out))))
})

# Projection rules that read every tie

# The statements of each speaker about each concept, counted by stance.
stance_counts <- function(net, stance){
  ties <- net$ties[net$ties$weight == stance, ]
  n <- sum(!node_is_mode(net))
  k <- net_nodes(net) - n
  unclass(table(factor(ties$from, seq_len(n)), factor(ties$to - n, seq_len(k))))
}
no_diag <- function(m){ diag(m) <- 0; unname(m) }

test_that("agreement, disagreement, and signed agree with hand counts", {
  P <- stance_counts(irps_nuclear, 1)
  N <- stance_counts(irps_nuclear, -1)
  agree <- no_diag(P %*% t(P) + N %*% t(N))
  disagree <- no_diag(P %*% t(N) + N %*% t(P))
  expect_equal(unname(as_matrix(to_mode(irps_nuclear, 1, "agreement"))), agree)
  expect_equal(unname(as_matrix(to_mode(irps_nuclear, 1, "disagreement"))), disagree)
  expect_equal(unname(as_matrix(to_mode(irps_nuclear, 1, "signed"))),
               agree - disagree)
  # "count" reads no stance, so it is the two together
  expect_equal(unname(as_matrix(to_mode(irps_nuclear, 1))), agree + disagree)
})

test_that("the signed rule gives a signed projection", {
  out <- to_mode(irps_nuclear, "speakers", "signed")
  expect_true(any(tie_weights(out) < 0))
  expect_true(is_signed(out))
})

stance <- matrix(c(1, -1, 0,
                   1,  1, 1,
                  -1, -1, 1), 3, byrow = TRUE,
                 dimnames = list(c("x","y","z"), c("a","b","c")))

test_that("the stance rules count pairs of statements in a matrix", {
  # x and y agree on a, and disagree on b; x and z disagree on a, agree on b
  expect_equal(to_mode1(stance, "agreement")["x", ], c(x = 0, y = 1, z = 1))
  expect_equal(to_mode1(stance, "disagreement")["x", ], c(x = 0, y = 1, z = 1))
  expect_equal(to_mode1(stance, "signed")["y", ], c(x = 0, y = 0, z = -1))
  # the same from the ties of a network as from its matrix, which needs a
  # fourth column to be read as two-mode rather than as a square one-mode
  wide <- cbind(stance, d = c(1, 0, 0))
  expect_equal(as_matrix(to_mode1(as_igraph(wide), "signed")),
               to_mode1(wide, "signed"))
})

test_that("the stance rules weigh stances on a longer scale by proximity", {
  # stances of 2 and 1, or of -2 and -1, on a scale from -2 to 2 are a
  # quarter of it apart
  scaled <- matrix(c(2, 1, -2, -1), 2,
                   dimnames = list(c("x","y"), c("a","b")))
  expect_equal(to_mode1(scaled, "agreement")["x", "y"], 0.75 + 0.75)
  expect_equal(to_mode1(scaled, "disagreement")["x", "y"], 0.25 + 0.25)
})

authorship <- matrix(c(1,1,0,1, 0,1,1,1, 1,0,0,1), 4,
                     dimnames = list(c("a","b","c","d"), c("w1","w2","w3")))

test_that("fractional counting gives each work a total of 1 for each author", {
  out <- to_mode1(authorship, "fractional")
  expect_equal(out["a", "b"], 1/2)
  expect_equal(out["a", "d"], 1/2 + 1)
  # each author's total is the number of works it wrote with someone else
  expect_equal(rowSums(out), rowSums(authorship))
  # a work of one author adds nothing
  solo <- cbind(authorship, w4 = c(1, 0, 0, 0))
  expect_equal(to_mode1(solo, "fractional"), out)
})

test_that("association strength compares the count with independence", {
  out <- to_mode1(authorship, "association")
  count <- to_mode1(authorship)
  s <- rowSums(authorship)
  expect_equal(out["a", "b"], 3 * count["a", "b"] / (s["a"] * s["b"]),
               ignore_attr = TRUE)
  expect_equal(out["b", "c"], 1.5)
})

test_that("activity divides the count by the average activity of the pair", {
  out <- to_mode1(authorship, "activity")
  expect_equal(out["a", "d"], 2 / ((2 + 3)/2))
  # an author with no works is tied to no one
  idle <- rbind(authorship, e = 0)
  expect_equal(unname(to_mode1(idle, "activity")["e", ]), rep(0, 5))
})

test_that("the rules that read every tie work for every class", {
  ref <- to_mode2(authorship, "fractional")
  for(cl in c("igraph", "tidygraph", "network", "stocnet")){
    out <- to_mode2(get(paste0("as_", cl))(authorship), "fractional")
    expect_equal(as_matrix(out)[colnames(ref), colnames(ref)], ref,
                 info = cl)
  }
})

# Projecting through a mode of a network of three or more

works <- make_stocnet(
  nodes = data.frame(label = c("a1","a2","a3","w1","w2","k1","k2"),
                     mode = c(rep("authors", 3), rep("works", 2),
                              rep("keywords", 2))),
  ties = data.frame(from = c("a1","a2","a2","a3","w1","w2","w2","a1"),
                    to = c("w1","w1","w2","w2","k1","k1","k2","a2"),
                    layer = c(rep("authored", 4), rep("tagged", 3), "knows")),
  info = list(modes = c("authors", "works", "keywords")))

test_that("to_mode projects through all the other modes by default", {
  # w1 and w2 share the author a2 and the keyword k1
  expect_equal(as_matrix(to_mode(works, "works"))["w1", "w2"], 2)
  expect_equal(to_mode(works, "works"),
               to_mode(works, "works", through = c("authors", "keywords")))
  # authors have no ties to keywords, so only works tie them
  expect_equal(to_mode(works, "authors"),
               to_mode(works, "authors", through = "works"))
  expect_error(to_mode(works, "authors", through = "authors"), "other than")
  expect_error(to_mode(works, 1, through = c(2, 1)), "other than")
})

test_that("to_mode projects one mode of three through another", {
  co <- to_mode(works, "authors", through = "works")
  expect_equal(net_nodes(co), 3)
  # the tie between a1 and a2 that is not through a work is left out
  expect_equal(as_matrix(co)["a1", ], c(a1 = 0, a2 = 1, a3 = 0))
  kw <- to_mode(works, "keywords", through = 2)
  expect_equal(as_matrix(kw)["k1", "k2"], 1)
  expect_equal(as_matrix(to_mode(works, 3, "jaccard", through = "works")),
               as_matrix(to_mode(works, "keywords", "jaccard", "works")))
})

test_that("to_mode projects through a mode for other classes too", {
  expect_s3_class(to_mode(as_igraph(works), 1, through = 2), "igraph")
  expect_s3_class(to_mode(as_tidygraph(works), 1, through = 2), "tbl_graph")
})

test_that("to_mode takes through for a two-mode network where it names the other", {
  expect_equal(to_mode(ison_southern_women, "women", through = "events"),
               to_mode(ison_southern_women, "women"))
  expect_error(to_mode(ison_southern_women, 1, through = 1), "other than")
})

test_that("count adds the products of weights and of repeated ties", {
  weighted <- make_stocnet(
    nodes = data.frame(label = c("a", "b", "w", "v"),
                       mode = c("authors", "authors", "works", "works")),
    ties = data.frame(from = c(1L, 2L, 1L, 2L), to = c(3L, 3L, 4L, 4L),
                      weight = c(0.6, 0.4, 1, 1)),
    info = list(modes = c("authors", "works")))
  expect_equal(as_matrix(to_mode(weighted, "authors"))["a", "b"], 1.24)
  expect_equal(as_matrix(to_mode(weighted, "works"))["w", "v"], 1)
  expect_equal(as_matrix(to_mode1(as_igraph(weighted)))["a", "b"], 1.24)
  repeated <- make_stocnet(
    nodes = data.frame(label = c("a", "b", "w"),
                       mode = c("authors", "authors", "works")),
    ties = data.frame(from = c(1L, 1L, 2L), to = c(3L, 3L, 3L)),
    info = list(modes = c("authors", "works")))
  expect_equal(as_matrix(to_mode(repeated, "authors"))["a", "b"], 2)
  # a simple network still gives the shared nodes
  expect_equal(as_matrix(to_mode(ison_southern_women, 1))["Evelyn", "Laura"], 6)
})

# Mobility ####

# Four people and three places. Ann moves twice, Bob once, Cat never,
# and Dan moves and comes back.
mobility_panel <- make_stocnet(
  info = list(directed = FALSE, update = "replace"),
  nodes = data.frame(label = c("Ann", "Bob", "Cat", "Dan", "X", "Y", "Z"),
                     mode = rep(c("people", "places"), c(4, 3)),
                     size = c(NA, NA, NA, NA, 10, 20, 30)),
  ties = data.frame(from = rep(1:4, each = 3),
                    to = c(5, 6, 7,  5, 5, 6,  7, 7, 7,  6, 5, 6),
                    time = rep(1:3, 4)))
mobility_interval <- make_stocnet(
  info = list(directed = FALSE),
  nodes = mobility_panel$nodes,
  ties = data.frame(from = c(1, 1, 1, 2, 2, 3, 4, 4, 4),
                    to = c(5, 6, 7, 5, 6, 7, 6, 5, 6),
                    begin = c(1, 2, 3, 1, 3, 1, 1, 2, 3),
                    end = c(2, 3, NA, 3, NA, NA, 2, 3, NA)))

test_that("to_mobility gives one tie for each move", {
  for(net in list(mobility_panel, mobility_interval)){
    out <- to_mobility(net)
    expect_equal(net_nodes(out), 3)
    expect_true(is_directed(out))
    expect_equal(out$nodes$label, c("X", "Y", "Z"))
    expect_equal(out$nodes$size, c(10, 20, 30))
    ties <- out$ties
    expect_equal(nrow(ties), 5)
    expect_equal(ties$person, c("Ann", "Ann", "Bob", "Dan", "Dan"))
    expect_equal(ties$time, c(2, 3, 3, 2, 3))
    # each origin is the place where the person was before
    expect_equal(ties$from, c(1, 2, 1, 2, 1))
    expect_equal(ties$to, c(2, 3, 2, 1, 2))
    weighted <- to_mobility(net, weighted = TRUE)
    expect_equal(sum(weighted$ties$weight), 5)
    expect_equal(nrow(weighted$ties), 3)
  }
})

test_that("to_mobility returns the class it was given", {
  expect_s3_class(to_mobility(mobility_panel), "stocnet")
  expect_s3_class(to_mobility(as_tidygraph(mobility_panel)), "tbl_graph")
  expect_s3_class(to_mobility(as_igraph(mobility_panel)), "igraph")
  expect_equal(as.numeric(net_ties(to_mobility(as_igraph(mobility_panel)))), 5)
})

test_that("to_mobility stops when a person holds two places at one time", {
  twice <- make_stocnet(
    info = list(directed = FALSE),
    nodes = data.frame(label = c("Ann", "X", "Y"),
                       mode = c("people", "places", "places")),
    ties = data.frame(from = c(1, 1), to = c(2, 3),
                      begin = c(1, 2), end = c(3, 4)))
  expect_error(to_mobility(twice), "one place at a time")
  twice_panel <- make_stocnet(
    info = list(directed = FALSE, update = "replace"),
    nodes = twice$nodes,
    ties = data.frame(from = c(1, 1), to = c(2, 3), time = c(1, 1)))
  expect_error(to_mobility(twice_panel), "one place at a time")
  expect_error(to_mobility(ison_adolescents), "two modes")
})

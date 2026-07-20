# to_motifs() is unlike the other to_*() functions: rather than modifying the
# network passed to it, it returns a named list of small reference networks
# (one per motif). Its `.data` argument accepts either a network (whose size,
# direction, and signedness are inferred) or a plain integer (for teaching).
# The to_*() functional harness already checks that it returns an acceptable
# (list-of-networks) output across fixtures/classes; here we check the motifs.

# Helper: every element is a manynet network with `nodes` nodes.
expect_motif_list <- function(out, nodes) {
  expect_type(out, "list")
  expect_true(length(out) >= 1)
  expect_false(is.null(names(out)))
  expect_true(all(nzchar(names(out))))
  for (m in out) {
    expect_true(is_manynet(m))
    expect_equal(as.numeric(net_nodes(m)), nodes)
  }
}

test_that("to_motifs() returns a list of networks, not a single network", {
  out <- to_motifs(2)
  expect_false(is_manynet(out))
  expect_motif_list(out, 2)
})

test_that("to_motifs() accepts a plain number of nodes", {
  expect_named(to_motifs(3), c("Empty", "Edge", "Path", "Triangle"))
})

test_that("to_motifs() can take .data only, n only, or both", {
  triad <- c("Empty", "Edge", "Path", "Triangle")
  # n only, whether named or a bare first argument
  expect_named(to_motifs(n = 3), triad)
  expect_identical(names(to_motifs(n = 3)), names(to_motifs(3)))
  # .data only: n is inferred from the network's size
  expect_named(to_motifs(create_ring(3)), triad)
  expect_named(to_motifs(create_ring(8)), names(to_motifs(4))) # 8 -> clamp to 4
  # both: the network fixes the kind, n chooses the size (dyadic/triadic/...)
  expect_named(to_motifs(create_ring(8), n = 2), c("Null", "M"))
  expect_named(to_motifs(create_ring(8), n = 3), triad)
  # a directed network with an explicit n picks the directed motifs of that size
  expect_named(to_motifs(generate_random(8, directed = TRUE), n = 2),
               c("Null", "Asymmetric", "Mutual"))
})

test_that("to_motifs() errors when neither .data nor n is given", {
  expect_error(to_motifs(), "network|number of nodes|`n`")
})

test_that("to_motifs() undirected motifs have the expected sizes and names", {
  expect_named(to_motifs(2), c("Null", "M"))
  expect_motif_list(to_motifs(2), 2)

  out3 <- to_motifs(3)
  expect_named(out3, c("Empty", "Edge", "Path", "Triangle"))
  expect_motif_list(out3, 3)

  out4 <- to_motifs(4)
  expect_motif_list(out4, 4)
  expect_length(out4, 11)
})

test_that("to_motifs() directed motifs cover the dyad and triad census", {
  out2 <- to_motifs(2, directed = TRUE)
  expect_named(out2, c("Null", "Asymmetric", "Mutual"))
  expect_motif_list(out2, 2)
  expect_true(all(vapply(out2, is_directed, logical(1))))

  out3 <- to_motifs(3, directed = TRUE)
  expect_length(out3, 16) # the 16 MAN triad types
  expect_motif_list(out3, 3)
  expect_true("300" %in% names(out3))
})

test_that("to_motifs() falls back to the largest implemented size", {
  # snet_info() is silent under the default quiet verbosity, so assert the
  # clamped return value rather than the message.
  out <- to_motifs(5, directed = TRUE)
  expect_motif_list(out, 3)
  expect_length(out, 16)
  out <- to_motifs(6)
  expect_motif_list(out, 4)
  expect_length(out, 11)
})

test_that("to_motifs() announces the fall-back when not quiet", {
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old))
  expect_message(to_motifs(5, directed = TRUE), "n=3")
  expect_message(to_motifs(6), "n=4")
})

test_that("to_motifs() infers direction from a network passed as .data", {
  di <- to_motifs(generate_random(4, directed = TRUE))
  expect_named(di, c("003", "012", "102", "021D", "021U", "021C",
                     "111D", "111U", "030T", "030C", "201",
                     "120D", "120U", "120C", "210", "300"))
})

test_that("to_motifs() works across object classes for the same network", {
  net <- create_ring(6)                 # one-mode, undirected, 6 nodes -> n=4
  ref <- names(to_motifs(net))
  # igraph, matrix, and network preserve undirectedness; a bare edgelist
  # cannot encode it, so is inferred as directed (a coercion property, not a
  # to_motifs() one), and so is checked only for returning a valid motif list.
  for (obj in list(as_igraph(net), as_matrix(net), as_network(net))) {
    expect_named(to_motifs(obj), ref)
  }
  expect_motif_list(to_motifs(as_edgelist(net)), 3)
})

test_that("to_motifs() returns the bmotif bipartite motifs up to four nodes", {
  # Named by bmotif dictionary IDs (Simmons et al. 2019).
  for (tm in list(to_motifs(c(4, 6)), to_motifs(ison_southern_women))) {
    expect_named(tm, as.character(1:7))
    expect_true(all(vapply(tm, is_twomode, logical(1))))
    # node counts: motif 1 = 2, motifs 2-3 = 3, motifs 4-7 = 4
    expect_equal(vapply(tm, function(g) as.numeric(net_nodes(g)), numeric(1)),
                 c(`1` = 2, `2` = 3, `3` = 3, `4` = 4,
                   `5` = 4, `6` = 4, `7` = 4))
    # tie counts distinguish the four-node motifs: stars/path = 3, K22 = 4
    expect_equal(vapply(tm, function(g) as.numeric(net_ties(g)), numeric(1)),
                 c(`1` = 1, `2` = 2, `3` = 2, `4` = 3,
                   `5` = 3, `6` = 4, `7` = 3))
    # motif 5 is the 2x2 path (degrees 1,1,2,2), motif 6 the K22 (all degree 2),
    # distinguishing them from the four-node stars (motifs 4 and 7: 3,1,1,1)
    deg <- function(g) sort(unname(igraph::degree(as_igraph(g))))
    expect_equal(deg(tm[["4"]]), c(1, 1, 1, 3))
    expect_equal(deg(tm[["5"]]), c(1, 1, 2, 2))
    expect_equal(deg(tm[["6"]]), c(2, 2, 2, 2))
    expect_equal(deg(tm[["7"]]), c(1, 1, 1, 3))
  }
})

test_that("to_motifs() does not (yet) return signed two-mode motifs", {
  expect_null(to_motifs(c(4, 6), signed = TRUE))
})

# Signed motifs --------------------------------------------------------------

test_that("to_motifs() enumerates signed undirected motifs", {
  out2 <- to_motifs(2, signed = TRUE)
  expect_named(out2, c("Null", "+", "-"))
  expect_motif_list(out2, 2)
  expect_equal(unname(tie_signs(out2[["+"]])), 1)
  expect_equal(unname(tie_signs(out2[["-"]])), -1)

  out3 <- to_motifs(3, signed = TRUE)
  expect_named(out3, c("Empty", "+", "-", "++", "+-", "--",
                       "+++", "++-", "+--", "---"))
  expect_motif_list(out3, 3)
  expect_true(all(vapply(out3[-1], is_signed, logical(1))))
  expect_equal(sum(tie_signs(out3[["+++"]])), 3)
  expect_equal(sum(tie_signs(out3[["---"]])), -3)
  expect_equal(sort(unname(tie_signs(out3[["+--"]]))), c(-1, -1, 1))
})

test_that("to_motifs() infers signedness from a network passed as .data", {
  net <- to_signed(create_ring(4))
  out <- to_motifs(net)
  expect_named(out, c("Empty", "+", "-", "++", "+-", "--",
                      "+++", "++-", "+--", "---"))
})

test_that("to_motifs() enumerates the six signed directed dyads", {
  out <- to_motifs(2, directed = TRUE, signed = TRUE)
  expect_named(out, c("Null", "Asymmetric+", "Asymmetric-",
                      "Mutual++", "Mutual--", "Mutual+-"))
  expect_motif_list(out, 2)
  expect_true(all(vapply(out, is_directed, logical(1))))
  # Null has no ties; asymmetric dyads one arc; mutual dyads two arcs
  expect_equal(as.numeric(net_ties(out[["Null"]])), 0)
  expect_equal(as.numeric(net_ties(out[["Asymmetric+"]])), 1)
  expect_equal(as.numeric(net_ties(out[["Mutual++"]])), 2)
  # signs match the labels
  expect_equal(unname(tie_signs(out[["Asymmetric+"]])), 1)
  expect_equal(unname(tie_signs(out[["Asymmetric-"]])), -1)
  expect_equal(sum(tie_signs(out[["Mutual++"]])), 2)
  expect_equal(sum(tie_signs(out[["Mutual--"]])), -2)
  expect_equal(sort(unname(tie_signs(out[["Mutual+-"]]))), c(-1, 1))
})

test_that("to_motifs() signed directed motifs are inferred from a network", {
  net <- to_signed(generate_random(2, directed = TRUE, p = 1))
  out <- to_motifs(net)
  expect_named(out, c("Null", "Asymmetric+", "Asymmetric-",
                      "Mutual++", "Mutual--", "Mutual+-"))
})

test_that("to_motifs() clamps signed directed motifs to n=2", {
  out <- to_motifs(4, directed = TRUE, signed = TRUE)
  expect_named(out, c("Null", "Asymmetric+", "Asymmetric-",
                      "Mutual++", "Mutual--", "Mutual+-"))
  expect_motif_list(out, 2)
})

test_that("to_motifs() clamps signed motifs to the largest implemented size", {
  out <- to_motifs(5, signed = TRUE)
  expect_motif_list(out, 3)
  expect_length(out, 10)
})

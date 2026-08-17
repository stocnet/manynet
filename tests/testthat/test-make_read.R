# Test read family of functions

read_funs <- funs_objs[grepl("read_", names(funs_objs))]
write_funs <- funs_objs[grepl("write_", names(funs_objs))]
formats <- c(names(read_funs), names(write_funs))
formats <- gsub("read_|write_", "", formats)
formats <- formats[duplicated(formats)]
formats <- formats[!grepl("^graph$", formats)]

for(fm in formats) {
  test_that(paste("read and write", fm, "are compatible"), {
    skip_if(grepl("nodelist|pajek|gdf|dynetml", fm),
            message = "nodelist and pajek read/write not tested yet; gdf and dynetml require node ids, so unlabelled networks gain names on round-trip")
    skip_if_not_installed("readxl")
    file <-  tempfile() # Create file
    write_funs[[paste0("write_", fm)]](create_ring(5), filename = file)
    expect_equal(as_matrix(read_funs[[paste0("read_", fm)]](file)),
                 as_matrix(create_ring(5)))
    on.exit(unlink(file)) # Unlink file
  })
}

test_that("read_graphml reads all graphs and all key scopes", {
  skip_if_not_installed("xml2")
  nc <- read_graphml(testthat::test_path("sheets", "networkcanvas.graphml"))
  # both sessions are read, not just the first as igraph's reader does
  expect_equal(as.numeric(net_nodes(nc)), 6)
  expect_setequal(node_labels(nc),
                  c("Joshua", "Michael", "Jimbo", "Sarah", "Michelle", "Sophie"))
  # networkCanvasType is declared for="all", which igraph discards entirely
  expect_setequal(node_attribute(nc, "nodeset"),
                  c("Person", "Organisation", "ego"))
  # a stocnet object names the tie types of a multiplex network its layers
  expect_true("Friends" %in% tie_attribute(nc, "layer"))
  # session metadata is retained
  expect_setequal(node_attribute(nc, "sessionUUID"), c("aaaa-1111", "bbbb-2222"))
})

test_that("read_graphml records Network Canvas egos as reporters", {
  skip_if_not_installed("xml2")
  nc <- read_graphml(testthat::test_path("sheets", "networkcanvas.graphml"))
  expect_true(is_cognitive(nc))
  egos <- which(node_attribute(nc, "ego"))
  expect_length(egos, 2)
  expect_true(all(tie_attribute(nc, "by") %in% egos))
  # ego is tied to each of its own alters
  expect_equal(sum(tie_attribute(nc, "layer") == "ego"), 4)
  noego <- read_graphml(testthat::test_path("sheets", "networkcanvas.graphml"),
                        ego = FALSE)
  expect_equal(as.numeric(net_nodes(noego)), 4)
  expect_false(is_cognitive(noego))
})

test_that("read_graphml collapses only single-select categoricals", {
  skip_if_not_installed("xml2")
  nc <- read_graphml(testthat::test_path("sheets", "networkcanvas.graphml"))
  # closeness is single-select, so is collapsed using the declared option names
  expect_s3_class(node_attribute(nc, "closeness"), "factor")
  expect_equal(levels(node_attribute(nc, "closeness")), c("close", "very close"))
  # support is multi-select, so is left as indicators rather than dropped
  expect_true(all(c("support_emotional", "support_financial") %in%
                    net_node_attributes(nc)))
  # layout variables are exposed for plotting
  expect_true(all(c("x", "y") %in% net_node_attributes(nc)))
})

test_that("the read functions return stocnet objects", {
  skip_if_not_installed("xml2")
  expect_s3_class(read_gexf(testthat::test_path("sheets", "gephi.gexf")),
                  "stocnet")
  expect_s3_class(read_graphml(testthat::test_path("sheets",
                                                   "networkcanvas.graphml")),
                  "stocnet")
  expect_s3_class(read_ucinet(testthat::test_path("sheets", "ucinettest.##h")),
                  "stocnet")
  # these two import lists rather than networks, so they stay data frames
  expect_s3_class(read_edgelist(testthat::test_path("sheets",
                                                    "testCSVComma.csv")),
                  "data.frame")
  expect_s3_class(read_nodelist(testthat::test_path("sheets",
                                                    "testCSVComma.csv")),
                  "data.frame")
})

test_that("a network keeps its isolates and stays unlabelled on a round trip", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".gexf")
  on.exit(unlink(file))
  # an isolate appears in no tie, so only the nodes record it
  net <- igraph::disjoint_union(as_igraph(create_ring(5)),
                                igraph::make_empty_graph(2, directed = FALSE))
  write_gexf(net, filename = file)
  out <- read_gexf(file)
  expect_equal(as.numeric(net_nodes(out)), 7)
  expect_equal(igraph::vcount(as_igraph(out)), 7)
  expect_false(is_labelled(out))
  # a file's node ids are an export artefact, not names
  gml <- tempfile(fileext = ".gml")
  on.exit(unlink(gml), add = TRUE)
  write_gml(create_ring(5), filename = gml)
  expect_false(is_labelled(read_gml(gml)))
})

test_that("read_gexf reads declared attributes and visualisation elements", {
  skip_if_not_installed("xml2")
  gph <- read_gexf(testthat::test_path("sheets", "gephi.gexf"))
  expect_equal(as.numeric(net_nodes(gph)), 3)
  # nodes are named from their labels rather than from their ids
  expect_equal(node_labels(gph), c("Ada", "Grace", "Katherine"))
  # declared types are respected, and a declared default fills a missing value
  expect_equal(as.integer(node_attribute(gph, "seniority")), c(3L, 5L, 1L))
  expect_equal(node_attribute(gph, "remote"), c(TRUE, FALSE, FALSE))
  # tie attributes and weights are read
  expect_true(is_weighted(gph))
  expect_equal(tie_attribute(gph, "channel"),
               c("email", "phone", "email", "email"))
  # viz elements are read, and positions are available for plotting
  expect_equal(as.numeric(node_attribute(gph, "x")), c(10.5, -2.25, 3))
  expect_equal(as.numeric(node_attribute(gph, "size")), c(12, 20, 8))
  expect_equal(node_attribute(gph, "color"),
               c("#FF0000", "#0080FF", "#000000"))
  # meta is retained as information about the network
  expect_equal(as_infolist(gph)$creator, "Gephi 0.10.1")
})

test_that("read_gexf reciprocates ties declared mutual in a directed network", {
  skip_if_not_installed("xml2")
  gph <- read_gexf(testthat::test_path("sheets", "gephi.gexf"))
  expect_true(is_directed(gph))
  # the file declares three ties, one of them mutual
  expect_equal(as.numeric(net_ties(gph)), 4)
  expect_equal(as_matrix(gph)["Katherine", "Ada"], 3)
  expect_equal(as_matrix(gph)["Ada", "Katherine"], 3)
  expect_equal(as_matrix(gph)["Grace", "Ada"], 0)
})

test_that("read_gexf reads times, hierarchy, and undeclared classes", {
  skip_if_not_installed("xml2")
  dyn <- read_gexf(testthat::test_path("sheets", "gexfdynamic.gexf"))
  expect_true(is_dynamic(dyn))
  expect_equal(as.numeric(tie_attribute(dyn, "start")), c(1, 2))
  expect_equal(as.numeric(tie_attribute(dyn, "end")), c(3, NA))
  # times are read from an element's attributes or from its first spell
  expect_equal(as.numeric(node_attribute(dyn, "start")), c(1, 1, 2, 3))
  # attributes declared without a class are node attributes,
  # and an attvalue may be keyed by title rather than by id
  expect_equal(node_attribute(dyn, "role"),
               c("chair", "member", NA, NA))
  # hierarchy is read whether it is nested or declared with a 'pid'
  expect_equal(node_attribute(dyn, "parent"), c(NA, "n0", NA, "n2"))
  # the tie to an undeclared node is dropped rather than silently added
  expect_equal(as.numeric(net_ties(dyn)), 2)
  expect_false(is_directed(dyn))
})

test_that("write_gexf writes what read_gexf reads back", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".gexf")
  on.exit(unlink(file))
  net <- add_node_attribute(ison_adolescents, "group",
                            rep(c("A", "B"), each = 4))
  net <- add_tie_attribute(net, "weight", seq_len(net_ties(ison_adolescents)))
  write_gexf(net, filename = file)
  out <- read_gexf(file)
  expect_equal(as_matrix(out), as_matrix(net))
  expect_equal(node_labels(out), node_labels(net))
  expect_equal(node_attribute(out, "group"), node_attribute(net, "group"))
  expect_false(is_directed(out))
  # node ids are an export artefact, so an unlabelled network stays unlabelled
  write_gexf(create_ring(5), filename = file)
  expect_false(is_labelled(read_gexf(file)))
  # a directed network is written as such
  write_gexf(create_tree(5, directed = TRUE), filename = file)
  expect_true(is_directed(read_gexf(file)))
})

test_that("write_gexf writes times and visualisation elements", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".gexf")
  on.exit(unlink(file))
  net <- add_node_attribute(create_ring(3), "x", c(1, 2, 3))
  net <- add_node_attribute(net, "y", c(4, 5, 6))
  net <- add_node_attribute(net, "color", c("#FF0000", "#00FF00", "#0000FF"))
  net <- add_tie_attribute(net, "start", c(1, 2, 3))
  write_gexf(net, filename = file)
  expect_true(any(grepl("viz:position", readLines(file))))
  expect_true(any(grepl("mode=\"dynamic\"", readLines(file))))
  out <- read_gexf(file)
  expect_equal(as.numeric(node_attribute(out, "x")), c(1, 2, 3))
  expect_equal(node_attribute(out, "color"), c("#FF0000", "#00FF00", "#0000FF"))
  expect_true(is_dynamic(out))
  expect_equal(as.numeric(tie_attribute(out, "start")), c(1, 2, 3))
})

test_that("the write functions take what the read functions return", {
  skip_if_not_installed("xml2")
  net <- read_gexf(testthat::test_path("sheets", "gephi.gexf"))
  file <- tempfile()
  on.exit(unlink(file))
  for (fn in c("write_matrix", "write_edgelist", "write_nodelist",
               "write_pajek", "write_graphml", "write_gml", "write_gdf",
               "write_gexf", "write_dynetml")) {
    # a stocnet names the directedness of each layer, which the GML format
    # reserves for the network's own, so writing it must stay silent
    expect_no_warning(do.call(fn, list(net, filename = file)))
  }
})

test_that("write_gexf writes a colour it cannot resolve as an attribute", {
  skip_if_not_installed("xml2")
  file <- tempfile(fileext = ".gexf")
  on.exit(unlink(file))
  # a visualisation element holds red, green, and blue values,
  # so a colour named some other way is written as an ordinary attribute
  net <- add_node_attribute(create_ring(3), "color",
                            c("red", "#00FF00", "blue"))
  write_gexf(net, filename = file)
  expect_false(any(grepl("viz:color", readLines(file))))
  expect_equal(node_attribute(read_gexf(file), "color"),
               c("red", "#00FF00", "blue"))
})

# test_that("read_edgelist works", {
#   expect_equal(read_edgelist(testthat::test_path("sheets", "testCSVComma.csv"),
#                              sv = "comma"),
#                data.frame(From = c(1, 2, 2),
#                              To = c(2, 1, 3),
#                              Weight = c(1, 2, 3)))
#   expect_equal(read_edgelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
#                              sv = "semi-colon"),
#                data.frame(From = c(1, 2, 2),
#                              To = c(2, 1, 3),
#                              Weight = c(1, 2, 3)))
#   expect_error(read_edgelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
#                              sv = "SomethingWrong"))
#   testthat::skip_if_not_installed("readxl")
#   expect_equal(read_edgelist(testthat::test_path("sheets", "test.xlsx")),
#                dplyr::tibble(From = c(1, 2, 2),
#                              To = c(2, 1, 3),
#                              Weight = c(1, 2, 3)))
# })
# 
# test_that("write_edgelist works", {
#   file <-  tempfile() # Create file
#   file2 <- tempfile() # Create file
#   edgelisttest <- as_igraph(dplyr::tibble(from = c(1, 2, 2),
#                                           to = c(2, 1, 3),
#                                           weight = c(1, 2, 3)))
#   write_edgelist(edgelisttest,
#                  filename = file)
#   expect_equal(read.csv(file),
#                data.frame(from = c(1, 2, 2),
#                              to = c(2, 1, 3),
#                              weight = c(1, 2, 3)))
#   write_edgelist(filename = file2)
#   expect_equal(read.csv(file2), data.frame(from = c("A", "B", "C"),
#                                            to = c("B", "C", "A"),
#                                            weight = c(1.1, 11, 110)))
#   on.exit(unlink(file)) # Unlink file
#   on.exit(unlink(file2)) #Unlink file
# })
# 
# test_that("read_nodelist works", {
#   expect_equal(read_nodelist(testthat::test_path("sheets", "testCSVComma.csv"),
#                              sv = "comma"),
#                data.frame(From = c(1, 2, 2),
#                           To = c(2, 1, 3),
#                           Weight = c(1, 2, 3)))
#   expect_equal(read_nodelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
#                              sv = "semi-colon"),
#                data.frame(From = c(1, 2, 2),
#                           To = c(2, 1, 3),
#                           Weight = c(1, 2, 3)))
#   expect_error(read_nodelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
#                              sv = "SomethingWrong"))
#   testthat::skip_if_not_installed("readxl")
#   expect_equal(read_nodelist(testthat::test_path("sheets", "test.xlsx")),
#                dplyr::tibble(From = c(1, 2, 2),
#                              To = c(2, 1, 3),
#                              Weight = c(1, 2, 3)))
# })
# 
# test_that("write_nodelist works", {
#   file <-  tempfile() # Create file
#   file2 <- tempfile() # Create file
#   nodelisttest <- data.frame(data.frame(from = c("A", "B", "C"),
#                                         to = c("B", "A", "A")))
#   nodelisttest <- igraph::set_vertex_attr(as_igraph(nodelisttest),
#                                           name = "type",
#                                           value =c(FALSE, FALSE, TRUE) )
#   write_nodelist(nodelisttest,
#                  filename = file)
#   expect_equal(read.csv(file),
#                data.frame(name = c("A", "B", "C"),
#                           type = c(FALSE, FALSE, TRUE)))
#   write_nodelist(filename = file2)
#   expect_equal(read.csv(file2),
#                data.frame(type = c(FALSE, FALSE, TRUE),
#                           name = c("A", "B", "C")))
#   on.exit(unlink(file)) # Unlink file
#   on.exit(unlink(file2)) # Unlink file
# })
# 
# test_that("read_pajek and write_pajek works", {
#   testpaj <- read_pajek(testthat::test_path("sheets", "SouthernWomen.paj"))
#   expect_true(is.tbl_graph(testpaj))
#   edgetest <- as_edgelist(testpaj)
#   expect_equal(head(edgetest$from),
#                head(as_edgelist(testpaj)$from))
#   file <-  tempfile() # Create file
#   write_pajek(as_igraph(testpaj), file)
#   testpaj2 <- read_pajek(file)
#   expect_true(is.tbl_graph(testpaj2))
#   edgetest2 <- as_edgelist(testpaj2)
#   # Note, the igraph::write.graph forgets names.
#   expect_equal(head(edgetest2$from), as.character(rep(1, 6)))
#   on.exit(unlink(file)) # Unlink file
# })
# 
# test_that("read_ucinet and write_ucinet works", {
#   testuci <- read_ucinet(testthat::test_path("sheets", "ucinettest.##h"))
#   expect_true(is.tbl_graph(testuci))
#   expect_equal(nrow(as_edgelist(testuci)), 78)
#   expect_equal(ncol(as_edgelist(testuci)), 2)
#   expect_equal(igraph::vertex_attr(as_igraph(testuci), "name"), NULL)
#   expect_error(read_ucinet(testthat::test_path("sheets", "ucinettest")))
#   expect_error(read_ucinet(testthat::test_path("sheets", "ucinettest1.##h")))
#   file <-  tempfile() # Create file
#   write_ucinet(as_tidygraph(testuci), file)
#   testuci2 <- read_ucinet(paste0(file, ".##h"))
#   expect_true(is.tbl_graph(testuci2))
#   edgetest2 <- as_edgelist(testuci2)
#   # Note, the write ucinet function forgets certain attributes
#   expect_equal(length(edgetest2$from), length(as_edgelist(testuci2)$from))
#   on.exit(unlink(file)) # Unlink file
# })
# 
# # test_that("read_graphml and write_graphml works", {
# #   testgml <- read_graphml(testthat::test_path("sheets", "nassau.graphml"))
# #   expect_true(is.tbl_graph(testgml))
# #   expect_equal(nrow(as_edgelist(testgml)), 15598)
# #   expect_equal(ncol(as_edgelist(testgml)), 17)
# #   expect_length(net_node_attributes(testgml), 8)
# #   expect_length(net_tie_attributes(testgml), 15)
# #   expect_error(read_graphml(testthat::test_path("sheets", "nassau")))
# #   path <-  tempfile() # Create file
# #   write_graphml(testgml, path)
# #   testgml2 <- suppressWarnings(read_graphml(paste0(path)))
# #   expect_true(is.tbl_graph(testgml2))
# #   edgegml <- as_edgelist(testgml2)
# #   expect_length(edgegml$from, length(as_edgelist(testgml)$from))
# #   on.exit(unlink(path)) # Unlink file
# # })
# 
# test_that("read_ and write_ matrix works", {
#   Abruzzo <- Campania <- Calabria <- Puglia <- NULL
#   Abruzzo <- c(1, 0.76, 0.8, 0.90)
#   Campania <- c(0.76, 1, 0.62, 0.69)
#   Calabria <- c(0.80, 0.62, 1, 0.85)
#   Puglia <- c(0.90, 0.69, 0.85, 1)
#   out <-data.frame(Abruzzo, Campania, Calabria, Puglia)
#   row.names(out) <- c('Abruzzo','Campania', 'Calabria', 'Puglia')
#   expect_equal(as_matrix(read_matrix(testthat::test_path("sheets", "test_matrix.csv"))),
#                as.matrix(out))
#   # file <-  tempfile() # Create file
#   # write_matrix(out, filename = file)
#   # expect_equal(as_matrix(read_matrix(file)),
#   #              as.matrix(out))
#   # on.exit(unlink(file)) # Unlink file
# })

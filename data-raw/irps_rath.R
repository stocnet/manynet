# Build the irps_rath multiplex network from the cleaned Excel workbook.
library(migraph)
library(openxlsx)
data_file <- file.path("data-raw", "multiplex_corruption_data.xlsx")

# The workbook contains three binary, undirected adjacency matrices and two
# binary actor attributes. Names here become the multiplex tie-layer names.
network_sheets <- c(
  collaboration = "collaboration",
  resource_transfer = "resource_transfer",
  pre_existing_ties = "pre-existing_ties"
)

read_adjacency <- function(sheet) {
  out <- openxlsx::read.xlsx(
    data_file,
    sheet = sheet,
    rowNames = TRUE,
    colNames = TRUE,
    check.names = FALSE
  )
  out <- as.matrix(out)
  storage.mode(out) <- "numeric"
  out
}

networks <- lapply(network_sheets, read_adjacency)

attributes <- openxlsx::read.xlsx(
  data_file,
  sheet = "attributes",
  rowNames = TRUE,
  colNames = TRUE,
  check.names = FALSE
)

# Check that matrices are square, binary, undirected, loopless, and use the
# same actor names and ordering. Each expression should evaluate to TRUE.
node_names <- rownames(networks[[1L]])
all(vapply(networks, function(x) nrow(x) == ncol(x), logical(1)))
all(vapply(networks, function(x) identical(rownames(x), colnames(x)), logical(1)))
all(vapply(networks, function(x) identical(rownames(x), node_names), logical(1)))
all(vapply(networks, function(x) !anyNA(x) && all(x %in% c(0, 1)), logical(1)))
all(vapply(networks, function(x) isTRUE(all.equal(x, t(x))), logical(1)))
all(vapply(networks, function(x) all(diag(x) == 0), logical(1)))
identical(rownames(attributes), node_names)
identical(names(attributes), c("politician", "gender"))
!anyNA(attributes) && all(as.matrix(attributes) %in% c(0, 1))

# Construct the manynet multiplex object and attach node and dataset metadata.
# Coding follows the workbook: politician (0 = no, 1 = yes) and gender
# (0 = man, 1 = woman).
irps_rath <- do.call(
  manynet::from_ties,
  lapply(networks, manynet::as_stocnet)
) |>
  manynet::add_node_attribute("politician", attributes$politician) |>
  manynet::add_node_attribute("gender", attributes$gender) |>
  manynet::add_info(
    name = "Czech Rath affair corruption network",
    nodes = "actors",
    ties = names(networks),
    collection = "archival",
    year = 2019,
    doi = "10.1007/s12117-018-9334-y",
    description = paste(
      "A multiplex reconstruction of the Czech political corruption network",
      "known as the Rath affair, based on publicly available archival data.",
      "The 11 actors are connected through three binary, undirected layers:",
      "collaboration (including communication and jointly carrying out tasks),",
      "resource transfer (including bribes and other transfers), and",
      "pre-existing ties (including kinship, friendship, and shared political",
      "or professional affiliations). Node attributes identify politicians and",
      "gender. The data accompany Diviák, Dijkstra, and Snijders (2019),",
      "'Structure, multiplexity, and centrality in a corruption network:",
      "the Czech Rath affair', Trends in Organized Crime 22(3), 274-297."
    ),
    attribute_coding = paste(
      "politician: 0 = no, 1 = yes;",
      "gender: 0 = man, 1 = woman"
    )
  )

irps_rath

# Basic post-construction checks.
length(networks) == 3L
length(node_names) == 11L
nrow(attributes) == length(node_names)

usethis::use_data(irps_rath, overwrite = TRUE, compress = "xz")

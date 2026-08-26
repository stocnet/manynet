# Build the irps_corruption multiplex network from the cleaned Excel workbook.
library(manynet)
library(readxl)
data_file <- file.path("data-raw", "multiplex_corruption_data.xlsx")

# The workbook contains three binary, undirected adjacency matrices and two
# binary actor attributes. The names here become the layer names, and follow
# the one-word convention of the other multiplex datasets in this package
# (e.g. ison_lawfirm's "friends", "advice", "cowork").
network_sheets <- c(
  collaboration = "collaboration",
  transfers = "resource_transfer",
  preexisting = "pre-existing_ties"
)

# `readxl` returns a tibble, so the first column holds the actor names that the
# workbook uses as row names.
read_adjacency <- function(sheet) {
  out <- readxl::read_excel(data_file, sheet = sheet, col_names = TRUE)
  node_names <- out[[1L]]
  out <- as.matrix(out[, -1L])
  storage.mode(out) <- "numeric"
  dimnames(out) <- list(node_names, colnames(out))
  out
}

networks <- lapply(network_sheets, read_adjacency)

attributes <- readxl::read_excel(data_file, sheet = "attributes",
                                 col_names = TRUE)
attributes <- data.frame(attributes[, -1L], row.names = attributes[[1L]],
                         check.names = FALSE)

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

# The workbook separates each actor's given name from their family name with
# an underscore, which is how the names print, so a space is used instead.
networks <- lapply(networks, function(x) {
  dimnames(x) <- list(gsub("_", " ", node_names), gsub("_", " ", node_names))
  x
})

# The workbook codes both attributes as 0/1: politician (0 = no, 1 = yes) and
# gender (0 = man, 1 = woman). Each is given the type it describes, so that
# `politician` can be filtered on directly and `gender` prints its categories.
politician <- attributes$politician == 1
gender <- ifelse(attributes$gender == 1, "female", "male")

irps_corruption <- do.call(
  manynet::from_ties,
  lapply(networks, manynet::as_stocnet)
) |>
  manynet::add_node_attribute("politician", politician) |>
  manynet::add_node_attribute("gender", gender) |>
  manynet::add_info(
    name = "Czech Rath affair corruption network",
    modes = "actors",
    layers = names(networks),
    directed = stats::setNames(rep(FALSE, length(networks)), names(networks)),
    source = "empirical",
    method = "archival",
    location = "Central Bohemia, Czech Republic",
    date = 2012,
    boundary = "roster",
    observation = stats::setNames(rep("cross-sectional", length(networks)),
                                  names(networks)),
    doi = "https://doi.org/10.1007/s12117-018-9334-y"
  )

irps_corruption

# Basic post-construction checks.
length(networks) == 3L
length(node_names) == 11L
nrow(attributes) == length(node_names)

usethis::use_data(irps_corruption, overwrite = TRUE, compress = "xz")

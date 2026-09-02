# Converts the temporal datasets that were still 'mnet' objects into 'stocnet'
# objects. A stocnet holds what these networks know about themselves that an
# 'mnet' cannot: which layer was observed how ('info$observation'), how each
# record of a tie relates to the one before it ('info$update'), and the mode
# and layer names under the names a stocnet reserves for them.
# It also spells the moment each tie was recorded at in a 'time' column, which
# is the moment column in every class.
#
# Re-runnable: converting a stocnet returns it unchanged, and the info entries
# are set to what they already say.

devtools::load_all(quiet = TRUE)

# 'nodes' and 'ties' were the names an mnet gave the mode and layer names,
# before 'modes' and 'layers' were reserved for them.
conform_names <- function(x){
  info <- x$info
  if(!is.null(info$nodes) && is.null(info$modes)) info$modes <- info$nodes
  if(!is.null(info$ties) && is.null(info$layers)) info$layers <- info$ties
  info$nodes <- NULL
  info$ties <- NULL
  x$info <- info
  x
}

fict_potter <- as_stocnet(manynet::fict_potter) |> conform_names() |>
  add_info(observation = "panel", update = "replace")

fict_starwars <- as_stocnet(manynet::fict_starwars) |> conform_names() |>
  add_info(observation = "panel", update = "replace")

# Only the 'like' layer of Sampson's monks was observed at every wave; the
# other three were recorded once, and state something holding throughout.
ison_monks <- as_stocnet(manynet::ison_monks) |> conform_names()
ison_monks <- add_info(ison_monks,
                       observation = stats::setNames(
                         ifelse(layer_names(ison_monks) == "like",
                                "panel", "cross-sectional"),
                         layer_names(ison_monks)),
                       update = "replace")

# A 'sign' column beside a 'weight' column records twice what one signed weight
# records once, and a matrix can hold only one value per tie, so the sign is
# the one that a coercion to a matrix drops. The weights of these ties rank the
# first choice 3, the second 2, and the third 1, so a signed weight runs from
# -3 to 3 and both the valence and the rank survive.
if("sign" %in% names(ison_monks$ties)){
  ison_monks$ties$weight <- ison_monks$ties$weight * ison_monks$ties$sign
  ison_monks$ties$sign <- NULL
}

usethis::use_data(fict_potter, overwrite = TRUE, compress = "bzip2")
usethis::use_data(fict_starwars, overwrite = TRUE, compress = "bzip2")
usethis::use_data(ison_monks, overwrite = TRUE, compress = "bzip2")

# The four networks that `tie_is_parallel()` marks (#158) were still 'mnet'
# objects. A stocnet records what each of them knows about itself that an
# 'mnet' cannot: how it was collected, where, when, and how each record of a
# tie relates to the one before it.

# Euler presented the problem to the St Petersburg Academy on 26 August 1735
# and it was published in 1741 as Eneström 53. The seven bridges are the
# network, so the two pairs of parallel bridges are the point of it and are
# left as parallel ties rather than collapsed into a weight.
ison_koenigsberg <- as_stocnet(manynet::ison_koenigsberg) |> conform_names() |>
  add_info(name = "Seven Bridges of Koenigsberg",
           observation = "cross-sectional",
           directed = FALSE,
           source = "Empirical", method = "Archival", boundary = "roster",
           location = "Koenigsberg, Prussia",
           date = 1735,
           doi = "https://scholarlycommons.pacific.edu/euler-works/53/")

# Adamic and Glance gathered blog URLs from the eTalkingHead, BlogCatalog,
# CampaignLine, and Blogarama directories, retrieved a front page for each on
# 8 February 2005, then added the blogs those pages cited 17 or more times and
# retrieved their pages on 22 February 2005. A roster drawn from directories
# and then extended by citation is a snowball.
irps_blogs <- as_stocnet(manynet::irps_blogs) |> conform_names() |>
  add_info(observation = "cross-sectional",
           directed = TRUE,
           source = "Empirical", method = "Archival", boundary = "snowball",
           location = "United States",
           date = "2005-02",
           doi = "10.1145/1134271.1134277")
# 'collection' was the mnet field for how a network was collected, before
# 'method' was reserved for it.
irps_blogs$info$collection <- NULL

# Each row is one claim by one speaker about one concept on one day, so the
# network records a stream of events rather than a panel. A claim is
# supportive or critical, which `as_stocnet()` carries into the reserved
# 'weight' column as a sign of 1 or -1.
irps_nuclear <- as_stocnet(manynet::irps_nuclear) |> conform_names() |>
  add_info(name = "German nuclear discourse network",
           observation = "event", update = "increment",
           directed = FALSE,
           sender = "speakers", receiver = "concepts",
           source = "Empirical", method = "Archival",
           location = "Germany",
           date = "2011",
           doi = "10.1017/nws.2022.31")

# Both layers are undirected: a relationship holds between two characters, and
# an affiliation between a character and a team.
fict_marvel <- as_stocnet(manynet::fict_marvel) |> conform_names() |>
  add_info(name = "Marvel universe",
           observation = "cross-sectional",
           directed = stats::setNames(c(FALSE, FALSE),
                                      c("relationship", "affiliation")),
           sender = "characters", receiver = "teams",
           source = "Empirical", method = "Archival", boundary = "roster",
           date = 2017)
# Only the relationship layer is signed, so `as_stocnet()` leaves the
# affiliation ties with an NA weight. An NA weight is how manynet records a
# tie whose value is unknown, which would make every affiliation missing and
# `as_matrix()` return a matrix of NAs. An affiliation is a positive tie, so
# it is weighted 1 instead.
fict_marvel$ties$weight[is.na(fict_marvel$ties$weight)] <- 1

usethis::use_data(ison_koenigsberg, overwrite = TRUE, compress = "bzip2")
usethis::use_data(irps_blogs, overwrite = TRUE, compress = "bzip2")
usethis::use_data(irps_nuclear, overwrite = TRUE, compress = "bzip2")
usethis::use_data(fict_marvel, overwrite = TRUE, compress = "bzip2")

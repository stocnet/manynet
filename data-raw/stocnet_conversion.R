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

# Davis, Gardner and Gardner read the women's attendance off the society
# columns of the 'Old City Herald' over nine months of 1936. 'Old City' is the
# book's pseudonym for Natchez, Mississippi. Fourteen dated events and eighteen
# named women are a fixed list rather than a sample, so the boundary is a
# roster, and fourteen dated occurrences are a stream of events rather than a
# few re-observations of one network.
# The date belongs to the event and not to the attendance: every tie to an
# event carries the same date, and no woman carries one of her own. Each event
# therefore enters the network on the date it is held, which the changes
# component records, and `is_changing()` marks the network rather than
# `is_dynamic()`. There is no 'update' to declare, since no tie is ever
# restated.
ison_southern_women <- as_stocnet(manynet::ison_southern_women) |>
  conform_names() |>
  add_info(observation = "event",
           directed = FALSE,
           sender = "women", receiver = "social events",
           source = "Empirical", method = "Archival", boundary = "roster",
           location = "Natchez, Mississippi, United States",
           date = 1936)
# 'year' was the mnet field for when a network was collected, before 'date'
# was reserved for it. It held the year of publication rather than of
# collection; the events themselves ran from February to November 1936.
ison_southern_women$info$year <- NULL

usethis::use_data(ison_southern_women, overwrite = TRUE, compress = "bzip2")

# The eleven networks below were the 'mnet' objects that could become
# 'stocnet' objects without the released versions of 'netrics', 'autograph',
# or 'migraph' noticing. The remaining 'mnet' objects wait for releases of
# those packages that no longer rely on their class or on the 'type' tie
# attribute (see the issue tracker).

# Some 'mnet' objects kept their metadata in a 'grand' list instead, under
# 'vertex1' for the mode and 'edge.pos' for the layer. Once those are read
# into the names a stocnet reserves, what is left of the old fields restates
# them and is dropped.
conform_grand <- function(x){
  info <- x$info
  if(!is.null(info$vertex1) && is.null(info$modes)) info$modes <- info$vertex1
  if(!is.null(info$edge.pos) && is.null(info$layers) &&
     !"layer" %in% names(x$ties)) info$layers <- info$edge.pos
  info[c("vertex1", "vertex2", "positive", "negative", "edge.pos", "edge.neg",
         "mode", "year", "collection")] <- NULL
  x$info <- info
  conform_names(x)
}

# Robinson parsed the script of the 2003 film for which characters appear in
# which scenes. The four layers among the characters were added from a diagram
# of the film's interconnections. As in `fict_marvel`, every layer is
# undirected, and the appearance layer runs from the characters to the scenes.
fict_actually <- as_stocnet(manynet::fict_actually) |> conform_grand()
fict_actually <- add_info(fict_actually,
           name = "Love Actually",
           observation = "cross-sectional",
           directed = stats::setNames(rep(FALSE, 5), layer_names(fict_actually)),
           sender = "characters", receiver = "scenes",
           source = "Empirical", method = "Archival", boundary = "roster",
           date = 2003,
           doi = "http://varianceexplained.org/r/love-actually-network/")

# McNulty scraped the scripts of all ten seasons, which aired from 1994 to
# 2004. The weight of a tie counts the scenes two characters share across all
# of them, so the seasons are already aggregated and there is one observation.
fict_friends <- as_stocnet(manynet::fict_friends) |> conform_grand() |>
  add_info(name = "Friends",
           observation = "cross-sectional",
           directed = FALSE,
           source = "Empirical", method = "Archival", boundary = "roster",
           date = "1994-2004",
           doi = "https://github.com/keithmcnulty/friends_analysis/")

# Weissman recorded the sexual contacts from the story lines and fan pages,
# and Lind added the attributes of the characters. The characters joined the
# show in its first ten seasons, the first of which aired in 2005 and the
# tenth of which ended in 2014.
fict_greys <- as_stocnet(manynet::fict_greys) |> conform_grand() |>
  add_info(observation = "cross-sectional",
           directed = FALSE,
           source = "Empirical", method = "Archival", boundary = "roster",
           date = "2005-2014",
           doi = "https://gweissman.github.io/post/grey-s-anatomy-network-of-sexual-relations/")

# Glander scraped the kinship ties from 'A Wiki of Ice and Fire' in 2017. A
# parent tie runs from the parent to the child. A spouse tie is recorded in
# both directions, so the network as a whole is directed.
fict_thrones <- as_stocnet(manynet::fict_thrones) |> conform_grand() |>
  add_info(observation = "cross-sectional",
           directed = TRUE,
           source = "Empirical", method = "Archival",
           date = 2017,
           doi = "https://datascienceplus.com/network-analysis-of-game-of-thrones/")

# Krebs (2002) built the network from what major newspapers reported in the
# weeks after 11 September 2001. He began with the 19 hijackers and added
# their associates as they were named, which is a snowball from a seed set.
irps_911 <- as_stocnet(manynet::irps_911) |> conform_grand()
irps_911 <- add_info(irps_911,
           name = "9/11 hijackers and their associates",
           observation = "cross-sectional",
           directed = stats::setNames(c(FALSE, FALSE), layer_names(irps_911)),
           source = "Empirical", method = "Archival", boundary = "snowball",
           date = 2001)

# Krebs collected the books that Amazon.com reported as bought together.
# The paper that describes the collection is not to hand, so the date and
# the boundary are left for when it is.
irps_books <- as_stocnet(manynet::irps_books) |> conform_grand() |>
  add_info(observation = "cross-sectional",
           directed = FALSE,
           source = "Empirical", method = "Archival",
           location = "United States")

# Antal, Krapivsky and Redner (2006, Fig. 10) give the relations among the
# six powers from the Three Emperors' League of 1872 to the British-Russian
# agreement of 1907, and the data runs them on to the end of the war in 1918.
# Each tie holds from its 'begin' to its 'end', which is a
# state over an interval rather than an event or a wave of a panel, so no
# observation design is declared. The sign of a relation is its weight.
irps_wwi <- as_stocnet(manynet::irps_wwi) |> conform_grand() |>
  add_info(directed = FALSE,
           source = "Empirical", method = "Archival", boundary = "roster",
           location = "Europe",
           date = "1872-1918",
           doi = "10.1016/j.physd.2006.09.028")

# Lusseau et al. (2003) surveyed the fjord from November 1994 to November
# 2001 and photo-identified the adult members of each of 1,292 schools. A tie
# joins two dolphins seen in the same school more often than expected by
# chance, by a half-weight index tested against permuted schools (Lusseau
# 2003). The community is closed and every identified adult is in it, so the
# boundary is a roster.
ison_dolphins <- as_stocnet(manynet::ison_dolphins) |> conform_grand() |>
  add_info(observation = "cross-sectional",
           directed = FALSE,
           source = "Empirical", method = "Observation", boundary = "roster",
           location = "Doubtful Sound, New Zealand",
           date = "1994-2001",
           doi = "10.1098/rsbl.2003.0057")

# Trampe, Quoidbach and Taquet (2015) asked the users of a francophone
# smartphone application which of 18 emotions they felt at random moments.
# The paper does not date the collection, so no date is recorded.
ison_emotions <- as_stocnet(manynet::ison_emotions) |> conform_grand() |>
  add_info(observation = "cross-sectional",
           directed = TRUE,
           source = "Empirical", method = "Survey",
           doi = "10.1371/journal.pone.0145450")

# Bastazini compiled the combinations from the judo literature in 2025. The
# 33 nodes are attacks, and an arc says one can follow another.
ison_judo_moves <- as_stocnet(manynet::ison_judo_moves) |> conform_grand() |>
  add_info(observation = "cross-sectional",
           directed = TRUE,
           source = "Empirical", method = "Archival",
           date = 2025,
           doi = "https://geekcologist.wordpress.com/2025/05/27/the-dynamics-of-the-gentle-way-exploring-judo-attack-combinations-as-networks-in-r/")

# Zachary (1977) observed the club for three years, from 1970 to 1972. The 34
# members are those who met outside the club's classes and meetings; the other
# 26 or so members did not, and are left out. The weight of a tie counts the
# contexts outside the club in which the two met. 'year' held the year of
# publication.
ison_karateka <- as_stocnet(manynet::ison_karateka) |> conform_grand() |>
  add_info(name = "Zachary's karate club",
           observation = "cross-sectional",
           directed = FALSE,
           source = "Empirical", method = "Ethnography", boundary = "roster",
           location = "United States",
           date = "1970-1972",
           doi = "10.1086/jar.33.4.3629752")

usethis::use_data(fict_actually, overwrite = TRUE, compress = "bzip2")
usethis::use_data(fict_friends, overwrite = TRUE, compress = "bzip2")
usethis::use_data(fict_greys, overwrite = TRUE, compress = "bzip2")
usethis::use_data(fict_thrones, overwrite = TRUE, compress = "bzip2")
usethis::use_data(irps_911, overwrite = TRUE, compress = "bzip2")
usethis::use_data(irps_books, overwrite = TRUE, compress = "bzip2")
usethis::use_data(irps_wwi, overwrite = TRUE, compress = "bzip2")
usethis::use_data(ison_dolphins, overwrite = TRUE, compress = "bzip2")
usethis::use_data(ison_emotions, overwrite = TRUE, compress = "bzip2")
usethis::use_data(ison_judo_moves, overwrite = TRUE, compress = "bzip2")
usethis::use_data(ison_karateka, overwrite = TRUE, compress = "bzip2")

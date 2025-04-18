% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manip_split.R
\name{manip_split}
\alias{manip_split}
\alias{to_egos}
\alias{to_subgraphs}
\alias{to_components}
\alias{to_waves}
\alias{to_slices}
\title{Splitting networks into lists}
\usage{
to_egos(.data, max_dist = 1, min_dist = 0, direction = c("out", "in"))

to_subgraphs(.data, attribute)

to_components(.data)

to_waves(.data, attribute = "wave", panels = NULL, cumulative = FALSE)

to_slices(.data, attribute = "time", slice = NULL)
}
\arguments{
\item{.data}{An object of a manynet-consistent class:
\itemize{
\item matrix (adjacency or incidence) from \code{{base}} R
\item edgelist, a data frame from \code{{base}} R or tibble from \code{{tibble}}
\item igraph, from the \code{{igraph}} package
\item network, from the \code{{network}} package
\item tbl_graph, from the \code{{tidygraph}} package
}}

\item{max_dist}{The maximum breadth of the neighbourhood.
By default 1.}

\item{min_dist}{The minimum breadth of the neighbourhood.
By default 0.
Increasing this to 1 excludes the ego,
and 2 excludes ego's direct alters.}

\item{direction}{String, either "out" or "in".}

\item{attribute}{One or two attributes used to slice data.}

\item{panels}{Would you like to select certain waves?
NULL by default.
That is, a list of networks for every available wave is returned.
Users can also list specific waves they want to select.}

\item{cumulative}{Whether to make wave ties cumulative.
FALSE by default. That is, each wave is treated isolated.}

\item{slice}{Character string or character list indicating the date(s)
or integer(s) range used to slice data (e.g slice = c(1:2, 3:4)).}
}
\value{
The returned object will be a list of network objects.
}
\description{
These functions offer tools for splitting manynet-consistent objects
(matrices, igraph, tidygraph, or network objects) into lists of networks.

Not all functions have methods available for all object classes.
Below are the currently implemented S3 methods:\tabular{lrrrrrr}{
    \tab data.frame \tab diff_model \tab igraph \tab matrix \tab network \tab tbl_graph \cr
   to_components \tab 1 \tab 0 \tab 1 \tab 1 \tab 1 \tab 1 \cr
   to_egos \tab 1 \tab 0 \tab 1 \tab 1 \tab 1 \tab 1 \cr
   to_slices \tab 0 \tab 0 \tab 1 \tab 0 \tab 0 \tab 1 \cr
   to_subgraphs \tab 0 \tab 0 \tab 1 \tab 0 \tab 1 \tab 1 \cr
   to_waves \tab 1 \tab 1 \tab 1 \tab 0 \tab 0 \tab 1 \cr
}
}
\section{Functions}{
\itemize{
\item \code{to_egos()}: Returns a list of ego (or focal)
networks.

\item \code{to_subgraphs()}: Returns a list of subgraphs
on some given node attribute.

\item \code{to_components()}: Returns a list of the components
in a network.

\item \code{to_waves()}: Returns a network
with some discrete observations over time
into a list of those observations.

\item \code{to_slices()}: Returns a list of a network
with some continuous time variable at some time slice(s).

}}
\examples{
  to_egos(ison_adolescents)
  # graphs(to_egos(ison_adolescents,2))
ison_adolescents \%>\%
  mutate(unicorn = sample(c("yes", "no"), 8,
                          replace = TRUE)) \%>\%
  to_subgraphs(attribute = "unicorn")
  to_components(ison_marvel_relationships)
ison_adolescents \%>\%
  mutate_ties(wave = sample(1995:1998, 10, replace = TRUE)) \%>\%
  to_waves(attribute = "wave")
ison_adolescents \%>\%
  mutate_ties(time = 1:10, increment = 1) \%>\% 
  add_ties(c(1,2), list(time = 3, increment = -1)) \%>\% 
  to_slices(slice = 7)
}
\seealso{
Other modifications: 
\code{\link{manip_as}},
\code{\link{manip_correlation}},
\code{\link{manip_deformat}},
\code{\link{manip_from}},
\code{\link{manip_levels}},
\code{\link{manip_miss}},
\code{\link{manip_nodes}},
\code{\link{manip_paths}},
\code{\link{manip_permutation}},
\code{\link{manip_preformat}},
\code{\link{manip_project}},
\code{\link{manip_reformat}},
\code{\link{manip_scope}},
\code{\link{manip_ties}}
}
\concept{modifications}

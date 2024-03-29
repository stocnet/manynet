% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_attributes.R
\name{attributes}
\alias{attributes}
\alias{node_attribute}
\alias{node_names}
\alias{node_mode}
\alias{tie_attribute}
\alias{tie_weights}
\alias{tie_signs}
\title{Describing attributes of nodes or ties in a network}
\usage{
node_attribute(.data, attribute)

node_names(.data)

node_mode(.data)

tie_attribute(.data, attribute)

tie_weights(.data)

tie_signs(.data)
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

\item{attribute}{Character string naming an attribute in the object.}
}
\description{
These functions extract certain attributes from network data:
\itemize{
\item \code{node_attribute()} returns an attribute's values for the nodes in a network.
\item \code{node_names()} returns the names of the nodes in a network.
\item \code{node_mode()} returns the mode of the nodes in a network.
\item \code{tie_attribute()} returns an attribute's values for the ties in a network.
\item \code{tie_weights()} returns the weights of the ties in a network.
\item \code{tie_signs()} returns the signs of the ties in a network.
}

These functions are also often used as helpers within other functions.
\verb{node_*()} and \verb{tie_*()} always return vectors the same length
as the number of nodes or ties in the network, respectively.
}
\examples{
node_attribute(ison_lotr, "Race")
node_names(ison_southern_women)
node_mode(ison_southern_women)
tie_attribute(ison_algebra, "task_tie")
tie_weights(to_mode1(ison_southern_women))
tie_signs(ison_marvel_relationships)
}
\seealso{
Other mapping: 
\code{\link{autographr}()},
\code{\link{autographs}()},
\code{\link{autographt}()},
\code{\link{configuration_layouts}},
\code{\link{partition_layouts}},
\code{\link{properties}}
}
\concept{mapping}

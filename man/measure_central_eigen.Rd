% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_centrality.R
\name{measure_central_eigen}
\alias{measure_central_eigen}
\alias{node_eigenvector}
\alias{node_power}
\alias{node_alpha}
\alias{node_pagerank}
\alias{node_authority}
\alias{node_hub}
\alias{tie_eigenvector}
\alias{net_eigenvector}
\title{Measures of eigenvector-like centrality and centralisation}
\usage{
node_eigenvector(.data, normalized = TRUE, scale = TRUE)

node_power(.data, normalized = TRUE, scale = FALSE, exponent = 1)

node_alpha(.data, alpha = 0.85)

node_pagerank(.data)

node_authority(.data)

node_hub(.data)

tie_eigenvector(.data, normalized = TRUE)

net_eigenvector(.data, normalized = TRUE)
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

\item{normalized}{Logical scalar, whether the centrality scores are normalized.
Different denominators are used depending on whether the object is one-mode or two-mode,
the type of centrality, and other arguments.}

\item{scale}{Logical scalar, whether to rescale the vector so the maximum score is 1.}

\item{exponent}{Decay rate or attentuation factor for
the Bonacich power centrality score.
Can be positive or negative.}

\item{alpha}{A constant that trades off the importance of external influence against the importance of connection.
When \eqn{\alpha = 0}, only the external influence matters.
As \eqn{\alpha} gets larger, only the connectivity matters and we reduce to eigenvector centrality.
By default \eqn{\alpha = 0.85}.}
}
\value{
A numeric vector giving the eigenvector centrality measure of each node.

A numeric vector giving each node's power centrality measure.
}
\description{
These functions calculate common eigenvector-related centrality
measures, or walk-based eigenmeasures, for one- and two-mode networks:
\itemize{
\item \code{node_eigenvector()} measures the eigenvector centrality of nodes
in a network.
\item \code{node_power()} measures the Bonacich, beta, or power centrality of
nodes in a network.
\item \code{node_alpha()} measures the alpha or Katz centrality of nodes in a
network.
\item \code{node_pagerank()} measures the pagerank centrality of nodes in a network.
\item \code{node_hub()} measures how well nodes in a network serve as hubs pointing
to many authorities.
\item \code{node_authority()} measures how well nodes in a network serve as
authorities from many hubs.
\item \code{tie_eigenvector()} measures the eigenvector centrality of ties in a
network.
\item \code{net_eigenvector()} measures the eigenvector centralization for a
network.
}

All measures attempt to use as much information as they are offered,
including whether the networks are directed, weighted, or multimodal.
If this would produce unintended results,
first transform the salient properties using e.g. \code{\link[=to_undirected]{to_undirected()}} functions.
All centrality and centralization measures return normalized measures
by default, including for two-mode networks.
}
\details{
We use \code{{igraph}} routines behind the scenes here for consistency and because they are often faster.
For example, \code{igraph::eigencentrality()} is approximately 25\% faster than \code{sna::evcent()}.
}
\section{Eigenvector centrality}{

Eigenvector centrality operates as a measure of a node's influence in a network.
The idea is that being connected to well-connected others results in a higher score.
Each node's eigenvector centrality can be defined as:
\deqn{x_i = \frac{1}{\lambda} \sum_{j \in N} a_{i,j} x_j}
where \eqn{a_{i,j} = 1} if \eqn{i} is linked to \eqn{j} and 0 otherwise,
and \eqn{\lambda} is a constant representing the principal eigenvalue.
Rather than performing this iteration,
most routines solve the eigenvector equation \eqn{Ax = \lambda x}.
Note that since \code{{igraph}} v2.1.1,
the values will always be rescaled so that the maximum is 1.
}

\section{Power or beta (or Bonacich) centrality}{

Power centrality includes an exponent that weights contributions to a node's
centrality based on how far away those other nodes are.
\deqn{c_b(i) = \sum A(i,j) (\alpha = \beta c(j))}
Where \eqn{\beta} is positive, this means being connected to central people
increases centrality.
Where \eqn{\beta} is negative, this means being connected to central people
decreases centrality
(and being connected to more peripheral actors increases centrality).
When \eqn{\beta = 0}, this is the outdegree.
\eqn{\alpha} is calculated to make sure the root mean square equals
the network size.
}

\section{Alpha centrality}{

Alpha or Katz (or Katz-Bonacich) centrality operates better than eigenvector centrality
for directed networks.
Eigenvector centrality will return 0s for all nodes not in the main strongly-connected component.
Each node's alpha centrality can be defined as:
\deqn{x_i = \frac{1}{\lambda} \sum_{j \in N} a_{i,j} x_j + e_i}
where \eqn{a_{i,j} = 1} if \eqn{i} is linked to \eqn{j} and 0 otherwise,
\eqn{\lambda} is a constant representing the principal eigenvalue,
and \eqn{e_i} is some external influence used to ensure that even nodes beyond the main
strongly connected component begin with some basic influence.
Note that many equations replace \eqn{\frac{1}{\lambda}} with \eqn{\alpha},
hence the name.

For example, if \eqn{\alpha = 0.5}, then each direct connection (or alter) would be worth \eqn{(0.5)^1 = 0.5},
each secondary connection (or tertius) would be worth \eqn{(0.5)^2 = 0.25},
each tertiary connection would be worth \eqn{(0.5)^3 = 0.125}, and so on.

Rather than performing this iteration though,
most routines solve the equation \eqn{x = (I - \frac{1}{\lambda} A^T)^{-1} e}.
}

\examples{
node_eigenvector(ison_southern_women)
node_power(ison_southern_women, exponent = 0.5)
tie_eigenvector(ison_adolescents)
net_eigenvector(ison_southern_women)
}
\references{
\subsection{On eigenvector centrality}{

Bonacich, Phillip. 1991.
“Simultaneous Group and Individual Centralities.”
\emph{Social Networks} 13(2):155–68.
\doi{10.1016/0378-8733(91)90018-O}
}

\subsection{On power centrality}{

Bonacich, Phillip. 1987.
“Power and Centrality: A Family of Measures.”
\emph{The American Journal of Sociology}, 92(5): 1170–82.
\doi{10.1086/228631}.
}

\subsection{On alpha centrality}{

Katz, Leo 1953.
"A new status index derived from sociometric analysis".
\emph{Psychometrika}. 18(1): 39–43.

Bonacich, P. and Lloyd, P. 2001.
“Eigenvector-like measures of centrality for asymmetric relations”
\emph{Social Networks}. 23(3):191-201.
}

\subsection{On pagerank centrality}{

Brin, Sergey and Page, Larry. 1998.
"The anatomy of a large-scale hypertextual web search engine".
\emph{Proceedings of the 7th World-Wide Web Conference}. Brisbane, Australia.
}

\subsection{On hub and authority centrality}{

Kleinberg, Jon. 1999.
"Authoritative sources in a hyperlinked environment".
\emph{Journal of the ACM} 46(5): 604–632.
\doi{10.1145/324133.324140}
}
}
\seealso{
Other centrality: 
\code{\link{measure_central_between}},
\code{\link{measure_central_close}},
\code{\link{measure_central_degree}}

Other measures: 
\code{\link{measure_attributes}},
\code{\link{measure_central_between}},
\code{\link{measure_central_close}},
\code{\link{measure_central_degree}},
\code{\link{measure_closure}},
\code{\link{measure_cohesion}},
\code{\link{measure_diffusion_infection}},
\code{\link{measure_diffusion_net}},
\code{\link{measure_diffusion_node}},
\code{\link{measure_features}},
\code{\link{measure_heterogeneity}},
\code{\link{measure_hierarchy}},
\code{\link{measure_holes}},
\code{\link{measure_periods}},
\code{\link{measure_properties}},
\code{\link{member_diffusion}}
}
\concept{centrality}
\concept{measures}

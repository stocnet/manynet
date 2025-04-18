% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_create.R
\name{make_ego}
\alias{make_ego}
\alias{create_ego}
\title{Making ego networks through interviewing}
\usage{
create_ego(
  ego = NULL,
  max_alters = Inf,
  roster = NULL,
  interpreter = FALSE,
  interrelater = FALSE,
  twomode = FALSE
)
}
\arguments{
\item{ego}{A character string.
If desired, the name of ego can be declared as an argument.
Otherwise the first prompt of the function will be to enter a name for ego.}

\item{max_alters}{The maximum number of alters to collect.
By default infinity, but many name generators will expect a maximum of
e.g. 5 alters to be named.}

\item{roster}{A vector of node names to offer as potential alters for ego.}

\item{interpreter}{Logical. If TRUE, then it will ask for which attributes
to collect and give prompts for each attribute for each node in the network.
By default FALSE.}

\item{interrelater}{Logical. If TRUE, then it will ask for the contacts from
each of the alters perspectives too.}

\item{twomode}{Logical. If TRUE, then it will assign ego to the first mode
and all alters to a second mode.}
}
\description{
This function creates an ego network through interactive interview questions.
It currently only supports a simplex, directed network of one
or two modes.
These directed networks can be reformatted as undirected using \code{to_undirected()}.
Multiplex networks can be collected separately and then joined together
afterwards.

The function supports the use of rosters or a maximum number of
alters to collect. If a roster is provided it will offer ego all names.
The function can also prompt ego to interpret each node's attributes,
or about how ego considers their alters to be related.
}
\seealso{
Other makes: 
\code{\link{make_cran}},
\code{\link{make_create}},
\code{\link{make_explicit}},
\code{\link{make_learning}},
\code{\link{make_motifs}},
\code{\link{make_play}},
\code{\link{make_random}},
\code{\link{make_read}},
\code{\link{make_stochastic}},
\code{\link{make_write}}
}
\concept{makes}

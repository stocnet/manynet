% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_networks.R
\name{make_mnet}
\alias{make_mnet}
\alias{print.mnet}
\alias{print_all}
\title{Multilevel, multiplex, multimodal, signed, dynamic or longitudinal changing networks}
\usage{
\method{print}{mnet}(x, ..., n = 12)

print_all(x, ...)
}
\arguments{
\item{x}{An object of class "mnet" or "tbl_graph".}

\item{...}{Other arguments passed to or from other methods.}

\item{n}{Number of observations to print across all network components,
i.e. nodes, changes, and ties.
By default 12.}
}
\description{
The 'mnet' class of network object is an additional class layered on top of
the 'igraph' and 'tbl_graph' classes.
Under the hood it is an 'igraph' object, which enables all the igraph
functions to operate.
It is also a 'tbl_graph' object, which enables it to be used with \code{{ggraph}}.
However, 'mnet' objects offer prettier printing and
a consistent structure that enables more complex forms of networks
to be contained in a single object.
}
\section{Nodes}{

Nodes are held as vertices and vertex attributes in the 'igraph' object,
but printed as a nodelist.
Here the convention is for the first column of the nodelist to be called
'name' and records the labels of the nodes.
Additional reserved columns include 'active' for changing networks and
'type' for multimodal networks.
}

\section{Changes}{

Changes, that is a list of changes to the nodes in the network,
are held internally as a graph attribute in the 'igraph' object,
but printed as a changelist.
Here the convention is for the 'wave' or 'time' column to appear first,
followed by 'node' indicating to which node the change applies,
'var' for the variable to which the change applies,
and 'value' for the new value to be applied.
}

\section{Ties}{

Ties are held as edges and edge attributes in the 'igraph' object,
but printed as an edgelist.
Here the convention is for the first column of the edgelist to be called
'from' and the second column 'to', even if the network is not directed.
Additional reserved columns include 'weight' for weighted networks,
'wave' for longitudinal networks, 'type' for multiplex networks,
and 'sign' for signed networks.
}

\section{Printing}{

When printed, 'mnet' objects will print to the console any information
stored about the network's name, or its types of nodes or ties.
It will also describe key features of the network,
such as whether the network is multiplex, weighted, directed, etc.

It will then print tibbles for the nodes, changes, and ties in the network,
as appropriate.
That is, if there is no nodal data
(e.g. it is an unlabelled network without any other nodal attributes),
then this will be skipped.
Similarly, if no nodal changes are logged, this information will be skipped
too.
}


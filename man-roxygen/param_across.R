#' @param across Which margin of the network the operation is taken over.
#'   "rows" is the sending margin, so that each node is treated by the ties it
#'   sends; "columns" is the receiving margin; and "both" combines them.
#'   The two margins coincide for an undirected network,
#'   where all three options therefore agree.
#'   A two-mode network raises no such question,
#'   so "rows" and "columns" there are just the two nodesets.
#'
#'   Note that both the default and what "both" combines differ by function.
#'   `to_normalised()` defaults to "both", where it divides by the square root
#'   of the two denominators multiplied together, since that is the only
#'   rescaling that preserves a network's symmetry.
#'   `to_proximity()` defaults to "rows", the conventional profile for
#'   structural equivalence, and there "both" compares each node's sent and
#'   received ties together.

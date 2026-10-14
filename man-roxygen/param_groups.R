#' @param groups How the nodes are divided into groups, in one of three ways:
#'   \itemize{
#'   \item A single integer, e.g. `groups = 3`, is the number of groups,
#'   which are then as equal in size as they can be.
#'   \item A vector of integers that sum to the number of nodes,
#'   e.g. `groups = c(10, 20, 30)` for 60 nodes, is the size of each group.
#'   \item Any other two integers, e.g. `groups = c(10, 30)`, are the
#'   smallest and the largest that a group can be,
#'   and the sizes of the groups are drawn at random from between them.
#'   }
#'   Two integers that sum to the number of nodes are read as two sizes,
#'   and not as the smallest and the largest.
#'   In a two-mode network, a group has nodes of both modes,
#'   in the proportion of the two modes in the network as a whole,
#'   and its size is the size of the two together.
#'   The group of each node is recorded in the node attribute `community`.
#'   <%= groups_detail %>

#' Quickly graph networks with sensible defaults
#' 
#' @description 
#' The aim of this function is to provide users with a quick and easy
#' graphing function that makes best use of the data,
#' whatever its composition.
#' Users can also tailor the plot according to their
#' preferences regarding node size, colour, and shape.
#' The function also supports visualisation
#' of network measures such as centrality.
#' @family mapping
#' @param .data A manynet-consistent object.
#' @param layout An igraph, ggraph, or manynet layout algorithm.
#'   If not declared, defaults to "triad" for networks with 3 nodes,
#'   "quad" for networks with 4 nodes,
#'   "stress" for all other one mode networks,
#'   or "hierarchy" for two mode networks.
#'   For "hierarchy" layout, one can further split graph by
#'   declaring the "center" argument as the "events", "actors",
#'   or by declaring a node name.
#'   For "concentric" layout algorithm please declare the "membership" as an 
#'   extra argument.
#'   The "membership" argument expects either a quoted node attribute present
#'   in data or vector with the same length as nodes to draw concentric circles.
#'   For "multilevel" layout algorithm please declare the "level"
#'   as extra argument.
#'   The "level" argument expects either a quoted node attribute present
#'   in data or vector with the same length as nodes to hierarchically
#'   order categories.
#'   If "level" is missing, function will look for 'lvl' node attribute in data.
#'   The "lineage" layout ranks nodes in Y axis according to values.
#'   For "lineage" layout algorithm please declare the "rank"
#'   as extra argument.
#'   The "rank" argument expects either a quoted node attribute present
#'   in data or vector with the same length as nodes.
#' @param labels Logical, whether to print node names
#'   as labels if present.
#' @param node_shape Node variable to be used for shaping the nodes.
#'   It is easiest if this is added as a node attribute to
#'   the graph before plotting.
#'   Nodes can also be shaped by declaring a shape instead.
#' @param node_size Node variable to be used for sizing the nodes.
#'   This can be any continuous variable on the nodes of the network.
#'   Since this function expects this to be an existing variable,
#'   it is recommended to calculate all node-related statistics prior
#'   to using this function.
#'   Nodes can also be sized by declaring a numeric size or vector instead.
#' @param node_color Node variable to be used for coloring the nodes.
#'   It is easiest if this is added as a node attribute to
#'   the graph before plotting.
#'   Nodes can also be colored by declaring a color instead.
#' @param node_group Node variable to be used for grouping the nodes.
#'   It is easiest if this is added as a hull over
#'   groups before plotting.
#'   Group variables should have a minimum of 3 nodes,
#'   if less, number groups will be reduced by
#'   merging categories with lower counts into one called "other".
#' @param edge_color Tie variable to be used for coloring the nodes.
#'   It is easiest if this is added as an edge or tie attribute 
#'   to the graph before plotting.
#'   Edges can also be colored by declaring a color instead.
#' @param edge_size Edge variable to be used for sizing the edges.
#'   This can be any continuous variable on the nodes of the network.
#'   Since this function expects this to be an existing variable,
#'   it is recommended to calculate all edge-related statistics prior
#'   to using this function.
#'   Edges can also be sized by declaring a numeric size or vector instead.
#' @param ... Extra arguments to pass on to layout.
#' @return A ggplot2::ggplot() object.
#' @importFrom ggraph geom_edge_link geom_node_text geom_conn_bundle
#' get_con geom_node_point scale_edge_width_continuous geom_node_label
#' @importFrom ggplot2 aes arrow unit scale_color_brewer scale_fill_brewer
#' @name autographing
NULL

#' @describeIn autographing Graphs a network with sensible defaults
#' @examples
#' autographr(ison_adolescents)
#' autographr(ison_algebra, layout = "circle",
#'            node_size = 8, node_color = "orange", node_shape = "square",
#'            edge_color = "blue", edge_size = 2)
#' autographr(ison_southern_women, layout = "concentric",
#'            node_color = "type", membership = "type")
#' autographr(play_diffusion(ison_karateka))
#' #autographr(ison_algebra, edge_color = "type",
#' #           node_size = migraph::node_betweenness(ison_algebra)*100)
#' #ison_adolescents %>%
#' #   mutate(year = rep(c(1985, 1990, 1995, 2000), times = 2)) %>%
#' #   autographr(layout = "lineage", rank = "year")
#' #ison_adolescents %>%
#' #   mutate(cut = node_is_cutpoint(ison_adolescents)) %>%
#' #autographr(node_color = "cut", node_shape = "cut")
#' #autographr(ison_lotr, node_color = Race,
#' #           node_size = migraph::node_degree(ison_lotr)*2,
#' #           edge_color = "darkgreen",
#' #           edge_size = migraph::tie_degree(ison_lotr))
#' #autographr(ison_karateka, node_group = allegiance,
#' #           edge_size = migraph::tie_closeness(ison_karateka))
#' #autographr(ison_southern_women, layout = "hierarchy", center = "events")
#' #autographr(ison_lotr, layout = "multilevel", 
#' #           node_color = "Race", level = "Race")
#' @export
autographr <- function(.data, layout, labels = TRUE,
                       node_color, node_shape, node_size, node_group,
                       edge_color, edge_size, ...) {
  g <- as_tidygraph(.data)
  if (missing(layout)) {
    if (length(g) == 3) {
      layout <- "triad" 
    } else if (length(g) == 4) {
      layout <- "quad" 
    } else if (is_twomode(g)) {
      layout <- "hierarchy"
    } else layout <- "stress"
  }
  if (missing(node_color)) node_color <- NULL else
    node_color <- as.character(substitute(node_color))
  if (missing(node_shape)) node_shape <- NULL else
    node_shape <- as.character(substitute(node_shape))
  if (missing(node_size)) node_size <- NULL else if (!is.numeric(node_size)) {
    node_size <- as.character(substitute(node_size))
  }
  if (missing(node_group)) node_group <- NULL else {
    node_group <- as.character(substitute(node_group))
    g <- activate(g, "nodes") %>%
      mutate(node_group = reduce_categories(g, node_group))
    }
  if (missing(edge_color)) edge_color <- NULL else
    edge_color <- as.character(substitute(edge_color))
  if (missing(edge_size)) edge_size <- NULL else if (!is.numeric(edge_size)) {
    edge_size <- as.character(substitute(edge_size))
  }
  # Add layout ----
  p <- .graph_layout(g, layout, labels, node_group, ...)
  # Add edges ----
  p <- .graph_edges(p, g, edge_color, edge_size)
  # Add nodes ----
  p <- .graph_nodes(p, g, node_color, node_shape, node_size)
  p
}

#' @describeIn autographing Graphs a list of networks 
#'   with sensible defaults
#' @param netlist A list of manynet-compatible networks.
#' @param waves The number of plots to be displayed side-by-side.
#'   If missing, the number of plots will be reduced to the first and last
#'   when there are more than four plots.
#' @param based_on Whether the layout of the joint plots should
#'   be based on the "first" or the "last" network.
#' @source http://blog.schochastics.net/post/animating-network-evolutions-with-gganimate/
#' @examples
#' #autographs(to_egos(ison_adolescents))
#' #autographs(to_egos(ison_adolescents), waves = 8)
#' #autographs(to_egos(ison_adolescents), waves = c(2, 4, 6))
#' #autographs(play_diffusion(ison_adolescents))
#' @export
autographs <- function(netlist, waves,
                       based_on = "first", ...) {
  thisRequires("patchwork")
  if (any(class(netlist) == "diff_model")) netlist <- to_waves(netlist)
  if (missing(waves)) {
    if (length(netlist) > 4) {
      netlist <- netlist[c(1, length(netlist))]
      message("Plotting first and last waves side-by-side, to to control the number of waves plotted use the 'waves'.")
    }
  } else if (!missing(waves)) {
    if (length(waves) == 1) netlist <- netlist[c(1:waves)] else 
      netlist <- netlist[waves]
  }
  if (is.null(names(netlist))) names(netlist) <- rep("", length(netlist))
  if (length(unique(unname(lapply(netlist, length)))) == 1) {
    if (based_on == "first") {
      lay <- autographr(netlist[[1]], ...)
      x <- lay$data$x
      y <- lay$data$y
    } else if (based_on == "last") {
      lay <- autographr(netlist[[length(netlist)]], ...)
      x <- lay$data$x
      y <- lay$data$y
    } else if (based_on == "both") {
      lay <- autographr(netlist[[1]], ...)
      x1 <- lay$data$x
      y1 <- lay$data$y
      lay1 <- autographr(netlist[[length(netlist)]], ...)
      x <- (lay1$data$x + x1)/2
      y <- (lay1$data$y + y1)/2
    }
    gs <- lapply(1:length(netlist), function(i)
      autographr(netlist[[i]], x = x, y = y, ...) + ggtitle(names(netlist)[i]))
  } else {
    message("Layouts were not standardised since nodes appear across waves.")
    gs <- lapply(1:length(netlist), function(i)
      autographr(netlist[[i]], ...) + ggtitle(names(netlist)[i]))
  }
  if (all(c("Infected", "Exposed", "Recovered") %in% names(gs[[1]]$data))) {
    gs <- collapse_guides(gs)
  }
  do.call(patchwork::wrap_plots, c(gs, list(guides = "collect")))
}

#' @describeIn autographing Graphs an dynamic (animated) network
#'   with sensible defaults
#' @param tlist The same migraph-compatible network listed according to
#'   a time attribute, waves, or slices.
#' @param keep_isolates Would you like to remove vertices that do not have
#'   any adjacent edges in each frame?
#'   TRUE by default.
#'   If FALSE, deletes isolated vertices in each frame.
#' @importFrom igraph gsize as_data_frame get.edgelist
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_text
#' scale_alpha_manual theme_void
#' @importFrom ggraph create_layout
#' @importFrom dplyr mutate select distinct left_join %>%
#' @source http://blog.schochastics.net/post/animating-network-evolutions-with-gganimate/
#' @examples
#' #ison_adolescents %>%
#' #  mutate_ties(year = sample(1995:1998, 10, replace = TRUE)) %>%
#' #  to_waves(attribute = "year", cumulative = TRUE) %>%
#' #  autographd()
#' #ison_adolescents %>% 
#' #  mutate(gender = rep(c("male", "female"), times = 4),
#' #         hair = rep(c("black", "brown"), times = 4),
#' #         age = sample(11:16, 8, replace = TRUE)) %>%
#' #  mutate_ties(year = sample(1995:1998, 10, replace = TRUE),
#' #              links = sample(c("friends", "not_friends"), 10, replace = TRUE),
#' #              weekly_meetings = sample(c(3, 5, 7), 10, replace = TRUE)) %>%
#' #  to_waves(attribute = "year") %>%
#' #  autographd(layout = "concentric", membership = "gender",
#' #             node_shape = "gender", node_color = "hair",
#' #             node_size =  "age", edge_color = "links",
#' #             edge_size = "weekly_meetings")
#' #autographd(play_diffusion(ison_adolescents, seeds = 5, recovery = 0.1))
#' @export
autographd <- function(tlist, layout, labels = TRUE,
                       node_color, node_shape, node_size,
                       edge_color, edge_size, keep_isolates = TRUE, ...) {
  thisRequires("gganimate")
  thisRequires("gifski")
  thisRequires("png")
  x <- y <- name <- status <- frame <- NULL
  # Check arguments
  if (missing(layout)) {
    if (length(tlist[[1]]) == 3) {
      layout <- "triad" 
    } else if (length(tlist[[1]]) == 4) {
      layout <- "quad" 
    } else if (is_twomode(tlist[[1]])) {
      layout <- "hierarchy"
    } else layout <- "stress"
  }
  if (missing(node_color)) node_color <- NULL else
    node_color <- as.character(substitute(node_color))
  if (missing(node_shape)) node_shape <- NULL else
    node_shape <- as.character(substitute(node_shape))
  if (missing(node_size)) node_size <- NULL else if (!is.numeric(node_size)) {
    node_size <- as.character(substitute(node_size))
  }
  if (missing(edge_color)) edge_color <- NULL else
    edge_color <- as.character(substitute(edge_color))
  if (missing(edge_size)) edge_size <- NULL else if (!is.numeric(edge_size)) {
    edge_size <- as.character(substitute(edge_size))
  }
  # Check if diffusion model
  if (inherits(tlist, "diff_model")) tlist <- to_waves(tlist)
  # Check if object is a list of lists
  if (!is.list(tlist[[1]])) {
    stop("Please declare a migraph-compatible network listed according
         to a time attribute, waves, or slices.")
  }
  # Remove lists without edges
  tlist <- Filter(function(x) igraph::gsize(x) > 0, tlist)
  # Check names for groups
  if (!"name" %in% names(node_attribute(tlist[[1]]))) {
    labels <- FALSE
    for (i in seq_len(length(tlist))) {
      tlist[[i]] <- add_node_attribute(tlist[[i]], "name",
                                       as.character(seq_len(igraph::vcount(tlist[[i]]))))
    }
  }
  # Create an edge list
  edges_lst <- lapply(1:length(tlist), function(i)
    cbind(igraph::as_data_frame(tlist[[i]], "edges"),
          frame = ifelse(is.null(names(tlist)), i, names(tlist)[i])))
  # Check if all names are present in all lists
  if (length(unique(unname(lapply(tlist, length)))) != 1) {
    if (any(c(node_shape, node_color, node_size) %in% names(node_attribute(tlist[[1]])))) {
      node_info <- dplyr::distinct(do.call(rbind, lapply(1:length(tlist), function(i)
        tlist[[i]] %>% activate("nodes") %>% data.frame()))) # keep node info for latter
    } else node_info <- NULL
    tlist <- to_waves(as_tidygraph(do.call("rbind", edges_lst)), attribute = "frame")
  } else node_info <- NULL
  # Add separate layouts for each time point
  lay <- lapply(1:length(tlist), function(i)
    ggraph::create_layout(tlist[[i]], layout, ...))
  # Create a node list for each time point
  nodes_lst <- lapply(1:length(tlist), function(i) {
    cbind(igraph::as_data_frame(tlist[[i]], "vertices"),
          x = lay[[i]][, 1], y = lay[[i]][, 2],
          frame = ifelse(is.null(names(tlist)), i, names(tlist)[i]))
  })
  # Create an edge list for each time point
  edges_lst <- time_edges_lst(tlist, edges_lst, nodes_lst)
  # Get edge IDs for all edges
  all_edges <- do.call("rbind", lapply(tlist, igraph::get.edgelist))
  all_edges <- all_edges[!duplicated(all_edges), ]
  all_edges <- cbind(all_edges, paste0(all_edges[, 1], "-", all_edges[, 2]))
  # Add edges level information for edge transitions
  edges_lst <- transition_edge_lst(tlist, edges_lst, nodes_lst, all_edges)
  # Bind nodes and edges list
  edges_out <- do.call("rbind", edges_lst)
  nodes_out <- do.call("rbind", nodes_lst)
  if (!is.null(node_info)) {
    nodes_out <- dplyr::left_join(nodes_out, node_info[!duplicated(node_info$name),], by = "name")
  }
  # Delete nodes for each frame if isolate
  if (isFALSE(keep_isolates)) {
    nodes_out <- remove_isolates(edges_out, nodes_out)
  } else {
    if (nrow(nodes_out)/length(unique(nodes_out$frame)) > 30 &
       any(unlist(lapply(tlist, node_is_isolate)) == TRUE)) {
      message("Please considering deleting isolates to improve visualisation.")
    } 
    nodes_out$status <- TRUE
  }
  # Plot with ggplot2/ggraph and animate with gganimate
  p <- map_dynamic(edges_out, nodes_out, edge_color, node_shape,
                   node_color, node_size, edge_size, labels) +
    gganimate::transition_states(states = frame, transition_length = 5,
                                 state_length = 10, wrap = FALSE) +
    gganimate::enter_fade() +
    gganimate::exit_fade() +
    ggplot2::labs(title = "{closest_state}")
  gganimate::animate(p, duration = 2*length(tlist), start_pause = 5,
                     end_pause = 10, renderer = gganimate::gifski_renderer())
}

reduce_categories <- function(g, node_group) {
  limit <- toCondense <- NULL
  if (sum(table(node_attribute(g, node_group)) <= 2) > 2 &
      length(unique(node_attribute(g, node_group))) > 2) {
    toCondense <- names(which(table(node_attribute(g, node_group)) <= 2))
    out <- ifelse(node_attribute(g, node_group) %in% toCondense,
                  "Other", node_attribute(g, node_group))
    message("The number of groups was reduced since there were groups with less than 2 nodes.")
  } else if (sum(table(node_attribute(g, node_group)) <= 2) == 2 &
             length(unique(node_attribute(g, node_group))) > 2) {
    limit <- stats::reorder(node_attribute(g, node_group),
                            node_attribute(g, node_group),
                            FUN = length, decreasing = TRUE)
    if (sum(utils::tail(attr(limit, "scores"), 2))) {
      toCondense <- utils::tail(levels(limit), 3)
    } else {
      toCondense <- utils::tail(levels(limit), 2)
    }
    out <- ifelse(node_attribute(g, node_group) %in% toCondense, "Other",
                  node_attribute(g, node_group))
    message("The number of groups was reduced since there were groups with less than 2 nodes.")
  } else if (sum(table(node_attribute(g, node_group)) <= 2) == 1 &
             length(unique(node_attribute(g, node_group))) > 2) {
    limit <- stats::reorder(node_attribute(g, node_group),
                            node_attribute(g, node_group),
                            FUN = length, decreasing = TRUE)
    toCondense <- utils::tail(levels(limit), 2)
    out <- ifelse(node_attribute(g, node_group) %in% toCondense, "Other",
                  node_attribute(g, node_group))
    message("The number of groups was reduced since there were groups with less than 2 nodes.")
  } else if (sum(table(node_attribute(g, node_group)) <= 2) == 1 &
             length(unique(node_attribute(g, node_group))) == 2) {
    out <- as.factor(node_attribute(g, node_group))
    message("Node groups with 2 nodes or less can be cause issues for plotting ...")
  } else out <- as.factor(node_attribute(g, node_group))
  out
}

.graph_layout <- function(g, layout, labels, node_group, ...){
  name <- NULL
  dots <- list(...)
  if ("x" %in% names(dots) & "y" %in% names(dots)) {
    lo <- ggraph::create_layout(g, layout = "manual",
                                x = dots[["x"]], y = dots[["y"]])
  } else lo <- ggraph::create_layout(g, layout, ...)
  if ("graph" %in% names(attributes(lo))) {
    if (!setequal(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
      for (n in setdiff(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
        lo[n] <- igraph::vertex_attr(g, n)
      }
    }
  }
  # if (layout == "stress" & .is_diamond(g)) {
  #   turn <- matrix(c(cos(0.71), -sin(0.71), sin(0.71), cos(0.71)), 2, 2)
  #   coord <- matrix(cbind(lo[,1], lo[,2]), ncol = 2) %*% turn
  #   lo[,1] <- coord[,1]
  #   lo[,2] <- coord[,2]
  # }
  p <- ggraph::ggraph(lo) + ggplot2::theme_void()
  if (labels & is_labelled(g)) {
    if (layout == "circle") {
      # https://stackoverflow.com/questions/57000414/ggraph-node-labels-truncated?rq=1
      angles <- as.data.frame(cart2pol(as.matrix(lo[,1:2])))
      angles$degree <- angles$phi * 180/pi
      angles <- dplyr::case_when(lo[,2] == 0 & lo[,1] == 0 ~ 0.1,
                                 lo[,2] >= 0 & lo[,1] > 0 ~ angles$degree, 
                                 lo[,2] < 0 & lo[,1] > 0 ~ angles$degree,
                                 lo[,1] == 1 ~ angles$degree,
                                 TRUE ~ angles$degree - 180)
      if (network_nodes(g) < 20) {
        hj <- ifelse(lo[,1] >= 0, -0.4, 1.4)
        vj <- ifelse(lo[,2] >= 0, -0.4, 1.4)
      } else {
        hj <- ifelse(lo[,1] >= 0, -0.2, 1.2)
        vj <- ifelse(lo[,2] >= 0, -0.2, 1.2)
      }
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE,
                                      size = 3, hjust = hj, angle = angles) +
        ggplot2::coord_cartesian(xlim=c(-1.2,1.2), ylim=c(-1.2,1.2))
    } else if (layout == "concentric") {
      if (network_nodes(g) < 20) {
        hj <- ifelse(lo[,1] >= 0, -0.8, 1.8)
        vj <- ifelse(lo[,2] >= 0, -0.8, 1.8)
      } else if (network_nodes(g) > 20 & network_nodes(g) < 30) {
        hj <- ifelse(lo[,1] >= 0, -0.4, 1.4)
        vj <- ifelse(lo[,2] >= 0, -0.4, 1.4)
      } else {
        hj <- ifelse(lo[,1] >= 0, -0.2, 1.2)
        vj <- ifelse(lo[,2] >= 0, -0.2, 1.2)
      }
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = name),  vjust = vj,
                                      size = 3, hjust = hj, repel = TRUE) +
        ggplot2::coord_cartesian(xlim=c(-1.2,1.2), ylim=c(-1.2,1.2))
    } else if (layout %in% c("bipartite", "railway") |
               (layout == "hierarchy" & length(unique(lo[,2])) <= 2)) {
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), angle = 90,
                                      size = 3, hjust = "outward", repel = TRUE,
                                      nudge_y = ifelse(lo[,2] == 1, 0.05, -0.05)) +
        ggplot2::coord_cartesian(ylim=c(-0.2, 1.2))
    } else if (layout == "hierarchy" & length(unique(lo[,2])) > 2) {
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), size = 3,
                                      hjust = "inward", vjust = -0.4, repel = TRUE)
    } else if (layout %in% c("alluvial", "lineage")) {
      p <- p + ggraph::geom_node_label(ggplot2::aes(label = name), size = 3,
                                       repel = TRUE, label.size = 0,
                                       nudge_x = ifelse(lo[,1] == 1, 0.02, -0.02))
    } else {
      p <- p + ggraph::geom_node_label(ggplot2::aes(label = name), label.size = 0,
                                       repel = TRUE, seed = 1234, size = 3)
    }
  }
  if (!is.null(node_group)) {
    x <- y <- NULL
    thisRequires("ggforce")
    p <- p + 
      ggforce::geom_mark_hull(ggplot2::aes(x, y, fill = node_group,
                                           label = node_group), data = lo) +
      ggplot2::scale_fill_manual(values = colorsafe_palette,
                                 guide = ggplot2::guide_legend(""))
  }
  p
}

.graph_edges <- function(p, g, edge_color, edge_size) {
  weight <- NULL
  esize <- .infer_esize(g, edge_size)
  # Begin plotting edges in various cases
  if (is_directed(g)) {
    bend <- .infer_bend(g)
    if (is_weighted(g)) {
      if (!is.null(edge_color)) {
        if (edge_color %in% names(tie_attribute(g))) {
          p <- p + ggraph::geom_edge_arc(ggplot2::aes(
            width = esize, colour = as.factor(tie_attribute(g, edge_color))),
                                         edge_alpha = 0.4, strength = bend,
                                         edge_linetype = "solid",
                                         arrow = ggplot2::arrow(angle = 15,
                                                                length = ggplot2::unit(2, 'mm'),
                                                                type = "closed"), 
                                         end_cap = ggraph::circle(1.5, 'mm')) +
            ggraph::scale_edge_width_continuous(range = c(0.2, 2.5), guide = "none") +
            ggraph::scale_edge_colour_manual(values = colorsafe_palette,
                                             guide = ggplot2::guide_legend(""))
        } else {
          p <- p + ggraph::geom_edge_arc(ggplot2::aes(width = esize),
                                         colour = edge_color,
                                         edge_alpha = 0.4, strength = bend,
                                         edge_linetype = "solid",
                                         arrow = ggplot2::arrow(angle = 15,
                                                                length = ggplot2::unit(2, 'mm'),
                                                                type = "closed"), 
                                         end_cap = ggraph::circle(1.5, 'mm')) +
            ggraph::scale_edge_width_continuous(range = c(0.2, 2.5), guide = "none")
        }
      } else if (is_signed(g)) {
        p <- p + ggraph::geom_edge_arc(
          ggplot2::aes(width = esize,
                       colour = ifelse(igraph::E(g)$sign >= 0, "#d73027", "#4575b4"),
                       linetype = ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")),
                                        edge_alpha = 0.4, strength = bend,
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(2, 'mm'),
                                                               type = "closed"), 
                                        end_cap = ggraph::circle(1.5, 'mm')) +
          ggraph::scale_edge_width_continuous(range = c(0.2, 2.5), guide = "none")
      } else {
        p <- p + ggraph::geom_edge_arc(ggplot2::aes(width = esize),
                                        edge_colour = "black",
                                        edge_alpha = 0.4, strength = bend,
                                        edge_linetype = "solid",
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(2, 'mm'),
                                                               type = "closed"), 
                                        end_cap = ggraph::circle(1.5, 'mm')) +
          ggraph::scale_edge_width_continuous(range = c(0.2, 2.5), guide = "none")
      }
    } else {
      if (!is.null(edge_color)) {
        if (edge_color %in% names(tie_attribute(g))) {
        p <- p + ggraph::geom_edge_arc(ggplot2::aes(
          colour = as.factor(tie_attribute(g, edge_color))),
                                        edge_alpha = 0.4, strength = bend,
                                        edge_linetype = "solid",
                                        edge_width = esize,
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(3, "mm"),
                                                               type = "closed"),
                                        end_cap = ggraph::circle(3, "mm")) +
          ggraph::scale_edge_colour_manual(values = colorsafe_palette,
                                           guide = ggplot2::guide_legend(""))
        } else {
          p <- p + ggraph::geom_edge_arc(colour = edge_color,
                                         edge_alpha = 0.4, strength = bend,
                                         edge_linetype = "solid",
                                         edge_width = esize,
                                         arrow = ggplot2::arrow(angle = 15,
                                                                length = ggplot2::unit(3, "mm"),
                                                                type = "closed"),
                                         end_cap = ggraph::circle(3, "mm"))
        }
      } else if (is_signed(g)) {
        p <- p + ggraph::geom_edge_arc(
          ggplot2::aes(colour = ifelse(igraph::E(g)$sign >= 0, "#d73027", "#4575b4"),
                       linetype = ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")),
          edge_alpha = 0.4, strength = bend, edge_width = esize,
          arrow = ggplot2::arrow(angle = 15, length = ggplot2::unit(3, "mm"),
                                 type = "closed"), end_cap = ggraph::circle(3, "mm"))
      } else {
        p <- p + ggraph::geom_edge_arc(edge_colour = "black",
                                        edge_alpha = 0.4, strength = bend,
                                        edge_linetype = "solid",
                                        edge_width = esize,
                                        arrow = ggplot2::arrow(angle = 15,
                                                               length = ggplot2::unit(3, "mm"),
                                                               type = "closed"),
                                        end_cap = ggraph::circle(3, "mm"))
      }
    }
  } else {
    if (is_weighted(g)) { # weighted and undirected
      if (!is.null(edge_color)) {
        if (edge_color %in% names(tie_attribute(g))) {
        p <- p + ggraph::geom_edge_link0(ggplot2::aes(
          width = esize, colour = as.factor(tie_attribute(g, edge_color))),
                                        edge_alpha = 0.4, edge_linetype = "solid") +
          ggraph::scale_edge_width_continuous(range = c(0.2, 1), guide = "none") +
          ggraph::scale_edge_colour_manual(values = colorsafe_palette,
                                           guide = ggplot2::guide_legend(""))
        } else {
          p <- p + ggraph::geom_edge_link0(ggplot2::aes(width = weight),
                                          colour = edge_color,
                                          edge_alpha = 0.4,
                                          edge_linetype = "solid") +
            ggraph::scale_edge_width_continuous(range = c(0.2, 1), guide = "none")
        }
      } else if (is_signed(g)) {
        p <- p + ggraph::geom_edge_link0(
          ggplot2::aes(width = weight,
                       colour = ifelse(igraph::E(g)$sign >= 0, "#d73027", "#4575b4"),
                       linetype = ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")),
          edge_alpha = 0.4) +
          ggraph::scale_edge_width_continuous(range = c(0.2, 1), guide = "none")
      } else {
        p <- p + ggraph::geom_edge_link0(ggplot2::aes(width = weight),
                                         edge_colour = "black",
                                         edge_linetype = "solid",
                                         edge_alpha = 0.4) + 
          ggraph::scale_edge_width_continuous(range = c(0.2, 1), guide = "none")
      }
    } else { # unweighted and undirected
      if (!is.null(edge_color)) {
        if (edge_color %in% names(tie_attribute(g))) {
        p <- p + ggraph::geom_edge_link0(ggplot2::aes(
          colour = as.factor(tie_attribute(g, edge_color))),
                                         edge_linetype = "solid",
                                         edge_alpha = 0.4, edge_width = esize) +
          ggraph::scale_edge_colour_manual(values = colorsafe_palette,
                                           guide = ggplot2::guide_legend(""))
        } else {
          p <- p + ggraph::geom_edge_link0(colour = edge_color,
                                           edge_linetype = "solid",
                                           edge_alpha = 0.4, edge_width = esize)
        }
      } else if (is_signed(g)) {
        p <- p + ggraph::geom_edge_link0(
          ggplot2::aes(colour = ifelse(igraph::E(g)$sign >= 0,  "#d73027", "#4575b4"),
                       linetype = ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")),
          edge_alpha = 0.4, edge_width = esize)
      } else {
        p <- p + ggraph::geom_edge_link0(edge_colour = "black",
                                         edge_linetype = "solid",
                                         edge_alpha = 0.4, edge_width = esize)
      }
    }
  }
  if (is_complex(g)) {
    lsize <- .infer_lsize(g, edge_size)
    if (is_signed(g)) {
      p <- p + ggraph::geom_edge_loop(
        ggplot2::aes(width = weight,
                     colour = ifelse(igraph::E(g)$sign >= 0, "#d73027", "#4575b4"),
                     linetype = ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")),
        edge_alpha = 0.4, edge_width = lsize) +
        ggraph::scale_edge_width_continuous(range = c(0.2, 1), guide = "none")
    } else if (!is.null(edge_color)) {
      if (edge_color %in% names(tie_attribute(g))) {
        p <- p + ggraph::geom_edge_loop(ggplot2::aes(
          colour = as.factor(tie_attribute(g, edge_color))),
          edge_linetype = "solid",
          edge_alpha = 0.4, edge_width = lsize) +
          ggraph::scale_edge_colour_manual(values = colorsafe_palette,
                                           guide = ggplot2::guide_legend(""))
      } else {
        p <- p + ggraph::geom_edge_link0(colour = edge_color,
                                         edge_linetype = "solid",
                                         edge_alpha = 0.4, edge_width = lsize)
      } 
    } else {
      p <- p + ggraph::geom_edge_loop(edge_colour = "black",
                                      edge_linetype = "solid",
                                      edge_alpha = 0.4, edge_width = lsize)
    }
  }
  p
}

.graph_nodes <- function(p, g, node_color, node_shape, node_size){
  nsize <- .infer_nsize(g, node_size)
  nshape <- .infer_shape(g, node_shape)
  if (is.null(node_color) & "Infected" %in% names(node_attribute(g))) {
    node_color <- as.factor(ifelse(node_attribute(g, "Exposed"), "Exposed",
                                   ifelse(node_attribute(g, "Infected"),"Infected", 
                                          ifelse(node_attribute(g, "Recovered"), "Recovered",
                                                 "Susceptible"))))
    p <- p + ggraph::geom_node_point(ggplot2::aes(color = node_color),
                                     size = nsize, shape = nshape) +
      ggplot2::scale_color_manual(name = NULL, guide = ggplot2::guide_legend(""),
                                  values = c("Infected" = "red",
                                             "Susceptible" = "blue",
                                             "Exposed" = "orange",
                                             "Recovered" = "darkgreen"))
  } else if (is.null(node_color) & any("diff_model" %in% names(attributes(g)))) {
    node_adopts <- .node_adoption_time(g)
    nshape <- ifelse(node_adopts == min(node_adopts), "Seed(s)",
                     ifelse(node_adopts == Inf, "Non-Adopter", "Adopter"))
    node_color <- ifelse(is.infinite(node_adopts), 
                         max(node_adopts[!is.infinite(node_adopts)]) + 1, 
                         node_adopts)
    p <- p + ggraph::geom_node_point(ggplot2::aes(shape = nshape,
                                                  color = node_color),
                                     size = nsize) +
      ggplot2::scale_color_gradient(low = "red", high = "blue",
                                    breaks=c(min(node_color)+1, 
                                             ifelse(any(nshape=="Non-Adopter"),
                                                    max(node_color)-1,
                                                    max(node_color))),
                                    labels=c("Early\nadoption", "Late\nadoption"),
                                    name = "Time of\nAdoption\n") +
      ggplot2::scale_shape_manual(name = "",
                                  breaks = c("Seed(s)", "Adopter", "Non-Adopter"),
                                  values = c("Seed(s)" = "triangle",
                                             "Adopter" = "circle",
                                             "Non-Adopter" = "square")) +
      ggplot2::guides(color = ggplot2::guide_colorbar(order = 1, reverse = TRUE),
                      shape = ggplot2::guide_legend(order = 2))
  } else {
    if (is_twomode(g)) {
      if (!is.null(node_color)) {
        if (node_color %in% names(node_attribute(g))) {
          if (is_mark_attrib(node_attribute(g, node_color))) {
            node_color <- factor(node_attribute(g, node_color),
                                 levels = c("TRUE", "FALSE"))
          } else node_color <- factor(node_attribute(g, node_color))
          p <- p + ggraph::geom_node_point(ggplot2::aes(color = node_color),
                                           size = nsize, shape = nshape) +
            ggplot2::scale_colour_manual(values = colorsafe_palette,
                                         guide = ggplot2::guide_legend(""))
        } else {
          p <- p + ggraph::geom_node_point(color = node_color,
                                           size = nsize, shape = nshape) 
        }
      } else {
        p <- p + ggraph::geom_node_point(size = nsize, shape = nshape)
      }
    } else {
      if (!is.null(node_color)) {
        if (node_color %in% names(node_attribute(g))) {
          if (is_mark_attrib(node_attribute(g, node_color))) {
            node_color <- factor(node_attribute(g, node_color),
                                 levels = c("TRUE", "FALSE"))
          } else node_color <- factor(node_attribute(g, node_color))
          p <- p + ggraph::geom_node_point(aes(color = node_color),
                                           size = nsize, shape = nshape) +
            ggplot2::scale_colour_manual(values = colorsafe_palette, guide = ggplot2::guide_legend(""))
        } else {
          p <- p + ggraph::geom_node_point(color = node_color,
                                           size = nsize, shape = nshape)
        }
      } else {
        p <- p + ggraph::geom_node_point(size = nsize, shape = nshape)
      }
    }
  }
  p
}

.infer_bend <- function(g) {
  if (length(igraph::E(g)) > 100) {
    out <- 0
  } else {
    out <- ifelse(igraph::which_mutual(g), 0.2, 0)
  }
  out
}

.infer_nsize <- function(g, node_size){
  if (!is.null(node_size)) {
    if (is.character(node_size)) {
      out <- node_attribute(g, node_size)
    } else if (is.numeric(node_size)) {
      out <- node_size
    } else {
      out <- node_size(g)
    }
    if(length(node_size > 1) & all(out <= 1 & out >= 0)) out <- out*10
  } else {
    out <- ifelse(network_nodes(g) <= 10, 5, (100 / network_nodes(g)) / 2)
  }
  out
}

.infer_shape <- function(g, node_shape) {
  if (!is.null(node_shape)) {
    if (node_shape %in% names(node_attribute(g))) {
      out <- as.factor(node_attribute(g, node_shape))
    } else if (length(node_shape) == 1) {
      out <- rep(node_shape, network_nodes(g)) 
    }
  } else if (is_twomode(g)) {
    out <- ifelse(igraph::V(g)$type, "square", "circle")
  } else {
    out <- "circle"
  }
  out
}

.infer_esize <- function(g, edge_size) {
  if (!is.null(edge_size)) {
    if (is.character(edge_size)) {
      out <- tie_attribute(g, edge_size)
    } else {
      out <- edge_size
    }
    if (length(out > 1) & all(out <= 1 & out >= 0)) out <- out*10
  } else if (is.null(edge_size) & is_weighted(g)) {
    out <- tie_attribute(g, "weight")
  } else {
    out <- 0.5
  }
  out
}

.infer_lsize <- function(g, edge_size) {
  if (!is.null(edge_size)) {
    if (is.character(edge_size)) {
      out <- tie_attribute(g, edge_size)
    } else {
      out <- edge_size
    }
    if (length(out > 1) & all(out <= 1 & out >= 0)) {
      out <- sum(out)/length(out)*10
    } else if (length(out > 1)) {
      out <- sum(out)/length(out)
    }
  } else {
    out <- 0.5
  }
  out
}

.is_diamond <- function(x) {
  x <- as_matrix(x)
  if (is.numeric(x)) {
    if (length(x) == 100 | length(x) == 10000 &
        suppressWarnings(all(unique(rowSums(x)) == c(3, 5, 8)))) {
      TRUE
    } else FALSE 
  } else FALSE
}

.node_adoption_time <- function(g){
  diff_model <- attr(g, "diff_model")
  event <- nodes <- NULL
  out <- summary(diff_model) %>% dplyr::filter(event == "I") %>% 
    dplyr::distinct(nodes, .keep_all = TRUE) %>% 
    dplyr::select(nodes,t)
  net <- attr(diff_model, "network")
  if(!is_labelled(net))
    out <- dplyr::arrange(out, nodes) else if (is.numeric(out$nodes))
      out$nodes <- node_names(net)[out$nodes]
  out <- stats::setNames(out$t, out$nodes)
  if(length(out) != network_nodes(net)){
    full <- rep(Inf, network_nodes(net))
    names(full) <- `if`(is_labelled(net), 
                        node_names(net), 
                        as.character(seq_len(network_nodes(net))))
    full[match(names(out), names(full))] <- out
    out <- `if`(is_labelled(net), full, unname(full))
  }
  out
}

cart2pol <- function(xyz){
  stopifnot(is.numeric(xyz))
  if (is.vector(xyz) && (length(xyz) == 2 || length(xyz) == 
                         3)) {
    x <- xyz[1]
    y <- xyz[2]
    m <- 1
    n <- length(xyz)
  }
  else if (is.matrix(xyz) && (ncol(xyz) == 2 || ncol(xyz) == 
                              3)) {
    x <- xyz[, 1]
    y <- xyz[, 2]
    m <- nrow(xyz)
    n <- ncol(xyz)
  }
  else stop("Input must be a vector of length 3 or a matrix with 3 columns.")
  phi <- atan2(y, x)
  r <- hypot(x, y)
  if (n == 2) {
    if (m == 1) 
      prz <- c(phi, r)
    else prz <- cbind(phi, r)
  }
  else {
    if (m == 1) {
      z <- xyz[3]
      prz <- c(phi, r, z)
    }
    else {
      z <- xyz[, 3]
      prz <- cbind(phi, r, z)
    }
  }
  return(prz)
}

hypot <- function (x, y) {
  if ((length(x) == 0 && is.numeric(y) && length(y) <= 1) || 
      (length(y) == 0 && is.numeric(x) && length(x) <= 1)) 
    return(vector())
  if (!is.numeric(x) && !is.complex(x) || !is.numeric(y) && 
      !is.complex(y)) 
    stop("Arguments 'x' and 'y' must be numeric or complex.")
  if (length(x) == 1 && length(y) > 1) {
    x <- rep(x, length(y))
    dim(x) <- dim(y)
  }
  else if (length(x) > 1 && length(y) == 1) {
    y <- rep(y, length(x))
    dim(y) <- dim(x)
  }
  if ((is.vector(x) && is.vector(y) && length(x) != length(y)) || 
      (is.matrix(x) && is.matrix(y) && dim(x) != dim(y)) || 
      (is.vector(x) && is.matrix(y)) || is.matrix(x) && is.vector(y)) 
    stop("Arguments 'x' and 'y' must be of the same size.")
  x <- abs(x)
  y <- abs(y)
  m <- pmin(x, y)
  M <- pmax(x, y)
  ifelse(M == 0, 0, M * sqrt(1 + (m/M)^2))
}

time_edges_lst <- function(tlist, edges_lst, nodes_lst, edge_color) {
  lapply(1:length(tlist), function(i) {
    edges_lst[[i]]$x <- nodes_lst[[i]]$x[match(edges_lst[[i]]$from,
                                               nodes_lst[[i]]$name)]
    edges_lst[[i]]$y <- nodes_lst[[i]]$y[match(edges_lst[[i]]$from,
                                               nodes_lst[[i]]$name)]
    edges_lst[[i]]$xend <- nodes_lst[[i]]$x[match(edges_lst[[i]]$to,
                                                  nodes_lst[[i]]$name)]
    edges_lst[[i]]$yend <- nodes_lst[[i]]$y[match(edges_lst[[i]]$to,
                                                  nodes_lst[[i]]$name)]
    edges_lst[[i]]$id <- paste0(edges_lst[[i]]$from, "-", edges_lst[[i]]$to)
    edges_lst[[i]]$status <- TRUE
    edges_lst[[i]]
  })
}

transition_edge_lst <- function(tlist, edges_lst, nodes_lst, all_edges) {
  x <- lapply(1:length(tlist), function(i) {
    idx <- which(!all_edges[, 3] %in% edges_lst[[i]]$id)
    if (length(idx) != 0) {
      tmp <- data.frame(from = all_edges[idx, 1], to = all_edges[idx, 2],
                        id = all_edges[idx, 3])
      tmp$x <- nodes_lst[[i]]$x[match(tmp$from, nodes_lst[[i]]$name)]
      tmp$y <- nodes_lst[[i]]$y[match(tmp$from, nodes_lst[[i]]$name)]
      tmp$xend <- nodes_lst[[i]]$x[match(tmp$to, nodes_lst[[i]]$name)]
      tmp$yend <- nodes_lst[[i]]$y[match(tmp$to, nodes_lst[[i]]$name)]
      tmp$frame <- ifelse(is.null(names(tlist)), i, names(tlist)[i])
      tmp$status <- FALSE
      edges_lst[[i]] <- dplyr::bind_rows(edges_lst[[i]], tmp)
    }
    edges_lst[[i]]
  })
}

remove_isolates <- function(edges_out, nodes_out) {
  status <- frame <- from <- to <- framen <- NULL
  # Create node metadata for node presence in certain frame
  meta <- edges_out %>%
    dplyr::filter(status == TRUE) %>%
    dplyr::mutate(framen = match(frame, unique(frame)),
                  meta = ifelse(framen > 1, paste0(from, (framen - 1)), from)) %>%
    dplyr::select(meta, status) %>%
    dplyr::distinct()
  metab <- edges_out %>%
    dplyr::filter(status == TRUE) %>%
    dplyr::mutate(framen = match(frame, unique(frame)),
                  meta = ifelse(framen > 1, paste0(to, (framen - 1)), to)) %>%
    dplyr::select(meta, status) %>%
    rbind(meta) %>%
    dplyr::distinct()
  # Mark nodes that are isolates
  nodes_out$meta <- rownames(nodes_out)
  # Join data
  nodes_out <- dplyr::left_join(nodes_out, metab, by = "meta") %>%
    dplyr::mutate(status = ifelse(is.na(status), FALSE, TRUE)) %>%
    dplyr::distinct()
}

map_dynamic <- function(edges_out, nodes_out, edge_color, node_shape,
                        node_color, node_size, edge_size, labels) {
  x <- xend <- y <- yend <- id <- status <- Infected <- name <- NULL
  alphad <- ifelse(nodes_out$status == TRUE, 1, 0)
  alphae <- ifelse(edges_out$status == TRUE, 1, 0)
  if (all(unique(alphae) == 1)) alphae <- 0.8
  # Plot edges
  if (!is.null(edge_color)) {
    # Remove NAs in edge color, if declared
    if (edge_color %in% names(edges_out)) {
      edge_color <- check_color(edges_out[[edge_color]])
    }
  } else edge_color <- "black"
  if (!is.null(edge_size)) {
    if (edge_size %in% names(edges_out)) {
    edge_size <- as.numeric(edges_out[[edge_size]])
    edge_size <- ifelse(is.na(edge_size), 0.5, edge_size)
    }
  } else edge_size <- 0.5
  p <- ggplot2::ggplot() + 
    ggplot2::geom_segment(aes(x = x, xend = xend, y = y, yend = yend, group = id),
                          alpha = alphae, data = edges_out, color = edge_color,
                          linewidth = edge_size, show.legend = FALSE)
  # Set node shape, color, and size
  if (!is.null(node_shape)) {
    if (node_shape %in% names(nodes_out)) {
      node_shape <- as.factor(nodes_out[[node_shape]])
      if (!any(grepl("circle|square|triangle", node_shape))) {
        node_shape <- c("circle", "square", "triangle")[node_shape]
      }
    }
    } else node_shape <- "circle"
  if (!is.null(node_color)) {
    if (node_color %in% names(nodes_out)) {
      node_color <- check_color(nodes_out[[node_color]])
    }
  } else if (is.null(node_color) & "Infected" %in% names(nodes_out)) {
    node_color <- as.factor(ifelse(nodes_out[["Exposed"]], "Exposed",
                                   ifelse(nodes_out[["Infected"]],"Infected", 
                                          ifelse(nodes_out[["Recovered"]], "Recovered",
                                                 "Susceptible"))))
  } else node_color <- "darkgray"
  if (!is.null(node_size)) {
    if (node_size %in% names(nodes_out)) {
      node_size <- nodes_out[[node_size]]
    }
  } else if (nrow(nodes_out) > 100) {
    node_size <- 3
  } else node_size <- nrow(nodes_out)/length(unique(nodes_out$frame))
  # Add labels
  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(aes(x, y, label = name), alpha = alphad,
                                data = nodes_out, color = "black",
                                hjust = -0.2, vjust = -0.2, show.legend = FALSE)
  }
  # Plot nodes
  if ("Infected" %in% names(nodes_out)) {
    p <- p + ggplot2::geom_point(aes(x, y, group = name, color = node_color),
                                 size = node_size, shape = node_shape, data = nodes_out) +
      ggplot2::scale_color_manual(name = NULL, values = c("Infected" = "red",
                                                          "Susceptible" = "blue",
                                                          "Exposed" = "orange",
                                                          "Recovered" = "darkgreen")) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom")
  } else {
    p <- p + ggplot2::geom_point(aes(x, y, group = name), alpha = alphad,
                                 size = node_size, data = nodes_out,
                                 color = node_color, shape = node_shape,
                                 show.legend = FALSE) +
      ggplot2::theme_void()
  }
  p
}

check_color <- function(v) {
  color <- grDevices::colors()
  color <- color[!color %in% "black"]
  v <- ifelse(is.na(v), "black", v)
  if (!any(grepl(paste(color, collapse = "|"), v)) | any(grepl("#", v))) {
    for(i in unique(v)) {
      if (i != "black") {
        v[v == i] <- sample(color, 1)
      }
    }
  }
  v
}

collapse_guides <- function(plist) {
  glist <- list()
  for (i in seq_len(length(plist))) {
    glist[[i]] <- names(which(apply(plist[[i]]$data[c("Infected", "Exposed",
                                                      "Recovered")],
                                    2, function(x) length(unique(x)) > 1)))
  }
  if (any(as.logical(lapply(glist, function(x) length(x) == 0))) &
      length(unique(glist)) == 2 | length(unique(glist)) == 1 |
      any(lengths(glist) > 1)) {
    kl <- which.max(unlist(lapply(glist, length)))
    for (i in setdiff(seq_len(length(plist)), kl)) {
      plist[[i]]$guides <- NULL
    }
  }
  plist
}

is_mark_attrib <- function(x) {
  if ("node_mark" %in% class(x)) TRUE else FALSE
}

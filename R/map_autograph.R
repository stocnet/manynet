# Single graphs ####

#' Easily graph networks with sensible defaults
#' 
#' @description 
#'   This function provides users with an easy way to graph
#'   (m)any network data for exploration, investigation, inspiration, 
#'   and communication.
#'   
#'   It builds upon `{ggplot2}` and `{ggraph}` to offer
#'   pretty and extensible graphing solutions.
#'   However, compared to those solutions, 
#'   `graphr()` contains various algorithms to provide better looking
#'   graphs by default.
#'   This means that just passing the function some network data
#'   will often be sufficient to return a reasonable-looking graph.
#'   
#'   The function also makes it easy to modify many of the most
#'   commonly adapted aspects of a graph, 
#'   including node and edge size, colour, and shape,
#'   as arguments rather than additional functions that you need to remember.
#'   These can be defined outright, e.g. `node_size = 8`, or
#'   in reference to an attribute of the network, e.g. `node_size = "wealth"`.
#'   
#'   Lastly, `graphr()` uses `{ggplot2}`-related theme information, so
#'   it is easy to make colour palette and fonts institution-specific and consistent.
#'   See e.g. `theme_iheid()` for more.
#' @name map_graphr
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
#' @param node_color,node_colour Node variable to be used for coloring the nodes.
#'   It is easiest if this is added as a node attribute to
#'   the graph before plotting.
#'   Nodes can also be colored by declaring a color instead.
#' @param node_group Node variable to be used for grouping the nodes.
#'   It is easiest if this is added as a hull over
#'   groups before plotting.
#'   Group variables should have a minimum of 3 nodes,
#'   if less, number groups will be reduced by
#'   merging categories with lower counts into one called "other".
#' @param edge_color,edge_colour Tie variable to be used for coloring the nodes.
#'   It is easiest if this is added as an edge or tie attribute 
#'   to the graph before plotting.
#'   Edges can also be colored by declaring a color instead.
#' @param edge_size Tie variable to be used for sizing the edges.
#'   This can be any continuous variable on the nodes of the network.
#'   Since this function expects this to be an existing variable,
#'   it is recommended to calculate all edge-related statistics prior
#'   to using this function.
#'   Edges can also be sized by declaring a numeric size or vector instead.
#' @param ... Extra arguments to pass on to the layout algorithm, if necessary.
#' @return A `ggplot2::ggplot()` object.
#'   The last plot can be saved to the file system using `ggplot2::ggsave()`.
#' @importFrom ggraph geom_edge_link geom_node_text geom_conn_bundle
#'   get_con geom_node_point scale_edge_width_continuous geom_node_label
#' @importFrom ggplot2 aes arrow unit scale_color_brewer scale_fill_brewer
#' @examples
#' graphr(ison_adolescents)
#' ison_adolescents %>%
#'   mutate(color = rep(c("extrovert", "introvert"), times = 4),
#'          size = ifelse(node_is_cutpoint(ison_adolescents), 6, 3)) %>%
#'   mutate_ties(ecolor = rep(c("friends", "acquaintances"), times = 5)) %>%
#'   graphr(node_color = "color", node_size = "size",
#'          edge_size = 1.5, edge_color = "ecolor")
#' graphr(ison_lotr, node_color = Race, node_size = node_degree(ison_lotr)*2,
#'        edge_color = "yellow", edge_size = tie_degree(ison_lotr))
#' graphr(ison_karateka, node_group = allegiance,
#'        edge_size = tie_closeness(ison_karateka))
#' @export
graphr <- function(.data, layout, labels = TRUE,
                   node_color, node_shape, node_size, node_group,
                   edge_color, edge_size, ...,
                   node_colour, edge_colour) {
  g <- as_tidygraph(.data)
  if (missing(layout)) {
    if (length(g) == 3 | length(g) == 4) {
      layout <- "configuration"
    } else if (is_twomode(g)) {
      layout <- "hierarchy"
    } else layout <- "stress"
  }
  if (missing(node_color) && missing(node_colour)) {
    node_color <- NULL
  } else if (missing(node_color)) {
    node_color <- as.character(substitute(node_colour))
  } else {
    node_color <- as.character(substitute(node_color))
  }
  if (missing(node_shape)) node_shape <- NULL else
    node_shape <- as.character(substitute(node_shape))
  if (missing(node_size)) node_size <- NULL else if (!is.numeric(node_size)) {
    node_size <- as.character(substitute(node_size))
  }
  if (missing(node_group)) node_group <- NULL else {
    node_group <- as.character(substitute(node_group))
    g <- tidygraph::activate(g, "nodes") %>%
      tidygraph::mutate(node_group = reduce_categories(g, node_group))
  }
  if (missing(edge_color) && missing(edge_colour)) {
    edge_color <- NULL
  } else if (missing(edge_color)) {
    edge_color <- as.character(substitute(edge_colour))
  } else {
    edge_color <- as.character(substitute(edge_color))
  }
  if (missing(edge_size)) edge_size <- NULL else if (!is.numeric(edge_size)) {
    edge_size <- as.character(substitute(edge_size))
  }
  # Add layout ----
  p <- .graph_layout(g, layout, labels, node_group, ...)
  # Add edges ----
  p <- .graph_edges(p, g, edge_color, edge_size, node_size)
  # Add nodes ----
  p <- .graph_nodes(p, g, node_color, node_shape, node_size)
  # Add labels ----
  if (isTRUE(labels) & is_labelled(g)) {
    p <- .graph_labels(p, g, layout)
  }
  p
}

.graph_layout <- function(g, layout, labels, node_group, ...) {
  name <- NULL
  dots <- list(...)
  if ("x" %in% names(dots) & "y" %in% names(dots)) {
    lo <- ggraph::create_layout(g, layout = "manual",
                                x = dots[["x"]], y = dots[["y"]])
  } else lo <- suppressWarnings(ggraph::create_layout(g, layout, ...))
  if ("graph" %in% names(attributes(lo))) {
    if (!setequal(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
      for (n in setdiff(names(as.data.frame(attr(lo, "graph"))), names(lo))) {
        lo[n] <- igraph::vertex_attr(g, n)
      }
    }
  }
  p <- ggraph::ggraph(lo) + ggplot2::theme_void()
  if (!is.null(node_group)) {
    x <- y <- NULL
    thisRequires("ggforce")
    p <- p + 
      ggforce::geom_mark_hull(ggplot2::aes(x, y, fill = node_group,
                                           label = node_group), data = lo) +
      ggplot2::scale_fill_manual(values = colorsafe_palette,
                                 guide = ggplot2::guide_legend("Group"))
  }
  p
}

.graph_edges <- function(p, g, edge_color, edge_size, node_size) {
  out <- .infer_edge_mapping(g, edge_color, edge_size, node_size)
  if (is_directed(g)) {
    p <- p + ggraph::geom_edge_arc(ggplot2::aes(edge_colour = out[["ecolor"]],
                                                edge_width = out[["esize"]],
                                                end_cap = ggraph::circle(c(out[["end_cap"]]), 'mm')),
                                   edge_alpha = 0.4, edge_linetype = out[["line_type"]],
                                   strength = ifelse(igraph::which_mutual(g), 0.2, 0),
                                   arrow = ggplot2::arrow(angle = 15, type = "closed",
                                                          length = ggplot2::unit(2, 'mm')))
  } else {
    p <- p + ggraph::geom_edge_link0(ggplot2::aes(edge_width = out[["esize"]],
                                                  edge_colour = out[["ecolor"]]),
                                     edge_alpha = 0.4, edge_linetype = out[["line_type"]])
  }
  if (is_complex(g)) {
    p <- p + ggraph::geom_edge_loop0(edge_alpha = 0.4)
  }
  # Check legends
  if (length(unique(out[["esize"]])) == 1) {
    p <- p + ggplot2::guides(edge_width = "none")
  } else p <- p + ggraph::scale_edge_width_continuous(range = c(0.3, 3),
                                                      guide = ggplot2::guide_legend(
                                                        ifelse(is.null(edge_size) &
                                                                 is_weighted(g),
                                                               "Edge Weight", "Edge Size")))
  if (length(unique(out[["ecolor"]])) == 1) {
    p <- p + ggplot2::guides(edge_colour = "none")
  } else p <- p + ggraph::scale_edge_colour_manual(values = colorsafe_palette,
                                                   guide = ggplot2::guide_legend(
                                                     ifelse(is.null(edge_color) &
                                                              is_signed(g),
                                                            "Edge Sign", "Edge Color")))
  p
}

.graph_nodes <- function(p, g, node_color, node_shape, node_size) {
  out <- .infer_node_mapping(g, node_color, node_size, node_shape)
  if (is.null(node_color) & "Infected" %in% names(node_attribute(g))) {
    p <- map_infected_nodes(p, g, out)
  } else if (is.null(node_color) & any("diff_model" %in% names(attributes(g)))) {
    p <- map_diff_model_nodes(p, g, out)
  } else {
    p <- map_nodes(p, out)
    # Check legends
    if (length(unique(out[["nsize"]])) > 1)
      p <- p + ggplot2::guides(size = ggplot2::guide_legend(title = "Node Size"))
    if (length(unique(out[["nshape"]])) > 1) 
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
        title = ifelse(is_twomode(g) & is.null(node_shape), "Node Mode", "Node Shape")))
    if (length(unique(out[["ncolor"]])) > 1)
      p <- p + ggplot2::scale_colour_manual(values = colorsafe_palette,
                                            guide = ggplot2::guide_legend("Node Color"))
  }
  # Consider rescaling nodes
  p <- p + ggplot2::scale_size(range = c(1/net_nodes(g)*50, 1/net_nodes(g)*100))
  p
}

.graph_labels <- function(p, g, layout) {
  if (layout == "circle" | layout == "concentric") {
    angles <- as.data.frame(cart2pol(as.matrix(p[["data"]][,1:2])))
    angles$degree <- angles$phi * 180/pi
    angles <- dplyr::case_when(p[["data"]][,2] == 0 & p[["data"]][,1] == 0 ~ 0.1,
                               p[["data"]][,2] >= 0 & p[["data"]][,1] > 0 ~ angles$degree,
                               p[["data"]][,2] < 0 & p[["data"]][,1] > 0 ~ angles$degree,
                               p[["data"]][,1] == 1 ~ angles$degree,
                               TRUE ~ angles$degree - 180)
    if (net_nodes(g) < 10) {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.8, 1.8)
    } else if (net_nodes(g) < 20) {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.4, 1.4)
    } else {
      hj <- ifelse(p[["data"]][,1] >= 0, -0.2, 1.2)
    }
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE,
                                    size = 3, hjust = hj, angle = angles) +
      ggplot2::coord_cartesian(xlim=c(-1.2,1.2), ylim=c(-1.2,1.2))
  } else if (layout %in% c("bipartite", "railway") | layout == "hierarchy" &
             length(unique(p[["data"]][["y"]])) <= 2) {
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name), angle = 90,
                                    size = 3, hjust = "outward", repel = TRUE,
                                    nudge_y = ifelse(p[["data"]][,2] == 1,
                                                     0.05, -0.05)) +
      ggplot2::coord_cartesian(ylim=c(-0.2, 1.2))
  } else if (layout == "hierarchy" & length(unique(p[["data"]][["y"]])) > 2) {
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = name),
                                    size = 3, hjust = "inward", repel = TRUE)
  } else if (layout %in% c("alluvial", "lineage")) {
    p <- p + ggraph::geom_node_label(ggplot2::aes(label = name), size = 3,
                                     repel = TRUE, nudge_x = ifelse(p[["data"]][,1] == 1, 
                                                                    0.02, -0.02))
  } else {
    p <- p + ggraph::geom_node_label(ggplot2::aes(label = name),
                                     repel = TRUE, seed = 1234, size = 3)
  }
}

# `graphr()` helper functions
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

.infer_edge_mapping <- function(g, edge_color, edge_size, node_size) {
  check_edge_variables(g, edge_color, edge_size)
  if (is_directed(g)) {
    list("ecolor" = .infer_ecolor(g, edge_color),
         "esize" = .infer_esize(g, edge_size),
         "line_type" = .infer_line_type(g, edge_color),
         "end_cap" = .infer_end_cap(g, node_size))
  } else {
    list("ecolor" = .infer_ecolor(g, edge_color),
         "esize" = .infer_esize(g, edge_size),
         "line_type" = .infer_line_type(g, edge_color))
  }
}

.infer_ecolor <- function(g, edge_color){
  if (!is.null(edge_color)) {
    if (edge_color %in% names(tie_attribute(g))) {
      if ("tie_mark" %in% class(tie_attribute(g, edge_color))) {
        out <- factor(as.character(tie_attribute(g, edge_color)),
                      levels = c("FALSE", "TRUE"))
      } else out <- as.factor(as.character(tie_attribute(g, edge_color)))
      if (length(unique(out)) == 1) {
        out <- rep("black", net_ties(g))
        message("Please indicate a variable with more than one value or level when mapping edge colors.")
      }
    } else if (length(edge_color) == 1) {
        out <- rep(edge_color, net_ties(g))
    } else {
        out <- edge_color
    }
  } else if (is.null(edge_color) & is_signed(g)) {
    out <- as.factor(ifelse(igraph::E(g)$sign >= 0, "Positive", "Negative"))
    if (length(unique(out)) == 1) {
      out <- rep("black", net_ties(g))
    }
  } else {
    out <- rep("black", net_ties(g))
  }
  out
}

.infer_esize <- function(g, edge_size){
  if (!is.null(edge_size)) {
    if (any(edge_size %in% names(tie_attribute(g)))) {
      out <- tie_attribute(g, edge_size)
    } else if (is.numeric(edge_size) & length(edge_size) == 1) {
      out <- rep(edge_size, net_ties(g))
    } else {
      out <- edge_size
    }
  } else if (is.null(edge_size) & is_weighted(g)) {
    out <- tie_attribute(g, "weight")
  } else {
    out <- rep(0.5, net_ties(g))
  }
  as.numeric(out)
}

.infer_end_cap <- function(g, node_size) {
  nsize <- .infer_nsize(g, node_size)/2
  # Accounts for rescaling
  if (length(unique(nsize)) == 1) {
    out <- rep(unique(nsize), net_ties(g))
  } else {
    out <- g %>%
      tidygraph::activate("edges") %>%
      data.frame() %>% 
      dplyr::left_join(data.frame(node_id = 1:length(node_names(g)),
                                  nsize = nsize),
                       by = c("to" = "node_id"))
    out <- out$nsize
    out <- ((out - min(out)) / (max(out) - min(out))) *
      ((1 / net_nodes(g) * 100) - (1 / net_nodes(g)*50)) + (1 / net_nodes(g) * 50)
  }
  out
}

.infer_line_type <- function(g, edge_color) {
  if (is_signed(g)) {
    out <- ifelse(igraph::E(g)$sign >= 0, "solid", "dashed")
    ifelse(length(unique(out)) == 1, unique(out), out)
  } else "solid"
}

check_edge_variables <- function(g, edge_color, edge_size) {
  if (!is.null(edge_color)) {
    if (any(!tolower(edge_color) %in% tolower(igraph::edge_attr_names(g))) &
        any(!edge_color %in% grDevices::colors())) {
      message("Please make sure you spelled `edge_color` variable correctly.")
    } 
  }
  if (!is.null(edge_size)) {
    if (!is.numeric(edge_size) & any(!tolower(edge_size) %in% tolower(igraph::edge_attr_names(g)))) {
      message("Please make sure you spelled `edge_size` variable correctly.")
    } 
  }
}

.infer_node_mapping <- function(g, node_color, node_size, node_shape) {
  check_node_variables(g, node_color, node_size)
  list("nshape" = .infer_nshape(g, node_shape),
       "nsize" = .infer_nsize(g, node_size),
       "ncolor" = .infer_ncolor(g, node_color))
}

.infer_nsize <- function(g, node_size) {
  if (!is.null(node_size)) {
    if (is.character(node_size)) {
      out <- node_attribute(g, node_size)
    } else out <- node_size
    if (length(node_size > 1) & all(out <= 1 & out >= 0)) out <- out * 10
  } else {
    out <- min(20, (250 / net_nodes(g)) / 2)
  }
  as.numeric(out)
}

.infer_nshape <- function(g, node_shape) {
  if (!is.null(node_shape)) {
    if (node_shape %in% names(node_attribute(g))) {
      out <- as.factor(as.character(node_attribute(g, node_shape)))
    } else out <- node_shape
  } else if (is_twomode(g) & is.null(node_shape)) {
    out <- ifelse(igraph::V(g)$type, "One", "Two")
  } else {
    out <- "circle"
  }
  out
}

.infer_ncolor <- function(g, node_color) {
  if (!is.null(node_color)) {
    if (node_color %in% names(node_attribute(g))) {
      if ("node_mark" %in% class(node_attribute(g, node_color))) {
        out <- factor(as.character(node_attribute(g, node_color)),
                      levels = c("FALSE", "TRUE"))
      } else out <- as.factor(as.character(node_attribute(g, node_color)))
      if (length(unique(out)) == 1) {
        out <- rep("black", net_nodes(g))
        message("Please indicate a variable with more than one value or level when mapping node colors.")
      }
    } else out <- node_color
  } else {
    out <- "black"
  }
  out
}

check_node_variables <- function(g, node_color, node_size) {
  if (!is.null(node_color)) {
    if (any(!tolower(node_color) %in% tolower(igraph::vertex_attr_names(g))) &
        any(!node_color %in% grDevices::colors())) {
      message("Please make sure you spelled `node_color` variable correctly.")
    } 
  }
  if (!is.null(node_size)) {
    if (!is.numeric(node_size) & any(!tolower(node_size) %in% tolower(igraph::vertex_attr_names(g)))) {
      message("Please make sure you spelled `node_size` variable correctly.")
    }
  }
}

map_infected_nodes<- function(p, g, out) {
  node_color <- as.factor(ifelse(node_attribute(g, "Exposed"), "Exposed",
                                 ifelse(node_attribute(g, "Infected"),"Infected", 
                                        ifelse(node_attribute(g, "Recovered"), "Recovered",
                                               "Susceptible"))))
  p + ggraph::geom_node_point(ggplot2::aes(color = node_color),
                              size = out[["nsize"]], shape = out[["nshape"]]) +
    ggplot2::scale_color_manual(name = NULL, guide = ggplot2::guide_legend(""),
                                values = c("Infected" = "#d73027",
                                           "Susceptible" = "#4575b4",
                                           "Exposed" = "#E6AB02",
                                           "Recovered" = "#66A61E"))
}

map_diff_model_nodes <- function(p, g, out) {
  node_adopts <- node_adoption_time(attr(g,"diff_model"))
  nshape <- ifelse(node_adopts == min(node_adopts), "Seed(s)",
                   ifelse(node_adopts == Inf, "Non-Adopter", "Adopter"))
  node_color <- ifelse(is.infinite(node_adopts), 
                       max(node_adopts[!is.infinite(node_adopts)]) + 1, 
                       node_adopts)
  p + ggraph::geom_node_point(ggplot2::aes(shape = nshape, color = node_color),
                              size = out[["nsize"]]) +
    ggplot2::scale_color_gradient(low = "#d73027", high = "#4575b4",
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
}

map_nodes <- function(p, out) {
  if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(colour = out[["ncolor"]], size = out[["nsize"]],
                                     shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]]), 
                                     size = out[["nsize"]], shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(size = out[["nsize"]]),
                                     colour = out[["ncolor"]], shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(shape = out[["nshape"]]),
                                     colour = out[["ncolor"]], size = out[["nsize"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) == 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]], size = out[["nsize"]]),
                                     shape = out[["nshape"]])
  } else if (length(out[["ncolor"]]) > 1 & length(out[["nsize"]]) == 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]], shape = out[["nshape"]]),
                                     size = out[["nsize"]])
  } else if (length(out[["ncolor"]]) == 1 & length(out[["nsize"]]) > 1 & length(out[["nshape"]]) > 1) {
    p <- p + ggraph::geom_node_point(ggplot2::aes(size = out[["nsize"]], shape = out[["nshape"]]),
                                     colour = out[["ncolor"]])
  } else {
    p <- p + ggraph::geom_node_point(ggplot2::aes(colour = out[["ncolor"]],
                                                  shape = out[["nshape"]],
                                                  size = out[["nsize"]]))
  }
  p
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
  else cli::cli_abort("Input must be a vector of length 3 or a matrix with 3 columns.")
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
    cli::cli_abort("Arguments 'x' and 'y' must be numeric or complex.")
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
    cli::cli_abort("Arguments 'x' and 'y' must be of the same size.")
  x <- abs(x)
  y <- abs(y)
  m <- pmin(x, y)
  M <- pmax(x, y)
  ifelse(M == 0, 0, M * sqrt(1 + (m/M)^2))
}

# Longitudinal or comparative networks ####

#' Easily graph a set of networks with sensible defaults
#' 
#' @description 
#'   This function provides users with an easy way to graph
#'   lists of network data for comparison.
#'   
#'   It builds upon this package's `graphr()` function, 
#'   and inherits all the same features and arguments.
#'   See `graphr()` for more.
#'   However, it uses the `{patchwork}` package to plot the graphs
#'   side by side and, if necessary, in successive rows.
#'   This is useful for lists of networks that represent, for example, 
#'   ego or component subgraphs of a network,
#'   or a list of a network's different types of tie or across time.
#'   By default just the first and last network will be plotted,
#'   but this can be overridden by the "waves" parameter.
#'   
#'   Where the graphs are of the same network (same nodes),
#'   the graphs may share a layout to facilitate comparison.
#'   By default, successive graphs will use the layout calculated for 
#'   the "first" network, but other options include the "last" layout,
#'   or a mix, "both", of them.
#' @family mapping
#' @param netlist A list of manynet-compatible networks.
#' @param waves Numeric, the number of plots to be displayed side-by-side.
#'   If missing, the number of plots will be reduced to the first and last
#'   when there are more than four plots.
#'   This argument can also be passed a vector selecting the waves to plot.
#' @param based_on Whether the layout of the joint plots should
#'   be based on the "first" or the "last" network, or "both".
#' @param ... Additional arguments passed to `graphr()`.
#' @return Multiple `ggplot2::ggplot()` objects displayed side-by-side.
#' @name map_graphs
#' @examples
#' #graphs(to_egos(ison_adolescents))
#' #graphs(to_egos(ison_adolescents), waves = 8)
#' #graphs(to_egos(ison_adolescents), waves = c(2, 4, 6))
#' #graphs(play_diffusion(ison_adolescents))
#' @export
graphs <- function(netlist, waves,
                   based_on = c("first", "last", "both"), ...) {
  thisRequires("patchwork")
  based_on <- match.arg(based_on)
  if (any(class(netlist) == "diff_model")){
    if (is_list(attr(netlist, "network"))) netlist <- attr(netlist, "network") else
        netlist <- to_waves(netlist)
  } 
  if (missing(waves)) {
    if (length(netlist) > 4) {
      netlist <- netlist[c(1, length(netlist))]
      message("Plotting first and last waves side-by-side. \nTo set the waves plotted use the 'waves = ' argument.")
    }
  } else if (!missing(waves)) {
    if (length(waves) == 1) netlist <- netlist[c(1:waves)] else 
      netlist <- netlist[waves]
  }
  if (is.null(names(netlist))) names(netlist) <- rep("", length(netlist))
  if (length(unique(lapply(netlist, length))) == 1) {
    if (based_on == "first") {
      lay <- graphr(netlist[[1]], ...)
      x <- lay$data$x
      y <- lay$data$y
    } else if (based_on == "last") {
      lay <- graphr(netlist[[length(netlist)]], ...)
      x <- lay$data$x
      y <- lay$data$y
    } else if (based_on == "both") {
      lay <- graphr(netlist[[1]], ...)
      x1 <- lay$data$x
      y1 <- lay$data$y
      lay1 <- graphr(netlist[[length(netlist)]], ...)
      x <- (lay1$data$x + x1)/2
      y <- (lay1$data$y + y1)/2
    }
    gs <- lapply(1:length(netlist), function(i)
      graphr(netlist[[i]], x = x, y = y, ...) + ggtitle(names(netlist)[i]))
  } else {
    thisRequires("methods")
    if (!methods::hasArg("layout") & is_ego_network(netlist)) {
      gs <- lapply(1:length(netlist), function(i)
        graphr(netlist[[i]], layout = "star", center = names(netlist)[[i]], ...) + 
          ggtitle(names(netlist)[i]))
    } else {
      message("Layouts were not standardised since not all nodes appear across waves.")  
      gs <- lapply(1:length(netlist), function(i)
        graphr(netlist[[i]], ...) + ggtitle(names(netlist)[i]))
    }
  }
  # if (all(c("Infected", "Exposed", "Recovered") %in% names(gs[[1]]$data))) {
  #   gs <- .collapse_guides(gs)
  # }
  do.call(patchwork::wrap_plots, c(gs, list(guides = "collect")))
}

# `graphs()` helper functions
is_ego_network <- function(nlist) {
  if (all(unique(names(nlist)) != "")) {
    length(names(nlist)) == length(unique(unlist(unname(lapply(nlist, node_names))))) &
    all(order_alphabetically(names(nlist)) ==
          order_alphabetically(unique(unlist(unname(lapply(nlist, node_names))))))
  } else FALSE
}

order_alphabetically <- function(v) {
  v[order(names(stats::setNames(v, v)))]
}

# Dynamic networks ####

#' Easily animate dynamic networks with sensible defaults
#' 
#' @description 
#'   This function provides users with an easy way to graph
#'   dynamic network data for exploration and presentation.
#'   
#'   It builds upon this package's `graphr()` function, 
#'   and inherits all the same features and arguments.
#'   See `graphr()` for more.
#'   However, it uses the `{gganimate}` package to animate the changes
#'   between successive iterations of a network.
#'   This is useful for networks in which the ties and/or the node or tie
#'   attributes are changing.
#'   
#'   A progress bar is shown if it takes some time to encoding all the
#'   .png files into a .gif.
#' @name map_grapht
#' @family mapping
#' @param tlist The same migraph-compatible network listed according to
#'   a time attribute, waves, or slices.
#' @param keep_isolates Logical, whether to keep isolate nodes in the graph.
#'   TRUE by default.
#'   If FALSE, removes nodes from each frame they are isolated in.
#' @inheritParams map_graphr
#' @importFrom igraph gsize as_data_frame get.edgelist
#' @importFrom ggplot2 ggplot geom_segment geom_point geom_text
#' scale_alpha_manual theme_void
#' @importFrom ggraph create_layout
#' @importFrom dplyr mutate select distinct left_join %>%
#' @source https://blog.schochastics.net/posts/2021-09-15_animating-network-evolutions-with-gganimate/
#' @return Shows a .gif image. Assigning the result of the function
#'   saves the gif to a temporary folder and the object holds the path to this file.
#' @examples
#' #ison_adolescents %>%
#' #  mutate_ties(year = sample(1995:1998, 10, replace = TRUE)) %>%
#' #  to_waves(attribute = "year", cumulative = TRUE) %>%
#' #  grapht()
#' #ison_adolescents %>% 
#' #  mutate(gender = rep(c("male", "female"), times = 4),
#' #         hair = rep(c("black", "brown"), times = 4),
#' #         age = sample(11:16, 8, replace = TRUE)) %>%
#' #  mutate_ties(year = sample(1995:1998, 10, replace = TRUE),
#' #              links = sample(c("friends", "not_friends"), 10, replace = TRUE),
#' #              weekly_meetings = sample(c(3, 5, 7), 10, replace = TRUE)) %>%
#' #  to_waves(attribute = "year") %>%
#' #  grapht(layout = "concentric", membership = "gender",
#' #             node_shape = "gender", node_color = "hair",
#' #             node_size =  "age", edge_color = "links",
#' #             edge_size = "weekly_meetings")
#' #grapht(play_diffusion(ison_adolescents, seeds = 5))
#' @export
grapht <- function(tlist, keep_isolates = TRUE,
                   layout, labels = TRUE,
                   node_color, node_shape, node_size,
                   edge_color, edge_size, ...,
                   node_colour, edge_colour) {
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
  if (missing(node_color) && missing(node_colour)) {
    node_color <- NULL
  } else if (missing(node_color)) {
    node_color <- as.character(substitute(node_colour))
  } else {
    node_color <- as.character(substitute(node_color))
  }
  if (missing(node_shape)) node_shape <- NULL else
    node_shape <- as.character(substitute(node_shape))
  if (missing(node_size)) node_size <- NULL else if (!is.numeric(node_size)) {
    node_size <- as.character(substitute(node_size))
  }
  if (missing(edge_color) && missing(edge_colour)) {
    edge_color <- NULL
  } else if (missing(edge_color)) {
    edge_color <- as.character(substitute(edge_colour))
  } else {
    edge_color <- as.character(substitute(edge_color))
  }
  if (missing(edge_size)) edge_size <- NULL else if (!is.numeric(edge_size)) {
    edge_size <- as.character(substitute(edge_size))
  }
  # Check if diffusion model
  if (inherits(tlist, "diff_model")) tlist <- to_waves(tlist)
  # Check if object is a list of lists
  if (!is.list(tlist[[1]])) {
    cli::cli_abort("Please declare a migraph-compatible network listed according
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
      edge_color <- .check_color(edges_out[[edge_color]])
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
      node_color <- .check_color(nodes_out[[node_color]])
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
      ggplot2::scale_color_manual(name = NULL, values = c("Infected" = "#d73027",
                                                          "Susceptible" = "#4575b4",
                                                          "Exposed" = "#E6AB02",
                                                          "Recovered" = "#66A61E")) +
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

# `graphd()` helper functions
.check_color <- function(v) {
  color <- grDevices::colors()
  color <- color[!color %in% "black"]
  v <- ifelse(is.na(v), "black", v)
  if (!any(grepl(paste(color, collapse = "|"), v)) | any(grepl("^#", v))) {
    for(i in unique(v)) {
      if (i != "black") {
        v[v == i] <- sample(color, 1)
      }
    }
  }
  v
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

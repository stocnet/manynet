# Simplifying ####

#' Modifying network complexity
#' @name modif_plexity
#' @description
#'   These functions reformat manynet-consistent data.
#' 
#'   - `to_anti()` reformats network data into its complement, where only ties _not_ present in the original network
#'   are included in the new network.
#'   - `to_simplex()` reformats complex network data, containing loops, to simplex network data, without any loops.
#'   Parallel ties are kept, since a second tie between two nodes is not a
#'   loop; `tie_is_parallel()` marks them.
#'   - `to_uniplex()` reformats multiplex network data to a single type of tie.
#'   `to_layer()` is an alias, using the layer-based vocabulary of
#'   `layer_names()`, `net_layers()`, and `to_layers()`.
#'   Use `to_layers()` to split a network into all of its layers at once.
#'   - `to_aggregated()` combines the ties of a network that differ only in
#'   one tie column, dyad by dyad, according to a rule:
#'   its layers, the reports of its reporters, the gossip about each target,
#'   or its moments. It can also combine parallel ties.
#'   Where `to_uniplex()` selects one layer and discards the rest,
#'   `to_aggregated()` retains what every layer records.
#'   `to_flat()` is an alias that combines layers, and takes the `rule` as
#'   its second argument.
#'   - `to_disaggregated()` turns tie weights back into that many parallel
#'   ties, reversing `to_aggregated(over = NULL, rule = "sum")`.
#' 
#'   If the format condition is not met,
#'   for example `to_undirected()` is used on a network that is already undirected,
#'   the network data is returned unaltered.
#'   No warning is given so that these functions can be used to ensure conformance.
#'   
#'   Unlike the `as_*()` group of functions,
#'   these functions always return the same class as they are given,
#'   only transforming these objects' properties.
#' @details
#'   Not all functions have methods available for all object classes.
#'   Below are the currently implemented S3 methods:
#'  
#'   ```{r, echo = FALSE, comment=""}
#'   available_methods(collect_functions("to_.*(anti|plex|layer$|flat$|aggregated$)"))
#'   ```
#' @template param_data
#' @template fam_modif
NULL

#' @rdname modif_plexity
#' @importFrom igraph complementer
#' @examples
#' to_anti(ison_southern_women)
#' @export
to_anti <- function(.data) UseMethod("to_anti")

#' @export
to_anti.default <- function(.data) {
  as_input(.data, to_anti)
}

#' @export
to_anti.matrix <- function(.data){
  matrix(1, nrow(.data), ncol(.data)) - .data
}

#' @export
to_anti.data.frame <- function(.data){
  as_edgelist.matrix(to_anti.matrix(as_matrix(.data)))
}

#' @export
to_anti.igraph <- function(.data){
  if(is_twomode(.data)){
    as_igraph(to_anti.matrix(as_matrix(.data)))
  } else {
    igraph::complementer(as_igraph(.data), 
                         loops = is_complex(.data))
  }
}

#' @export
to_anti.tbl_graph <- function(.data){
  if(is_twomode(.data)){
    as_tidygraph(to_anti.matrix(as_matrix(.data)))
  } else {
    as_tidygraph(igraph::complementer(as_igraph(.data), 
                                      loops = is_complex(.data)))
  }
}

#' @rdname modif_plexity
#' @importFrom igraph simplify
#' @export
to_simplex <- function(.data) UseMethod("to_simplex")

#' @export
to_simplex.default <- function(.data) {
  as_input(.data, to_simplex)
}

#' @export
to_simplex.igraph <- function(.data) {
  # Only the loops are removed. A second tie between two nodes is not a loop,
  # and may be a second record, a second observation, or a second report.
  igraph::simplify(.data, remove.multiple = FALSE, remove.loops = TRUE)
}

#' @export
to_simplex.stocnet <- function(.data) {
  if(is.null(.data$ties) || nrow(.data$ties) == 0) return(.data)
  ties <- .data$ties
  # Only the loops are removed, as in every other method.
  keep_ties(.data, which(ties$from != ties$to)) |>
    .record_exclusion(.data, "loops", "ties")
}

#' @export
to_simplex.tbl_graph <- function(.data) {
  # the record is written here rather than in the igraph method because
  # `.record_transformation()` returns a 'tbl_graph', which would change what
  # the igraph method gives back to the methods that delegate to it
  as_tidygraph(to_simplex(as_igraph(.data))) |>
    .record_exclusion(.data, "loops", "ties")
}

#' @export
to_simplex.matrix <- function(.data) {
  out <- .data
  diag(out) <- 0
  out
}

#' @export
to_simplex.data.frame <- function(.data) {
  out <- .data[.data$from != .data$to,]
  out
}

# Layers are held in a 'type' tie attribute in tidygraph/igraph objects and in
# a 'layer' column in stocnet objects; both survive coercion. Note that
# layer_names() returns the names of the layers, not the tie attribute in
# which they are held, and falls back to the network's tie label where there
# are no layers, so it cannot be used to detect this.
.layer_attribute <- function(.data) {
  intersect(c("type", "layer"), net_tie_attributes(.data))[1]
}

#' @rdname modif_plexity
#' @param layer Character string naming one of the layers, or tie types,
#'   in the network, i.e. one of those returned by `layer_names()`,
#'   to which the network should be reduced.
#'   Where a network holds no tie types, it is already uniplex
#'   and is returned unchanged.
#' @param tie Deprecated name for `layer`, retained for one version.
#' @examples
#' as_tidygraph(create_filled(5)) |>
#'   mutate_ties(type = sample(c("friend", "enemy"), 10, replace = TRUE)) |>
#'   to_uniplex("friend")
#' @export
to_uniplex <- function(.data, layer, tie) UseMethod("to_uniplex")

#' @export
to_uniplex.default <- function(.data, layer, tie) {
  # `tie` was the name of this argument until version 2.3.0, when it was
  # renamed to agree with `to_layers()`, `from_layers()`, and `layer_names()`
  if(missing(layer) && !missing(tie)){
    snet_warn("The {.arg tie} argument is now called {.arg layer}.")
    layer <- tie
  } else if(missing(layer)) layer <- NULL
  as_input(.data, to_uniplex, layer = layer)
}

#' @export
to_uniplex.tbl_graph <- function(.data, layer, tie){
  if(missing(layer) && !missing(tie)){
    snet_warn("The {.arg tie} argument is now called {.arg layer}.")
    layer <- tie
  } else if(missing(layer)) layer <- NULL
  layer_attr <- .layer_attribute(.data)
  if(is.na(layer_attr)){
    snet_info("This network holds no tie types, so is already uniplex.")
    return(.data)
  }
  types <- tie_attribute(.data, layer_attr)
  ties_avail <- unique(types)
  if(is.null(layer) || length(layer) != 1){
    snet_abort("Please name the layer to which the network should be",
               "reduced, one of {.val {ties_avail}} (see {.fn layer_names}).")
  } else if(!layer %in% ties_avail){
    snet_abort("There is no layer {.val {layer}} in this network.",
               "Please name one of {.val {ties_avail}}",
               "(see {.fn layer_names}).")
  }
  out <- delete_ties(.data, which(!types %in% layer))
  out <- delete_tie_attribute(out, layer_attr)
  if(is_signed(out) && "sign" %in% net_tie_attributes(out) &&
     (all(tie_signs(out)==1) || all(is.na(tie_signs(out)))))
    out <- delete_tie_attribute(out, "sign")
  # Weights of nothing but ones record no more than the ties themselves do,
  # unless some of them are missing, which records which ties are missing
  if("weight" %in% net_tie_attributes(out) && !anyNA(tie_weights(out)) &&
     all(tie_weights(out)==1))
    out <- delete_tie_attribute(out, "weight")
  if(is_longitudinal(out) && length(unique(tie_attribute(out, "wave")))==1)
    out <- delete_tie_attribute(out, "wave")
  if(is_twomode(out) && all(!tie_is_twomode(out))){ # if only one-mode left
    retain <- node_is_mode(out)[igraph::as_edgelist(out, names = FALSE)[1,1]]
    out <- tidygraph::activate(out, "nodes") |> 
      filter_nodes(type == retain) |> 
      mutate_nodes(type = NULL)
  }
  out <- out |> mutate_info(ties = layer)
  out <- .retain_layer_info(out, layer, setdiff(ties_avail, layer))
  # the ties of the other layers and, where a two-mode network is left with
  # one mode, the nodes of the mode that went with them, are two exclusions by
  # two criteria, so each is recorded on its own
  out <- .record_exclusion(out, .data,
                           paste0("layers other than '", layer, "'"), "ties") |>
    .record_exclusion(.data, paste0("not tied by '", layer, "'"), "nodes")
  # A network with both directed and undirected layers is directed as a whole,
  # and holds its undirected layers as reciprocated arcs. Once only such a
  # layer is left, the result is an undirected network, so the arcs collapse
  # back to one tie per dyad.
  if(isFALSE(unname(igraph::graph_attr(out, "directed")[layer])))
    out <- to_undirected(out)
  tidygraph::activate(out, "nodes")
}

# Reduce the per-layer info to the one layer that `to_uniplex()` retains.
# Without this, 'layers' and the named 'directed'/'observation'/'update'
# vectors keep describing layers that are no longer in the network, which
# `validate_stocnet()` rejects once the result is coerced back to a stocnet.
.retain_layer_info <- function(.data, layer, dropped){
  out <- .data
  fields <- intersect(c("layers", "directed", "observation", "update", "focal"),
                      igraph::graph_attr_names(out))
  info <- igraph::graph_attr(out)[fields]
  # `.prune_layer_info()` reads the layers from the information it prunes, so
  # where the network names none they are named here for it, and the network
  # is left without them again afterwards.
  had_layers <- "layers" %in% fields
  if(!had_layers) info$layers <- unique(c(layer, dropped))
  info <- .prune_layer_info(info, layer)
  if(!had_layers) info$layers <- NULL
  for(field in fields) igraph::graph_attr(out, field) <- info[[field]]
  out
}

#' @rdname modif_plexity
#' @export
to_layer <- to_uniplex

#' @rdname modif_plexity
#' @param over The tie column over which ties are combined, one of
#'   the columns that `as_stocnet()` takes from the third dimension of an
#'   array:
#'   - "layer" (the default) combines the layers of a multiplex network,
#'   - "by" combines the reports of the reporters in a cognitive social
#'   structure,
#'   - "about" combines the gossip about each target,
#'   - "time" combines the moments of a longitudinal network.
#'
#'   `NULL` combines only parallel ties, the ties that `tie_is_parallel()`
#'   marks. `to_disaggregated()` reverses this where `rule = "sum"`.
#'
#'   Each tie counts by its weight, or by its sign where it has no weight,
#'   and otherwise as 1. Signed values can cancel one another out,
#'   and a combined value of zero is no tie.
#' @template param_rule
#' @param reporters Which reports are combined where `over = "by"`:
#'   - "all" (the default) combines the reports of every reporter.
#'   - "sender" takes, for the tie from i to j, the report of i.
#'   - "receiver" takes, for the tie from i to j, the report of j.
#'   - "both" combines the reports of i and j by the `rule`.
#'   With `rule = "min"` this is the intersection locally aggregated structure
#'   of Krackhardt (1987), and with `rule = "max"` it is the union one.
#' @section Aggregating reports:
#'   Over reporters, "max" gives a tie where any reporter reports one,
#'   "min" where all of them do, "mean" the share of the reporters that do,
#'   and "sum" how many of them do.
#'   A consensus structure is therefore
#'   `to_aggregated(.data, over = "by", rule = "mean")` followed by
#'   `to_unweighted(threshold = 0.5)`.
#'   A reporter that did not report is left out of the reports that are
#'   combined, rather than making every tie missing, and a tie is missing
#'   only where none of the reporters it is pooled over reported it.
#'   Egocentric data has no roster that all the egos report on,
#'   so its reports cannot be combined.
#' @references
#' ## On aggregating cognitive social structures
#'   Krackhardt, David. 1987.
#'   "Cognitive social structures".
#'   _Social Networks_, 9(2): 109-134.
#'   \doi{10.1016/0378-8733(87)90009-8}
#' @examples
#' to_aggregated(ison_florentine, rule = "sum")
#' @export
to_aggregated <- function(.data, over = "layer",
                          rule = c("max","min","mean","sum","product"),
                          reporters = c("all","sender","receiver","both"))
  UseMethod("to_aggregated")

#' @export
to_aggregated.default <- function(.data, over = "layer",
                                  rule = c("max","min","mean","sum","product"),
                                  reporters = c("all","sender","receiver","both")) {
  rule <- match.arg(rule)
  reporters <- match.arg(reporters)
  .check_over(over)
  # Layers are combined as `to_flat()` combined them before there was anything
  # else to combine over, which goes through a tbl_graph. The other columns
  # are held most plainly in a stocnet's ties.
  out <- if(identical(over, "layer"))
    to_aggregated.tbl_graph(as_tidygraph(.data), over, rule, reporters) else
      to_aggregated.stocnet(as_stocnet(.data), over, rule, reporters)
  .as_class_of(out, .data)
}

#' @export
to_aggregated.array <- function(.data, over = "layer",
                                rule = c("max","min","mean","sum","product"),
                                reporters = c("all","sender","receiver","both")) {
  rule <- match.arg(rule)
  reporters <- match.arg(reporters)
  .check_over(over)
  # An array holds one value for each cell, so no parallel ties, and what its
  # third dimension holds is what `over` says it holds.
  if(is.null(over))
    snet_abort("An array holds no parallel ties to combine.")
  as_matrix(to_aggregated(as_stocnet(.data, attribute = over), over, rule,
                          reporters))
}

#' @export
to_aggregated.tbl_graph <- function(.data, over = "layer",
                                    rule = c("max","min","mean","sum","product"),
                                    reporters = c("all","sender","receiver","both")) {
  rule <- match.arg(rule)
  reporters <- match.arg(reporters)
  .check_over(over)
  if(!identical(over, "layer"))
    return(as_tidygraph(to_aggregated.stocnet(as_stocnet(.data), over, rule,
                                              reporters)))
  layers <- to_layers(.data)
  if(length(layers) > 1) return(.combine_networks(layers, rule))
  # `join_ties()` marks each network's ties in a column of its own rather than
  # naming them all in one 'type' column, and `is_multiplex()` counts such a
  # network as multiplex, so those columns are its layers
  marks <- setdiff(net_tie_attributes(.data), reserved_tie_attr)
  if(length(marks) > 1) return(.combine_marks(.data, marks, rule))
  # a network holding no tie types is already flat; `to_layers()` says so
  .data
}

#' @export
to_aggregated.stocnet <- function(.data, over = "layer",
                                  rule = c("max","min","mean","sum","product"),
                                  reporters = c("all","sender","receiver","both")) {
  rule <- match.arg(rule)
  reporters <- match.arg(reporters)
  .check_over(over)
  if(identical(over, "layer"))
    return(as_stocnet(to_aggregated.tbl_graph(as_tidygraph(.data), over, rule,
                                              reporters)))
  .aggregate_ties(.data, over, rule, reporters)
}

#' @rdname modif_plexity
#' @export
to_flat <- function(.data, rule = c("max","min","mean","sum","product"),
                    over = "layer") {
  # `to_flat()` took the rule as its second argument before it could combine
  # over anything but layers, so it keeps it there.
  to_aggregated(.data, over = over, rule = match.arg(rule))
}

#' @rdname modif_plexity
#' @section Disaggregating:
#'   `to_disaggregated()` turns each tie of weight w into w parallel ties,
#'   and so reverses `to_aggregated(over = NULL, rule = "sum")`.
#'   No other aggregation can be reversed, since the values it combined are
#'   not kept; `to_layers()`, `to_reporters()`, and `to_times()` split a
#'   network into its parts without losing them.
#'   The weights must be whole numbers.
#'   A negative weight becomes that many negative ties, held as weights of -1,
#'   which is how a signed network holds its signs.
#'   A matrix cannot hold parallel ties, so it is returned as it is.
#' @examples
#' to_disaggregated(to_aggregated(ison_koenigsberg, over = NULL, rule = "sum"))
#' @export
to_disaggregated <- function(.data) UseMethod("to_disaggregated")

#' @export
to_disaggregated.default <- function(.data) {
  .as_class_of(to_disaggregated.stocnet(as_stocnet(.data)), .data)
}

#' @export
to_disaggregated.matrix <- function(.data) {
  snet_info("A matrix cannot hold parallel ties, so it is returned as it is.")
  .data
}

#' @export
to_disaggregated.stocnet <- function(.data) {
  ties <- .data$ties
  if(is.null(ties) || !"weight" %in% names(ties)){
    snet_info("This network has no weights to disaggregate.")
    return(.data)
  }
  w <- ties$weight
  if(anyNA(w) || any(w != round(w)))
    snet_abort(paste("Only weights that are whole numbers",
                     "can be disaggregated into that many parallel ties."))
  out <- .data
  out$ties <- ties[rep(seq_len(nrow(ties)), times = abs(w)), , drop = FALSE]
  # A signed network holds its signs as weights of -1 and 1, so a weight of
  # -2 is two negative ties, which a sum of the parallel ties gives back.
  if(any(w < 0)) out$ties$weight <- sign(out$ties$weight) else
    out$ties$weight <- NULL
  .record_transformation(out, "disaggregation",
                         paste0("weights as parallel ties (",
                                sum(abs(w)) - sum(w != 0), " added)"))
}

.check_over <- function(over){
  if(!is.null(over) &&
     !(length(over) == 1 && over %in% c("layer", "by", "about", "time")))
    snet_abort(paste("{.arg over} must be one of {.val layer}, {.val by},",
                     "{.val about}, or {.val time}, or {.code NULL} to combine",
                     "parallel ties only."))
  invisible(over)
}

# Combining ####

# Where the layers are marked one column each, as `join_ties()` marks them,
# each column gives the tie values of one layer. A tie the column does not
# mark is untied in that layer.
.combine_marks <- function(.data, marks, rule){
  el <- as_edgelist(.data)
  n <- as.numeric(net_nodes(.data))
  # an edgelist names its nodes only where the network is labelled
  labels <- if(is_labelled(.data)) node_names(.data) else NULL
  idx <- if(is.character(el$from))
    cbind(match(el$from, labels), match(el$to, labels)) else
      cbind(as.integer(el$from), as.integer(el$to))
  mats <- lapply(marks, function(m){
    v <- el[[m]]
    v[is.na(v)] <- 0
    out <- matrix(0, n, n, dimnames = if(is.null(labels)) NULL else
      list(labels, labels))
    out[idx] <- v
    if(!is_directed(.data)) out[idx[, 2:1, drop = FALSE]] <- v
    out
  })
  out <- .combine_networks(mats, rule)
  as_tidygraph(out) |> bind_node_attributes(.data) |>
    .record_transformation("aggregation", paste0("layers (", rule, ")"))
}

# Reconciles networks' tie values into a single value per dyad, cell by cell,
# and returns the result in the class of the first network given. This is the
# engine behind `to_flat()`, which combines one network's layers, and
# `join_ties(rule = )`, which combines two networks.
.combine_networks <- function(netlist, rule){
  first <- netlist[[1]]
  out <- Reduce(function(x, y) .combine_matrices(x, y, rule),
                lapply(netlist, as_matrix))
  if(is.matrix(first)) return(out)
  if(is.data.frame(first) && !inherits(first, "stocnet"))
    return(as_edgelist(out))
  net <- bind_node_attributes(as_tidygraph(out), as_tidygraph(first)) |>
    .record_transformation("aggregation", paste0("layers (", rule, ")"))
  if(inherits(first, "stocnet")) as_stocnet(net)
  else if(inherits(first, "network")) as_network(net)
  else if(inherits(first, "tbl_graph")) net
  else if(inherits(first, "igraph")) as_igraph(net)
  else net
}

.combine_matrices <- function(x, y, rule) {
  # nodes are matched by name where both networks are labelled, since two
  # networks recording the same nodes in a different order would otherwise
  # be combined cell by cell and give a result for dyads that do not exist.
  # Combining over the union of the two node sets also means an edgelist,
  # which carries only the nodes that happen to be tied, can be combined
  # with one that names a different subset of the same nodes.
  if(is_labelled(x) && is_labelled(y)){
    rn <- union(rownames(x), rownames(y))
    cn <- union(colnames(x), colnames(y))
    if(!setequal(rownames(x), rownames(y)) ||
       !setequal(colnames(x), colnames(y)))
      snet_info(paste0("The networks name different nodes, ",
                       "so they are combined over all {length(rn)} of them, ",
                       "counting a node absent from one as untied there."))
    x <- .align_matrix(x, rn, cn)
    y <- .align_matrix(y, rn, cn)
  } else if(!identical(dim(x), dim(y)))
    snet_abort(paste0("The two networks must be the same size, ",
                      "or else labelled so that their nodes can be matched, ",
                      "but they are {nrow(x)}x{ncol(x)} and ",
                      "{nrow(y)}x{ncol(y)}."))
  out <- switch(rule,
                "max"     = pmax(x, y),
                "min"     = pmin(x, y),
                "mean"    = (x + y)/2,
                "sum"     = x + y,
                "product" = x * y)
  # `pmin()` and `pmax()` return a vector, so the shape is restored here
  matrix(out, nrow(x), ncol(x), dimnames = dimnames(x))
}

# Places a matrix into one spanning the given row and column names, so that
# two networks over overlapping but unequal node sets can be combined. Nodes
# a matrix does not name are absent from it, and so untied.
.align_matrix <- function(x, rn, cn){
  out <- matrix(0, length(rn), length(cn), dimnames = list(rn, cn))
  out[rownames(x), colnames(x)] <- x
  out
}

# Aggregating ####

# Combines the ties of a stocnet over one of its tie columns, or, where `over`
# is NULL, combines its parallel ties. Each tie is a value in one slice (one
# report, one target, one moment), and the ties that share every other column
# are pooled. A slice that holds no tie for a dyad counts as a zero there, so
# that "min" and "mean" mean what they say: all the slices, and the share of
# them.
.aggregate_ties <- function(.data, over, rule, reporters){
  if(identical(over, "by") && is_egocentric(.data))
    snet_abort(paste("The egos of egocentric data each report on their own",
                     "alters, so there is no roster over which their reports",
                     "could be combined."))
  if(!is.null(over) && identical(over, "time") &&
     !identical(.time_rule(.data), "replace"))
    .data <- .restate_moments(.data)
  ties <- .data$ties
  if(!is.null(over) && (is.null(ties) || !over %in% names(ties))){
    snet_info("This network has no {.val {over}} column, so there is nothing",
              "to combine over.")
    return(.data)
  }
  if(!identical(reporters, "all") && !identical(over, "by"))
    snet_abort("{.arg reporters} only applies where {.code over = \"by\"}.")
  directed <- is_directed(.data)
  if(reporters %in% c("sender", "receiver") && !directed)
    snet_abort(paste("An undirected tie has no sender or receiver,",
                     "so please use {.code reporters = \"both\"}."))
  missing <- as_missinglist(.data)
  rows <- .value_rows(ties, missing)
  if(is.null(over) && !any(duplicated(.pool_key(rows, .third_cols(rows),
                                                directed)))){
    snet_info("This network has no parallel ties, so there is nothing to",
              "combine.")
    return(.data)
  }
  keep <- setdiff(.third_cols(rows), over)
  # A tie that names no reporter, or no target, is not one report among
  # several, such as a layer taken from records, so it is kept as it is
  # rather than pooled with the reports.
  if(!is.null(over) && over %in% c("by", "about")){
    unpooled <- rows[is.na(rows[[over]]), , drop = FALSE]
    rows <- rows[!is.na(rows[[over]]), , drop = FALSE]
  } else unpooled <- rows[0, , drop = FALSE]
  # The reports a tie is pooled over: every reporter, or the reporters that
  # are one of its two ends.
  if(identical(over, "by") && reporters != "all"){
    pick <- switch(reporters,
                   sender = rows$by == rows$from,
                   receiver = rows$by == rows$to,
                   both = rows$by == rows$from | rows$by == rows$to)
    rows <- rows[pick %in% TRUE, , drop = FALSE]
  }
  slices <- if(is.null(over)) NULL else
    if(over %in% c("by", "about")) seq_len(as.numeric(net_nodes(.data))) else
      sort(unique(rows[[over]]))
  key <- .pool_key(rows, keep, directed)
  groups <- split(seq_len(nrow(rows)), factor(key, levels = unique(key)))
  first <- vapply(groups, `[`, integer(1), 1)
  value <- vapply(groups, function(i){
    v <- rows$value[i]
    if(is.null(over)) return(.pool(v, rule))
    held <- rows[[over]][i]
    # A reporter that did not report is left out of the pool rather than
    # making the pooled tie missing, since the other reports still stand.
    # Elsewhere a missing value stays missing, as it does between layers.
    if(identical(over, "by")){
      gone <- is.na(v)
      silent <- setdiff(.pooled_over(rows[i[1], ], reporters, slices), held)
      if(all(gone) && !length(silent)) return(NA_real_)
      return(.pool(c(v[!gone], rep(0, length(silent))), rule))
    }
    .pool(c(v, rep(0, length(setdiff(slices, held)))), rule)
  }, numeric(1))
  out <- rows[first, c("from", "to", keep), drop = FALSE]
  out$weight <- unname(value)
  unpooled <- unpooled[, c("from", "to", keep, "value"), drop = FALSE]
  names(unpooled)[names(unpooled) == "value"] <- "weight"
  out <- dplyr::bind_rows(out, unpooled)
  out <- out[is.na(out$weight) | out$weight != 0, , drop = FALSE]
  out$na <- is.na(out$weight)
  # Values of nothing but ones record no more than the ties themselves do.
  if(all(out$weight[!out$na] == 1)) out$weight <- NULL
  if(!any(out$na)) out$na <- NULL
  info <- .data$info
  info$transformations <- NULL
  if(identical(over, "by")) info <- .drop_cognitive(info)
  if(identical(over, "time")) info$update <- NULL
  # A layer whose every tie was combined away is no longer in the network.
  if("layer" %in% names(out) && !is.null(info$layers))
    info <- .prune_layer_info(info, intersect(info$layers, unique(out$layer)))
  net <- .clear_missing(.data)
  res <- make_stocnet(info = info, nodes = net$nodes, ties = out,
                      changes = if(identical(over, "time")) NULL else
                        net$changes,
                      globals = net$globals)
  res$info$transformations <- .data$info$transformations
  .record_transformation(res, "aggregation", .aggregation_entry(over, rule,
                                                               reporters))
}

# Each tie as a value: its weight, its sign where it has no weight, and 1
# otherwise. A missing tie is a missing value.
.value_rows <- function(ties, missing){
  ties <- if(is.null(ties)) dplyr::tibble(from = integer(0), to = integer(0)) else
    ties
  ties$value <- if("weight" %in% names(ties)) as.numeric(ties$weight) else
    if("sign" %in% names(ties)) as.numeric(ties$sign) else
      rep(1, nrow(ties))
  cols <- c("from", "to", .third_cols(ties), "value")
  ties <- ties[, cols, drop = FALSE]
  if(!is.null(missing) && nrow(missing)){
    missing <- missing[, intersect(names(missing), cols), drop = FALSE]
    missing$value <- NA_real_
    ties <- dplyr::bind_rows(ties, missing)
  }
  ties
}

.third_cols <- function(ties) intersect(c("layer", "by", "about", "time"),
                                        names(ties))

# The string that names which ties are pooled together: the two ends, in a
# fixed order where the network is undirected, and each column kept.
.pool_key <- function(rows, keep, directed){
  from <- rows$from
  to <- rows$to
  if(!directed){
    lo <- pmin(from, to)
    to <- pmax(from, to)
    from <- lo
  }
  parts <- c(list(from, to), lapply(keep, function(col) as.character(rows[[col]])))
  do.call(paste, c(parts, sep = "\r"))
}

# The reporters whose reports on a tie are pooled.
.pooled_over <- function(row, reporters, slices){
  switch(reporters,
         all = slices,
         sender = row$from,
         receiver = row$to,
         both = unique(c(row$from, row$to)))
}

.pool <- function(v, rule){
  if(!length(v)) return(0)
  switch(rule,
         max = max(v),
         min = min(v),
         mean = mean(v),
         sum = sum(v),
         product = prod(v))
}

.aggregation_entry <- function(over, rule, reporters){
  what <- if(is.null(over)) "parallel ties" else
    switch(over, layer = "layers", about = "targets", time = "moments",
           by = switch(reporters,
                       all = "reporters",
                       sender = "senders' reports",
                       receiver = "receivers' reports",
                       both = "locally aggregated reports"))
  paste0(what, " (", rule, ")")
}

# A network whose moments are deltas or intervals is restated as a panel,
# each moment holding the ties as they stood then, so that its moments can be
# combined as the waves of a panel are.
.restate_moments <- function(.data){
  moments <- to_times(.data)
  sizes <- vapply(moments, function(x) as.numeric(net_nodes(x)), numeric(1))
  if(any(sizes != as.numeric(net_nodes(.data))))
    snet_abort(paste("The nodes of this network change over time, so its",
                     "moments cannot yet be combined. Please use",
                     "{.fn to_times} to take them one by one."))
  from_times(moments)
}

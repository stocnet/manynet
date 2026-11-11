#' Coercing into stocnet objects
#' @name coerce_stocnet
#' @description
#'   These functions coerce networks into the 'stocnet' class of `{manynet}`,
#'   and from it into the data objects of the modelling packages:
#'   - `as_stocnet()` coerces the object into a 'stocnet' object
#'   (see [make_stocnet()]).
#'   - `as_siena()` coerces the object into a `{RSiena}` 'sienadata' object.
#'   - `as_monan()` coerces a two-mode network of people and places
#'   into a `{MoNAn}` 'processState.monan' object.
#'
#'   An effort is made for all of these coercion routines to be as lossless
#'   as possible, though some object classes are better at retaining certain
#'   kinds of information than others.
#' @family coercions
#' @template param_data
#' @template param_two
#' @examples
#' test <- data.frame(from = c("A","B","B","C","C"), to = c("I","G","I","G","H"))
#' as_stocnet(test)
#' @return
#'   `as_stocnet()` returns a 'stocnet' object,
#'   `as_siena()` a 'sienadata' object,
#'   and `as_monan()` a 'processState.monan' object.
NULL

# stocnet ####

#' @rdname coerce_stocnet
#' @section Text:
#'   `as_stocnet()` reads a document-feature matrix of `{quanteda}` ('dfm')
#'   and a term-document or document-term matrix of `{tm}`
#'   ('TermDocumentMatrix', 'DocumentTermMatrix').
#'   Each gives a two-mode network of "documents" and "terms",
#'   with the count (or other weighting) of each term in each document as the
#'   weight of its tie, as [collect_text()] gives.
#'   The document variables of a 'dfm' become attributes of the documents.
#'   Neither `{quanteda}` nor `{tm}` needs to be installed.
#' @section Records:
#'   `as_stocnet()` reads the bibliographic records of `{bibliometrix}`
#'   ('bibliometrixDB', as `bibliometrix::convert2df()` gives)
#'   as [collect_bibliography()] does,
#'   into a multimodal network of works, authors, keywords, and sources.
#'   The other coercions, such as `as_igraph()` and `as_tidygraph()`,
#'   read the records through `as_stocnet()`,
#'   so the functions of the stocnet packages take the records directly.
#'   The records cannot be rebuilt from a network,
#'   so a function that would return the class it was given
#'   returns a 'stocnet' for them.
#'   `{bibliometrix}` does not need to be installed.
#' @export
as_stocnet <- function(.data,
                    twomode = FALSE, ...) UseMethod("as_stocnet")

#' @export
as_stocnet.stocnet <- function(.data, twomode = FALSE, ...) {
  .data
}

#' @export
as_stocnet.data.frame <- function(.data, twomode = FALSE, ...) {
  out <- .data
  # make sure that the data frame has the right columns, rename them if necessary,
  # and then reorder them if necessary
   if (!all(c("from", "to") %in% colnames(out))) {
     if (all(c("source", "target") %in% colnames(out))) {
       snet_minor_info("Renaming 'source' and 'target' columns to 'from' and 'to'.")
       out <- out |> dplyr::rename(from = source, to = target)
     } else if (all(c("sender", "receiver") %in% colnames(out))) {
       snet_minor_info("Renaming 'sender' and 'receiver' columns to 'from' and 'to'.")
       out <- out |> dplyr::rename(from = sender, to = receiver)
     } else if (all(c("ego", "alter") %in% colnames(out))) {
       snet_minor_info("Renaming 'ego' and 'alter' columns to 'from' and 'to'.")
       out <- out |> dplyr::rename(from = ego, to = alter)
     } else snet_abort("Edgelist must have columns named 'from' and 'to'.")
   }
  if (!"weight" %in% colnames(out)) {
    if ("replace" %in% colnames(out)) {
       snet_minor_info("Renaming 'replace' column to 'weight'.")
      out <- out |> dplyr::rename(weight = replace)
    } else if ("increment" %in% colnames(out)) {
      snet_minor_info("Renaming 'increment' column to 'weight'.")
      out <- out |> dplyr::rename(weight = increment)
    } else if ("value" %in% colnames(out)) {
       snet_minor_info("Renaming 'value' column to 'weight'.")
      out <- out |> dplyr::rename(weight = value)
    }
  }
  out <- out |> dplyr::select(from, to, dplyr::everything())
  if(!is.numeric(out$from) || !is.numeric(out$to)){
   # A reporter or a target need not be the end of any tie, so they are
   # nodes too and are added after the ends.
   cols <- intersect(.tie_node_cols, names(out))
   nodes <- unique(unlist(lapply(cols, function(col) as.character(out[[col]]))))
   nodes <- nodes[!is.na(nodes)]
   out <- .index_tie_nodes(out, nodes)
   out <- make_stocnet(ties = out, nodes = data.frame(label = nodes))
  } else {
    # A reporter or a target may be numbered beyond the ends of every tie,
    # and is a node of the network all the same, so the nodes are counted
    # there too. Otherwise the ends alone say how many nodes there are.
    ends <- max(c(0, out$from, out$to), na.rm = TRUE)
    named <- unlist(lapply(intersect(c("by", "about"), names(out)),
                           function(col) out[[col]]))
    n <- max(c(ends, named), na.rm = TRUE)
    out <- make_stocnet(ties = out,
                        nodes = if(n > ends) dplyr::tibble(.rows = n) else NULL)
  }
  if("increment" %in% colnames(.data)) out <- out |> 
    mutate_info(update = "increment")
  if("replace" %in% colnames(.data)) out <- out |> 
    mutate_info(update = "replace")
  out
}

#' @export
as_stocnet.igraph <- function(.data, twomode = FALSE, ...) {
  info <- as_infolist(.data)
  nodes <- as_nodelist(.data)
  changes <- as_changelist(.data)
  ties <- as_edgelist(.data)
  globals <- as_globallist(.data)
  # A stocnet holds the sign of a tie as the sign of its weight, so that the
  # network is signed is recorded, or it is lost where no tie is negative.
  if("sign" %in% igraph::edge_attr_names(.data)) info$signed <- TRUE
  # A network whose nodes have neither names nor attributes has no nodelist,
  # and its isolates appear in no tie, so its size is recorded as a table of
  # rows without columns rather than lost.
  if(is.null(nodes) || ncol(nodes) == 0)
    nodes <- dplyr::tibble(.rows = igraph::vcount(.data))

  if(is_labelled(.data)){
    # Where only some of the nodes are labelled, a label cannot say which
    # node a tie is to, so the ends of the ties are read by position.
    if(anyNA(nodes$name) && !is.null(ties)){
      ends <- igraph::as_edgelist(as_igraph(.data), names = FALSE)
      ties$from <- as.integer(ends[, 1])
      ties$to <- as.integer(ends[, 2])
    }
    ties <- .index_tie_nodes(ties, nodes$name)
    nodes$label <- nodes$name
    nodes$name <- NULL
  } else {
    ties <- .index_tie_nodes(ties, NULL)
  }
  # 'nodes' and 'ties' were the names an mnet gave the mode and layer names,
  # before 'modes' and 'layers' were reserved for them, so a network coerced
  # from a stocnet names its modes and layers under either.
  modes <- info$nodes %||% info$modes
  layers <- info$ties %||% info$layers
  if(is_twomode(.data)){
    if(!is.null(modes) && length(modes) == 2){
      nodes$mode <- modes[(nodes$type*1+1)]
    } else {
      nodes$mode <- as.character(nodes$type)
    }
    nodes$type <- NULL
    # `to_multilevel()` writes 'lvl' beside 'type'. Where 'type' has named the
    # modes, the level numbers say the same thing less well, so they go.
    nodes$lvl <- NULL
  } else if(!is.null(nodes) && "lvl" %in% names(nodes)){
    # `to_multilevel.igraph()` records the levels of a network in a 'lvl'
    # attribute, since an igraph 'type' attribute forbids ties within a mode.
    # A 'stocnet' has no such constraint and names its levels in 'mode',
    # which, unlike 'lvl', more than two levels can also share.
    lvls <- sort(unique(nodes$lvl))
    nodes$mode <- if(!is.null(modes) && length(modes) == length(lvls))
      modes[match(nodes$lvl, lvls)] else as.character(nodes$lvl)
    nodes$lvl <- NULL
  }
  # A multiplex network may name its layers in the older 'type' column
  if(is_multiplex(.data) && "type" %in% names(ties)){
    ties$layer <- ties$type
    ties$type <- NULL
    if(is.null(layers)){
      info$ties <- unique(ties$layer)
    }
  }
  if(!is.null(info$changes)) info$changes <- NULL
  missing <- as_missinglist(.data)
  # Where the network came from a stocnet with undirected layers, those layers
  # were reciprocated on the way out and are collapsed again here, so that the
  # per-layer record of directedness survives the round trip. Otherwise the
  # network is simply as directed as the object it came from.
  collapsed <- .collapse_layers(ties, info$directed, is_directed(.data))
  if(is.null(collapsed)) info$directed <- is_directed(.data) else {
    ties <- collapsed$ties
    info$directed <- collapsed$directed
  }
  if(!is.null(nodes))
    nodes <- dplyr::select(nodes, 
                           dplyr::any_of(c("label", "mode")), 
                           dplyr::everything())
  
  if(is.null(missing)){
    # Without an attribute recording them, a missing weight is the only mark a
    # tie recorded as missing can carry, which is how a matrix holds one. Where
    # the attribute is there, a missing weight is a tie of unknown value
    # instead, and is left among the ties as such.
    split <- .split_missing_input(.mark_missing_weights(ties))
    ties <- split$ties
    missing <- split$missing
  }
  # The structural zeros travel beside the missing ties, marked as fixed.
  zeros <- if(inherits(.data, "igraph") &&
              "missings" %in% igraph::graph_attr_names(.data))
    igraph::graph_attr(.data, "missings") else NULL
  zeros <- if(!is.null(zeros)) zeros[.fixed_rows(zeros), , drop = FALSE]
  if(!is.null(zeros) && nrow(zeros))
    missing <- dplyr::bind_rows(missing, dplyr::as_tibble(zeros))
  out <- list(info = info, nodes = nodes, ties = ties,
              changes = changes, globals = globals, missings = NULL)
  class(out) <- c("stocnet", class(out))
  # The ties recorded as missing are held as the nodes that did not report
  # them, so that a round trip through another class does not return one
  # record for every tie.
  out <- .compress_missing(out, .missing_in(missing, nodes))
  out <- rename_nodes(out)
  out <- rename_ties(out)
  out <- .conform_info(out)
  validate_stocnet(out)
  # A graph marked as holding a hypergraph holds each hyperedge as a node of
  # its last mode, which becomes a tie among its members again here.
  if(is_hypergraph(.data)){
    out$info$hyper <- NULL
    out <- .twomode_to_hyper(out, .hyperedge_nodes(.data))
  }
  out
}

# Which nodes of a graph marked as a hypergraph are its hyperedges: those of
# the second mode, or of the last level where the nodes have modes themselves.
.hyperedge_nodes <- function(.data){
  attrs <- igraph::vertex_attr_names(.data)
  if("type" %in% attrs) return(as.logical(igraph::V(.data)$type))
  if("lvl" %in% attrs){
    lvl <- igraph::V(.data)$lvl
    return(lvl == max(lvl))
  }
  rep(FALSE, igraph::vcount(.data))
}

# The info of a network read from a file holds whatever that file recorded
# about the network as a whole, which need not follow the conventions a stocnet
# expects of a name it reserves. Such an entry is dropped here, since keeping it
# would build an object that any later call rebuilding it would abort on.
.conform_info <- function(out) {
  info <- out$info
  if(is.null(info) || length(info) == 0) return(out)
  conforms <- function(nm, class, len = NULL, pool = NULL) {
    if(!nm %in% names(info)) return(TRUE)
    value <- info[[nm]]
    if(length(intersect(class(value), class)) == 0) return(FALSE)
    if(!is.null(len) && length(value) != len) return(FALSE)
    if(!is.null(pool) && !all(value %in% pool)) return(FALSE)
    TRUE
  }
  keeps <- c(
    name = conforms("name", "character"),
    modes = conforms("modes", "character", len = net_modes(out)),
    layers = conforms("layers", "character", len = net_layers(out)),
    observation = conforms("observation", "character",
                           pool = c("panel", "event", "cross-sectional",
                                    "egocentric", "cognitive")),
    sender = conforms("sender", "character", pool = mode_names(out)),
    receiver = conforms("receiver", "character", pool = mode_names(out)),
    update = conforms("update", "character",
                      pool = c("increment", "replace")),
    focal = conforms("focal", "character",
                     pool = unique(c(layer_names(out),
                                     net_node_attributes(out),
                                     as.character(out$changes$var)))),
    centered = conforms("centered", "logical"))
  dropped <- names(keeps)[!keeps]
  if(length(dropped) == 0) return(out)
  snet_minor_info(paste("Dropped {length(dropped)} network attribute{?s}",
                        "that a stocnet object reserves: {.val {dropped}}."))
  out$info[dropped] <- NULL
  out
}

#' @rdname coerce_stocnet
#' @param compare For a node measure or a node membership, how the values of
#'   two nodes are compared to give the tie between them;
#'   see `as_matrix()`.
#'   Comparing by "same" or "absdiff" gives an undirected network,
#'   and comparing by "diff", "sender", or "receiver" a directed one.
#'   A comparison of zero is no tie.
#' @export
as_stocnet.node_measure <- function(.data, twomode = FALSE,
                                    compare = c("absdiff", "diff", "sender",
                                                "receiver"), ...) {
  compare <- match.arg(compare)
  .stocnet_from_comparison(as_matrix(.data, compare = compare),
                           directed = compare != "absdiff",
                           twomode = .from_twomode(.data))
}

#' @rdname coerce_stocnet
#' @export
as_stocnet.node_member <- function(.data, twomode = FALSE,
                                   compare = "same", ...) {
  .stocnet_from_comparison(as_matrix(.data, twomode = twomode,
                                     compare = compare),
                           directed = FALSE,
                           twomode = isTRUE(twomode) || .from_twomode(.data))
}

# Whether a vector of node values comes from a two-mode network, in which
# case its comparison pairs the first mode with the second.
.from_twomode <- function(.data){
  mode <- attr(.data, "mode")
  !is.null(mode) && any(mode) && !all(mode)
}

# A matrix of comparisons is symmetric for some comparisons and not for
# others, but which it is follows from the comparison and not from the values:
# the values of every node can happen to be equal.
# Nor does being two-mode follow from the shape of the matrix: modes of equal
# size give a square one.
.stocnet_from_comparison <- function(mat, directed, twomode = FALSE){
  out <- as_stocnet(mat, twomode = twomode)
  if(directed && !isTRUE(any(out$info$directed))){
    ties <- out$ties
    if(!is.null(ties) && nrow(ties) && !is_twomode(out)){
      back <- ties[ties$from != ties$to, , drop = FALSE]
      back[c("from", "to")] <- back[c("to", "from")]
      out$ties <- dplyr::bind_rows(ties, back)
    }
    out$info$directed <- TRUE
  }
  out
}

# A 'hypergraph' object of the `{HyperG}` package is a list of one sparse
# incidence matrix, 'M', with a row for each hyperedge and a column for each
# node. It always names its nodes, by their positions where they have no names.
# Both functions leave the sparse matrix to igraph, which reads and writes
# one, so that neither `{HyperG}` nor `{Matrix}` is needed here.
#' @export
as_stocnet.hypergraph <- function(.data, twomode = FALSE, ...) {
  M <- .data$M
  # the hyperedges are the first mode here, since they are the rows
  ends <- igraph::as_edgelist(igraph::graph_from_biadjacency_matrix(M),
                              names = FALSE)
  ends <- ends[order(ends[, 1], ends[, 2]), , drop = FALSE]
  ties <- dplyr::tibble(from = NA_integer_, to = NA_integer_,
                        with = unname(split(as.integer(ends[, 2] - nrow(M)),
                                            factor(ends[, 1],
                                                   levels = seq_len(nrow(M))))))
  if(!is.null(rownames(M))) ties$label <- rownames(M)
  nodes <- dplyr::tibble(label = colnames(M) %||% as.character(seq_len(ncol(M))))
  if(.hif_counted(nodes$label)) nodes <- dplyr::tibble(.rows = ncol(M))
  make_stocnet(info = list(directed = FALSE), nodes = nodes, ties = ties)
}

#' @export
as_stocnet.dfm <- function(.data, twomode = FALSE, ...) {
  # A 'dfm' is a column-compressed sparse matrix (an S4 'dgCMatrix'), so its
  # slots are read directly, with no call to `{quanteda}` or `{Matrix}`.
  dims <- .data@Dim
  names <- .data@Dimnames
  docvars <- tryCatch(as.data.frame(.data@docvars), error = function(e) NULL)
  if (!is.null(docvars))
    docvars <- docvars[setdiff(names(docvars),
                               c("docname_", "docid_", "segid_"))]
  .text_incidence(docs = names[[1]] %||% paste0("text", seq_len(dims[1])),
                  terms = names[[2]] %||% paste0("feat", seq_len(dims[2])),
                  doc = .data@i + 1L,
                  term = rep(seq_len(dims[2]), diff(.data@p)),
                  weight = .data@x, attrs = docvars)
}

#' @export
as_stocnet.TermDocumentMatrix <- function(.data, twomode = FALSE, ...) {
  # A `{tm}` matrix is a 'simple_triplet_matrix' of `{slam}`, a list of the
  # row, column, and value of each cell that is not zero.
  .text_incidence(docs = .data$dimnames$Docs %||%
                    as.character(seq_len(.data$ncol)),
                  terms = .data$dimnames$Terms %||%
                    as.character(seq_len(.data$nrow)),
                  doc = .data$j, term = .data$i, weight = .data$v)
}

#' @export
as_stocnet.DocumentTermMatrix <- function(.data, twomode = FALSE, ...) {
  .text_incidence(docs = .data$dimnames$Docs %||%
                    as.character(seq_len(.data$nrow)),
                  terms = .data$dimnames$Terms %||%
                    as.character(seq_len(.data$ncol)),
                  doc = .data$i, term = .data$j, weight = .data$v)
}

# Records ####

# Bibliographic records are not a network, but give one, so every coercion
# reads them through `collect_bibliography()`.

#' @export
as_stocnet.bibliometrixDB <- function(.data, twomode = FALSE, ...) {
  collect_bibliography(.data)
}

#' @export
as_igraph.bibliometrixDB <- function(.data, twomode = FALSE) {
  as_igraph(as_stocnet(.data))
}

#' @export
as_tidygraph.bibliometrixDB <- function(.data, twomode = FALSE) {
  as_tidygraph(as_stocnet(.data))
}

#' @export
as_network.bibliometrixDB <- function(.data, twomode = FALSE) {
  as_network(as_stocnet(.data))
}

#' @export
as_matrix.bibliometrixDB <- function(.data, twomode = NULL, ...) {
  as_matrix(as_stocnet(.data), twomode = twomode, ...)
}

#' @export
as_edgelist.bibliometrixDB <- function(.data, twomode = FALSE) {
  as_edgelist(as_stocnet(.data))
}

#' @export
as_nodelist.bibliometrixDB <- function(.data) {
  as_nodelist(as_stocnet(.data))
}

#' @export
is_manynet.bibliometrixDB <- function(.data) TRUE

# The records are also a data frame, so every function with a method for a
# data frame, which reads an edgelist, would take them as one. Each such
# function gets a method for the records, registered when the package loads,
# that reads them as a network first, so that no function is left out as
# more are added. Methods written above are kept.
.register_record_methods <- function(ns) {
  methods <- grep("\\.data\\.frame$", ls(ns), value = TRUE)
  for (method in methods) {
    generic <- sub("\\.data\\.frame$", "", method)
    if (exists(paste0(generic, ".bibliometrixDB"), envir = ns,
               inherits = FALSE) ||
        !exists(generic, envir = ns, mode = "function", inherits = FALSE))
      next
    registerS3method(generic, "bibliometrixDB", local({
      fun <- get(generic, envir = ns)
      function(.data, ...) fun(as_stocnet(.data), ...)
    }), envir = ns)
  }
  invisible(NULL)
}

# Assembles a two-mode network of documents and terms from the cells of a
# sparse matrix, in the form `collect_text()` returns.
.text_incidence <- function(docs, terms, doc, term, weight, attrs = NULL) {
  keep <- weight != 0
  ties <- data.frame(from = as.integer(doc[keep]),
                     to = length(docs) + as.integer(term[keep]),
                     weight = weight[keep])
  if (all(ties$weight == round(ties$weight)))
    ties$weight <- as.integer(ties$weight)
  ties <- ties[order(ties$from, ties$to), , drop = FALSE]
  rownames(ties) <- NULL
  units <- data.frame(label = as.character(docs), mode = "documents",
                      stringsAsFactors = FALSE)
  if (!is.null(attrs) && ncol(attrs) > 0 && nrow(attrs) == length(docs))
    units <- cbind(units, attrs)
  nodes <- dplyr::bind_rows(units,
                            data.frame(label = as.character(terms),
                                       mode = rep("terms", length(terms)),
                                       stringsAsFactors = FALSE))
  make_stocnet(info = list(directed = FALSE, modes = c("documents", "terms"),
                           sender = "documents", receiver = "terms"),
               nodes = nodes, ties = ties)
}

#' @export
as_stocnet.matrix <- function(.data,
                           twomode = FALSE, ...) {
 as_stocnet(as_tidygraph(.data, twomode = twomode))
}

#' @rdname coerce_stocnet
#' @param attribute For a three-dimensional array, what its third dimension
#'   holds, which becomes a column of the ties:
#'   "time" for the waves of a panel,
#'   "by" for the reporters of a cognitive social structure,
#'   "about" for the targets of gossip,
#'   or "layer" for the layers of a multiplex network.
#'   An array cannot say which of these it holds.
#'   Where its third dimension is as long as the network has nodes,
#'   it could hold any of them, so this must be given.
#'   Otherwise it cannot hold reporters or targets, and "time" is assumed.
#' @param ... Further arguments passed to or from other methods,
#'   such as `attribute` for an array.
#' @export
as_stocnet.array <- function(.data, twomode = FALSE, ...,
                             attribute = c("time", "by", "about", "layer")) {
  d <- dim(.data)
  if(length(d) != 3)
    # a two-dimensional array is a matrix, and is read by the matrix method
    snet_abort("A network can be read from a matrix or a three-dimensional",
               "array, but not from an array of {length(d)} dimensions.")
  twomode <- isTRUE(twomode) || d[1] != d[2]
  n <- if(twomode) d[1] + d[2] else d[1]
  if(missing(attribute)){
    if(d[3] == n)
      snet_abort("The third dimension of this array has one slice for each",
                 "node, so it could hold reporters, targets, waves, or layers.",
                 "Please say which with {.arg attribute},",
                 "e.g. {.code attribute = \"by\"}.")
    attribute <- "time"
  }
  attribute <- match.arg(attribute)
  if(attribute %in% c("by", "about") && d[3] != n)
    snet_abort("'{attribute}' needs one slice for each of the {n} nodes,",
               "but this array has {d[3]}.")
  # The nodes are read from the first slice, as a matrix of them would be,
  # without what that slice alone records about who did not report.
  nodes <- as_stocnet(.data[, , 1], twomode = twomode)$nodes
  if(!is.null(nodes)) nodes$na <- NULL
  labels <- nodes[["label"]]
  slices <- dimnames(.data)[[3]]
  third <- switch(attribute,
                  "by" = ,
                  "about" = if(is.null(slices) || is.null(labels))
                    seq_len(d[3]) else match(slices, labels),
                  "time" = if(is.null(slices)) seq_len(d[3]) else
                    if(!anyNA(suppressWarnings(as.numeric(slices))))
                      as.numeric(slices) else slices,
                  "layer" = if(is.null(slices)) as.character(seq_len(d[3])) else slices)
  if(anyNA(third))
    snet_abort("The slices {slices[is.na(third)]} do not name nodes of the network.")
  symmetric <- vapply(seq_len(d[3]), function(k)
    isSymmetric(unname(.data[, , k])), logical(1))
  # Layers are different relations, so each is as directed as its own slice.
  # Otherwise the slices hold one relation, which is directed wherever any of
  # them is.
  directed <- if(attribute == "layer") !twomode & !symmetric else
    rep(!twomode && !all(symmetric), d[3])
  # Whether the ties carry values is decided for the array as a whole, so that
  # the slices that hold only ones do not read as ties of unknown value.
  cells <- .data[!is.na(.data)]
  valued <- any(cells != 0 & cells != 1)
  # Each slice is a network of its own, and they are joined as the matching
  # `from_*()` function joins a list of them, so that the two agree.
  nets <- lapply(seq_len(d[3]), function(k){
    slice <- .data[, , k]
    # An undirected slice holds each tie once, on its dyad.
    if(!twomode && !directed[k]) slice[lower.tri(slice)] <- 0
    idx <- which(is.na(slice) | slice != 0, arr.ind = TRUE)
    ties <- dplyr::tibble(from = as.integer(idx[, 1]),
                          to = as.integer(idx[, 2] + if(twomode) d[1] else 0L))
    if(valued) ties$weight <- slice[idx]
    # A missing cell records a tie that was not observed, which is split
    # from the ties by `make_stocnet()`.
    if(anyNA(slice[idx])) ties$na <- is.na(slice[idx])
    make_stocnet(info = list(directed = directed[k]), nodes = nodes,
                 ties = ties)
  })
  names(nets) <- if(attribute %in% c("by", "about") && !is.null(labels))
    labels[third] else as.character(third)
  # The moments of a network are counted from its ties, so a slice that holds
  # no tie leaves no moment behind it.
  if(attribute == "time"){
    empty <- vapply(seq_len(d[3]), function(k)
      all(!is.na(.data[, , k]) & .data[, , k] == 0), logical(1))
    if(any(empty))
      snet_warn("{sum(empty)} slice{?s} of this array hold{?s/} no ties",
                "({.val {as.character(third[empty])}}),",
                "and a moment without ties is not recorded in the network.")
  }
  switch(attribute,
         "time" = from_times(nets),
         "by" = from_reporters(nets),
         "about" = .join_third(nets, "about"),
         "layer" = if(length(nets) == 1)
           mutate_info(mutate_ties(nets[[1]], layer = third[1]),
                       layers = third[1],
                       directed = stats::setNames(directed[1], third[1])) else
             from_layers(nets))
}
  
#' @export
as_stocnet.network <- function(.data,
                           twomode = FALSE, ...) {
  # Read edges together with all their attributes in a single, aligned pass so
  # that tie attributes (e.g. layer, time, weight) stay matched to their edges.
  hyper <- network::is.hyper(.data)
  edf <- if(hyper) data.frame() else
    network::as.data.frame.network(.data, unit = "edges", na.rm = FALSE)
  # An edge marked in the reserved 'na' attribute records a missing tie rather
  # than a tie, so it is read out of the edges here and compressed below.
  missing <- if(hyper) NULL else as_missinglist(.data)
  if (hyper) {
    # `as.data.frame.network()` expects an edge to have one node at each end,
    # so the edges of a hypergraph are read from the object itself.
    ties <- .network_to_hyper(.data)
  } else if (nrow(edf) == 0) {
    ties <- dplyr::tibble(from = integer(0), to = integer(0))
  } else {
    # `.tail`/`.head` are reported as vertex names; convert them back to the
    # integer node indices that stocnet ties tables use.
    vnames <- as.character(network::network.vertex.names(.data))
    edf$from <- match(as.character(edf$.tail), vnames)
    edf$to <- match(as.character(edf$.head), vnames)
    # 'na' is not among the columns `as.data.frame.network()` reports, since
    # `{network}` treats it as bookkeeping of its own, so it is read directly.
    na <- if ("na" %in% network::list.edge.attributes(.data))
      unlist(network::get.edge.attribute(.data, "na", null.na = FALSE)) else NULL
    edf[c(".tail", ".head", "na")] <- NULL
    if (!is.null(na) && length(na) == nrow(edf))
      edf <- edf[!(!is.na(na) & na), , drop = FALSE]
    # Drop the weight column when all weights are unity (an unweighted network)
    if ("weight" %in% names(edf) && !anyNA(edf$weight) && all(edf$weight == 1))
      edf[["weight"]] <- NULL
    ties <- dplyr::as_tibble(edf) |>
      dplyr::select("from", "to", dplyr::everything())
  }
  # Read back the network attributes that `as_network()` wrote, less those
  # that belong to the structure of a 'network' object rather than to the
  # network it describes, and less those held as their own components.
  info <- .data$gal[setdiff(names(.data$gal),
                            c("n", "mnext", "directed", "hyper", "loops",
                              "multiple", "bipartite", "changes", "globals", "global",
                              "layer_directed", "layer_hyper",
                              "hyperedges_held"))]
  # As for igraph, an undirected layer reciprocated on the way out is
  # collapsed again here, so that the round trip is lossless.
  # A hypergraph is written without reciprocating any layer, since its ties
  # are not all dyads, so there is nothing to collapse in one.
  collapsed <- if(hyper) NULL else
    .collapse_layers(ties, .data$gal$layer_directed, is_directed(.data))
  # see `as_stocnet.igraph()` for why this is recorded
  if("sign" %in% network::list.edge.attributes(.data)) info$signed <- TRUE
  if(is.null(collapsed)) info$directed <-
    (if(hyper) .data$gal$layer_directed) %||% is_directed(.data) else {
    ties <- collapsed$ties
    info$directed <- collapsed$directed
  }
  out <- list(info = info,
              nodes = as_nodelist(.data),
              ties = ties,
              changes = as_changelist(.data),
              globals = as_globallist(.data),
              missings = NULL)
  class(out) <- c("stocnet", class(out))
  # Integer vertex names are anonymous node ids, not real labels, so drop them.
  if(!is.null(out$nodes) &&
     inherits(network::network.vertex.names(.data), "integer")){
    out$nodes$label <- NULL
    if(ncol(out$nodes) == 0) out$nodes <- NULL
  }
  .compress_missing(out, .missing_in(missing, out$nodes))
}

# RSiena ####

#' @rdname coerce_stocnet
#' @export
as_siena <- function(.data,
                     twomode = FALSE) UseMethod("as_siena")

#' @export
as_siena.default <- function(.data, twomode = FALSE) {
  # Any object that can be coerced to a 'stocnet' (igraph, tidygraph/mnet,
  # matrix, network, edgelist, ...) reaches SIENA through that richer path,
  # so that multiplex layers, waves, covariates, etc. are carried across.
  as_siena(as_stocnet(.data, twomode = twomode), twomode = twomode)
}

#' @export
as_siena.stocnet <- function(.data, twomode = FALSE) {
  thisRequires("RSiena")
  x <- .data
  # SIENA holds a missing tie as a missing cell of the array, which it imputes,
  # so the ties recorded as missing join the ties here, marked as such, and the
  # array builder writes them out as missing rather than as observed non-ties.
  x$ties <- .join_missing_ties(x)
  # A structural zero is written as SIENA's 10. The dyads of an absent actor
  # are not, since the composition change already tells SIENA about them.
  x$ties <- .join_fixed_zeros(x$ties, .fixed_registry(.data))
  # The longitudinal column may arrive as 'wave' (mnet convention) or 'time'.
  x$ties <- .siena_wave_to_time(x$ties)
  x$changes <- .siena_wave_to_time(x$changes)
  info <- x$info %||% list()
  smeta <- info$siena %||% list()
  svars <- smeta$vars %||% list()
  focal <- info$focal %||% character(0)
  centered <- info$centered %||% logical(0)
  nodes <- x$nodes %||% dplyr::tibble(label = character(0))
  # Node sets (modes) ----
  if ("mode" %in% names(nodes)) {
    set_names <- unique(as.character(nodes[["mode"]]))
    sizes <- vapply(set_names, function(m) sum(nodes[["mode"]] == m), integer(1))
  } else {
    set_names <- (info$modes %||% "Actors")[1]
    sizes <- stats::setNames(nrow(nodes), set_names)
  }
  offsets <- stats::setNames(cumsum(c(0, sizes))[seq_along(sizes)], set_names)
  labelled <- isTRUE(smeta$labelled)
  labs_of <- function(m) if (labelled)
    as.character(nodes$label[.siena_mode_rows(nodes, m, set_names)]) else NULL
  nodeSetObjs <- lapply(set_names, function(m)
    RSiena::sienaNodeSet(unname(sizes[m]), nodeSetName = m, names = labs_of(m)))
  W <- smeta$observations %||% .siena_infer_waves(x)
  cent_of <- function(nm) if (nm %in% names(centered)) unname(centered[nm]) else TRUE
  ns_of <- function(nm, default) svars[[nm]]$nodeSet %||% default
  objs <- list()
  # Tie layers: dependent networks, or dyadic covariates ----
  if (!is.null(x$ties) && nrow(x$ties)) {
    has_layer <- "layer" %in% names(x$ties)
    tie_layers <- if (has_layer) unique(as.character(x$ties$layer)) else "network"
    # With no declared dependent, treat the first tie layer as the dependent net.
    if (!length(focal)) focal <- tie_layers[1]
    for (L in tie_layers) {
      sub <- if (has_layer) x$ties[as.character(x$ties$layer) == L, , drop = FALSE] else x$ties
      meta <- svars[[L]]
      ns <- ns_of(L, set_names[1])
      off_from <- unname(offsets[ns[1]]); off_to <- unname(offsets[ns[length(ns)]])
      sz_from <- unname(sizes[ns[1]]);   sz_to <- unname(sizes[ns[length(ns)]])
      if (L %in% focal) { # dependent network
        if (W < 2)
          snet_abort(c("A SIENA dependent network requires at least two waves,",
                       "i" = "but the '{L}' layer has only one.",
                       "i" = paste("Add a 'time'/'wave' column to the ties, or",
                                   "mark the longitudinal layer(s) via info$focal.")))
        type <- meta$type %||% if (length(ns) > 1 || twomode) "bipartite" else "oneMode"
        arr <- .siena_ties_to_array(sub, W, sz_from, sz_to, off_from, off_to,
                                    onemode = identical(type, "oneMode"),
                                    fixed = TRUE)
        if (!all(arr %in% c(0, 1, 10, 11) | is.na(arr)))
          snet_abort(c("A SIENA dependent network must have binary ties,",
                       "x" = "but the '{L}' layer is valued or signed.",
                       "i" = "Dichotomise it first with `to_unweighted()`."))
        if (labelled) dimnames(arr) <- list(labs_of(ns[1]), labs_of(ns[length(ns)]), NULL)
        objs[[L]] <- RSiena::sienaDependent(arr, type = type, nodeSet = ns,
                                            allowOnly = meta$allowOnly %||% TRUE)
      } else { # dyadic covariate
        has_time <- "time" %in% names(sub) && any(!is.na(sub$time))
        if (has_time) {
          arr <- .siena_ties_to_array(sub, max(1, W - 1), sz_from, sz_to,
                                      off_from, off_to, onemode = FALSE)
          objs[[L]] <- RSiena::varDyadCovar(arr, nodeSets = ns,
                                            centered = cent_of(L))
        } else {
          m <- .siena_ties_to_array(sub, 1, sz_from, sz_to, off_from, off_to,
                                    onemode = FALSE)[, , 1]
          objs[[L]] <- RSiena::coDyadCovar(m, nodeSets = ns, centered = cent_of(L))
        }
      }
    }
  }
  # Change variables: behavioural dependents, or varying covariates ----
  # A change of 'active' composes the node set, and a change of 'na' records
  # that a node did not answer, which `.join_missing_ties()` already writes
  # into the dependent network as missing cells. Neither is a covariate, and
  # the nodal columns of the same two names are passed over below for the
  # same reason.
  chvars <- setdiff(unique(as.character(x$changes$var)), c("active", "na"))
  for (V in chvars) {
    ns <- ns_of(V, set_names[1])
    rows <- .siena_mode_rows(nodes, ns[1], set_names)
    off <- unname(offsets[ns[1]]); sz <- unname(sizes[ns[1]])
    if (V %in% focal) { # behavioural dependent [n, 1, W]
      mat <- .siena_changes_to_matrix(x$changes, V, sz, off, W)
      arr <- array(mat, dim = c(sz, 1, W))
      objs[[V]] <- RSiena::sienaDependent(arr, type = "behavior", nodeSet = ns[1],
                                          allowOnly = svars[[V]]$allowOnly %||% TRUE)
    } else { # varying covariate [n, W-1]
      mat <- .siena_changes_to_matrix(x$changes, V, sz, off, NULL)
      objs[[V]] <- RSiena::varCovar(mat, nodeSet = ns[1], centered = cent_of(V))
    }
  }
  # Constant covariates from (non-reserved) nodal columns ----
  reserved <- c("label", "mode", "active", "na", "type", "name")
  for (C in setdiff(names(nodes), reserved)) {
    # A variable measured at every wave is held both as a nodal column, for the
    # first wave, and as changes, for the rest. The changes cover every wave,
    # so the object built from them stands and the column is passed over.
    if (C %in% names(objs)) next
    ns <- ns_of(C, set_names[1])
    rows <- .siena_mode_rows(nodes, ns[1], set_names)
    vals <- nodes[[C]][rows]
    # SIENA covariates must be plain numeric vectors, so encode any categorical
    # attribute as numeric codes and strip stray attributes/classes.
    vals <- if (is.numeric(vals)) as.vector(vals) else as.numeric(as.factor(vals))
    if (all(is.na(vals))) {
      snet_info("Dropping the '{C}' attribute, since it holds no observed values.")
      next
    }
    objs[[C]] <- RSiena::coCovar(vals, nodeSet = ns[1], centered = cent_of(C))
  }
  # Composition change (from active-variable changes) ----
  comp <- .siena_build_composition(x, sizes, offsets, set_names, W, smeta)
  args <- objs
  if (!is.null(comp)) args <- c(args, list(comp))
  args <- c(args, list(nodeSets = nodeSetObjs))
  do.call(RSiena::sienaDataCreate, args)
}

# MoNAn ####

#' @rdname coerce_stocnet
#' @param layer For a network of people and places with more than one layer
#'   of ties between people and places,
#'   the name of the layer that says where each person is.
#' @param time1,time2 The two times at which the places of the people are
#'   taken, as the origin and the destination.
#'   The defaults are the first and the last time of the network.
#' @section Mobility:
#'   `as_monan()` takes a two-mode network,
#'   with people as the first mode and places as the second mode.
#'   The ties between people and places say where each person is.
#'   They have `begin` and `end` times, or the `time` of each wave of a panel.
#'   A person holds one place at a time.
#'   `as_monan()` takes the place of each person at `time1` and `time2`,
#'   and gives these elements:
#'
#'   - `moves`, the dependent variable, with one row for each person:
#'   the place at `time1` and the place at `time2`.
#'   A person who stays has the same place twice.
#'   - `people` and `places`, the two node sets.
#'   - each numeric attribute of the people or of the places,
#'   under its own name.
#'   - each layer of ties among the places, such as adjacency or distance,
#'   under its layer name.
#'
#'
#'   `as_monan()` also takes a network of places with an `occupant` record,
#'   as [play_mobility()] gives.
#'   The occupants are then the people,
#'   and the attributes of the occupants at the start are
#'   the attributes of the people.
#'   The start is time 0.
#'
#'   A person who has no place at one of the two times is not included.
#'   MoNAn requires that every place occurs in `moves`,
#'   so a place that no person holds at either time is removed.
#'   See also [to_mobility()], which gives the moves as a network of places.
#' @export
as_monan <- function(.data, layer = NULL, time1, time2) {
  thisRequires("MoNAn")
  net <- as_stocnet(.data)
  mobility <- .mobility_stays(net, layer = layer)
  stays <- mobility$stays
  times <- sort(unique(stays$begin[is.finite(stays$begin)]))
  if(!length(times))
    snet_abort("The ties between people and places have no times.")
  if(missing(time1)) time1 <- times[1]
  if(missing(time2)) time2 <- times[length(times)]
  place_at <- function(t){
    held <- stays[stays$begin <= t & stays$end > t, ]
    held$place[match(unique(stays$person), held$person)]
  }
  people <- unique(stays$person)
  origin <- place_at(time1)
  destination <- place_at(time2)
  keep <- !is.na(origin) & !is.na(destination)
  if(any(!keep))
    snet_info("{sum(!keep)} {cli::qty(sum(!keep))}person{?s} with no place",
              "at one of the two times {?is/are} not included.")
  people <- people[keep]
  origin <- origin[keep]
  destination <- destination[keep]
  places <- mobility$places
  modes <- if(mobility$occupant) rep(2L, nrow(net$nodes)) else
    .mode_index(net$nodes$mode, mode_names(net))
  # The rows of the nodes that hold the attributes of the people:
  # for an occupant record, the start places of the occupants
  person_rows <- if(mobility$occupant)
    stays$place[match(people, stays$person)] else people
  held <- places[places %in% c(origin, destination)]
  if(length(held) < length(places))
    snet_info("{length(places) - length(held)}",
              "{cli::qty(length(places) - length(held))}place{?s}",
              "that no person holds at either time {?is/are} removed.")
  places <- held
  moves <- cbind(match(origin, places), match(destination, places))
  elements <- list(
    moves = MoNAn::createEdgelist(moves,
                                  nodeSet = c("places", "places", "people")),
    people = MoNAn::createNodeSet(length(people)),
    places = MoNAn::createNodeSet(length(places)))
  # The ties among the places, one network for each layer
  ties <- net$ties
  within <- modes[ties$from] == 2L & modes[ties$to] == 2L &
    ties$from %in% places & ties$to %in% places
  if(any(within)){
    sub <- ties[within, ]
    lyr <- if("layer" %in% names(sub)) sub$layer else rep("ties", nrow(sub))
    for(l in unique(lyr)){
      s <- sub[lyr == l, ]
      m <- matrix(0, length(places), length(places))
      w <- if("weight" %in% names(s)) s$weight else rep(1, nrow(s))
      m[cbind(match(s$from, places), match(s$to, places))] <- w
      symmetric <- !is_directed(net)
      if(symmetric)
        m[cbind(match(s$to, places), match(s$from, places))] <- w
      elements[[l]] <- MoNAn::createNetwork(m, isSymmetric = symmetric,
                                            nodeSet = c("places", "places"))
    }
  }
  # The numeric attributes of the people and of the places
  reserved <- c("label", "mode", "active", "na", "name", "type",
                "occupant", "satisfied")
  for(att in setdiff(names(net$nodes), reserved)){
    vals <- net$nodes[[att]]
    if(!is.numeric(vals)) next
    if(!anyNA(vals[person_rows]))
      elements[[att]] <- MoNAn::createNodeVariable(as.numeric(vals[person_rows]),
                                                   nodeSet = "people")
    # the attributes of an occupant record all describe the occupants
    else if(!mobility$occupant && !anyNA(vals[places]))
      elements[[att]] <- MoNAn::createNodeVariable(as.numeric(vals[places]),
                                                   nodeSet = "places")
  }
  MoNAn::createProcessState(elements, dependentVariable = "moves")
}

# Mobility readers ####

# The places that each person holds, and when, from a two-mode network of
# people (the first mode) and places (the second mode). One row for each stay:
# the person and the place as node indices of the network, and the `begin`
# and `end` of the stay. A stay with no end lasts to `Inf`, and a network with
# no times has stays from `-Inf` to `Inf`. The ties are read from intervals
# (`begin`/`end`) or from the waves of a panel (`time`), where a stay begins
# at the wave in which the person is first at the place, and ends at the next
# wave in which the person is not there.
# A reader of another form of mobility data, such as the `occupant` record of
# `play_mobility()`, gives the same table, so that `to_mobility()` and
# `as_monan()` read any form through it.
.person_places <- function(.data, layer = NULL){
  net <- as_stocnet(.data)
  if(net_modes(net) != 2 || is.null(net$nodes$mode))
    snet_abort("A network of people and places must have two modes:",
               "the people first, then the places.")
  modes <- .mode_index(net$nodes$mode, mode_names(net))
  ties <- net$ties
  if(!is.null(layer)){
    if(!"layer" %in% names(ties) || !layer %in% ties$layer)
      snet_abort("There is no layer {.val {layer}} in this network.")
    ties <- ties[ties$layer %in% layer, ]
  }
  between <- modes[ties$from] != modes[ties$to]
  ties <- ties[between, ]
  if("layer" %in% names(ties) && length(unique(ties$layer)) > 1)
    snet_abort("More than one layer has ties between people and places.",
               "Give the layer that says where each person is in {.arg layer}.")
  person <- ifelse(modes[ties$from] == 1L, ties$from, ties$to)
  place <- ifelse(modes[ties$from] == 1L, ties$to, ties$from)
  atts <- names(ties)
  begin_col <- intersect(c("begin", "beg", "start"), atts)[1]
  time_col <- intersect(c("time", "wave", "panel"), atts)[1]
  if(!is.na(begin_col)){
    begin <- .bare_time(ties[[begin_col]])
    end <- if("end" %in% atts) .bare_time(ties[["end"]]) else rep(Inf, nrow(ties))
    end[is.na(end)] <- Inf
    out <- dplyr::tibble(person = person, place = place,
                         begin = begin, end = end)
  } else if(!is.na(time_col)){
    time <- .bare_time(ties[[time_col]])
    waves <- sort(unique(time))
    dup <- duplicated(data.frame(person, time))
    if(any(dup))
      .two_places_abort(net, person[dup][1], time[dup][1])
    out <- lapply(split(seq_along(person), person), function(i){
      w <- match(time[i], waves)
      i <- i[order(w)]
      w <- sort(w)
      # a new stay begins where the place changes or a wave is skipped
      new <- c(TRUE, place[i][-1] != place[i][-length(i)] | diff(w) != 1)
      run <- cumsum(new)
      last <- tapply(w, run, max)
      dplyr::tibble(person = person[i][new], place = place[i][new],
                    begin = waves[w[new]],
                    end = c(waves, Inf)[as.integer(last) + 1])
    })
    out <- dplyr::bind_rows(out)
  } else {
    out <- dplyr::tibble(person = person, place = place,
                         begin = -Inf, end = Inf)
  }
  out <- out[order(out$person, out$begin), ]
  .check_one_place(net, out)
  out
}

# The places that each occupant holds, and when, from a network of places
# with an `occupant` record, as `play_mobility()` gives. The same table as
# `.person_places()`, but `person` is the name of the occupant, as in the
# `occupant` column, and `place` is the node index of the place.
# The occupants of the nodes hold their places from time 0, the start.
.occupant_places <- function(.data){
  net <- as_stocnet(.data)
  current <- as.character(net$nodes$occupant)
  held <- which(!is.na(current))
  out <- list(dplyr::tibble(person = current[held], place = held,
                            begin = 0, end = Inf))
  changes <- net$changes
  if(!is.null(changes) && nrow(changes)){
    changes <- changes[changes$var == "occupant", , drop = FALSE]
    value <- changes$value
    if(is.list(value)) value <- unlist(lapply(value, function(v)
      if(length(v)) as.character(v)[1] else NA_character_))
    changes$value <- as.character(value)
    if(is.character(changes$node))
      changes$node <- match(changes$node, net$nodes$label)
    for(t in sort(unique(.bare_time(changes$time)))){
      now <- changes[.bare_time(changes$time) == t, , drop = FALSE]
      now <- now[!duplicated(now$node, fromLast = TRUE), , drop = FALSE]
      # the stays at these places end, and the new stays begin
      ends <- now$node[!is.na(current[now$node])]
      out <- c(out, list(dplyr::tibble(person = current[ends], place = ends,
                                       begin = NA_real_, end = t)))
      current[now$node] <- now$value
      begins <- now$node[!is.na(now$value)]
      out <- c(out, list(dplyr::tibble(person = current[begins], place = begins,
                                       begin = t, end = Inf)))
    }
  }
  rows <- dplyr::bind_rows(out)
  # each end closes the open stay of that person at that place
  stays <- rows[!is.na(rows$begin), ]
  ends <- rows[is.na(rows$begin), ]
  for(k in seq_len(nrow(ends))){
    open <- which(stays$person == ends$person[k] & stays$place == ends$place[k] &
                    is.infinite(stays$end))
    stays$end[open[length(open)]] <- ends$end[k]
  }
  stays <- stays[stays$begin < stays$end, ]
  stays <- stays[order(stays$person, stays$begin), ]
  .check_one_place(net, stays)
  stays
}

# Whether a network is a network of places with an `occupant` record,
# rather than a network of people and places
.is_occupant_record <- function(net){
  "occupant" %in% names(net$nodes) && net_modes(net) < 2
}

# The stays of the people at the places, from either form of mobility data,
# with the node indices of the places
.mobility_stays <- function(net, layer = NULL){
  if(.is_occupant_record(net))
    return(list(stays = .occupant_places(net), places = seq_len(nrow(net$nodes)),
                occupant = TRUE))
  stays <- .person_places(net, layer = layer)
  modes <- .mode_index(net$nodes$mode, mode_names(net))
  list(stays = stays, places = which(modes == 2L), occupant = FALSE)
}

# One place for each person at each time
.check_one_place <- function(net, stays){
  for(p in unique(stays$person)){
    s <- stays[stays$person == p, ]
    if(nrow(s) > 1){
      clash <- which(s$begin[-1] < s$end[-nrow(s)])
      if(length(clash)) .two_places_abort(net, p, s$begin[clash[1] + 1])
    }
  }
}

.two_places_abort <- function(net, person, time){
  who <- if(!is.character(person) && is_labelled(net)) net$nodes$label[person] else person
  snet_abort("A person can hold one place at a time,",
             "but {.val {who}} holds two places at time {time}.")
}

# stocnet from sienadata ####

#' @export
as_stocnet.sienadata <- function(.data, twomode = FALSE, ...) {
  thisRequires("RSiena")
  sd <- .data
  W <- sd$observations
  nsets <- sd$nodeSets
  set_names <- names(nsets)
  set_sizes <- stats::setNames(vapply(nsets, length, integer(1)), set_names)
  offsets <- stats::setNames(cumsum(c(0, set_sizes))[seq_along(set_sizes)],
                             set_names)
  labs <- .siena_node_labels(sd, set_names, set_sizes)
  labelled <- any(vapply(seq_along(set_names), function(i)
    !identical(labs[[i]], as.character(seq_len(set_sizes[i]))), logical(1)))
  multimode <- length(set_names) > 1
  nodes <- dplyr::tibble(label = unlist(labs, use.names = FALSE))
  if (multimode) nodes$mode <- rep(set_names, set_sizes)
  ties <- list(); changes <- list(); zeros <- list()
  focal <- character(0); centered <- logical(0)
  directed <- logical(0); svars <- list()
  # Dependent variables ----
  for (nm in names(sd$depvars)) {
    dv <- sd$depvars[[nm]]
    type <- attr(dv, "type"); ns <- attr(dv, "nodeSet")
    focal <- c(focal, nm)
    svars[[nm]] <- .siena_prune(list(role = "dependent", type = type, nodeSet = ns,
                                     allowOnly = attr(dv, "allowOnly"),
                                     uponly = attr(dv, "uponly"),
                                     downonly = attr(dv, "downonly"),
                                     sparse = attr(dv, "sparse")))
    if (identical(type, "behavior")) {
      changes[[nm]] <- .siena_behavior_to_changes(dv, nm, offsets[ns[1]], W)
    } else {
      directed[nm] <- !isTRUE(attr(dv, "symmetric"))
      ties[[nm]] <- .siena_array_to_ties(dv, nm, ns, offsets, W,
                                         onemode = identical(type, "oneMode"),
                                         fixed = TRUE)
      zeros[[nm]] <- attr(ties[[nm]], "zeros")
      attr(ties[[nm]], "zeros") <- NULL
    }
  }
  # Constant covariates -> nodal columns ----
  for (nm in names(sd$cCovars)) {
    cc <- sd$cCovars[[nm]]; ns <- attr(cc, "nodeSet")
    col <- rep(NA_real_, nrow(nodes))
    col[.siena_mode_rows(nodes, ns, set_names)] <- .siena_uncenter(cc)
    nodes[[nm]] <- col
    centered[nm] <- isTRUE(attr(cc, "centered"))
    svars[[nm]] <- .siena_prune(list(role = "covar", kind = "coCovar", nodeSet = ns))
  }
  # Varying covariates -> change rows ----
  for (nm in names(sd$vCovars)) {
    vc <- sd$vCovars[[nm]]; ns <- attr(vc, "nodeSet")
    changes[[paste0(".vc_", nm)]] <- .siena_matrix_to_changes(
      matrix(.siena_uncenter(vc), nrow = dim(vc)[1]), nm, offsets[ns[1]])
    centered[nm] <- isTRUE(attr(vc, "centered"))
    svars[[nm]] <- .siena_prune(list(role = "covar", kind = "varCovar", nodeSet = ns))
  }
  # Constant dyadic covariates -> tie layer (no time) ----
  for (nm in names(sd$dycCovars)) {
    dc <- sd$dycCovars[[nm]]; ns <- attr(dc, "nodeSet")
    ties[[nm]] <- .siena_matrix_to_ties(dc, nm, ns, offsets)
    centered[nm] <- isTRUE(attr(dc, "centered"))
    svars[[nm]] <- .siena_prune(list(role = "covar", kind = "coDyadCovar",
                                     nodeSet = ns, type = attr(dc, "type"),
                                     sparse = attr(dc, "sparse")))
  }
  # Varying dyadic covariates -> tie layer (with time) ----
  for (nm in names(sd$dyvCovars)) {
    vd <- sd$dyvCovars[[nm]]; ns <- attr(vd, "nodeSet")
    ties[[nm]] <- .siena_array_to_ties(vd, nm, ns, offsets, dim(vd)[3],
                                       onemode = FALSE)
    centered[nm] <- isTRUE(attr(vd, "centered"))
    svars[[nm]] <- .siena_prune(list(role = "covar", kind = "varDyadCovar",
                                     nodeSet = ns, type = attr(vd, "type"),
                                     sparse = attr(vd, "sparse")))
  }
  # Composition change -> active change rows ----
  siena_meta <- list(version = attr(sd, "version"), observations = W,
                     labelled = labelled, vars = svars,
                     nodeSetRelations = .siena_prune(
                       list(higher = attr(sd, "higher"),
                            disjoint = attr(sd, "disjoint"),
                            atLeastOne = attr(sd, "atLeastOne"))))
  if (length(sd$compositionChange)) {
    ccobj <- sd$compositionChange[[1]]
    ns <- attr(ccobj, "nodeSet") %||% set_names[1]
    changes[["active"]] <- .siena_composition_to_changes(ccobj, ns, offsets, W)
    siena_meta$ccOption <- attr(sd$compositionChange, "ccOption")
    siena_meta$compositionNodeSet <- ns
  }
  # Assemble info ----
  info <- list(modes = set_names)
  tie_layers <- names(ties)
  if (length(tie_layers)) info$layers <- tie_layers
  if (length(directed)) info$directed <- directed
  if (length(focal)) info$focal <- focal
  if (length(centered)) info$centered <- centered
  info$siena <- siena_meta
  # SIENA holds a dyad observed as missing as a missing value, which a stocnet
  # records in a column of its own. This is done over the layers together,
  # since binding them would otherwise leave an unvalued layer missing weights.
  # SIENA holds a missing tie as a missing value, which `make_stocnet()` reads
  # from an 'na' column and compresses into nonresponse records. Marking them
  # over the layers together stops a layer without values, bound beside one
  # with them, being read as missing throughout.
  ties_tbl <- if (length(ties)) .mark_missing_weights(dplyr::bind_rows(ties)) else NULL
  changes_tbl <- if (length(changes)) dplyr::bind_rows(changes) else NULL
  # SIENA's structural zeros are held as fixed rows of the missings, apart
  # from the dyads of absent actors, which the composition change implies.
  zeros_tbl <- if (length(zeros)) dplyr::bind_rows(zeros) else NULL
  make_stocnet(info = info, nodes = nodes, ties = ties_tbl,
               changes = changes_tbl, missings = zeros_tbl)
}

# siena coercion helpers ####

# Rows of the (global) node table belonging to a given mode/nodeSet.
.siena_mode_rows <- function(nodes, mode, set_names) {
  if ("mode" %in% names(nodes)) which(as.character(nodes[["mode"]]) == mode) else
    seq_len(nrow(nodes))
}

# Drop NULL and empty elements from a metadata list.
.siena_prune <- function(x) x[!vapply(x, function(e) is.null(e) || length(e) == 0,
                                      logical(1))]

# Recover the raw (uncentered) values of a nodal siena covariate. RSiena stores
# nodal covariates mean-centered; adding the stored mean back recovers the input
# so that re-centering on the way out reproduces the original exactly.
.siena_uncenter <- function(cov) {
  vals <- as.vector(cov)
  m <- attr(cov, "mean")
  if (isTRUE(attr(cov, "centered")) && !is.null(m)) vals <- vals + m
  vals
}

# Treat a 'wave' column as the canonical 'time' column.
.siena_wave_to_time <- function(tbl) {
  if (is.null(tbl)) return(tbl)
  if ("wave" %in% names(tbl) && !"time" %in% names(tbl))
    tbl <- dplyr::rename(tbl, time = "wave")
  tbl
}

# Infer the number of observations (waves) from the ties/changes of a stocnet.
.siena_infer_waves <- function(x) {
  tt <- if (!is.null(x$ties) && "time" %in% names(x$ties)) x$ties$time else NULL
  ct <- if (!is.null(x$changes) && "time" %in% names(x$changes)) x$changes$time else NULL
  suppressWarnings(max(c(1, as.numeric(tt), as.numeric(ct)), na.rm = TRUE))
}

# Collect node labels per node set from depvar/covar dimnames.
.siena_node_labels <- function(sd, set_names, set_sizes) {
  labs <- stats::setNames(lapply(set_names, function(m)
    as.character(seq_len(set_sizes[m]))), set_names)
  for (dv in sd$depvars) {
    ns <- attr(dv, "nodeSet"); dn <- dimnames(dv)
    if (is.null(dn)) next
    if (!is.null(dn[[1]]) && length(dn[[1]]) == set_sizes[ns[1]])
      labs[[ns[1]]] <- dn[[1]]
    if (length(ns) > 1 && length(dn) > 1 && !is.null(dn[[2]]) &&
        length(dn[[2]]) == set_sizes[ns[2]]) labs[[ns[2]]] <- dn[[2]]
  }
  labs
}

# A [n, n(/m), W] siena array -> tie tibble (from, to, layer, weight, time).
# With `fixed`, SIENA's structural codes are read as such rather than as
# values: 11 as a tie fixed by design, and 10 as a dyad fixed at no tie.
# A structural zero is not a tie, so those dyads are returned apart, as the
# 'zeros' attribute, for `make_stocnet()` to hold in the missings.
.siena_array_to_ties <- function(arr, nm, ns, offsets, W, onemode,
                                 fixed = FALSE) {
  off_from <- unname(offsets[ns[1]]); off_to <- unname(offsets[ns[length(ns)]])
  zeros <- list()
  rows <- lapply(seq_len(W), function(w) {
    g <- if (length(dim(arr)) == 3) arr[, , w] else arr
    if (onemode) diag(g) <- 0
    if (fixed) {
      zero <- which(!is.na(g) & g == 10, arr.ind = TRUE)
      if (nrow(zero))
        zeros[[length(zeros) + 1]] <<- dplyr::tibble(
          from = as.integer(off_from + zero[, 1]),
          to = as.integer(off_to + zero[, 2]), layer = nm, time = w,
          fixed = TRUE)
      g[!is.na(g) & g == 10] <- 0
    }
    idx <- which(g != 0 | is.na(g), arr.ind = TRUE)
    if (!nrow(idx)) return(NULL)
    out <- dplyr::tibble(from = off_from + idx[, 1], to = off_to + idx[, 2],
                         layer = nm, weight = as.vector(g[idx]), time = w)
    if (fixed) {
      out$fixed <- !is.na(out$weight) & out$weight == 11
      out$weight[out$fixed] <- 1
    }
    out
  })
  out <- dplyr::bind_rows(rows)
  if (fixed && "fixed" %in% names(out) && !any(out$fixed)) out$fixed <- NULL
  if (length(zeros)) attr(out, "zeros") <- dplyr::bind_rows(zeros)
  out
}

# A constant [n, n] siena matrix -> tie tibble (no time).
.siena_matrix_to_ties <- function(m, nm, ns, offsets) {
  m <- as.matrix(m)
  off_from <- unname(offsets[ns[1]]); off_to <- unname(offsets[ns[length(ns)]])
  idx <- which(m != 0 | is.na(m), arr.ind = TRUE)
  if (!nrow(idx)) return(NULL)
  dplyr::tibble(from = off_from + idx[, 1], to = off_to + idx[, 2],
                layer = nm, weight = as.vector(m[idx]))
}

# A behaviour [n, 1, W] siena array -> change rows for all waves.
.siena_behavior_to_changes <- function(dv, nm, off, W) {
  off <- unname(off)
  rows <- lapply(seq_len(W), function(w) {
    v <- as.vector(dv[, 1, w])
    dplyr::tibble(time = w, node = as.integer(off + seq_along(v)), var = nm,
                  value = as.list(v))
  })
  dplyr::bind_rows(rows)
}

# A varying covariate [n, periods] matrix -> change rows.
.siena_matrix_to_changes <- function(vc, nm, off) {
  off <- unname(off); vc <- as.matrix(vc)
  rows <- lapply(seq_len(ncol(vc)), function(p) {
    v <- vc[, p]
    dplyr::tibble(time = p, node = as.integer(off + seq_along(v)), var = nm,
                  value = as.list(v))
  })
  dplyr::bind_rows(rows)
}

# Composition change list -> active change rows (only non-trivial nodes).
.siena_composition_to_changes <- function(cclist, ns, offsets, W) {
  off <- unname(offsets[ns])
  rows <- lapply(seq_along(cclist), function(i) {
    v <- as.numeric(cclist[[i]])
    if (identical(v, c(1, W))) return(NULL)
    dplyr::tibble(time = v[1], node = as.integer(off + i), var = "active",
                  value = list(v))
  })
  dplyr::bind_rows(rows)
}

# The ties with the structural zeros joined to them, marked in a '.zero'
# column that only `.siena_ties_to_array()` reads.
.join_fixed_zeros <- function(ties, zeros){
  if (is.null(zeros) || !nrow(zeros)) return(ties)
  zeros <- zeros[intersect(names(zeros), c("from", "to", names(ties)))]
  zeros$.zero <- TRUE
  out <- dplyr::bind_rows(ties, zeros)
  out$.zero[is.na(out$.zero)] <- FALSE
  out
}

# Tie rows -> a [sz_from, sz_to, W] array (weights, preserving NA).
# With `fixed`, a tie fixed by design is written as SIENA's structural one.
.siena_ties_to_array <- function(sub, W, sz_from, sz_to, off_from, off_to,
                                 onemode, fixed = FALSE) {
  arr <- array(0, dim = c(sz_from, sz_to, W))
  if (nrow(sub)) {
    tv <- if ("time" %in% names(sub)) sub$time else rep(1, nrow(sub))
    wv <- if ("weight" %in% names(sub)) sub$weight else rep(1, nrow(sub))
    # A dyad recorded as missing is missing to SIENA too, which imputes it,
    # rather than an observed non-tie, which would bias the estimates.
    if ("na" %in% names(sub)) wv[!is.na(sub$na) & sub$na] <- NA
    # A tie that is present by design is one SIENA must not change, which it
    # reads from a code of 11. Validation has already ruled out a weight of 0.
    if (fixed && "fixed" %in% names(sub))
      wv[!is.na(sub$fixed) & sub$fixed] <- 11
    # A dyad fixed at no tie is SIENA's structural zero, and is no tie at all
    # wherever the array has no code for one, as a covariate has not.
    if (".zero" %in% names(sub))
      wv[!is.na(sub$.zero) & sub$.zero] <- if (fixed) 10 else 0
    # all cells are written at once, the later of two rows for the same cell
    # taking its place as it would have one row at a time
    w <- ifelse(is.na(tv), 1L, as.integer(tv))
    keep <- w >= 1 & w <= W
    arr[cbind(sub$from - off_from, sub$to - off_to, w)[keep, , drop = FALSE]] <-
      wv[keep]
  }
  if (onemode) for (w in seq_len(W)) diag(arr[, , w]) <- 0
  arr
}

# Change rows for one variable -> a [sz, W] matrix (NULL W -> infer periods).
.siena_changes_to_matrix <- function(chg, V, sz, off, W) {
  sub <- chg[as.character(chg$var) == V, , drop = FALSE]
  periods <- if (is.null(W)) max(as.numeric(sub$time)) else W
  mat <- matrix(NA_real_, nrow = sz, ncol = periods)
  for (r in seq_len(nrow(sub))) {
    val <- sub$value[[r]]
    mat[sub$node[r] - off, as.integer(sub$time[r])] <- as.numeric(val)[1]
  }
  mat
}

# Build a sienaCompositionChange from active-variable change rows. Two encodings
# are supported: the interval encoding produced by as_stocnet.sienadata (each
# value a numeric c(enter, leave, ...) vector), and the native manynet encoding
# (initial presence in nodes$active plus per-wave logical 'active' changes).
.siena_build_composition <- function(x, sizes, offsets, set_names, W, smeta) {
  if (is.null(x$changes) || !any(as.character(x$changes$var) == "active"))
    return(NULL)
  ns <- smeta$compositionNodeSet %||% set_names[1]
  off <- unname(offsets[ns]); sz <- unname(sizes[ns])
  sub <- x$changes[as.character(x$changes$var) == "active", , drop = FALSE]
  option <- smeta$ccOption %||% 1
  interval_encoded <- any(vapply(sub$value,
                                 function(v) length(as.numeric(unlist(v))) >= 2,
                                 logical(1)))
  if (interval_encoded) {
    changelist <- lapply(seq_len(sz), function(i) c(1, W)) # default present
    for (r in seq_len(nrow(sub))) {
      local <- sub$node[r] - off
      if (local >= 1 && local <= sz)
        changelist[[local]] <- as.numeric(unlist(sub$value[[r]]))
    }
  } else { # native per-wave presence -> intervals
    initial <- rep(TRUE, sz)
    if ("active" %in% names(x$nodes)) {
      av <- as.logical(x$nodes[["active"]])[.siena_mode_rows(x$nodes, ns, set_names)]
      if (length(av) == sz) initial <- av
    }
    present <- matrix(rep(initial, W), nrow = sz)
    ord <- order(as.numeric(sub$time))
    for (r in ord) {
      local <- sub$node[r] - off; w <- as.integer(sub$time[r])
      if (local >= 1 && local <= sz && !is.na(w) && w >= 1 && w <= W)
        present[local, w:W] <- as.logical(sub$value[[r]])[1]
    }
    changelist <- lapply(seq_len(sz), function(i)
      .siena_presence_to_interval(present[i, ], W))
  }
  RSiena::sienaCompositionChange(changelist, nodeSet = ns, option = option)
}

# A logical presence vector across waves -> RSiena (enter, leave, ...) intervals.
.siena_presence_to_interval <- function(p, W) {
  if (!any(p)) return(c(1, 1))
  r <- rle(p); times <- numeric(0); pos <- 1
  for (k in seq_along(r$lengths)) {
    len <- r$lengths[k]
    if (isTRUE(r$values[k])) times <- c(times, pos, pos + len - 1)
    pos <- pos + len
  }
  times
}


make_node_measure <- function(out, .data) {
  if(is_labelled(.data)) names(out) <- node_labels(.data)
  class(out) <- c("node_measure", class(out))
  attr(out, "mode") <- node_is_mode(.data)
  out
}

make_tie_measure <- function(out, .data) {
  class(out) <- c("tie_measure", class(out))
  .name_ties(out, .data)
}

make_network_measure <- function(out, .data, call) {
  class(out) <- c("network_measure", class(out))
  attr(out, "mode") <- mode_nodes(.data)
  attr(out, "call") <- call
  out
}

# Printing ####

# Prints a concise, subtle one-line header describing what was measured,
# how the values were rescaled, and the range they can fall within, e.g.
# "# Strength centrality [0, Inf)" or "# Degree centrality, normalised [0, 1]".
# Measure objects made by older versions of the measure-making packages,
# or by other packages, will not carry these attributes,
# in which case nothing is printed.
measure_header <- function(x) {
  out <- paste0(measure_label(attr(x, "measure")),
                measure_scale(attr(x, "normalization"), attr(x, "range")))
  out <- trimws(out)
  if(!nzchar(out)) return(invisible(NULL))
  # the making packages record what was measured, e.g. "strength centrality";
  # how that reads in a header is a question of presentation, so it is
  # capitalised here rather than there. Only the first character is touched,
  # leaving names such as "PageRank" or "E-I index" as they were given.
  out <- paste0(toupper(substring(out, 1, 1)), substring(out, 2))
  cat(pillar::style_subtle(paste0("# ", out, "\n")))
  invisible(NULL)
}

measure_label <- function(measure) {
  if(is.null(measure) || all(is.na(measure))) return("")
  as.character(measure)[1]
}

# How the values were rescaled is named by the measure-making package,
# e.g. "normalised", "scaled", or "proportion", and is surfaced here as
# given rather than translated, so that its vocabulary can grow without
# this method having to know about it. A measure rescaled in no way is
# given its range alone, e.g. "Strength centrality [0, Inf)", while one
# that was says so first, e.g. "Degree centrality, normalised [0, 1]".
measure_scale <- function(normalization, range) {
  range <- measure_range(range)
  normalization <- measure_norm(normalization)
  if(!nzchar(normalization))
    return(if(nzchar(range)) paste0(" ", range) else "")
  paste0(", ", trimws(paste(normalization, range)))
}

# Ranges may be given as a numeric pair, e.g. `c(0,1)`, in which case
# infinite bounds are printed as open, or as a ready-made string.
measure_range <- function(range) {
  if(is.null(range) || all(is.na(range))) return("")
  if(is.numeric(range) && length(range) == 2){
    paste0(ifelse(is.finite(range[1]), "[", "("),
           format(range[1]), ", ", format(range[2]),
           ifelse(is.finite(range[2]), "]", ")"))
  } else {
    range <- paste(as.character(range), collapse = ", ")
    if(!nzchar(range) || grepl("^[\\[(]", range)) range else
      paste0("[", range, "]")
  }
}

# "none" is the absence of any rescaling, so goes unmentioned.
measure_norm <- function(normalization) {
  if(is.null(normalization) || all(is.na(normalization))) return("")
  normalization <- as.character(normalization)[1]
  if(tolower(normalization) == "none") "" else normalization
}

#' @importFrom cli spark_bar
#' @export
print.node_measure <- function(x, ...,
                          n = NULL, digits = 3, spark = TRUE){
  measure_header(x)
  if(spark && cli::is_utf8_output()){
    counts <- graphics::hist(x, plot = FALSE)$counts
    cat(cli::spark_bar(counts/sum(counts)), "\n")
  }
  if (any(attr(x, "mode"))) {
    for(m in c(FALSE, TRUE)){
      print_tblvec(y = round(as.numeric(x)[attr(x, "mode") == m], 
                             digits = digits), 
                   names = list(names(x)[attr(x, "mode") == m]),
                   n = n)
      if(!m) cat("\n")
    }
  } else {
    print_tblvec(y = round(as.numeric(x), 
                           digits = digits), 
                 names = list(names(x)),
                 n = n)
  }
  invisible(x)
}

#' @export
print.tie_measure <- function(x, ...,
                               n = NULL,
                               digits = 3) {
  measure_header(x)
  print_tblvec(y = round(as.numeric(x), digits = digits),
               names = list(names(x)), n = n)
  invisible(x)
}

#' @export
print.network_measure <- function(x, ...,
                               digits = 3) {
    measure_header(x)
    if (length(attr(x, "mode")) == 1) {
      print(as.numeric(x), digits = digits)
    } else {
      y <- as.numeric(x)
      if (length(y) == 2)
        names(y) <- paste("Mode", seq_along(attr(x, "mode")))
      print(y, digits = digits)
    }
  invisible(x)
}

# @param FUN A function by which the values should be aggregated
# or summarised when a membership vector is given. By default `mean()`.
# summary(node_degree(mpn_elite_mex),
#         membership = node_structural_equivalence(mpn_elite_mex, k = "elbow"))
#' @export
summary.node_measure <- function(object, ...,
                                 membership,
                                 FUN = mean) {
  if(missing(membership)){
    out <- c(Minimum = min(object, na.rm = TRUE), 
             Maximum = max(object, na.rm = TRUE), 
             Mean = mean(object, na.rm = TRUE), 
             StdDev = stats::sd(object, na.rm = TRUE),
             Missing = sum(is.na(object))
    )
  } else {
    out <- vapply(unique(membership),
                  function(x) FUN(object[membership == x]), FUN.VALUE = 1)
    names(out) <- unique(membership)
  }
  out
}

#' @export
summary.network_measure <- function(object, ...,
                                  null = c("random","configuration"), 
                                  times = 500) {
  null <- paste0("generate_", match.arg(null))
  callItems <- trimws(strsplit(attr(object, "call"), 
                               split = "\\(|\\)|,")[[1]])
  idFun <- which(grepl("^net_", callItems))[1]
  fun <- callItems[idFun]
  dat <- callItems[idFun+1]
  if(length(callItems)>2) oth <- callItems[3:length(callItems)] else
    oth <- NULL
  nulls <- vapply(snet_progress_seq(times), function(r){
    if(is.null(oth))
      suppressMessages(get(fun)(get(null)(get(dat)))) else
        suppressMessages(get(fun)(get(null)(get(dat)), 
                                  gsub("\"", "", oth)))
  }, FUN.VALUE = numeric(1))
  out <- (object - mean(nulls))/stats::sd(nulls)
  out[is.nan(out)] <- 0
  p <- 2 * stats::pnorm(out, 
             mean = mean(nulls), sd = stats::sd(nulls), 
             lower.tail = ifelse(out>0, FALSE, TRUE))
  paste(round(object,3), 
        paste0("(z = ", round(out, 2), ", p = ", round(p, 3), ")"))
}

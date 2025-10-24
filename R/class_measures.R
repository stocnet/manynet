make_node_measure <- function(out, .data) {
  if(is_labelled(.data)) names(out) <- node_names(.data)
  class(out) <- c("node_measure", class(out))
  attr(out, "mode") <- node_is_mode(.data)
  out
}

make_tie_measure <- function(out, .data) {
  class(out) <- c("tie_measure", class(out))
  if(is_labelled(.data)){
    tie_names <- attr(igraph::E(.data), "vnames")
    if(is_directed(.data)) 
      names(out) <- gsub("\\|", "->", tie_names) else 
        names(out) <- gsub("\\|", "-", tie_names)
  } else {
    ties <- as_edgelist(.data)[,1:2]
    if(is_directed(.data)) 
      names(out) <- paste0(ties$from, "->", ties$to) else 
        names(out) <- paste0(ties$from, "-", ties$to)
  }
  out
}

make_network_measure <- function(out, .data, call) {
  class(out) <- c("network_measure", class(out))
  attr(out, "mode") <- net_dims(.data)
  attr(out, "call") <- call
  out
}

# Printing ####
#' @importFrom cli spark_bar
#' @export
print.node_measure <- function(x, ...,
                          n = NULL, digits = 3, spark = TRUE){
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
}

#' @export
print.tie_measure <- function(x, ...,
                               n = NULL,
                               digits = 3) {
  print_tblvec(y = round(as.numeric(x), digits = digits), 
               names = list(names(x)), n = n)
}

#' @export
print.network_measure <- function(x, ...,
                               digits = 3) {
    if (length(attr(x, "mode")) == 1) {
      print(as.numeric(x), digits = digits)
    } else {
      y <- as.numeric(x)
      if (length(y) == 2)
        names(y) <- paste("Mode", seq_len(length(attr(x, "mode"))))
      print(y, digits = digits)
    }
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

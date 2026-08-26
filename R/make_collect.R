# Collections ####
# nocov start
#' Making ego networks through interviewing
#' @name make_ego
#' @description
#'   This function creates an ego network through interactive interview questions.
#'   It currently only supports a simplex, directed network of one
#'   or two modes.
#'   These directed networks can be reformatted as undirected using `to_undirected()`. 
#'   Multiplex networks can be collected separately and then joined together
#'   afterwards.
#'   
#'   The function supports the use of rosters or a maximum number of
#'   alters to collect. If a roster is provided it will offer ego all names.
#'   The function can also prompt ego to interpret each node's attributes,
#'   or about how ego considers their alters to be related.
#' @param ego A character string.
#'   If desired, the name of ego can be declared as an argument.
#'   Otherwise the first prompt of the function will be to enter a name for ego.
#' @param max_alters The maximum number of alters to collect.
#'   By default infinity, but many name generators will expect a maximum of
#'   e.g. 5 alters to be named.
#' @param roster A vector of node names to offer as potential alters for ego.
#' @param interpreter Logical. If TRUE, then it will ask for which attributes
#'   to collect and give prompts for each attribute for each node in the network.
#'   By default FALSE.
#' @param interrelater Logical. If TRUE, then it will ask for the contacts from
#'   each of the alters perspectives too.
#' @param twomode Logical. If TRUE, then it will assign ego to the first mode
#'   and all alters to a second mode.
#' @family makes
#' @export
collect_ego <- function(ego = NULL,
                        max_alters = Inf,
                        roster = NULL,
                        interpreter = FALSE,
                        interrelater = FALSE,
                        twomode = FALSE){
  snet_minor_info("Make sure you assign this function, e.g. {.code obj <- create_ego()}")
  if(is.null(ego)){
    snet_prompt("What is ego's name?")
    ego <- readline()
    if(!is.null(roster)){
      if(ego %in% roster) roster <- setdiff(roster, ego)
    }
  }
  snet_prompt("What is the relationship you are collecting?")
  snet_minor_info("Name the relationship in the singular, e.g. 'friendship'")
  ties <- readline()
  # cli::cli_text("Is this a weighted network?")
  # weighted <- q_yes()
  alters <- as.character(vector())
  if(!is.null(roster)){
    for (alt in roster){
      snet_prompt("Is {ego} connected by a {ties} tie to {alt}?")
      alters <- c(alters, q_yes())
    }
    alters <- roster[alters]
  } else {
    repeat{
      contacts <- length(alters)
      snet_prompt("Please name {cli::qty(contacts)} {?a/another/another} {ties} contact of {ego}:")
      alters <- c(alters, readline())
      if(length(alters) == max_alters){
        snet_info("{.code max_alters} reached.")
        break
      }
      if (q_yes("Are these all the contacts?")) break
    }
  }
  out <- as_tidygraph(as.data.frame(cbind(ego, alters)))
  if(interpreter){
    attr <- vector()
    repeat{
      snet_prompt("Please name an attribute you are collecting, or press [Enter] to continue.")
      attr <- c(attr, readline())
      if (attr[length(attr)]==""){
        attr <- attr[-length(attr)]
        break
      } 
    }
    if(length(attr)>0){
      for(att in attr){
        values <- vector()
        for (alt in c(ego, alters)){
          snet_prompt("What value does {alt} have for {att}:")
          values <- c(values, readline())
        }
        out <- add_node_attribute(out, att, values)
      }
    }
  }
  if(interrelater){
    for(alt in alters){
      others <- setdiff(c(ego,alters), alt)
      extra <- vector()
      for(oth in others){
        snet_prompt("Is {alt} connected by {ties} to {oth}?")
        extra <- c(extra, q_yes())
      }
      # cat(c(rbind(alt, others[extra])))
      out <- add_ties(out, c(rbind(alt, others[extra])))
    }
  }
  if(!is.null(roster) && any(!roster %in% node_labels(out))){
    isolates <- roster[!roster %in% node_labels(out)]
    out <- add_nodes(out, length(isolates), list(name = isolates))
  }
  out <- add_info(out, ties = ties, name = paste("Ego network of", ego),
                  collection = "Interview",
                  year = format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))
  if(twomode) out <- to_twomode(out, c(F, rep(T,net_nodes(out)-1)))
  out
}

q_yes <- function(msg = NULL){
  if(!is.null(msg)) snet_prompt(msg)
  out <- readline()
  if(is.logical(out)) return(out)
  if(out=="") return(FALSE)
  choices <- c("yes","no","true","false")
  out <- c(TRUE,FALSE,TRUE,FALSE)[pmatch(tolower(out), tolower(choices))]
  out
}
# nocov end

# Dependencies ####

#' Making networks of inter- and intra-package dependencies
#'
#' @description
#' These functions create networks of the dependencies between or within
#' R packages:
#'
#' - `collect_cran()` creates a network of the dependencies among the packages
#'    available on CRAN.
#'    It reads the `Depends`, `Imports`, `LinkingTo`, `Suggests`,
#'    and `Enhances` fields of each package's DESCRIPTION file,
#'    and creates a network in which the nodes are packages
#'    and the ties are dependencies of a given type.
#' - `collect_pkg()` creates a network of the dependencies among the functions
#'    defined in a directory of R scripts.
#'    It uses R's own parser to establish where each function is defined
#'    and which functions it calls,
#'    and creates a network in which the nodes are functions
#'    and the ties are calls.
#' @details
#'   Dependency networks grow quickly, and are most useful once scoped.
#'   `collect_cran()` therefore collects only the `Depends`, `Imports`,
#'   and `LinkingTo` fields by default, since these are the dependencies that
#'   must be installed alongside a package, as in `utils::install.packages()`.
#'   Adding `Suggests` grows the dependency closure of a package by
#'   two orders of magnitude.
#'   For the same reason, `collect_pkg()` collects only calls to the functions
#'   defined in the directory by default.
#'
#'   Both return networks that can be scoped further using, for example,
#'   [to_ego()], [to_uniplex()], [to_giant()], [delete_isolates()],
#'   [to_blockmodel()], or [to_subgraph()].
#'
#'   `collect_cran()` relies on `utils::available.packages()`,
#'   which caches the repository index for an hour by default.
#'   Set `options(max.repo.cache.age = )` for a fresher or staler snapshot.
#'
#'   Note that these functions are not as actively maintained as others
#'   in the package, so please let us know if any are not currently working
#'   for you or if there are missing import routines
#'   by [raising an issue on Github](https://github.com/stocnet/manynet/issues).
#' @return A `tidygraph` object representing the network of package dependencies
#'   or function dependencies in a package.
#' @importFrom utils available.packages contrib.url getParseData
#' @name make_collect
#' @family makes
#' @seealso [to_ego()], [to_uniplex()], [delete_isolates()]
NULL

#' @rdname make_collect
#' @param pkg A character vector of one or more package names,
#'   from which dependencies are collected.
#'   By default "all", which collects the dependencies among all the packages
#'   currently available on CRAN.
#' @param dependencies A character vector naming the dependency fields to
#'   collect, from "Depends", "Imports", "LinkingTo", "Suggests",
#'   and "Enhances".
#'   By default `c("Depends", "Imports", "LinkingTo")`,
#'   the dependencies that must be installed alongside a package.
#' @param max_dist The maximum number of steps from `pkg` to collect.
#'   By default infinite, i.e. the whole dependency closure.
#' @param direction Whether to collect the packages that `pkg` depends upon,
#'   "out" by default, the packages that depend upon `pkg`, "in",
#'   or both, "all".
#' @source
#' https://www.r-bloggers.com/2016/01/r-graph-objects-igraph-vs-network/
#' @examples
#' \dontrun{
#' # The packages {manynet} depends upon, directly and indirectly:
#' collect_cran("manynet")
#' # The packages that depend directly upon {manynet}:
#' collect_cran("manynet", direction = "in", max_dist = 1)
#' }
#' @export
collect_cran <- function(pkg = "all",
                         dependencies = c("Depends", "Imports", "LinkingTo"),
                         max_dist = Inf,
                         direction = c("out", "in", "all")) {
  direction <- match.arg(direction)
  fields <- match.arg(dependencies,
                      c("Depends", "Imports", "LinkingTo",
                        "Suggests", "Enhances"),
                      several.ok = TRUE)
  everything <- is.null(pkg) || (length(pkg) == 1L && pkg == "all")
  snet_progress_step("Downloading data about available packages from CRAN")
  db <- .cran_db()
  ties <- .parse_cran_deps(db, fields)
  nodes <- .cran_nodes(db, ties)
  out <- as_tidygraph(list(nodes = nodes, ties = ties))
  if (!everything) {
    unknown <- setdiff(pkg, nodes$name)
    if (length(unknown) > 0)
      snet_abort("{.val {unknown}} could not be found on CRAN.")
    out <- .scope_cran(out, pkg, max_dist, direction)
  }
  observed <- unique(as.character(tie_attribute(out, "type")))
  # Only mark the network as multiplex where more than one kind of tie remains.
  if (length(observed) < 2 && "type" %in% igraph::edge_attr_names(out))
    out <- delete_tie_attribute(out, "type")
  info <- list(out,
               name = if (everything) "CRAN dependency network" else
                 paste("Dependency network of", paste(pkg, collapse = ", ")),
               collection = "CRAN")
  if (length(observed) > 0) info$ties <- observed
  out <- do.call(add_info, info)
  if (everything)
    snet_info("Collected {net_nodes(out)} packages and {net_ties(out)}",
              "dependencies. Consider scoping this network with e.g.",
              "{.fn to_ego}, {.fn to_giant}, {.fn delete_isolates},",
              "or {.fn to_uniplex}.")
  out
}

# Returns the CRAN package database, defaulting the repository where none is
# set, as is the case in non-interactive sessions.
.cran_db <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || length(repos) == 0 ||
        any(repos == "@CRAN@") || !nzchar(repos[[1]])) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  as.data.frame(utils::available.packages(
    utils::contrib.url(repos, type = "source")
  ))
}

# Returns an edgelist of the dependencies declared in the named fields.
# Version constraints are stripped whatever their spacing, so that both
# "Matrix (>= 1.8-0)" and "Matrix(>= 1.8-0)" yield "Matrix".
.parse_cran_deps <- function(db, fields) {
  base_pkgs <- c("R", "base", "compiler", "datasets", "graphics", "grDevices",
                 "grid", "methods", "parallel", "splines", "stats", "stats4",
                 "tcltk", "tools", "translations", "utils")
  out <- lapply(fields, function(fl) {
    v <- db[[fl]]
    if (is.null(v)) return(NULL)
    keep <- !is.na(v) & nzchar(v)
    if (!any(keep)) return(NULL)
    spl <- strsplit(v[keep], ",", fixed = TRUE)
    to <- trimws(sub("[(].*", "", unlist(spl, use.names = FALSE)))
    from <- rep(db$Package[keep], lengths(spl))
    ok <- nzchar(to) & !to %in% base_pkgs & from != to
    data.frame(from = from[ok], to = to[ok], type = fl,
               stringsAsFactors = FALSE)
  })
  out <- unique(do.call(rbind, out))
  out$type <- factor(out$type, levels = fields)
  out
}

# Returns a nodelist of every package in the database, together with any
# dependency targets that are not themselves on CRAN.
.cran_nodes <- function(db, ties) {
  labs <- unique(c(db$Package, ties$from, ties$to))
  idx <- match(labs, db$Package)
  cols <- function(x) {
    if (is.null(db[[x]])) rep(NA_character_, length(idx)) else db[[x]][idx]
  }
  needs <- cols("NeedsCompilation")
  data.frame(name = labs,
             on_cran = !is.na(idx),
             version = cols("Version"),
             published = as.Date(cols("Published")),
             compiled = ifelse(is.na(idx), NA, !is.na(needs) & needs == "yes"),
             priority = cols("Priority"),
             license = cols("License"),
             stringsAsFactors = FALSE)
}

# Scopes the network to the neighbourhoods of the seed packages.
# This touches only the seeds, where to_ego() would materialise the
# neighbourhood of every node in the network.
.scope_cran <- function(.data, seeds, max_dist, direction) {
  order <- if (is.infinite(max_dist)) igraph::vcount(.data) else max_dist
  vs <- unique(unlist(igraph::ego(.data, order = order, nodes = seeds,
                                  mode = direction)))
  as_tidygraph(igraph::induced_subgraph(.data, vs))
}

#' @rdname make_collect
#' @param dir Character string with the path of the directory in which to
#'   look for R scripts.
#'   By default the current working directory.
#'   Where `dir` holds a DESCRIPTION file and an R folder, as a package does,
#'   the R folder is searched.
#' @param external Logical.
#'   Where TRUE, calls to functions that are not defined in `dir`,
#'   such as those from other packages, are included as nodes too.
#'   By default FALSE, since these are numerous and rarely of interest.
#' @source
#'   Inspired by Jakob Gepp's `helfRlein::get_network()`,
#'   https://github.com/STATWORX/helfRlein/blob/master/R/get_network.R
#' @examples
#' \dontrun{
#' # The network of calls among the functions in the working directory:
#' collect_pkg()
#' # Collapsed onto generics, where the directory is a package:
#' # to_blockmodel(collect_pkg(), node_attribute(collect_pkg(), "generic"))
#' }
#' @export
collect_pkg <- function(dir = getwd(), external = FALSE) {
  dir <- .pkg_resolve_dir(dir)
  files <- list.files(dir, pattern = "[.][Rr]$",
                      recursive = TRUE, full.names = TRUE)
  if (length(files) == 0)
    snet_abort("No R scripts were found in {.path {dir}}.")
  snet_progress_step("Parsing {length(files)} R scripts")
  parsed <- lapply(files, .pkg_parse_file)
  failed <- vapply(parsed, is.null, logical(1))
  if (any(failed))
    snet_warn("{.path {basename(files[failed])}} could not be parsed.")
  parsed <- parsed[!failed]
  if (length(parsed) == 0)
    snet_abort("None of the R scripts in {.path {dir}} could be parsed.")
  defs <- do.call(rbind, lapply(parsed, function(x) x$defs))
  calls <- do.call(rbind, lapply(parsed, function(x) x$calls))
  if (is.null(defs) || nrow(defs) == 0)
    snet_abort("No function definitions were found in {.path {dir}}.")
  dups <- duplicated(defs$name)
  if (any(dups))
    snet_minor_info("Merging {sum(dups)} function{?s} defined more than once")
  defs <- defs[!dups, ]
  nodes <- .pkg_nodes(defs, .pkg_exports(dir))
  ties <- .pkg_ties(calls, nodes, external)
  if (external) {
    extra <- setdiff(unique(ties$to), nodes$name)
    if (length(extra) > 0)
      nodes <- rbind(nodes, data.frame(name = extra, file = NA_character_,
                                       lines = NA_integer_,
                                       exported = NA, generic = extra))
    nodes$internal <- !nodes$name %in% extra
  }
  ties <- ties[ties$from %in% nodes$name & ties$to %in% nodes$name, ]
  out <- as_tidygraph(list(nodes = nodes, ties = ties))
  add_info(out, name = paste("Function network of", basename(dirname(dir))),
           collection = "Parsed")
}

# Resolves dir to the folder that holds the R scripts.
.pkg_resolve_dir <- function(dir) {
  if (length(dir) != 1)
    snet_abort("Please provide a single directory.")
  if (!dir.exists(dir))
    snet_abort("{.path {dir}} does not exist.")
  if (file.exists(file.path(dir, "DESCRIPTION")) &&
        dir.exists(file.path(dir, "R"))) {
    file.path(dir, "R")
  } else {
    dir
  }
}

# Extracts the function definitions and the calls within them from one script,
# using R's own parser so that neither comments nor strings are counted and
# names are matched exactly rather than as substrings.
# Returns NULL where the script cannot be parsed.
.pkg_parse_file <- function(path) {
  pd <- tryCatch(utils::getParseData(parse(path, keep.source = TRUE)),
                 error = function(e) NULL)
  if (is.null(pd) || nrow(pd) == 0) return(NULL)
  # The parser numbers rows bottom up, so reorder to get children in source
  # order before splitting them by their parent.
  pd <- pd[order(pd$line1, pd$col1, -pd$line2, -pd$col2), ]
  row_of <- seq_len(nrow(pd))
  names(row_of) <- as.character(pd$id)
  kids <- split(pd$id, pd$parent)
  defs <- .pkg_defs(pd, row_of, kids, path)
  calls <- .pkg_calls(pd, row_of, kids, defs)
  list(defs = defs[, c("name", "file", "lines")], calls = calls)
}

# Identifies assignments whose value is a function, covering `<-`, `<<-`, `=`,
# lambdas, and definitions whose `function` keyword falls on a later line.
.pkg_defs <- function(pd, row_of, kids, path) {
  assigns <- which(pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))
  found <- lapply(assigns, function(i) {
    sibs <- kids[[as.character(pd$parent[i])]]
    if (length(sibs) != 3 || sibs[2] != pd$id[i]) return(NULL)
    rhs <- row_of[as.character(sibs[3])]
    if (is.na(rhs)) return(NULL)
    grandkids <- kids[[as.character(sibs[3])]]
    if (length(grandkids) == 0) return(NULL)
    first <- row_of[as.character(grandkids[1])]
    if (is.na(first)) return(NULL)
    # The lambda token is named "\\", so match on its text rather than token.
    if (!(pd$token[first] == "FUNCTION" || pd$text[first] == "\\")) return(NULL)
    nm <- .pkg_symbol(pd, row_of, kids, sibs[1])
    if (is.na(nm)) return(NULL)
    data.frame(name = nm, id = sibs[3], file = path,
               lines = pd$line2[rhs] - pd$line1[rhs] + 1,
               stringsAsFactors = FALSE)
  })
  found <- do.call(rbind, found)
  if (is.null(found)) found <- data.frame(name = character(0), id = numeric(0),
                                          file = character(0),
                                          lines = integer(0))
  found
}

# Resolves the left hand side of an assignment to a single name, stripping the
# backticks or quotes that non-syntactic names such as `print.mnet` arrive with.
.pkg_symbol <- function(pd, row_of, kids, id) {
  i <- row_of[as.character(id)]
  if (is.na(i)) return(NA_character_)
  if (!pd$token[i] %in% c("SYMBOL", "STR_CONST")) {
    inner <- kids[[as.character(id)]]
    if (length(inner) != 1) return(NA_character_)
    i <- row_of[as.character(inner)]
    if (is.na(i) || !pd$token[i] %in% c("SYMBOL", "STR_CONST"))
      return(NA_character_)
  }
  gsub("^[`'\"]+|[`'\"]+$", "", pd$text[i])
}

# Attributes each call to the innermost function definition enclosing it,
# by walking up the parse tree. Calls that reach the top level are dropped.
.pkg_calls <- function(pd, row_of, kids, defs) {
  sites <- which(pd$token == "SYMBOL_FUNCTION_CALL")
  if (length(sites) == 0 || nrow(defs) == 0)
    return(data.frame(from = character(0), to = character(0)))
  def_name <- defs$name
  names(def_name) <- as.character(defs$id)
  found <- lapply(sites, function(i) {
    to <- .pkg_callee(pd, row_of, kids, i)
    p <- pd$parent[i]
    while (!is.na(p) && p > 0) {
      key <- as.character(p)
      if (key %in% names(def_name))
        return(data.frame(from = unname(def_name[key]), to = to,
                          stringsAsFactors = FALSE))
      p <- unname(pd$parent[row_of[key]])
    }
    NULL
  })
  found <- do.call(rbind, found)
  if (is.null(found)) found <- data.frame(from = character(0),
                                          to = character(0))
  found
}

# Qualifies a call with its package where it was made with :: or :::,
# so that e.g. igraph::V() is not confused with a locally defined V().
.pkg_callee <- function(pd, row_of, kids, i) {
  sibs <- kids[[as.character(pd$parent[i])]]
  pos <- match(pd$id[i], sibs)
  if (!is.na(pos) && pos > 2) {
    op <- row_of[as.character(sibs[pos - 1])]
    ns <- row_of[as.character(sibs[pos - 2])]
    if (!is.na(op) && !is.na(ns) &&
          pd$token[op] %in% c("NS_GET", "NS_GET_INT") &&
          pd$token[ns] == "SYMBOL_PACKAGE")
      return(paste0(pd$text[ns], "::", pd$text[i]))
  }
  pd$text[i]
}

# Reads the export and S3 method registrations from a package's NAMESPACE,
# which is authoritative where splitting a name on its first dot is not.
.pkg_exports <- function(dir) {
  path <- file.path(dirname(dir), "NAMESPACE")
  if (!file.exists(path)) path <- file.path(dir, "NAMESPACE")
  if (!file.exists(path)) return(NULL)
  ns <- tryCatch(parse(path), error = function(e) NULL)
  if (is.null(ns)) return(NULL)
  txt <- function(x) {
    if (is.character(x)) x else paste(deparse(x), collapse = "")
  }
  exports <- character(0)
  methods <- data.frame(generic = character(0), method = character(0))
  for (e in ns) {
    if (!is.call(e)) next
    directive <- as.character(e[[1]])
    args <- as.list(e)[-1]
    if (directive == "export" && length(args) > 0) {
      exports <- c(exports, vapply(args, txt, character(1)))
    } else if (directive == "S3method" && length(args) >= 2) {
      generic <- txt(args[[1]])
      method <- if (length(args) >= 3) txt(args[[3]]) else
        paste0(generic, ".", txt(args[[2]]))
      methods <- rbind(methods, data.frame(generic = generic, method = method,
                                           stringsAsFactors = FALSE))
    }
  }
  list(exports = unique(exports), methods = unique(methods))
}

# Assembles the nodelist, recording where each function is defined, how long
# it is, whether it is exported, and which generic it is a method for.
.pkg_nodes <- function(defs, ns) {
  generic <- defs$name
  exported <- rep(NA, nrow(defs))
  if (!is.null(ns)) {
    exported <- defs$name %in% ns$exports | defs$name %in% ns$methods$method
    hit <- match(defs$name, ns$methods$method)
    generic[!is.na(hit)] <- ns$methods$generic[hit[!is.na(hit)]]
  }
  data.frame(name = defs$name, file = basename(defs$file), lines = defs$lines,
             exported = exported, generic = generic, stringsAsFactors = FALSE)
}

# Assembles the tielist, weighting each tie by the number of call sites and
# adding a tie from each generic to its methods where both are defined here.
.pkg_ties <- function(calls, nodes, external) {
  if (is.null(calls) || nrow(calls) == 0)
    calls <- data.frame(from = character(0), to = character(0))
  if (!external) calls <- calls[calls$to %in% nodes$name, ]
  ties <- data.frame(from = character(0), to = character(0),
                     weight = integer(0), type = character(0))
  if (nrow(calls) > 0) {
    tab <- table(paste(calls$from, calls$to, sep = "\r"))
    parts <- do.call(rbind, strsplit(names(tab), "\r", fixed = TRUE))
    ties <- data.frame(from = parts[, 1], to = parts[, 2],
                       weight = as.integer(tab), type = "call",
                       stringsAsFactors = FALSE)
  }
  dispatch <- nodes[nodes$generic != nodes$name &
                      nodes$generic %in% nodes$name, ]
  if (nrow(dispatch) > 0)
    ties <- rbind(ties, data.frame(from = dispatch$generic, to = dispatch$name,
                                   weight = 1L, type = "dispatch",
                                   stringsAsFactors = FALSE))
  # Only mark the network as multiplex where both kinds of tie are present.
  if (length(unique(ties$type)) < 2) ties$type <- NULL
  ties
}

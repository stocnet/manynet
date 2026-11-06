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
#'   Each node of the network that `collect_cran()` returns has
#'   the attributes `on_cran`, `version`, `published`, `compiled`,
#'   `priority`, `license`, and `min_r`.
#'   `min_r` holds the minimum version of R that the package declares
#'   in its `Depends` field, or `NA` where it declares none.
#'   This version is a character string,
#'   so use `package_version()` to compare versions.
#'   For example, the highest `min_r` in the network that
#'   `collect_cran("manynet")` returns is the minimum version of R that
#'   {manynet} needs, together with all the packages that it depends upon.
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
#' # The minimum version of R that {manynet} and its dependencies need:
#' min_r <- node_attribute(collect_cran("manynet"), "min_r")
#' max(package_version(min_r[!is.na(min_r)]))
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
             min_r = .min_r(cols("Depends")),
             stringsAsFactors = FALSE)
}

# Returns the minimum version of R that each package declares in its
# `Depends` field, or NA where it declares none.
# The versions are kept as character strings, so compare them with
# `package_version()` and not as text, where "4.10" would sort before "4.9".
.min_r <- function(depends) {
  rx <- "(^|,)\\s*R\\s*\\(\\s*>=?\\s*([0-9][0-9.-]*)\\s*\\)"
  m <- regmatches(depends, regexec(rx, depends, perl = TRUE))
  vapply(m, function(x) if (length(x) == 3) x[3] else NA_character_,
         character(1))
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

# Emails ####

#' Making networks from email metadata
#'
#' @description
#' `collect_emails()` creates a network of who writes to whom from the
#' headers of a mailbox that has been exported to file.
#' It reads the `From`, `To`, `Cc`, `Bcc`, `Date`, and `Message-ID` fields of
#' each message, and creates a network in which the nodes are email addresses
#' and each tie runs from the sender of a message to one of its recipients.
#' Only the headers are read: neither the subject nor the body of any message
#' is collected.
#' @details
#'   Most email clients and services can export a mailbox to file.
#'   Google Takeout, Thunderbird (via "ImportExportTools NG"),
#'   and Apple Mail ("Mailbox > Export Mailbox...") write the 'mbox' format,
#'   in which the messages follow one another in a single file.
#'   Outlook and others can save messages as individual '.eml' files instead.
#'   `collect_emails()` reads either, as well as 'Maildir' folders,
#'   and needs neither a password nor a connection to the mail server.
#'
#'   Emails are events, and so the network returned is dynamic.
#'   By default each tie is stamped with the time its message was sent,
#'   and several messages between the same two addresses are several ties.
#'   Use [to_time()] or [to_aggregated()] to scope or aggregate them.
#'   Where `twomode = TRUE` the time belongs to the message rather than to
#'   any one of its ties, and so each message instead enters the network at
#'   the time it was sent.
#'
#'   An address is counted once for each message, as its sender if it sent it,
#'   and otherwise under the first of `To`, `Cc`, and `Bcc` that names it.
#'   Messages repeated in the export, as where a message carries several
#'   labels, are collected once, and messages without both a sender and at
#'   least one other recipient are dropped.
#' @section Ego:
#'   A mailbox holds the messages that its owner sent or received,
#'   and so its boundary is that of an ego network:
#'   two alters are only tied where ego was party to the message.
#'   It is not an egocentric network in the sense of [is_egocentric()], though,
#'   since the ties are records of events rather than ego's reports of them.
#'
#'   Not every export is one person's mailbox.
#'   By default, the address party to the most messages is taken to be ego
#'   where it is party to at least half of them, and the network is otherwise
#'   taken to have no ego, as in a corpus of several mailboxes.
#'   Note that the address of a mailing list will be found this way too.
#'   Name ego's address(es) in `ego` to override this, or use `ego = FALSE`.
#' @param file A character string with the path to an 'mbox' file,
#'   an '.eml' file, or a directory holding any number of either,
#'   including 'Maildir' folders.
#'   If left unspecified, an OS-specific file picker is opened to help users
#'   select a file.
#'   Note that a file picker cannot select a directory,
#'   so the path to a directory must be given.
#' @param ego A character vector of the email address(es) of the owner of
#'   the mailbox.
#'   By default NULL, in which case ego is inferred from the messages.
#'   Use FALSE where the messages are not from one mailbox.
#' @param twomode Logical.
#'   If TRUE, a two-mode network of addresses and the messages they sent or
#'   received is returned, in which the ties are layered by the role each
#'   address had in each message.
#'   By default FALSE, which returns a directed one-mode network of addresses.
#' @return A `stocnet` object.
#'   Nodes hold the address as their label, the name most often displayed
#'   alongside it, and whether the address is ego's.
#' @family makes
#' @seealso [collect_ego()], [to_time()], [to_aggregated()], [to_unlabelled()]
#' @source
#'   Inspired by Nathan Yau's "Downloading Your Email Metadata",
#'   https://flowingdata.com/2014/05/07/downloading-your-email-metadata/
#' @examples
#' mbox <- tempfile(fileext = ".mbox")
#' writeLines(c("From - Mon Jan  6 09:15:00 2025",
#'              "Message-ID: <1@example.org>",
#'              "Date: Mon, 6 Jan 2025 09:15:00 +0100",
#'              "From: Ada <ada@example.org>",
#'              "To: Ben <ben@example.org>, cy@example.org",
#'              "",
#'              "Shall we meet?",
#'              "",
#'              "From - Mon Jan  6 10:02:00 2025",
#'              "Message-ID: <2@example.org>",
#'              "Date: Mon, 6 Jan 2025 10:02:00 +0100",
#'              "From: Ben <ben@example.org>",
#'              "To: Ada <ada@example.org>",
#'              "Cc: cy@example.org",
#'              "",
#'              "Yes, let's."), mbox)
#' collect_emails(mbox)
#' collect_emails(mbox, twomode = TRUE)
#' @name make_emails
#' @export
collect_emails <- function(file = file.choose(), ego = NULL,
                           twomode = FALSE) {
  if (missing(file)) snet_success("Executing: collect_emails('{file}')")
  if (!is.character(file) || length(file) != 1)
    snet_abort("Please provide a single path.")
  if (!file.exists(file))
    snet_abort("{.path {file}} does not exist.")
  snet_progress_step("Reading email headers")
  lines <- .email_headers(file)
  if (length(lines$line) == 0)
    snet_abort("No emails were found in {.path {file}}.")
  snet_progress_step("Parsing email headers")
  fields <- .email_fields(lines)
  msgs <- .email_messages(fields)
  dups <- !is.na(msgs$id) & duplicated(msgs$id)
  if (any(dups))
    snet_minor_info("Dropping {sum(dups)} email{?s} collected more than once.")
  msgs <- msgs[!dups, ]
  blank <- is.na(msgs$id)
  msgs$id[blank] <- make.unique(c(msgs$id[!blank],
                                  paste0("message", which(blank))))[
                                    sum(!blank) + seq_len(sum(blank))]
  parties <- .email_parties(fields, msgs)
  # A message ties its sender to each other address it names, so it needs
  # both a sender and at least one other party.
  roles <- split(parties$role, factor(parties$msg, levels = msgs$msg))
  whole <- vapply(roles, function(r) "from" %in% r && any(r != "from"),
                  logical(1))
  if (!all(whole))
    snet_minor_info("Dropping {sum(!whole)} email{?s} without both a sender",
                    "and a recipient.")
  msgs <- msgs[whole, ]
  parties <- parties[parties$msg %in% msgs$msg, ]
  if (nrow(msgs) == 0)
    snet_abort("No emails with both a sender and a recipient were found",
               "in {.path {file}}.")
  owner <- .email_ego(parties, fields, msgs, ego)
  nodes <- .email_nodes(parties, owner)
  info <- list(name = "Email network", source = "Empirical",
               method = "Archival", observation = "event")
  if (length(owner) > 0) info$boundary <- "ego"
  parties <- parties[parties$role != "delivered", ]
  parties$id <- msgs$id[match(parties$msg, msgs$msg)]
  parties$time <- msgs$time[match(parties$msg, msgs$msg)]
  if (twomode) .email_twomode(info, nodes, parties, msgs) else
    .email_onemode(info, nodes, parties)
}

# The header fields that are collected. Those that say where a message was
# delivered are only used to establish whose mailbox this is.
.email_wanted <- c("from", "to", "cc", "bcc", "date", "message-id",
                   "delivered-to", "x-original-to")

# Returns the header lines of the wanted fields for every message found at
# the path, together with an index of the message each line belongs to.
.email_headers <- function(path) {
  if (dir.exists(path)) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE)
    files <- files[!dir.exists(files)]
    maildir <- basename(dirname(files)) %in% c("cur", "new")
    eml <- grepl("[.]eml$", files, ignore.case = TRUE)
    mbox <- grepl("[.](mbox|mbx)$", files, ignore.case = TRUE) |
      basename(files) == "mbox"
    files <- files[maildir | eml | mbox]
    single <- (maildir | eml)[maildir | eml | mbox]
  } else {
    files <- path
    single <- grepl("[.]eml$", path, ignore.case = TRUE) ||
      !.email_is_mbox(path)
  }
  out <- list(line = character(0), msg = integer(0))
  for (i in seq_along(files)) {
    found <- if (single[i]) .email_read_eml(files[i]) else
      .email_read_mbox(files[i])
    out$line <- c(out$line, found$line)
    out$msg <- c(out$msg, found$msg + max(0L, out$msg))
  }
  out
}

# An mbox file separates its messages by lines such as
# "From ada@example.org Mon Jan  6 09:15:00 2025".
.email_from_line <- "^From \\S+ +(Mon|Tue|Wed|Thu|Fri|Sat|Sun)[a-z]* "

.email_is_mbox <- function(path) {
  first <- readLines(path, n = 1L, warn = FALSE, skipNul = TRUE)
  length(first) == 1 && grepl(.email_from_line, first, useBytes = TRUE)
}

# Keeps, from the lines of a header, those of the wanted fields together with
# the lines that continue them. `open` says whether the header line before
# these was itself kept, where the header continues from an earlier chunk.
.email_keep <- function(x, open = FALSE) {
  if (length(x) == 0) return(logical(0))
  folded <- grepl("^[ \t]", x, useBytes = TRUE)
  wanted <- !folded & tolower(sub(":.*", "", x, useBytes = TRUE)) %in%
    .email_wanted
  last <- cummax(ifelse(folded, 0L, seq_along(x)))
  ifelse(last == 0L, open, wanted[pmax(last, 1L)])
}

# Reads an mbox file a chunk of lines at a time, so that a mailbox of several
# gigabytes is never held in memory, keeping only the header lines wanted.
# The body of a message can hold anything, and so the file is read as bytes.
.email_read_mbox <- function(path, chunk = 50000L) {
  con <- file(path, open = "rb")
  on.exit(close(con))
  lines <- list()
  msgs <- list()
  count <- 0L          # messages begun so far
  in_header <- FALSE   # whether the last line read was in a header
  after_blank <- TRUE  # whether the last line read was blank
  open <- FALSE        # whether the last header line read was kept
  repeat {
    x <- readLines(con, n = chunk, warn = FALSE, skipNul = TRUE)
    if (length(x) == 0) break
    x <- sub("\r$", "", x, useBytes = TRUE)
    blank <- x == ""
    begins <- c(after_blank, blank[-length(x)]) &
      grepl(.email_from_line, x, useBytes = TRUE)
    msg <- cumsum(begins)
    # A header runs from the line that begins a message to its first blank.
    blanks <- cumsum(blank)
    since <- blanks - c(0L, blanks[begins])[msg + 1L]
    header <- since == 0L & !begins & (msg > 0L | in_header)
    kept <- header
    kept[header] <- .email_keep(x[header], open)
    lines[[length(lines) + 1L]] <- x[kept]
    msgs[[length(msgs) + 1L]] <- msg[kept] + count
    count <- count + msg[length(x)]
    in_header <- (since == 0L & (msg > 0L | in_header))[length(x)]
    after_blank <- blank[length(x)]
    if (any(header)) open <- kept[max(which(header))]
    if (!in_header) open <- FALSE
  }
  list(line = as.character(unlist(lines, use.names = FALSE)),
       msg = as.integer(unlist(msgs, use.names = FALSE)))
}

# Reads the header of a file that holds a single message.
.email_read_eml <- function(path, chunk = 500L) {
  con <- file(path, open = "rb")
  on.exit(close(con))
  lines <- character(0)
  repeat {
    x <- readLines(con, n = chunk, warn = FALSE, skipNul = TRUE)
    if (length(x) == 0) break
    x <- sub("\r$", "", x, useBytes = TRUE)
    blank <- which(x == "")
    if (length(blank) > 0) {
      lines <- c(lines, x[seq_len(blank[1] - 1L)])
      break
    }
    lines <- c(lines, x)
  }
  lines <- lines[.email_keep(lines)]
  list(line = lines, msg = rep(1L, length(lines)))
}

# Unfolds the header lines into one row for each field of each message.
.email_fields <- function(lines) {
  x <- .email_utf8(lines$line)
  folded <- grepl("^[ \t]", x)
  field <- cumsum(!folded)
  keep <- field > 0
  value <- vapply(split(trimws(x[keep]), field[keep]), paste, character(1),
                  collapse = " ", USE.NAMES = FALSE)
  data.frame(msg = lines$msg[!folded],
             field = tolower(sub(":.*", "", value)),
             value = trimws(sub("^[^:]*:", "", value)),
             stringsAsFactors = FALSE)
}

# Headers are ASCII by standard, but in practice some hold raw UTF-8 or
# Latin-1, and a string that is neither valid nor marked cannot be matched.
.email_utf8 <- function(x) {
  valid <- validUTF8(x)
  x[!valid] <- iconv(x[!valid], "latin1", "UTF-8", sub = "?")
  Encoding(x) <- "UTF-8"
  x
}

# Returns one row for each message, with its identifier and when it was sent.
.email_messages <- function(fields) {
  msg <- unique(fields$msg)
  first <- function(name) {
    hit <- fields[fields$field == name, ]
    hit$value[match(msg, hit$msg)]
  }
  id <- gsub("^<|>$", "", trimws(first("message-id")))
  id[!is.na(id) & !nzchar(id)] <- NA_character_
  data.frame(msg = msg, id = id, time = .email_date(first("date")),
             stringsAsFactors = FALSE)
}

# Parses dates such as "Mon, 6 Jan 2025 09:15:00 +0100 (CET)" into UTC.
# Months are matched by name rather than with "%b", which follows the locale.
.email_date <- function(x) {
  pattern <- paste0("([0-9]{1,2})\\s+([A-Za-z]{3})[a-z]*\\s+([0-9]{2,4})\\s+",
                    "([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?",
                    "\\s*([+-][0-9]{4}|[A-Za-z]+)?")
  x[is.na(x)] <- ""
  parts <- regmatches(x, regexec(pattern, x, perl = TRUE))
  parts <- do.call(rbind, lapply(parts, function(p) {
    if (length(p) == 8) p else rep(NA_character_, 8)
  }))
  if (is.null(parts)) return(as.POSIXct(character(0), tz = "UTC"))
  year <- as.integer(parts[, 4])
  year <- year + ifelse(year < 50, 2000L, ifelse(year < 100, 1900L, 0L))
  seconds <- ifelse(is.na(parts[, 7]) | !nzchar(parts[, 7]), "0", parts[, 7])
  local <- ISOdatetime(year, match(tolower(parts[, 3]), tolower(month.abb)),
                       as.integer(parts[, 2]), as.integer(parts[, 5]),
                       as.integer(parts[, 6]), as.integer(seconds),
                       tz = "UTC")
  local - .email_offset(parts[, 8])
}

# The seconds by which a time zone is ahead of UTC. Zones may be numeric, or
# one of the names RFC 5322 still allows; any other name is read as UTC.
.email_offset <- function(zone) {
  named <- c(EST = -5, EDT = -4, CST = -6, CDT = -5,
             MST = -7, MDT = -6, PST = -8, PDT = -7) * 3600
  out <- unname(named[toupper(zone)])
  numeric <- grepl("^[+-][0-9]{4}$", zone)
  out[numeric] <- ifelse(substr(zone[numeric], 1, 1) == "-", -1, 1) *
    (as.integer(substr(zone[numeric], 2, 3)) * 3600 +
       as.integer(substr(zone[numeric], 4, 5)) * 60)
  out[is.na(out)] <- 0
  out
}

# Returns one row for each address in each message, with the role it has in
# that message. An address named more than once in a message is kept under
# the first of the roles in which it appears.
.email_parties <- function(fields, msgs) {
  roles <- c(from = "from", to = "to", cc = "cc", bcc = "bcc",
             "delivered-to" = "delivered", "x-original-to" = "delivered")
  held <- fields[fields$field %in% names(roles) & fields$msg %in% msgs$msg, ]
  found <- .email_addresses(held$value)
  out <- data.frame(msg = rep(held$msg, lengths(found$address)),
                    role = rep(unname(roles[held$field]),
                               lengths(found$address)),
                    address = as.character(unlist(found$address,
                                                  use.names = FALSE)),
                    display = as.character(unlist(found$display,
                                                  use.names = FALSE)),
                    stringsAsFactors = FALSE)
  out$role <- factor(out$role, levels = c("from", "to", "cc", "bcc",
                                          "delivered"))
  out <- out[order(out$msg, out$role), ]
  out <- out[!duplicated(out[, c("msg", "address")]), ]
  out$role <- as.character(out$role)
  rownames(out) <- NULL
  out
}

# Splits each field into its addresses and the names displayed with them.
# A quoted name may hold any of the characters that otherwise structure the
# field, as in `"Doe, Jane" <jane@example.org>`, and so these are masked
# while the field is split. Names are only decoded once split, since an
# encoded name may decode to those characters too.
.email_addresses <- function(x) {
  marks <- ",;:<>@"
  masks <- "\001\002\003\004\005\006"
  quoted <- gregexpr("\"(?:[^\"\\\\]|\\\\.)*\"", x, perl = TRUE)
  regmatches(x, quoted) <- lapply(regmatches(x, quoted), chartr,
                                  old = marks, new = masks)
  pieces <- strsplit(x, "[,;]")
  n <- lengths(pieces)
  piece <- as.character(unlist(pieces, use.names = FALSE))
  # A group, such as "Team: ada@example.org, ben@example.org;", is named
  # before a colon, and its name is not that of any one address.
  piece <- trimws(sub("^[^<]*:", "", piece))
  angled <- grepl("<[^<>]*>", piece)
  address <- ifelse(angled, sub(".*<([^<>]*)>.*", "\\1", piece), piece)
  address <- tolower(gsub("^'+|'+$", "", trimws(address)))
  display <- ifelse(angled, trimws(sub("<[^<>]*>.*", "", piece)), "")
  display <- gsub("\\\\(.)", "\\1", gsub("^\"|\"$", "", display))
  display <- .email_decode(chartr(masks, marks, display))
  ok <- grepl("^[^[:space:]<>()\",;:@]+@[^[:space:]<>()\",;:@]+$", address)
  group <- factor(rep(seq_along(x), n), levels = seq_along(x))
  list(address = unname(split(address[ok], group[ok])),
       display = unname(split(display[ok], group[ok])))
}

# Decodes names written as RFC 2047 encoded words, such as
# "=?UTF-8?B?SsO8cmdlbg==?=", which is how a header holds non-ASCII text.
.email_decode <- function(x) {
  coded <- grepl("=?", x, fixed = TRUE)
  if (!any(coded)) return(x)
  todo <- unique(x[coded])
  # White space between two encoded words is not part of the text.
  done <- gsub("(\\?=)\\s+(=\\?)", "\\1\\2", todo)
  words <- gregexpr("=\\?[^?[:space:]]+\\?[bBqQ]\\?[^?[:space:]]*\\?=", done)
  regmatches(done, words) <- lapply(regmatches(done, words), function(w) {
    vapply(w, .email_decode_word, character(1), USE.NAMES = FALSE)
  })
  x[coded] <- done[match(x[coded], todo)]
  x
}

# Decodes one encoded word, returning it as it was where it cannot be decoded.
.email_decode_word <- function(word) {
  parts <- strsplit(word, "?", fixed = TRUE)[[1]]
  charset <- sub("[*].*", "", parts[2])
  out <- tryCatch({
    bytes <- if (toupper(parts[3]) == "B") .email_base64(parts[4]) else
      .email_quoted(parts[4])
    iconv(rawToChar(bytes), from = charset, to = "UTF-8")
  }, error = function(e) NA_character_)
  if (is.na(out)) word else out
}

# Decodes base64 to bytes: each character holds six bits, and so each four
# characters hold three bytes.
.email_base64 <- function(x) {
  alphabet <- c(LETTERS, letters, 0:9, "+", "/")
  six <- match(strsplit(gsub("[^A-Za-z0-9+/]", "", x), "")[[1]], alphabet) - 1L
  n <- length(six)
  if (n == 0) return(raw(0))
  six <- matrix(c(six, rep(0L, -n %% 4)), nrow = 4)
  bytes <- rbind(six[1, ] * 4L + six[2, ] %/% 16L,
                 (six[2, ] %% 16L) * 16L + six[3, ] %/% 4L,
                 (six[3, ] %% 4L) * 64L + six[4, ])
  as.raw(bytes)[seq_len((n * 3L) %/% 4L)]
}

# Decodes the "Q" encoding to bytes: an underscore is a space, and "=E9" is
# the byte E9.
.email_quoted <- function(x) {
  x <- gsub("_", " ", x, fixed = TRUE)
  bytes <- charToRaw(x)
  at <- gregexpr("=[0-9A-Fa-f]{2}", x)[[1]]
  if (at[1] < 0) return(bytes)
  bytes[at] <- as.raw(strtoi(substring(x, at + 1L, at + 2L), 16L))
  bytes[-c(at + 1L, at + 2L)]
}

# Establishes whose mailbox the messages are from. Unless ego is named,
# ego is the address party to the most messages, where that is at least half
# of them. An address a message was delivered to is party to it, even where
# no header names it, as with a blind copy or a mailing list.
.email_ego <- function(parties, fields, msgs, ego) {
  if (isFALSE(ego)) return(character(0))
  if (is.character(ego)) {
    ego <- tolower(trimws(ego))
    unknown <- setdiff(ego, parties$address)
    if (length(unknown) > 0)
      snet_warn("{.val {unknown}} {?is/are} not party to any email collected.")
    return(ego)
  }
  if (!is.null(ego) && !isTRUE(ego))
    snet_abort("{.arg ego} should be NULL, FALSE, or the address(es) of ego.")
  tab <- sort(table(parties$address), decreasing = TRUE)
  share <- unname(tab[1]) / nrow(msgs)
  owner <- names(tab)[1]
  if (share < 0.5) {
    snet_info("No address is party to at least half of the emails,",
              "so no ego is recorded. Name one with {.arg ego} if there is.")
    return(character(0))
  }
  snet_info("Taking {.val {owner}} as ego, as party to",
            "{round(share * 100)}% of the emails.",
            "Use {.arg ego} to name ego, or {.code ego = FALSE} for none.")
  owner
}

# Assembles the nodelist of addresses, in order of appearance, each with the
# name most often displayed alongside it.
.email_nodes <- function(parties, owner) {
  tied <- parties[parties$role != "delivered", ]
  label <- unique(tied$address)
  named <- tied[nzchar(tied$display), ]
  display <- rep(NA_character_, length(label))
  if (nrow(named) > 0) {
    tab <- sort(table(paste(named$address, named$display, sep = "\r")),
                decreasing = TRUE)
    key <- do.call(rbind, strsplit(names(tab), "\r", fixed = TRUE))
    display <- key[match(label, key[, 1]), 2]
  }
  data.frame(label = label, display = display, ego = label %in% owner,
             stringsAsFactors = FALSE)
}

# A one-mode network ties the sender of each message to its other parties.
# Each tie is an event, stamped with when the message was sent, and so adds
# to rather than replaces the ties recorded before it.
.email_onemode <- function(info, nodes, parties) {
  senders <- parties[parties$role == "from", ]
  others <- parties[parties$role != "from", ]
  ties <- data.frame(from = senders$address[match(others$msg, senders$msg)],
                     to = others$address, layer = others$role,
                     time = others$time, message = others$id,
                     stringsAsFactors = FALSE)
  # Only mark the network as multiplex where more than one kind of tie remains.
  if (length(unique(ties$layer)) < 2) ties$layer <- NULL
  info$directed <- TRUE
  info$update <- "increment"
  make_stocnet(info = info, nodes = nodes, ties = ties)
}

# A two-mode network ties each address to the messages it is party to.
# Every tie to a message would carry the same time, so the time is instead
# recorded as when the message becomes active. Where it is not known when a
# message was sent, the message is active throughout.
.email_twomode <- function(info, nodes, parties, msgs) {
  dated <- !is.na(msgs$time)
  nodes$mode <- "addresses"
  nodes$active <- TRUE
  nodes <- rbind(nodes[, c("label", "mode", "display", "ego", "active")],
                 data.frame(label = msgs$id, mode = "messages",
                            display = NA_character_, ego = FALSE,
                            active = !dated, stringsAsFactors = FALSE))
  ties <- data.frame(from = parties$address, to = parties$id,
                     layer = parties$role, stringsAsFactors = FALSE)
  changes <- NULL
  if (any(dated))
    changes <- data.frame(time = msgs$time[dated], node = msgs$id[dated],
                          var = "active", value = TRUE,
                          stringsAsFactors = FALSE)
  info$directed <- FALSE
  info$sender <- "addresses"
  info$receiver <- "messages"
  make_stocnet(info = info, nodes = nodes, ties = ties, changes = changes)
}

# Text ####

#' Making networks from text
#'
#' @description
#' `collect_text()` creates a network from free text,
#' such as speeches, articles, posts, novels, or the lines of a play.
#' It splits the text into units, such as documents or sentences,
#' and ties each unit to the terms it uses, or to the names it mentions.
#' The result is a two-mode network of units and terms,
#' from which a network of terms that co-occur, or of units that share terms,
#' is a projection: see [to_mode()].
#'
#' Where the text records who speaks (and to whom),
#' the result is instead a one-mode network of speakers and what they mention.
#' @details
#'   Only base R is used, so no text-mining package is needed.
#'   Words are lowercased, and a word is a run of letters and digits,
#'   possibly joined by an apostrophe, as in "let's".
#'   So punctuation and hyphens separate words.
#'   For finer control, such as lemmas or only the nouns,
#'   tokenise the text elsewhere and pass the tokens as a data frame
#'   (see `text` and `token`).
#'
#'   Paragraphs are separated by a blank line,
#'   and sentences end with a full stop, a question mark, or an
#'   exclamation mark that is followed by a space.
#'   Abbreviations such as "e.g." therefore end a sentence too.
#'
#'   Where the units are paragraphs, sentences, or windows,
#'   and there are two or more of them,
#'   each unit is stamped with its position in the text,
#'   in a `time` column of the ties, so that [to_time()] and similar can
#'   scope the network to a part of the text.
#'   Documents are not stamped, since a corpus need not be in any order.
#'   Units are positions and not waves or events,
#'   so the network does not say how one record of a tie updates another.
#' @section Terms:
#'   By default, every word is a term.
#'   Terms are then ordered alphabetically.
#'   A dictionary in `terms` keeps only the terms it lists,
#'   in the order it lists them.
#'   It can be a character vector of terms,
#'   or a named list, each element of which gives the aliases of one name,
#'   such as `list(Harry = c("harry", "potter", "the boy who lived"))`.
#'   A term of several words matches where its words follow one another,
#'   and the longest match is taken where matches overlap,
#'   so "harry potter" is one mention of Harry and not two.
#'   The second mode is called "terms",
#'   or "characters" where the dictionary is a named list.
#'
#'   Stopwords are dropped from the terms and are not scored for sentiment,
#'   but a term of several words is still matched across them,
#'   so "bank of england" is found where "of" is a stopword.
#' @section Speakers:
#'   A text with speakers, such as a transcript or a play,
#'   gives a one-mode, directed network.
#'   With `from` only, each tie runs from the speaker to a term they mention.
#'   With both `from` and `to`, each tie runs from the speaker to the
#'   addressee, and the term mentioned is recorded in the 'about' column,
#'   as in gossip (see [is_gossip()]).
#'   A unit without a speaker (or an addressee) gives no ties.
#'   Speakers, addressees, and terms with the same label are one node,
#'   so name the dictionary entries as the speakers are named,
#'   since words are lowercased.
#' @section Sentiment:
#'   A `lexicon` scores each unit by the sum of the values of its words.
#'   Where units are nodes, they hold this score as a 'sentiment' attribute.
#'   Where the text has speakers, the ties from a unit of negative score
#'   are negative, so that the network is signed (positive and negative
#'   mentions, or gossip).
#'   `tidytext::get_sentiments("afinn")` and
#'   `tidytext::get_sentiments("bing")` are suitable lexicons.
#' @param text The text, as one or more documents.
#'   This can be:
#'   - a character vector, each element of which is one document,
#'   named by its names if it has any;
#'   - a data frame with a 'text' column, each row of which is one document,
#'   named by its 'doc_id', 'document', or 'id' column if it has one;
#'   further columns become attributes of the units;
#'   - a data frame with one token in each row,
#'   such as from `tidytext::unnest_tokens()` or `udpipe::udpipe()`,
#'   with a 'doc_id', 'document', or 'id' column naming the document of each.
#'   Paragraph and sentence units need 'paragraph_id' and 'sentence_id'
#'   columns, as `{udpipe}` gives.
#' @param unit The unit that ties to terms.
#'   One of "document" (the default), "paragraph", "sentence",
#'   or an integer for a moving window of that many words.
#'   The first mode is named after it, e.g. "sentences".
#' @param terms The terms to collect.
#'   By default `NULL`, every word.
#'   Otherwise a character vector or a named list of aliases (see Terms).
#' @param stopwords Words to drop.
#'   A character vector, or a data frame with a 'word' column,
#'   such as `tidytext::stop_words`.
#'   By default `NULL`, none.
#' @param from The speaker of each document.
#'   The name of a column of `text`,
#'   or a vector with one value for each document.
#'   The speaker is the 'from' of the ties of each unit in the document.
#'   By default `NULL`, no speakers, which gives a two-mode network.
#' @param to The addressee of each document, given in the same way as `from`.
#'   The addressee is the 'to' of the ties of each unit in the document.
#'   Needs `from`.
#' @param ties How ties are recorded.
#'   Either "counts" (the default), one tie for each unit and term,
#'   with the number of times the term is used in the unit as its weight,
#'   or "tokens", one tie for each use of the term.
#' @param token The column of a token data frame that holds the term,
#'   such as "lemma" for `{udpipe}` output.
#'   By default `NULL`, the 'word' or else the 'token' column.
#'   Filter the rows first to keep only some parts of speech,
#'   e.g. `dplyr::filter(tokens, upos == "NOUN")`.
#' @param lexicon A sentiment dictionary (see Sentiment).
#'   A named numeric vector of values for words,
#'   or a data frame with a 'word' column and either a numeric 'value' column
#'   or a 'sentiment' column of "positive" (+1) and "negative" (-1).
#'   By default `NULL`, no sentiment.
#' @return A `stocnet` object.
#'   A two-mode network of units and terms,
#'   or, with `from`, a one-mode directed network of speakers and terms.
#' @family makes
#' @seealso [to_mode()], [to_normalised()], [to_time()], [as_stocnet()]
#' @examples
#' speeches <- c(first = "Trade builds peace. Peace builds trade.",
#'               second = "War ends trade.\n\nPeace needs work.")
#' collect_text(speeches)
#' collect_text(speeches, unit = "sentence", stopwords = c("needs"))
#' collect_text(speeches, terms = c("peace", "war", "trade")) |>
#'   to_mode("terms")
#' play <- data.frame(speaker = c("Ann", "Bob", "Ann"),
#'                    addressee = c("Bob", "Ann", "Cy"),
#'                    text = c("Have you seen Cy?", "Cy left with Dee.",
#'                             "Dee is lovely, Cy is awful."))
#' cast <- list(Ann = "ann", Bob = "bob", Cy = "cy", Dee = "dee")
#' collect_text(play, terms = cast, from = "speaker")
#' collect_text(play, terms = cast, from = "speaker", to = "addressee",
#'              lexicon = c(lovely = 1, awful = -2))
#' @name make_text
#' @export
collect_text <- function(text, unit = "document", terms = NULL,
                         stopwords = NULL, from = NULL, to = NULL,
                         ties = c("counts", "tokens"), token = NULL,
                         lexicon = NULL) {
  ties <- match.arg(ties)
  if (!is.null(to) && is.null(from))
    snet_abort("{.arg to} needs {.arg from}: name the speaker as well as",
               "the addressee.")
  unit <- .text_unit(unit)
  snet_progress_step("Reading the text")
  docs <- .text_documents(text, token, from, to)
  snet_progress_step("Splitting the text into units")
  split <- .text_units(docs, unit)
  units <- split$units
  words <- split$words
  if (length(words$word) == 0)
    snet_warn("No words were found in {.arg text}.")
  stopwords <- .text_stopwords(stopwords)
  dict <- .text_dictionary(terms)
  snet_progress_step("Matching terms")
  found <- .text_match(words, dict, stopwords)
  lexicon <- .text_lexicon(lexicon)
  if (!is.null(lexicon))
    units$sentiment <- .text_sentiment(words, lexicon, stopwords,
                                       nrow(units))
  info <- list(name = "Text network", source = "Empirical",
               method = "Archival")
  if (is.null(docs$from)) {
    term_mode <- if (is.list(terms)) "characters" else "terms"
    .text_twomode(info, units, found, dict, term_mode, ties)
  } else .text_onemode(info, units, found, dict, ties, signed = !is.null(lexicon))
}

# Returns "document", "paragraph", "sentence", or a window size.
.text_unit <- function(unit) {
  if (is.numeric(unit)) {
    if (length(unit) != 1 || is.na(unit) || unit < 1 || unit != round(unit))
      snet_abort("A window {.arg unit} must be a single positive integer,",
                 "the number of words in each window.")
    return(as.integer(unit))
  }
  if (!is.character(unit) || length(unit) != 1)
    snet_abort("{.arg unit} must be {.val document}, {.val paragraph},",
               "{.val sentence}, or a window size.")
  unit <- sub("s$", "", tolower(unit))
  if (!unit %in% c("document", "paragraph", "sentence"))
    snet_abort("{.arg unit} must be {.val document}, {.val paragraph},",
               "{.val sentence}, or a window size, not {.val {unit}}.")
  unit
}

.text_doc_cols <- c("doc_id", "document", "id")

# Returns a list of the documents: their labels, attributes, speakers and
# addressees, and either their texts or, for a token data frame, their tokens.
.text_documents <- function(text, token, from, to) {
  if (is.character(text)) {
    labels <- names(text)
    if (is.null(labels) || anyNA(labels) || !all(nzchar(labels)))
      labels <- paste0("text", seq_along(text))
    out <- list(label = make.unique(labels), text = unname(text),
                attrs = NULL)
    out$from <- .text_party(from, NULL, length(text), "from")
    out$to <- .text_party(to, NULL, length(text), "to")
    return(out)
  }
  if (!is.data.frame(text))
    snet_abort("{.arg text} must be a character vector or a data frame,",
               "not {.cls {class(text)}}.")
  text <- as.data.frame(text, stringsAsFactors = FALSE)
  id <- intersect(.text_doc_cols, names(text))[1]
  if (is.null(token) && "text" %in% names(text)) {
    labels <- if (is.na(id)) paste0("text", seq_len(nrow(text))) else
      as.character(text[[id]])
    keep <- setdiff(names(text), c("text", id, from, to))
    return(list(label = make.unique(labels),
                text = as.character(text$text),
                attrs = text[keep],
                from = .text_party(from, text, nrow(text), "from"),
                to = .text_party(to, text, nrow(text), "to")))
  }
  if (is.null(token)) {
    token <- intersect(c("word", "token"), names(text))[1]
    if (is.na(token))
      snet_abort("{.arg text} has no {.val text}, {.val word},",
                 "or {.val token} column.",
                 "i" = "Name the column of terms with {.arg token}.")
  } else if (!token %in% names(text))
    snet_abort("{.val {token}} is not a column of {.arg text}.")
  if (is.na(id))
    snet_abort("A data frame of tokens needs a column naming the document",
               "of each token: {.val {(.text_doc_cols)}}.")
  doc <- as.character(text[[id]])
  label <- unique(doc)
  first <- match(label, doc)
  list(label = label,
       tokens = data.frame(doc = match(doc, label),
                           word = tolower(as.character(text[[token]])),
                           paragraph = text$paragraph_id %||% NA,
                           sentence = text$sentence_id %||% NA,
                           stringsAsFactors = FALSE),
       attrs = NULL,
       from = .text_party(from, text, nrow(text), "from")[first],
       to = .text_party(to, text, nrow(text), "to")[first])
}

# Returns the speakers (or addressees) as a character vector, from a column
# name or a vector as long as the text.
.text_party <- function(x, text, n, arg) {
  if (is.null(x)) return(NULL)
  if (!is.null(text) && is.character(x) && length(x) == 1 &&
      x %in% names(text)) return(as.character(text[[x]]))
  if (!is.null(text) && is.character(x) && length(x) == 1 && n != 1)
    snet_abort("{.val {x}} is not a column of {.arg text}.")
  if (length(x) != n)
    snet_abort("{.arg {arg}} must be a column of {.arg text} or a vector",
               "of {n} value{?s}, one for each document.")
  as.character(x)
}

# Words are lowercased runs of letters and digits, joined by apostrophes.
.text_words <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  x[is.na(x)] <- ""
  regmatches(x, gregexpr("[\\p{L}\\p{N}]+(?:['’][\\p{L}\\p{N}]+)*", x,
                         perl = TRUE))
}

.text_split <- function(x, unit) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  parts <- strsplit(x, "\n[[:blank:]]*\n", perl = TRUE)
  if (unit == "sentence")
    parts <- lapply(parts, function(p)
      unlist(strsplit(p, "(?<=[.!?]|[.!?][\"'’”)])\\s+", perl = TRUE)))
  lapply(parts, function(p) p[grepl("\\S", p)])
}

# Returns the units, one row each with its label, document, position, and
# speakers, and the words, one row each with the unit it belongs to.
.text_units <- function(docs, unit) {
  n <- length(docs$label)
  if (!is.null(docs$tokens)) {
    tok <- docs$tokens
    if (identical(unit, "document") || is.integer(unit)) {
      piece <- tok$doc
    } else {
      col <- tok[[unit]]
      if (all(is.na(col)))
        snet_abort("A data frame of tokens needs a {.val {paste0(unit, '_id')}}",
                   "column for {.val {unit}} units.")
      piece <- match(paste(tok$doc, col), unique(paste(tok$doc, col)))
    }
    pdoc <- tok$doc[!duplicated(piece)]
    words <- split(tok$word, factor(piece, levels = unique(piece)))
  } else if (identical(unit, "document") || is.integer(unit)) {
    pdoc <- seq_len(n)
    words <- .text_words(docs$text)
  } else {
    parts <- .text_split(docs$text, unit)
    pdoc <- rep(seq_len(n), lengths(parts))
    words <- .text_words(unlist(parts))
  }
  words <- unname(words)
  if (is.integer(unit)) {
    # every run of `unit` consecutive words of a document is a window
    starts <- lapply(lengths(words), function(k)
      seq_len(max(1L, k - unit + 1L)))
    pdoc <- rep(pdoc, lengths(starts))
    words <- unlist(Map(function(w, s)
      lapply(s, function(i) w[i:min(length(w), i + unit - 1L)])[seq_along(s)],
      words, starts), recursive = FALSE)
    words <- lapply(words, function(w) w[!is.na(w)])
  }
  within <- stats::ave(pdoc, pdoc, FUN = seq_along)
  label <- if (identical(unit, "document")) docs$label[pdoc] else
    paste(docs$label[pdoc], within, sep = ".")
  units <- data.frame(label = label, stringsAsFactors = FALSE)
  if (!identical(unit, "document")) units$document <- docs$label[pdoc]
  if (!is.null(docs$attrs) && ncol(docs$attrs) > 0)
    units <- cbind(units, docs$attrs[pdoc, , drop = FALSE])
  # documents need not be in any order, but the parts of a document are
  if (!identical(unit, "document")) units$time <- seq_along(pdoc)
  units$from <- docs$from[pdoc]
  units$to <- docs$to[pdoc]
  units$mode <- if (is.integer(unit)) "windows" else paste0(unit, "s")
  rownames(units) <- NULL
  list(units = units,
       words = data.frame(unit = rep(seq_along(words), lengths(words)),
                          word = unlist(words) %||% character(0),
                          stringsAsFactors = FALSE))
}

.text_stopwords <- function(stopwords) {
  if (is.null(stopwords)) return(character(0))
  if (is.data.frame(stopwords)) {
    if (!"word" %in% names(stopwords))
      snet_abort("A data frame of {.arg stopwords} needs a {.val word} column.")
    stopwords <- stopwords$word
  }
  if (!is.character(stopwords))
    snet_abort("{.arg stopwords} must be a character vector or a data frame",
               "with a {.val word} column.")
  unique(tolower(stopwords))
}

# Returns the dictionary as a data frame of aliases, each with the label of
# its term and its words, or NULL where every word is a term.
.text_dictionary <- function(terms) {
  if (is.null(terms)) return(NULL)
  if (is.list(terms)) {
    if (is.null(names(terms)) || any(!nzchar(names(terms))))
      snet_abort("A list of {.arg terms} must name each of its elements,",
                 "e.g. {.code list(Harry = c(\"harry\", \"potter\"))}.")
    label <- rep(names(terms), lengths(terms))
    alias <- unlist(terms, use.names = FALSE)
  } else if (is.character(terms)) {
    label <- alias <- terms
  } else snet_abort("{.arg terms} must be NULL, a character vector,",
                    "or a named list of aliases.")
  dict <- data.frame(label = as.character(label), stringsAsFactors = FALSE)
  dict$words <- .text_words(alias)
  dict <- dict[lengths(dict$words) > 0, ]
  dict[!duplicated(vapply(dict$words, paste, character(1), collapse = " ")), ]
}

# Returns the terms found, one row for each use, with its unit and label.
.text_match <- function(words, dict, stopwords) {
  if (is.null(dict)) {
    out <- words[!words$word %in% stopwords, ]
    return(data.frame(unit = out$unit, term = out$word,
                      stringsAsFactors = FALSE))
  }
  w <- words$word
  u <- words$unit
  n <- length(w)
  hits <- lapply(seq_len(nrow(dict)), function(d) {
    a <- dict$words[[d]]
    m <- length(a)
    if (n < m) return(NULL)
    s <- seq_len(n - m + 1L)
    ok <- rep(TRUE, length(s))
    for (k in seq_len(m))
      ok <- ok & w[s + k - 1L] == a[k] & u[s + k - 1L] == u[s]
    s <- s[ok]
    if (length(s) == 0) return(NULL)
    data.frame(start = s, end = s + m - 1L, entry = d)
  })
  hits <- do.call(rbind, hits)
  if (is.null(hits)) return(data.frame(unit = integer(0), term = character(0)))
  # where matches overlap, the one that starts first, and then the longest,
  # is taken, so that "harry potter" is not also a mention of "potter"
  hits <- hits[order(hits$start, -(hits$end - hits$start), hits$entry), ]
  if (any(hits$end > hits$start) || anyDuplicated(hits$start)) {
    keep <- logical(nrow(hits))
    last <- 0L
    for (i in seq_len(nrow(hits))) {
      if (hits$start[i] > last) {
        keep[i] <- TRUE
        last <- hits$end[i]
      }
    }
    hits <- hits[keep, ]
  }
  out <- data.frame(unit = u[hits$start], term = dict$label[hits$entry],
                    stringsAsFactors = FALSE)
  out[!tolower(out$term) %in% stopwords, ]
}

# Returns a named numeric vector of the value of each word.
.text_lexicon <- function(lexicon) {
  if (is.null(lexicon)) return(NULL)
  if (is.data.frame(lexicon)) {
    if (!"word" %in% names(lexicon))
      snet_abort("A data frame {.arg lexicon} needs a {.val word} column.")
    if ("value" %in% names(lexicon) && is.numeric(lexicon$value)) {
      value <- lexicon$value
    } else if ("sentiment" %in% names(lexicon)) {
      value <- c(positive = 1, negative = -1)[as.character(lexicon$sentiment)]
    } else snet_abort("A data frame {.arg lexicon} needs a numeric",
                      "{.val value} column or a {.val sentiment} column",
                      "of {.val positive} and {.val negative}.")
    lexicon <- stats::setNames(unname(value), lexicon$word)
  }
  if (!is.numeric(lexicon) || is.null(names(lexicon)))
    snet_abort("{.arg lexicon} must be a named numeric vector or a data frame",
               "with a {.val word} column.")
  lexicon <- lexicon[!is.na(lexicon)]
  names(lexicon) <- tolower(names(lexicon))
  lexicon[!duplicated(names(lexicon))]
}

# Returns the score of each unit: the sum of the values of its words.
.text_sentiment <- function(words, lexicon, stopwords, n) {
  value <- unname(lexicon[words$word])
  value[is.na(value) | words$word %in% stopwords] <- 0
  out <- vapply(split(value, factor(words$unit, levels = seq_len(n))), sum,
                numeric(1))
  unname(out)
}

# Counts uses by the columns given, keeping the first row of each group.
.text_count <- function(df, by) {
  key <- do.call(paste, c(df[by], sep = "\r"))
  first <- !duplicated(key)
  out <- df[first, , drop = FALSE]
  out$weight <- as.integer(table(factor(key, levels = key[first])))
  out
}

# A two-mode network ties each unit to the terms it uses.
.text_twomode <- function(info, units, found, dict, term_mode, ties) {
  terms <- if (is.null(dict)) sort(unique(found$term), method = "radix") else
    unique(dict$label)
  unit_mode <- units$mode[1] %||% "documents"
  nu <- nrow(units)
  out <- data.frame(from = found$unit, to = nu + match(found$term, terms))
  if (!is.null(units$time)) out$time <- units$time[found$unit]
  out <- out[order(out$from, out$to), , drop = FALSE]
  if (ties == "counts") out <- .text_count(out, c("from", "to"))
  rownames(out) <- NULL
  if (nu < 2) out$time <- NULL
  units$from <- units$to <- NULL
  nodes <- dplyr::bind_rows(units[c("label", "mode", setdiff(names(units),
                                                             c("label", "mode", "time")))],
                            data.frame(label = terms, mode = rep(term_mode,
                                                                 length(terms)),
                                       stringsAsFactors = FALSE))
  info$directed <- FALSE
  info$modes <- c(unit_mode, term_mode)
  info$sender <- unit_mode
  info$receiver <- term_mode
  snet_info("Collected {length(terms)} term{?s} from {nu} unit{?s}.")
  make_stocnet(info = info, nodes = nodes, ties = out)
}

# A one-mode network ties each speaker to what they mention, or to whom they
# address, with what they mention recorded as 'about'.
.text_onemode <- function(info, units, found, dict, ties, signed) {
  gossip <- !is.null(units$to)
  spoken <- !is.na(units$from)
  if (gossip) spoken <- spoken & !is.na(units$to)
  if (!all(spoken))
    snet_info("Dropping {sum(!spoken)} unit{?s} without a speaker",
              if (gossip) "or an addressee." else ".")
  found <- found[spoken[found$unit], , drop = FALSE]
  terms <- if (is.null(dict)) sort(unique(found$term), method = "radix") else
    unique(dict$label)
  labels <- unique(c(units$from[spoken], if (gossip) units$to[spoken],
                     terms[terms %in% found$term]))
  out <- data.frame(from = match(units$from[found$unit], labels))
  if (gossip) {
    out$to <- match(units$to[found$unit], labels)
    out$about <- match(found$term, labels)
  } else out$to <- match(found$term, labels)
  if (!is.null(units$time)) out$time <- units$time[found$unit]
  if (signed) out$sign <- ifelse(units$sentiment[found$unit] < 0, -1L, 1L)
  out <- out[do.call(order, out[intersect(c("time", "from", "to", "about"),
                                          names(out))]), , drop = FALSE]
  if (ties == "counts") {
    by <- setdiff(names(out), "sign")
    out <- .text_count(out, c(by, if (signed) "sign"))
    if (signed) out$weight <- out$weight * out$sign
    out$sign <- NULL
  } else if (signed) {
    out$weight <- out$sign
    out$sign <- NULL
  }
  rownames(out) <- NULL
  if (nrow(units) < 2) out$time <- NULL
  info$directed <- TRUE
  if (signed) info$signed <- TRUE
  snet_info("Collected {length(labels)} node{?s} from {nrow(units)} unit{?s}.")
  make_stocnet(info = info, nodes = data.frame(label = labels,
                                               stringsAsFactors = FALSE),
               ties = out)
}

# Bibliography ####

#' Making networks from bibliographic records
#'
#' @description
#' `collect_bibliography()` creates a network from bibliographic records,
#' such as an export from the Web of Science, Scopus, or OpenAlex.
#' The result is one multimodal network of the works,
#' their authors, their keywords, and their sources,
#' in which the works also cite one another.
#' The classic bibliometric networks, such as co-citation,
#' bibliographic coupling, co-authorship, and co-word networks,
#' are then projections or proximities of this network
#' (see Recipes).
#' @details
#'   Only base R is used, so no bibliometric package is needed.
#'   The records can come from:
#'   - `bibliometrix::convert2df()`, a data frame with one record in each row
#'   and the fields of the Web of Science as its columns:
#'   'AU' (authors), 'CR' (cited references), 'DE' (author keywords),
#'   'ID' (indexed keywords), 'SO' (source), 'PY' (year), 'DI' (DOI),
#'   'TI' (title), and 'SR' (a short label);
#'   - `openalexR::oa_fetch()`, a data frame of works with the columns
#'   'id', 'display_name', 'authorships' (or 'author'), 'publication_year',
#'   'doi', 'source_display_name' (or 'so'), 'referenced_works',
#'   'keywords', and 'concepts' or 'topics';
#'   - a BibTeX or BibLaTeX file, such as the BibTeX export of the
#'   Web of Science, which holds the cited references of each record in its
#'   'cited-references' field.
#'   A file that Zotero exports holds no cited references,
#'   so it gives works, authors, keywords, and sources, but no citations.
#'   The editors of a work are its authors where it has no authors.
#'   Records in other forms can be given as a data frame with the
#'   columns of `{bibliometrix}`.
#'
#'   The network has four modes and four layers:
#'
#'   | Layer | From | To | Directed |
#'   |:------|:-----|:---|:---------|
#'   | "cites" | works | works | yes |
#'   | "authored" | authors | works | no |
#'   | "tagged" | works | keywords | no |
#'   | "published" | works | sources | no |
#'
#'   A mode or layer with no nodes or ties is left out.
#'   The works have a 'citations' column with the number of times
#'   the database counts them as cited, in all the works it holds
#'   ('TC' in `{bibliometrix}`, 'cited_by_count' in OpenAlex,
#'   or 'times-cited' in BibTeX), where it gives one.
#'   This is the global citation count.
#'   The local citation count, how often the records cite a work,
#'   is the indegree of the work in the "cites" layer.
#'   Each "authored" tie has the position of its author among the authors
#'   of the work in a 'position' column, 1 for the first author,
#'   since some counts weight the first or last authors more.
#'   Each tie has the publication year of its work as its 'time',
#'   and for "cites" this is the year of the citing work.
#'   Since a work that is published stays published, and a citation stays
#'   made, the ties are events that increment the network,
#'   so `to_time(.data, 2010)` gives the literature up to 2010.
#' @section Cited works:
#'   A cited reference becomes a tie to a work.
#'   The reference is matched to a work in the records where possible:
#'
#'   1. by a normalised string of its first author, year, and source,
#'   as the Web of Science writes references
#'   (e.g. "GLANZEL W, 1996, SCIENTOMETRICS"),
#'   where this string identifies one record
#'   (the 'SR' column of `{bibliometrix}`, or else the same string made
#'   from the first author, year, and source of the record);
#'   2. else by its DOI, where both the reference and the record have one;
#'   3. else by its OpenAlex identifier, for OpenAlex records.
#'
#'   A reference that matches no record is a work too,
#'   since co-citation is about the works cited, most of which are not
#'   in the records.
#'   These works have `FALSE` in the 'recorded' column of the nodes,
#'   so `filter_nodes(.data, recorded | is.na(recorded))` keeps only
#'   the works in the records (and the other modes).
#'   They are not inactive, since they hold ties.
#'   References that are not matched to a record are one work
#'   where they have the same normalised string of first author, year,
#'   and source, as in `{bibliometrix}`, whether or not they give a DOI.
#'   The 'doi' of such a work is the first DOI that its references give.
#'   Two works of an author in the same year and source are therefore
#'   one work, even where their DOIs differ,
#'   and a work that is written differently in two references,
#'   such as with another spelling of its author, is two works.
#'   A reference that matches the record that cites it is another work,
#'   since a work does not cite itself.
#'   Scopus writes references in another order,
#'   so its references are matched to records only by DOI.
#' @section Recipes:
#'   The classic networks come from the existing functions:
#'
#'   | Network | Recipe |
#'   |:--------|:-------|
#'   | citation | `to_uniplex(.data, "cites")` |
#'   | co-citation | `to_proximity(to_uniplex(.data, "cites"), across = "columns")` |
#'   | bibliographic coupling | `to_proximity(to_uniplex(.data, "cites"), across = "rows")` |
#'   | co-authorship | `to_mode(.data, "authors", through = "works", similarity = "fractional")` |
#'   | co-word | `to_mode(.data, "keywords", through = "works")` |
#'   | authors and keywords | `to_twomode(.data, through = c("works", "sources"))` |
#'
#'   Co-citation ties two works that are cited by the same work,
#'   and coupling ties two works that cite the same work,
#'   in both cases weighted by the number of such works.
#'   "fractional" counting shares each work among its authors,
#'   so that a work of many authors does not dominate
#'   (Perianes-Rodriguez, Waltman, and van Eck 2016);
#'   use "count" for full counting.
#'   See [to_mode()] for other similarities,
#'   such as "association" strength.
#' @param records The bibliographic records.
#'   A data frame from `{bibliometrix}` or `{openalexR}`,
#'   or the path to a BibTeX or BibLaTeX file (see Details).
#' @param keywords Which keywords to collect.
#'   One of "author" (the default), the keywords that the authors give
#'   ('DE', or 'keywords' in BibTeX and OpenAlex),
#'   "indexed", the keywords that the database adds
#'   ('ID', 'keywords-plus' in BibTeX, or the concepts or topics of OpenAlex),
#'   or "both".
#'   Keywords are lowercased.
#' @return A `stocnet` object.
#'   A multimodal, multiplex network of works, authors, keywords,
#'   and sources.
#' @family makes
#' @seealso [to_uniplex()], [to_proximity()], [to_mode()], [to_time()]
#' @references
#' Perianes-Rodriguez, Antonio, Ludo Waltman, and Nees Jan van Eck. 2016.
#' "Constructing bibliometric networks: A comparison between full and
#' fractional counting".
#' _Journal of Informetrics_, 10(4): 1178-1195.
#' \doi{10.1016/j.joi.2016.10.006}
#' @examples
#' records <- data.frame(
#'   SR = c("SMITH J, 2001, SCIENTOMETRICS", "JONES K, 2003, SCIENTOMETRICS",
#'          "LI X, 2005, J INFORMETR"),
#'   AU = c("SMITH J;JONES K", "JONES K", "LI X;SMITH J"),
#'   TI = c("Citations", "Coupling", "Co-words"),
#'   PY = c(2001, 2003, 2005),
#'   SO = c("SCIENTOMETRICS", "SCIENTOMETRICS", "J INFORMETR"),
#'   DE = c("citation; network", "coupling; network", "network"),
#'   CR = c("PRICE DJD, 1965, SCIENCE, V149, P510",
#'          "SMITH J, 2001, SCIENTOMETRICS, V50, P1; PRICE DJD, 1965, SCIENCE",
#'          "SMITH J, 2001, SCIENTOMETRICS; JONES K, 2003, SCIENTOMETRICS"))
#' (bib <- collect_bibliography(records))
#' to_uniplex(bib, "cites") |> to_proximity(across = "columns")
#' to_mode(bib, "authors", through = "works", similarity = "fractional")
#' @name make_bibliography
#' @export
collect_bibliography <- function(records,
                                 keywords = c("author", "indexed", "both")) {
  keywords <- match.arg(keywords)
  snet_progress_step("Reading the records")
  recs <- .bib_records(records, keywords)
  if (length(recs$label) == 0)
    snet_abort("No records were found in {.arg records}.")
  snet_progress_step("Matching cited references to works")
  works <- .bib_match(recs)
  .bib_network(recs, works)
}

# Returns the records in one form: a list with, for each record, its label,
# title, year, DOI, OpenAlex identifier, and source, and lists of its
# authors, keywords, and references, and whether references are identifiers.
.bib_records <- function(records, keywords) {
  if (is.character(records) && length(records) == 1) {
    if (!file.exists(records))
      snet_abort("The file {.file {records}} does not exist.")
    return(.bib_from_bibtex(.bib_parse(records), keywords))
  }
  if (!is.data.frame(records))
    snet_abort("{.arg records} must be a data frame or the path to a",
               "BibTeX file, not {.cls {class(records)}}.")
  records <- as.data.frame(records, stringsAsFactors = FALSE)
  if ("referenced_works" %in% names(records) ||
      any(c("authorships", "publication_year") %in% names(records)))
    return(.bib_from_openalex(records, keywords))
  if ("AU" %in% names(records))
    return(.bib_from_fields(records, keywords))
  snet_abort("{.arg records} is not a data frame of bibliographic records.",
             "i" = "Give the columns of {.pkg bibliometrix}, such as",
             " " = "{.val AU}, {.val CR}, {.val PY}, and {.val SO},",
             "i" = "or the columns of {.pkg openalexR}, such as",
             " " = "{.val id} and {.val referenced_works}.")
}

# Returns a column as a character vector, or NAs where it is absent.
.bib_col <- function(df, col, n = nrow(df)) {
  if (!col %in% names(df)) return(rep(NA_character_, n))
  x <- as.character(df[[col]])
  x[!is.na(x) & !nzchar(trimws(x))] <- NA
  x
}

# Splits each of a vector of fields into trimmed, non-empty items.
.bib_split <- function(x, sep = ";") {
  x <- as.character(x)
  x[is.na(x)] <- ""
  lapply(strsplit(x, sep, perl = TRUE), function(i) {
    i <- trimws(gsub("\\s+", " ", i))
    i[nzchar(i) & i != "NA"]
  })
}

.bib_keywords <- function(author, indexed, keywords) {
  out <- switch(keywords, author = author, indexed = indexed,
                both = Map(c, author, indexed))
  lapply(out, function(k) unique(tolower(k)))
}

# A label in the form the Web of Science gives cited references,
# "AUTHOR, YEAR, SOURCE", for records that have no label of their own.
.bib_label <- function(authors, year, source) {
  first <- vapply(authors, function(a) if (length(a)) a[1] else NA_character_,
                  character(1))
  parts <- cbind(first, as.character(year), source)
  out <- apply(parts, 1, function(p) paste(p[!is.na(p)], collapse = ", "))
  out[!nzchar(out)] <- "work"
  out
}

.bib_from_fields <- function(df, keywords) {
  n <- nrow(df)
  authors <- .bib_split(.bib_col(df, "AU"))
  year <- suppressWarnings(as.integer(.bib_col(df, "PY")))
  source <- trimws(.bib_col(df, "SO"))
  label <- .bib_col(df, "SR")
  label[is.na(label)] <- .bib_label(authors, year, source)[is.na(label)]
  # bibliometrix reads "DOI;" for "DOI " in some exports
  refs <- .bib_split(gsub("DOI;", "DOI ", .bib_col(df, "CR"), fixed = TRUE))
  # some DOIs hold a semicolon, as in "...<1:AID-ASI1>3.0.CO;2-9", and the
  # pieces they are split into are joined again, since every reference
  # of the Web of Science separates its author and year with a comma
  refs <- lapply(refs, function(r) {
    if (length(r) < 2) return(r)
    tail <- !grepl(",", r) & c(FALSE, rep(TRUE, length(r) - 1))
    if (!any(tail)) return(r)
    group <- cumsum(!tail)
    unname(vapply(split(r, group), paste, character(1), collapse = ";"))
  })
  list(label = label, key = label, title = .bib_col(df, "TI"), year = year,
       citations = .bib_count(.bib_col(df, "TC")),
       doi = .bib_doi(.bib_col(df, "DI")), id = rep(NA_character_, n),
       source = source, authors = authors,
       keywords = .bib_keywords(.bib_split(.bib_col(df, "DE")),
                                .bib_split(.bib_col(df, "ID")), keywords),
       refs = refs, ids = FALSE)
}

# Returns the values of a column of a list column of data frames
# (or vectors), one character vector for each record.
.bib_nested <- function(x, cols) {
  if (is.null(x)) return(NULL)
  lapply(x, function(i) {
    if (is.null(i) || (length(i) == 1 && is.atomic(i) && is.na(i)))
      return(character(0))
    if (is.data.frame(i)) {
      col <- intersect(cols, names(i))[1]
      if (is.na(col)) return(character(0))
      if ("type" %in% names(i) && "topic" %in% i$type) i <- i[i$type == "topic", ]
      return(as.character(i[[col]][!is.na(i[[col]])]))
    }
    as.character(unlist(i))
  })
}

.bib_from_openalex <- function(df, keywords) {
  n <- nrow(df)
  none <- rep(list(character(0)), n)
  authors <- .bib_nested(df$authorships, c("display_name", "au_display_name")) %||%
    .bib_nested(df$author, c("au_display_name", "display_name")) %||% none
  year <- suppressWarnings(as.integer(.bib_col(df, "publication_year")))
  source <- .bib_col(df, "source_display_name")
  if (all(is.na(source))) source <- .bib_col(df, "so")
  title <- .bib_col(df, "display_name")
  if (all(is.na(title))) title <- .bib_col(df, "title")
  author_kw <- .bib_nested(df$keywords, c("display_name", "keyword")) %||% none
  indexed_kw <- .bib_nested(df$concepts, "display_name") %||%
    .bib_nested(df$topics, "display_name") %||% none
  refs <- .bib_nested(df$referenced_works, "id") %||% none
  list(label = .bib_label(authors, year, source),
       key = rep(NA_character_, n), title = title, year = year,
       citations = .bib_count(.bib_col(df, "cited_by_count")),
       doi = .bib_doi(.bib_col(df, "doi")),
       id = .bib_openalex_id(.bib_col(df, "id")), source = source,
       authors = authors,
       keywords = .bib_keywords(author_kw, indexed_kw, keywords),
       refs = lapply(refs, .bib_openalex_id), ids = TRUE)
}

# Writes names as the Web of Science does in references, "LEE A" for
# "Lee, Ann" or "Ann Lee".
.bib_short_name <- function(x) {
  if (length(x) == 0) return(x)
  comma <- grepl(",", x, fixed = TRUE)
  surname <- ifelse(comma, sub(",.*", "", x), sub(".*\\s", "", x))
  given <- ifelse(comma, sub("^[^,]*,", "", x),
                  ifelse(grepl("\\s", x), sub("\\s+\\S+$", "", x), ""))
  initials <- vapply(strsplit(trimws(given), "[[:space:].-]+"), function(g)
    paste(substr(g[nzchar(g)], 1, 1), collapse = ""), character(1))
  trimws(toupper(paste(surname, initials)))
}

# Reads a count of citations, which some exports write as "12 times".
.bib_count <- function(x) {
  suppressWarnings(as.integer(sub("^\\D*(\\d+).*$", "\\1", x)))
}

.bib_openalex_id <- function(x) toupper(sub("^https?://openalex\\.org/", "", x))

.bib_from_bibtex <- function(entries, keywords) {
  field <- function(f) vapply(entries, function(e) {
    v <- e[[f]]
    if (is.null(v)) NA_character_ else v
  }, character(1))
  # an edited book has editors and no authors
  people <- field("author")
  people[is.na(people)] <- field("editor")[is.na(people)]
  authors <- .bib_split(people, "\\s+and\\s+")
  # BibLaTeX, as Zotero can export, gives a date and a journal title
  year <- field("year")
  year[is.na(year)] <- field("date")[is.na(year)]
  year <- suppressWarnings(as.integer(sub(".*?(\\d{4}).*", "\\1", year)))
  source <- field("journal")
  for (f in c("journaltitle", "booktitle", "publisher", "school", "institution"))
    source[is.na(source)] <- field(f)[is.na(source)]
  refs <- field("cited-references")
  refs[is.na(refs)] <- field("references")[is.na(refs)]
  label <- vapply(entries, function(e) e$.key %||% NA_character_, character(1))
  label[is.na(label)] <- .bib_label(authors, year, source)[is.na(label)]
  list(label = label,
       key = .bib_label(lapply(authors, .bib_short_name), year, source),
       title = field("title"), year = year,
       citations = .bib_count(field("times-cited")),
       doi = .bib_doi(field("doi")), id = rep(NA_character_, length(entries)),
       source = source, authors = authors,
       keywords = .bib_keywords(.bib_split(field("keywords"), "[;,]"),
                                .bib_split(field("keywords-plus"), "[;,]"),
                                keywords),
       refs = .bib_split(refs, "\n|;"), ids = FALSE)
}

# Reads a BibTeX file into a list of entries, each a named list of fields,
# with the citation key as '.key'. Field names are lowercased, braces are
# dropped, and white space is collapsed except for newlines.
.bib_parse <- function(file) {
  txt <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  ch <- strsplit(txt, "", fixed = TRUE)[[1]]
  if (length(ch) == 0) return(list())
  depth <- cumsum((ch == "{") - (ch == "}"))
  at <- gregexpr("@[A-Za-z]+[[:space:]]*\\{", txt)[[1]]
  if (at[1] == -1) return(list())
  ends <- at + attr(at, "match.length") - 1L
  types <- tolower(gsub("[@{[:space:]]", "", regmatches(txt, list(at))[[1]]))
  entries <- list()
  for (i in seq_along(at)) {
    if (types[i] %in% c("comment", "string", "preamble")) next
    open <- ends[i]
    close <- which(depth[(open + 1L):length(ch)] == depth[open] - 1L)[1]
    if (is.na(close)) next
    body <- paste(ch[(open + 1L):(open + close - 1L)], collapse = "")
    entries[[length(entries) + 1L]] <- .bib_parse_entry(body)
  }
  entries
}

.bib_parse_entry <- function(body) {
  comma <- regexpr(",", body, fixed = TRUE)
  if (comma == -1) return(list(.key = trimws(body)))
  out <- list(.key = trimws(substr(body, 1, comma - 1)))
  rest <- substr(body, comma + 1, nchar(body))
  repeat {
    m <- regexpr("^[[:space:],]*([A-Za-z][A-Za-z0-9_:.+-]*)[[:space:]]*=[[:space:]]*",
                 rest, perl = TRUE)
    if (m == -1) break
    name <- tolower(gsub("[[:space:],=]", "", regmatches(rest, m)))
    rest <- substr(rest, attr(m, "match.length") + 1, nchar(rest))
    first <- substr(rest, 1, 1)
    if (first %in% c("{", "\"")) {
      ch <- strsplit(rest, "", fixed = TRUE)[[1]]
      depth <- cumsum((ch == "{") - (ch == "}"))
      end <- if (first == "{") which(depth == 0)[1] else
        which(ch == "\"" & depth == 0 & seq_along(ch) > 1)[1]
      if (is.na(end)) end <- length(ch)
      value <- paste(ch[2:max(2, end - 1)], collapse = "")
      if (end < 2) value <- ""
      rest <- substr(rest, end + 1, nchar(rest))
    } else {
      end <- regexpr(",", rest, fixed = TRUE)
      if (end == -1) end <- nchar(rest) + 1
      value <- substr(rest, 1, end - 1)
      rest <- substr(rest, end, nchar(rest))
    }
    value <- gsub("[{}]", "", value)
    value <- gsub("[ \t\r]+", " ", value)
    value <- trimws(gsub(" ?\n ?", "\n", value))
    if (name != "cited-references" && name != "references")
      value <- gsub("\n", " ", value)
    out[[name]] <- value
  }
  out
}

# Returns DOIs in one form: lowercased, without a resolver, and without
# trailing punctuation.
.bib_doi <- function(x) {
  x <- tolower(trimws(x))
  x <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:?\\s*", "", x)
  x <- sub("[.,;)]+$", "", x)
  x[!grepl("^10\\.", x)] <- NA
  x
}

# Returns the DOI that a cited reference names, if any.
.bib_ref_doi <- function(refs) {
  # a semicolon that is not followed by a space is part of the DOI
  m <- regmatches(refs, regexpr("10\\.[0-9]{4,9}/(?:[^\\s,;]|;(?=[^\\s;]))+",
                                refs, perl = TRUE), invert = NA)
  .bib_doi(vapply(m, function(p) if (length(p) == 3) p[2] else NA_character_,
                  character(1)))
}

# Normalises a reference or a label for matching: uppercase letters and
# digits only, separated by single spaces.
.bib_normalise <- function(x) {
  x <- toupper(x)
  x <- gsub("[^[:alnum:]]+", " ", x)
  trimws(x)
}

# The "AUTHOR, YEAR, SOURCE" key of a Web of Science reference.
.bib_ref_key <- function(refs) {
  parts <- strsplit(refs, ",", fixed = TRUE)
  out <- vapply(parts, function(p) paste(p[seq_len(min(3, length(p)))],
                                         collapse = " "), character(1))
  .bib_normalise(out)
}

# Returns the works: the records, then each cited work that matches no
# record, with the work that each reference of each record cites.
.bib_match <- function(recs) {
  n <- length(recs$label)
  ref <- unlist(recs$refs) %||% character(0)
  citing <- rep(seq_len(n), lengths(recs$refs))
  target <- rep(NA_integer_, length(ref))
  if (isTRUE(recs$ids)) {
    target <- match(ref, recs$id)
    identity <- ref
    doi <- rep(NA_character_, length(ref))
  } else {
    doi <- .bib_ref_doi(ref)
    ref_key <- .bib_ref_key(ref)
    # a cited work is named by its first author, year, and source, as in
    # bibliometrix, whether or not a reference to it gives a DOI
    key <- .bib_normalise(recs$key)
    key[duplicated(key) | duplicated(key, fromLast = TRUE)] <- NA
    target <- match(ref_key, key, incomparables = NA)
    # the DOI matches only where the key does not, as for Scopus references
    by_doi <- match(doi, recs$doi, incomparables = NA)
    target[is.na(target)] <- by_doi[is.na(target)]
    # a work does not cite itself, so a reference that matches the record
    # that cites it is another work, such as another paper of the same
    # author, year, and source
    target[!is.na(target) & target == citing] <- NA
    identity <- ref_key
  }
  # references to no record are works of their own, one for each identity
  other <- is.na(target)
  ids <- unique(identity[other])
  target[other] <- n + match(identity[other], ids)
  # the first reference with a DOI names a work, where it has one
  first <- match(ids, identity[order(is.na(doi))])
  first <- order(is.na(doi))[first]
  cited <- ref[first]
  if (isTRUE(recs$ids)) {
    label <- cited
    year <- rep(NA_integer_, length(ids))
  } else {
    parts <- strsplit(cited, ",", fixed = TRUE)
    label <- vapply(parts, function(p)
      paste(trimws(p[seq_len(min(3, length(p)))]), collapse = ", "),
      character(1))
    year <- vapply(parts, function(p) {
      y <- if (length(p) > 1) trimws(p[2]) else NA_character_
      if (!is.na(y) && grepl("^[0-9]{4}$", y)) as.integer(y) else NA_integer_
    }, integer(1))
  }
  works <- data.frame(label = c(recs$label, label),
                      title = c(recs$title, rep(NA_character_, length(ids))),
                      year = c(recs$year, year),
                      citations = c(recs$citations,
                                    rep(NA_integer_, length(ids))),
                      doi = c(recs$doi, doi[first]),
                      reference = c(rep(NA_character_, n),
                                    if (isTRUE(recs$ids)) rep(NA_character_,
                                                              length(ids)) else cited),
                      recorded = rep(c(TRUE, FALSE), c(n, length(ids))),
                      stringsAsFactors = FALSE)
  if (all(is.na(works$citations))) works$citations <- NULL
  if (isTRUE(recs$ids)) works$openalex <- c(recs$id, ids)
  # a work does not cite itself, and cites another work once
  cites <- data.frame(from = citing, to = target)
  cites <- cites[cites$from != cites$to & !duplicated(cites), , drop = FALSE]
  matched <- sum(target[!other] <= n)
  snet_info("Matched {matched} of {length(ref)} reference{?s} to records,",
            "and {length(ids)} other cited work{?s}.")
  list(works = works, cites = cites,
       references = data.frame(citing = citing, cited = target))
}

# Builds the network of works, authors, keywords, and sources.
.bib_network <- function(recs, works) {
  n <- length(recs$label)
  nodes <- works$works
  nodes$label <- make.unique(nodes$label, sep = " ")
  nodes$mode <- "works"
  nw <- nrow(nodes)
  incidence <- function(items, mode, layer, from_works) {
    rec <- rep(seq_len(n), lengths(items))
    item <- unlist(items) %||% character(0)
    position <- unlist(lapply(lengths(items), seq_len)) %||% integer(0)
    keep <- !duplicated(paste(rec, item, sep = "\r"))
    rec <- rec[keep]
    item <- item[keep]
    labels <- unique(item)
    list(nodes = data.frame(label = labels, mode = rep(mode, length(labels)),
                            stringsAsFactors = FALSE),
         rec = rec, item = match(item, labels), layer = layer,
         from_works = from_works, position = position[keep])
  }
  parts <- list(incidence(recs$authors, "authors", "authored", FALSE),
                incidence(recs$keywords, "keywords", "tagged", TRUE),
                incidence(lapply(recs$source, function(s) s[!is.na(s)]),
                          "sources", "published", TRUE))
  parts <- parts[vapply(parts, function(p) nrow(p$nodes) > 0, logical(1))]
  ties <- data.frame(from = works$cites$from, to = works$cites$to,
                     layer = rep("cites", nrow(works$cites)),
                     time = recs$year[works$cites$from],
                     position = rep(NA_integer_, nrow(works$cites)))
  offset <- nw
  for (p in parts) {
    other <- offset + p$item
    ties <- rbind(ties, data.frame(
      from = if (p$from_works) p$rec else other,
      to = if (p$from_works) other else p$rec,
      layer = rep(p$layer, length(p$rec)), time = recs$year[p$rec],
      # the order of the authors of a work matters to some counts
      position = if (p$layer == "authored") p$position else
        rep(NA_integer_, length(p$rec))))
    p$nodes$label <- make.unique(p$nodes$label, sep = " ")
    nodes <- dplyr::bind_rows(nodes, p$nodes)
    offset <- offset + nrow(p$nodes)
  }
  if (all(is.na(ties$time))) ties$time <- NULL
  if (all(is.na(ties$position))) ties$position <- NULL
  nodes <- nodes[c("label", "mode", setdiff(names(nodes), c("label", "mode")))]
  layers <- unique(ties$layer)
  modes <- unique(nodes$mode)
  ends <- list(cites = c("works", "works"), authored = c("authors", "works"),
               tagged = c("works", "keywords"), published = c("works", "sources"))[layers]
  info <- list(name = "Bibliographic network", source = "Empirical",
               method = "Archival", observation = "event",
               modes = modes, layers = layers,
               directed = stats::setNames(layers == "cites", layers),
               sender = vapply(ends, `[`, character(1), 1),
               receiver = vapply(ends, `[`, character(1), 2))
  if (!is.null(ties$time)) info$update <- "increment"
  snet_info("Collected {n} record{?s} and {nrow(works$works) - n} other",
            "cited work{?s}.")
  make_stocnet(info = info, nodes = nodes, ties = ties)
}

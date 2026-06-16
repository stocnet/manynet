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
  if(!is.null(roster) && any(!roster %in% node_names(out))){
    isolates <- roster[!roster %in% node_names(out)]
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

# Dependencies ####

#' Making networks of inter- and intra-package dependencies
#'
#' @description
#' These functions read information from CRAN or within an R package's
#' working directory to create a networks of a package's dependencies:
#'
#' - `read_cran()` creates a network of a package's dependencies on other
#'    packages available on CRAN.
#'    It looks for the `Depends`, `Imports` and `Suggests` fields
#'    in the package's DESCRIPTION file
#'    and creates a network where nodes are packages
#'    and ties are dependencies.
#' - `read_pkg()` creates a network of function dependencies
#'    from R scripts in a directory.
#'    It looks for function definitions and function calls
#'    within the scripts and creates a network
#'    where nodes are functions and edges are function calls.
#'    It can also include function calls
#'    to functions not defined within the scripts.
#' @details
#'   Note that these functions are not as actively maintained as others
#'   in the package, so please let us know if any are not currently working
#'   for you or if there are missing import routines 
#'   by [raising an issue on Github](https://github.com/stocnet/manynet/issues).
#' @param pkg The name 
#' @return A `tidygraph` object representing the network of package dependencies
#'   or function dependencies in a package.
#' @source 
#' https://www.r-bloggers.com/2016/01/r-graph-objects-igraph-vs-network/
#' @importFrom utils available.packages contrib.url
#' @name make_cran
#' @family makes
#' @seealso [as]
NULL

#' @rdname make_cran
#' @examples
#' # mnet <- collect_cran()
#' # mnet <- to_ego(mnet, "manynet", max_dist = 2)
#' @export
collect_cran <- function(pkg = "all"){
  snet_progress_step("Downloading data about available packages from CRAN")
  cranInfoDF <- as.data.frame(utils::available.packages(
    utils::contrib.url(getOption("repos"), type = "source")))
  if(pkg=="all") new <- cranInfoDF$Package else
    new <- pkg
  done <- c()
  out <- data.frame()
  continue <- TRUE
  while(!all(new %in% done) && continue){
    toAdd <- dplyr::bind_rows(lapply(new, function(x) {
      # print(paste("I am", x))
      sections <- cranInfoDF[cranInfoDF$Package==x, 
                             c('Depends','Imports','Suggests')]
      if(nrow(sections)>0){
        deps <- sections[!is.na(sections)]
        names(deps) <- names(sections)[!is.na(sections)]
        deps <- lapply(deps, function(y) {
          l <- strsplit(y, split="(,|, |,\n|\n,| ,| , )| \\(")
          if(is.list(l)) l <- l[[1]]
          l <- l[!c(sapply(l, grepl, pattern = ">=", fixed = TRUE))]
          l <- l[!c(sapply(l, grepl, pattern = ">", fixed = TRUE))]
          l <- l[!c(sapply(l, grepl, pattern = "==", fixed = TRUE))]
          l <- l[!l %in% c("R","base","compiler","datasets","graphics",
                           "grDevices","grid","methods","parallel","splines",
                           "stats","stats4","tcltk","tools","translations",
                           "utils")]
          l[!is.na(l)]
        })
        deps <- unlist(deps)
        if(length(deps)>0)
          data.frame(from = x, to = deps, type = gsub("[0-9]", "", names(deps)))
      } }))
    done <- c(done, new)
    new <- setdiff(unique(toAdd$to), done)
    if(pkg=="all") continue <- FALSE
    out <- rbind(out, toAdd)
  }
  out <- as_tidygraph(out)
  compile <- cranInfoDF$NeedsCompilation
  out <- out |> 
    mutate_nodes(Compilation = compile[match(node_names(out),
                                             cranInfoDF$Package)]=="yes")
  out
}

#' @rdname make_cran
#' @importFrom dplyr bind_rows mutate filter select everything
#' @importFrom igraph graph_from_data_frame
#' @param dir Character string or vector of character strings
#'   with the directory or directories to search for R scripts.
#'   If `NULL` (the default), the current working directory is used.
#' @author Jakob Gepp
#' @source https://github.com/STATWORX/helfRlein/blob/master/R/get_network.R
#' @examples
#' # mnet <- collect_pkg()
#' @export
collect_pkg <- function(dir = getwd()) {
  
  variations <- c("<- function",
                 " <- function",
                 "<-function",
                 " <-function")
  
  # check if dir exists
  if(!grepl(".*R$", dir)){
    dir <- paste0(dir, "/R")
  }
  if (!is.null(dir) && all(!dir.exists(dir))) {
    stop(paste0(dir, " does not exists"))
  }
  
  # get files and folder within dir
  files_path <- list.files(file.path(dir),
                           pattern = "\\.R$",
                           recursive = TRUE,
                           full.names = TRUE)
  files_path <- files_path[!grepl("testthat", files_path)]
  files_path <- files_path[!grepl("zzz", files_path)]
  files_path <- files_path[!grepl("defunct", files_path)]
  if (length(files_path) == 0) {
    snet_abort("No files with the given pattern")
  }
  
  # common_base_path <- function(paths) {
  #   # Split each path into its components
  #   split_paths <- strsplit(paths, "/")
  #   
  #   # Find the common path
  #   common_path <- Reduce(function(x, y) {
  #     # Get the length of the shorter vector
  #     min_length <- min(length(x), length(y))
  #     # Only compare the elements up to the length of the shorter vector
  #     common <- x[seq_len(min_length)] == y[seq_len(min_length)]
  #     # If there's a FALSE in common, only keep the elements before it
  #     if (any(!common)) x[seq_len(which(!common)[1] - 1)]
  #     else x
  #   }, split_paths)
  #   
  #   # Combine the common path components back into a single string
  #   common_path <- paste(common_path, collapse = "/")
  #   
  #   return(common_path)
  # }
  # 
  # dir_base <- common_base_path(paths = dir)
  # folder <- dirname(gsub(paste0(dir_base, "/"), "", files_path))
  folder <- paste0(dirname(files_path)[1], "/")
  
  # Get all scripts ####
  all_scripts <- lapply(files_path, readLines, warn = FALSE)
  # set names of scripts
  names(all_scripts) <- gsub("\\.R$", "", basename(files_path))
  snet_minor_info("Found {length(all_scripts)} scripts")
  
  # Check for empty scripts ####
  indx <- sapply(all_scripts, length) == 0
  if (any(indx)) {
    warning(paste0("Removing empty scripts: ",
                   paste0(names(all_scripts)[indx], collapse = ", ")))
    all_scripts <- all_scripts[!indx]
    folder <- folder[!indx]
  }
  
  # remove variations with "
  # this is done so that strings like "<- function" will not be counted
  for (i_var in variations) {
    all_scripts <- lapply(all_scripts,
                          function(x) gsub(paste0("\"", i_var), "", x))
  }
  
  # remove method / functions that start with [
  # otherwise the regular expression will be messed up later
  keep <- !startsWith(names(all_scripts), "[")
  snet_minor_info("Removing {sum(!keep)} scripts that start with [")
  all_scripts <- all_scripts[keep]
  # folder <- folder[keep]
  
  # remove leading spaces
  all_scripts <- lapply(all_scripts, function(x)  sub("^\\s+", "", x))
  
  # split before #
  # all_scripts <- lapply(all_scripts, 
  #                       function(x) unlist(strsplit(x = x, split = "#")[1]))
  # remove comments #
  all_scripts <- lapply(all_scripts, function(x) subset(x, !startsWith(x, "#")))
  
  # check for empty scripts
  indx <- sapply(all_scripts, length) == 0
  if (any(indx)) {
    warning(paste0("Removing empty scripts: ",
                   paste0(names(all_scripts)[indx], collapse = ", ")))
    all_scripts <- all_scripts[!indx]
    folder <- folder[!indx]
  }
  
  # # split before { and }
  # all_scripts <- lapply(all_scripts,
  #                       function(x) unlist(strsplit(x = x, split = "[\\{\\}]")[1]))
  # # split after { and }
  # all_scripts <- lapply(all_scripts,
  #                       function(x) unlist(strsplit(x = x, split = "[\\{\\}]")[2]))
  
  # remove leading spaces again
  all_scripts <- lapply(all_scripts, function(x)  sub("^\\s+", "", x))
  
  # remove empty lines
  all_scripts <- lapply(all_scripts, function(x) x[x != ""])
  
  # check for empty scripts
  indx <- sapply(all_scripts, length) == 0
  if (any(indx)) {
    warning(paste0("Removing empty scripts: ",
                   paste0(names(all_scripts)[indx], collapse = ", ")))
    all_scripts <- all_scripts[!indx]
    folder <- folder[!indx]
  }
  
  # filter only those with functions (variations) in it
  index_functions <- unique(unlist(sapply(variations, grep, all_scripts)))
  main_functions  <- all_scripts[index_functions]
  # folder_main     <- folder[index_functions]
  scripts         <- all_scripts[-index_functions]
  # folder_scripts  <- folder[-index_functions]
  
  snet_minor_info("Found {length(index_functions)} scripts containing function definitions")
  
  # get subfunctions
  getsubindex <- function(funlist,
                          variations) {
    def_function_index <-
      lapply(funlist,
             function(x) {
               sort(unique(unlist(
                 lapply(variations,
                        function(y) which(grepl(pattern = y, x))))
               ))
             }
      )
    
    # get internal functions
    with_internal <- which(sapply(def_function_index, length) > 1)
    internal <- funlist[with_internal]
    def_internal <- lapply(def_function_index[with_internal],
                           function(x) sort(x))
    
    open  <- lapply(internal, function(x) as.numeric(grepl("\\{", x)))
    close <- lapply(internal, function(x) as.numeric(grepl("\\}", x)))
    both <- mapply(function(x, y) cumsum(x - y), open, close, SIMPLIFY = FALSE)
    
    sub_index_end <- mapply(function(x, z) {
      sapply(z, function(y) {
        tmp <- which(x == x[y])
        tmp <- tmp[tmp > y]
        if (length(tmp) == 1) {
          tmp
        } else {
          if (all(diff(tmp) == 1)) {
            suppressWarnings(min(tmp, na.rm = TRUE))
          } else {
            suppressWarnings(min(tmp[c(diff(c(y, tmp)) > 1)], na.rm = TRUE))
          }
        }
      })},
      both, def_internal, SIMPLIFY = FALSE)
    
    
    # set Inf to max length
    max_length <- lapply(internal, length)
    sub_index_end <- mapply(function(x, y) ifelse(x == Inf, y, x),
                            sub_index_end, max_length, SIMPLIFY = FALSE)
    
    sub_index <- mapply(function(x, y) cbind(x, y),
                        def_internal, sub_index_end, SIMPLIFY = FALSE)
    
    # remove row if it is from first to last
    sub_index <- mapply(
      function(x, y) matrix(x[apply(x, 1, diff) < c(y - 2), ], ncol = 2),
      sub_index, max_length, SIMPLIFY = FALSE)
    
    out <- list()
    out$sub_index <- sub_index
    out$internal <- internal
    
    return(out)
  }
  
  tmp <- getsubindex(funlist = main_functions,
                     variations = variations)
  sub_index <- tmp$sub_index
  internal  <- tmp$internal
  
  
  sub_functions <-
    mapply(function(i, s) {
      lapply(seq_len(nrow(s)), function(t) i[s[t, 1]:s[t, 2]])
    },
    internal, sub_index, SIMPLIFY = FALSE)
  sub_functions <- do.call("c", sub_functions)
  
  # folder for sub_functions
  folder_index <- which(names(main_functions) %in% names(sub_index))
  # folder_sub <- rep(folder_main[folder_index], sapply(sub_index, nrow))
  
  snet_minor_info("Found {length(sub_functions)} sub-functions")
  
  def_sub_functions <-
    unlist(lapply(seq_along(sub_functions),
                  function(x) sub_functions[[x]][1]))
  
  
  if (!is.null(def_sub_functions)) {
    names(sub_functions) <-
      unlist(gsub(" ", "",
                  lapply(base::strsplit(def_sub_functions, "<-"), "[[", 1)))
  }
  
  
  # combine sub to all functions
  all_functions <- c(main_functions, sub_functions)
  # all_folder <- folder_main
  # all_folder    <- c(folder_main, folder_sub)
  snet_minor_info("Found {length(all_functions)} functions")
  
  # remove duplicates
  index <- !duplicated(all_functions)
  all_functions <- all_functions[index]
  # all_folder    <-  all_folder[index]
  snet_minor_info("{sum(!index)} duplicated functions")
  
  dup_names <- duplicated(names(all_functions))
  if (any(dup_names)) {
    warning(paste0("multiple function: ",
                   paste0(unique(names(all_functions)[dup_names]),
                          collapse = ", "),
                   " Using only the first!"))
    all_functions <- all_functions[!dup_names]
    # all_folder    <- all_folder[!dup_names]
  }
  
  # remove sub_functions from functions
  tmp <- getsubindex(funlist = all_functions,
                     variations = variations)
  
  for (i_name in names(tmp$sub_index)) {
    i_num <- which(names(all_functions) == i_name)
    s <- tmp$sub_index[[i_name]]
    if (nrow(s) == 0) next
    remove_index <- unique(unlist(sapply(seq_len(nrow(s)),
                                         function(t) s[t, 1]:s[t, 2])))
    all_functions[[i_num]][remove_index] <- ""
  }
  
  # remove empty lines
  all_functions <- lapply(all_functions, function(x) x[x != ""])
  
  # combine sub to all functions
  all_files  <- c(all_functions, scripts)
  # all_folder <- c(all_folder, folder_scripts)
  snet_minor_info("Found {length(all_files)} files")
  
  # check if there are functions
  if (length(all_files) == 0) {
    warning("no functions found")
    return(list(matrix = NULL, igraph = NULL))
  }
  
  # get number of line per function
  lines <- sapply(all_files, length)
  
  # update function definition
  def_function_index <-
    lapply(
      all_files,
      function(x) {
        unique(unlist(
          lapply(variations,
                 function(y) which(grepl(pattern = y, x))))
        )
      }
    )
  
  def_functions <- lapply(
    seq_along(all_files),
    function(x) all_files[[x]][def_function_index[[x]]]
  )
  tmp_def_idx <- sapply(def_functions, function(x) length(x) == 0)
  def_functions[tmp_def_idx] <- ""
  
  
  def_functions[!tmp_def_idx] <- gsub(
    pattern = " ",
    replacement = "",
    lapply(base::strsplit(unlist(def_functions[!tmp_def_idx]), "<-"), "[[", 1))
  
  tmp_def_idx2 <- def_functions == "" & !tmp_def_idx
  # check for empty entries
  if (any(tmp_def_idx2)) {
    warning(paste0("Missing function name. ",
                   "This would have led to missleading plots. ",
                   "Removed from script(s): '",
                   paste0(names(all_files)[tmp_def_idx2],
                          collapse = "', '"), "'"))
  }
  def_functions <- unique(unlist(
    def_functions[def_functions != "" & !tmp_def_idx]
  ))
  
  
  
  # used for later adjustments of the network matrix
  def_functions2 <-
    lapply(seq_along(all_files),
           function(x) all_files[[x]][def_function_index[[x]]])
  
  # check for non characters
  def_functions2 <-
    lapply(def_functions2, function(x) {
      if (is.character(x)) {
        x
      } else {
        character(0)
      }
    })
  
  
  def_functions2 <-
    lapply(def_functions2,
           function(x) {
             gsub(" ", "", sapply(base::strsplit(x, "<-"), "[[", 1))
           }
    )
  
  def_functions2 <-
    lapply(seq_along(def_functions2),
           function(x) {
             ifelse(length(def_functions2[[x]]) == 0,
                    names(all_files)[x],
                    def_functions2[[x]])
           }
    )
  
  # remove function definition
  keep_lines <- mapply(function(x, y) which(!1:y %in% x),
                       def_function_index, lapply(all_files, length),
                       SIMPLIFY = FALSE)
  
  
  clean_functions <- all_files
  clean_functions <-
    lapply(seq_along(clean_functions),
           function(x) clean_functions[[x]][keep_lines[[x]]])
  names(clean_functions) <- names(all_files)
  snet_minor_info("Check length: {length(clean_functions)} clean_functions")
  
  # remove duplicated names
  dub_rows <- !duplicated(names(clean_functions))
  if (!all(dub_rows)) {
    warning(paste0("Removing duplicates: ",
                   paste0(names(clean_functions)[!dub_rows], collapse = ", ")))
    clean_functions <- clean_functions[dub_rows]
    lines <- lines[dub_rows]
    # all_folder <- all_folder[dub_rows]
    def_functions2 <- def_functions2[dub_rows]
  }
  
  # create adjacency matrix: network
  network <-
    lapply(clean_functions,
           function(z) {
             sapply(paste0(def_functions), #, "\\("
                    function(x, y = z) sum(grepl(x, y), na.rm = TRUE))
           })
  
  network <- as.data.frame(do.call(rbind, network))
  snet_minor_info("Initial network has {nrow(network)} rows and {ncol(network)} cols")
  
  # adjust networks rows and columns
  names(network) <- gsub("\\\\\\(", "", names(network))
  new_columns <- rownames(network)[
    which(!rownames(network) %in% colnames(network))]
  new_rows <- colnames(network)[
    which(!colnames(network) %in% rownames(network))]
  network[, new_columns] <- 0
  network[new_rows, ] <- 0
  network <- network[rownames(network)]
  snet_minor_info("Adding {length(new_rows)} new rows and {length(new_columns)} new cols")
  snet_minor_info("Adjusted network has {nrow(network)} rows and {ncol(network)} cols")
  
  # adjust lines, folders
  old_names <- names(lines)
  lines <- c(lines, rep(0, length(new_rows)))
  names(lines) <- c(old_names, new_rows)
  snet_minor_info("Check length: {length(lines)} lines")
  
  # remove duplicated functions within def_functions2
  if (sum(duplicated(def_functions2)) > 0) {
    snet_minor_info("There are {sum(duplicated(def_functions2))} inner functions with the same name. Keeping only the first.")
  }
  
  tmp_index <- unlist(lapply(
    new_rows,
    function(y) {
      which(lapply(def_functions2, function(x) x == y) == TRUE)[1]
    }
  ))
  if (length(tmp_index) == 0) {
    tmp_index <- NULL
  }
  
  # all_folder <- c(all_folder, all_folder[tmp_index])
  # snet_minor_info("check length: {length(all_folder)} all_folder")
  
  # simplify - removing functions with no connections ####
  # if (simplify) {
  #   calls <- apply(1, X = network, FUN = sum) + apply(2, X = network, FUN = sum)
  #   keep <- which(calls != 0)
  #   network <- network[keep, keep]
  #   all_folder <- all_folder[keep]
  #   lines <- lines[keep]
  # }
  
  # create igraph
  g1 <- igraph::graph_from_adjacency_matrix(
    as.matrix(network),
    mode = "directed",
    weighted = TRUE,
    diag = TRUE,
    add.colnames = NULL,
    add.rownames = NA)
  
  igraph::V(g1)$label <- names(lines)
  igraph::V(g1)$lines <- lines
  # igraph::V(g1)$folder <- all_folder
  # igraph::V(g1)$color  <- as.numeric(as.factor(all_folder))
  
  # output
  return(as_tidygraph(g1))
}

# nocov end
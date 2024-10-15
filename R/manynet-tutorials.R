# Tutorials overview ####

#' Open and extract code from tutorials
#' 
#' @description 
#'   These functions make it easy to use the tutorials
#'   in the `{manynet}` and `{migraph}` packages:
#'   
#'   - `run_tute()` runs a `{learnr}` tutorial from 
#'   either the `{manynet}` or `{migraph}` packages,
#'   wraps `learnr::run_tutorial()` with some convenience.
#'   - `extract_tute()` extracts and opens just the solution code
#'   from a `{manynet}` or `{migraph}` tutorial,
#'   saving the .R script to the current working directory.
#'   
#' @param tute String, name of the tutorial (e.g. "tutorial2").
#' @importFrom dplyr %>% as_tibble select tibble
#' @name tutorials
NULL

#' @rdname tutorials 
#' @examples
#' #run_tute("tutorial2")
#' @export
run_tute <- function(tute) {
  thisRequires("learnr")
  stocnet <- c("manynet", "migraph")
  avail_pkgs <- stocnet[suppressWarnings(unlist(lapply(stocnet, function(x) nzchar(system.file(package = x)))))]
  if (missing(tute)) {
    tutelist <- lapply(cli::cli_progress_along(avail_pkgs, 
                                               name = "Checking tutorials in stocnet packages"), function(p){
      dplyr::as_tibble(learnr::available_tutorials(package = avail_pkgs[p]),
                       silent = TRUE) %>% dplyr::select(1:3)
    })
    dplyr::bind_rows(tutelist) %>% dplyr::arrange(name) %>% print()
    cli::cli_alert_info(paste(cli::col_grey("You can run one of these tutorials by typing e.g"), 
                              "`run_tute('tutorial1')`", cli::col_grey("or"), "`run_tute('Data')`", 
                              cli::col_grey("into the console.")))
  } else {
    try(learnr::run_tutorial(tute, "manynet"), silent = TRUE)
    try(learnr::run_tutorial(tute, "migraph"), silent = TRUE)
    cli::cli_alert_info("Didn't find a direct match, so looking for close matches...")
    tutelist <- lapply(cli::cli_progress_along(avail_pkgs, 
                                               name = "Checking tutorials in stocnet packages"), function(p){
      dplyr::as_tibble(learnr::available_tutorials(package = avail_pkgs[p]),
                       silent = TRUE) %>% dplyr::select(1:3)
    })
    avails <- dplyr::bind_rows(tutelist)
    inftit <- grepl(tute, avails$title, ignore.case = TRUE)
    if(!any(inftit) | sum(inftit)>1)
      inftit <- which.min(utils::adist(tute, avails$title, ignore.case = TRUE,
                                       costs = list(ins=0, del=1, sub=1)))
    if(any(inftit) & sum(inftit)==1){
      cli::cli_alert_success("And found one!")
      try(learnr::run_tutorial(avails$name[inftit], avails$package[inftit]), silent = TRUE)
    } else{
      cli::cli_alert_warning("...and couldn't find which one you meant. Please specify one of these titles:\n")
      print(avails)
    }
  }
}

#' @rdname tutorials 
#' @examples
#' #extract_tute("tutorial2")
#' @export
extract_tute <- function(tute) {
  if (missing(tute)) {
    thisRequires("learnr")
    stocnet <- c("manynet", "migraph")
    avail_pkgs <- stocnet[suppressWarnings(unlist(lapply(stocnet, function(x) nzchar(system.file(package = x)))))]
    tutelist <- lapply(cli::cli_progress_along(avail_pkgs, 
                                               name = "Checking tutorials in stocnet packages"), function(p){
                                                 dplyr::as_tibble(learnr::available_tutorials(package = avail_pkgs[p]),
                                                                  silent = TRUE) %>% dplyr::select(1:3)
                                               })
    dplyr::bind_rows(tutelist) %>% dplyr::arrange(name) %>% print()
    cli::cli_alert_info(paste(cli::col_grey("You can extract the code from one of these tutorials by typing e.g"), 
                              "`extract_tute('tutorial1')`", cli::col_grey("into the console.")))
  } else {
    thisRequires("knitr")
    pth <- file.path(path.package("manynet"), "tutorials", tute)
    if(!dir.exists(pth)) {
      thisRequires("migraph")
      pth <- gsub("manynet", "migraph", pth)
    }
    knitr::purl(file.path(pth, list.files(pth, pattern = "*.Rmd")),
                documentation = 1)
    utils::file.edit(gsub(".Rmd", ".R", list.files(pth, pattern = "*.Rmd")))
  }
}

# Data overview ####

#' Obtain overview of available network data
#' 
#' @description 
#'   This function makes it easy to get an overview of available data:
#'   
#'   - `table_data()` returns a tibble with details of the
#'   network datasets included in the packages.
#'   
#' @param pkg String, name of the package.
#' @importFrom dplyr %>% as_tibble select tibble
#' @name data_overview
NULL

#' @rdname data_overview 
#' @param ... Network marks, e.g. directed, twomode, or signed,
#'   that are used to filter the results.
#' @examples
#' table_data()
#' # to obtain list of all e.g. directed networks:
#' table_data(pkg = "manynet", directed)
#' # to obtain overview of unique datasets:
#' table_data() %>% 
#'   dplyr::distinct(directed, weighted, twomode, signed, 
#'                  .keep_all = TRUE)
#' @export
table_data <- function(pkg = c("manynet","migraph"),
                       ...) {
  nodes <- NULL
  pkg <- intersect(pkg, rownames(utils::installed.packages()))
  out <- lapply(pkg, function(x){
    datanames <- utils::data(package = x)$results[,"Item"]
    require(package = x, character.only = TRUE)
    datasets <- lapply(datanames, function(d) get(d))
    datanames <- datanames[!vapply(datasets, is_list, logical(1))]
    datasets <- datasets[!vapply(datasets, is_list, logical(1))]
    dplyr::tibble(dataset = tibble::char(datanames, min_chars = 18),
                         nodes = vapply(datasets, net_nodes, numeric(1)),
                         ties = vapply(datasets, net_ties, numeric(1)),
                         nattr = vapply(datasets, 
                                        function (x) length(net_node_attributes(x)), 
                                        numeric(1)),
                         tattr = vapply(datasets, 
                                        function (x) length(net_tie_attributes(x)), 
                                        numeric(1)),
                         directed = vapply(datasets, 
                                           is_directed, 
                                           logical(1)),
                         weighted = vapply(datasets, 
                                           is_weighted, 
                                           logical(1)),
                         twomode = vapply(datasets, 
                                          is_twomode, 
                                          logical(1)),
                         labelled = vapply(datasets, 
                                           is_labelled, 
                                           logical(1)),
                         signed = vapply(datasets, 
                                         is_signed, 
                                         logical(1)),
                         multiplex = vapply(datasets, 
                                            is_multiplex, 
                                            logical(1)),
                         acyclic = vapply(datasets, 
                                          is_acyclic, 
                                          logical(1)),
                         attributed = vapply(datasets, 
                                             is_attributed, 
                                             logical(1)))
    
  })
  out <- dplyr::bind_rows(out) %>% dplyr::arrange(nodes)
  if(!is.null(filter)) out <- dplyr::filter(out, ...)
  out
}

# Glossary ####


#' Adding network glossary items
#' 
#' @description 
#'   This function adds a glossary item, useful in tutorials.
#'   
#' @param text The text to appear.
#' @param ref The name of the glossary item to index.
#'   If NULL, then the function will search the glossary for 'text' instead.
#' @name glossary
NULL

#' @rdname glossary
#' @export
gloss <- function(text, ref = NULL){
  if(is.null(ref)) ref <- tolower(text)
  if(!ref %in% names(glossies)) 
    cli::cli_abort("No glossary entry for '{text}' exists.") else {
      defn <- glossies[which(names(glossies)==ref)]
      options(mnet_glossary = unique(c(ref, getOption("mnet_glossary", default = ""))))
      paste(paste0("<abbr><dfn title='", defn, "'>"), text, "</dfn></abbr>")
    }
}

glossies <- list(
  component = "A component is a connected subgraph not part of a larger connected subgraph.",
  giant = "The giant component is the component that includes the most nodes in the network",
  undirected = "An undirected network is one in which tie direction is undefined.",
  reciprocity = "A measure of how often nodes in a directed network are mutually linked.",
  transitivity = "Triadic closure is where if the connections A-B and A-C exist among three nodes, there is a tendency for B-C also to be formed."
)

# print_glossary(){
#   
# }

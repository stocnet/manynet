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
table_data <- function(..., pkg = c("manynet","migraph")) {
  nodes <- NULL
  pkg <- intersect(pkg, rownames(utils::installed.packages()))
  out <- lapply(pkg, function(x){
    datanames <- utils::data(package = x)$results[,"Item"]
    require(package = x, character.only = TRUE)
    datasets <- lapply(datanames, function(d) get(d))
    datanames <- datanames[!vapply(datasets, is_list, logical(1))]
    datasets <- datasets[!vapply(datasets, is_list, logical(1))]
    dplyr::tibble(dataset = tibble::char(datanames, min_chars = 18),
                  components = vapply(datasets, net_components, numeric(1)),
                  nodes = vapply(datasets, net_nodes, numeric(1)),
                         ties = vapply(datasets, net_ties, numeric(1)),
                         nattr = vapply(datasets, 
                                        function (x) length(net_node_attributes(x)), 
                                        numeric(1)),
                         tattr = vapply(datasets, 
                                        function (x) length(net_tie_attributes(x)), 
                                        numeric(1)),
                         directed = as.logi(vapply(datasets, 
                                           is_directed, 
                                           logical(1))),
                         weighted = as.logi(vapply(datasets, 
                                           is_weighted, 
                                           logical(1))),
                         twomode = as.logi(vapply(datasets, 
                                          is_twomode, 
                                          logical(1))),
                  labelled = as.logi(vapply(datasets, 
                                           is_labelled, 
                                           logical(1))),
                         signed = as.logi(vapply(datasets, 
                                         is_signed, 
                                         logical(1))),
                         multiplex = as.logi(vapply(datasets, 
                                            is_multiplex, 
                                            logical(1))),
                  longitudinal = as.logi(vapply(datasets, 
                                           is_longitudinal, 
                                           logical(1))),
                  dynamic = as.logi(vapply(datasets, 
                                             is_dynamic, 
                                             logical(1))),
                  changing = as.logi(vapply(datasets, 
                                             is_changing, 
                                             logical(1))),
                  acyclic = as.logi(vapply(datasets, 
                                          is_acyclic, 
                                          logical(1))),
                         attributed = as.logi(vapply(datasets, 
                                             is_attributed, 
                                             logical(1))))
    
  })
  out <- dplyr::bind_rows(out) %>% dplyr::arrange(nodes)
  if(!is.null(filter)) out <- dplyr::filter(out, ...)
  # out <- apply(out, 2, function(x) ifelse(is.logical(x), as.logi(x), x))
  out
}

as.logi <- function(x){
  class(x) <- c("logi", class(x))
  x
}

#' @noRd
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.logi <- function(x, ...) {
  pillar::new_pillar_shaft_simple(ifelse(x, pillar::style_bold(x),
                                         pillar::style_na(x)), align = "left")
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
    snet_abort("No glossary entry for '{text}' exists.") else {
      defn <- glossies[which(names(glossies)==ref)]
      options(mnet_glossary = unique(c(ref, getOption("mnet_glossary", default = ""))))
      paste(paste0("<abbr><dfn title='", defn, "'>"), text, "</dfn></abbr>")
    }
}

#' @rdname glossary
#' @export
print_glossary <- function(){
  defns <- getOption("mnet_glossary", default = "")
  if(length(defns)!=0){
    glossd <- glossies[names(glossies) %in% defns]
    glossn <- gsub("([[:alpha:]])([[:alpha:]]+)", "\\U\\1\\L\\2", names(glossd), perl=TRUE)
    glosst <- data.frame(term = paste("<dt>",glossn,"</dt>"), 
                         defn = paste("<dd>    ",glossd,"</dd>"))
    paste("<dl>",paste(paste(glosst$term, glosst$defn), collapse = " "),"</dl>")
  }
}

#' @rdname glossary
#' @export
clear_glossary <- function(){
  options(mnet_glossary = vector())
}

## Definitions ####

glossies <- list(
  acyclic = "An acyclic network is a network without any cycles.",
  aperiodic = "An aperiodic network is a network where the greatest common divisor of the lengths of its cycles is one.",
  adhesion = "The minimum number of ties to remove to increase the number of components.",
  anti = "An anti-tie, -edge, or antigraph is the complement of a network.",
  arc = "An ordered pair of nodes indicating a directed tie or edge from a tail to a head.",
  blockmodel = "A blockmodel reduces a network to a smaller comprehensible structure of the roles positions take with respect to one another.",
  bridge = "A bridge or isthmus is a tie whose deletion increases the number of components.",
  circumference = "A network's circumference is the length of its longest simple cycle.",
  clique = "A clique is a set of mutually adjacent nodes.",
  cohesion = "The minimum number of nodes to remove to increase the number of components.",
  complete = "A complete network is one where all ties that could exist are present.",
  component = "A component is a connected subgraph not part of a larger connected subgraph.",
  connected = "A connected network is one with a single (strong) component.",
  core = "A core-periphery is a bipartition of nodes into maximally dense and sparse blocks.",
  coreness = "A k-core is the induced subgraph formed by removing all nodes of degree less than k following earlier removals.",
  cutpoint = "A cutpoint or articulation point is a node whose deletion increases the number of components.",
  cycle = "A simple cycle is a closed walk without any repeated nodes.",
  degree = "A node's degree is the number of connections it has.",
  density = "The density of a network is the proportion of possible ties that are present.",
  diameter = "A network's diameter is the maximum length of any shortest path.",
  dominating = "A dominating set is the set of nodes that includes or is adjacent to every node in the network.",
  empty = "An empty network is a network without any ties.",
  Eulerian = "An Eulerian path is a walk that uses each tie in a network exactly once.",
  geodesic = "A geodesic is a shortest path between two nodes.",
  giant = "The giant component is the component that includes the most nodes in the network.",
  girth = "A network's girth is the length of its shortest cycle.",
  graphlet = "A graphlet is a small, connected, induced, non-isomorphic subgraphs.",
  height = "The height of a network is the maximum length of any directed path.",
  hit = "A herd immunity threshold is the proportion of the population that would need to be immune for a disease to cease being endemic.",
  independent = "An independent set is a set of nodes that induces an empty subgraph.",
  induced = "An induced subgraph comprises all ties in a subset of the nodes in a network.",
  internal = "An internal node is a node with degree larger than one.",
  intersection = "The intersection of two networks is their largest common subgraph.",
  intervention = "A network intervention is a process to accelerate behavioural change or improve performance in a network.",
  isolate = "An isolate is a node with degree equal to zero.",
  label = "A labelled network includes unique labels for each node (or ties) in the network.",
  lattice = "A network that can be drawn as a regular tiling.",
  loop = "A loop is a self-tie with a single node as both endpoints, forming a cycle of length 1.",
  LTM = "A linear threshold model is a diffusion model where nodes are resistant to activation up to some threshold.",
  matching = "A matching is a set of ties that do not share any nodes.",
  mixed = "A mixed network is a network that includes both directed and undirected ties.",
  modularity = "The modularity of a membership assignment is the difference of the cross-cluster ties from the expected value.",
  motif = "A subgraph that is exceptional or significant compared to a null model.",
  multilevel = "A network of more than one set of nodes that includes ties both between and within the different node sets.",
  multimodal = "A network that includes more than one set of nodes.",
  multiplex = "A network that includes multiple types of tie.",
  neighborhood = "A node's neighborhood is the set of other nodes to which that node is connected.",
  network = "A network comprises a set of nodes/vertices, a set of ties/edges among them, and usually some node or tie attributes.",
  orbit = "An orbit is a unique position in a subgraph.",
  order = "The order of a network is the number of its nodes.",
  path = "A path is a walk without repeated nodes.",
  pendant = "A pendant or leaf node has a degree of 1.",
  planar = "A planar graph has an embedding onto a Euclidean plane.",
  property = "A network property depends only on the structure and not on any labelled or attribute information.",
  radius = "A network's radius is the minimum eccentricity of any node.",
  reachability = "The ability of reach one node from another in a network.",
  reciprocity = "A measure of how often nodes in a directed network are mutually linked.",
  reduced = "A reduced graph is a contraction of a network into the ties within and between blocks.",
  regular = "A k-regular network includes only nodes with degree of k.",
  simplex = "A simplex network is one without loops or multiple adjacencies.",
  sink = "A sink is a node without outgoing ties.",
  size = "The size of a network is its number of ties.",
  smallworld = "A small-world network is a network where most nodes are not neighbours but can reach each other in a small number of steps.",
  source = "A source is a node without incoming ties.",
  spanning = "A spanning subgraph includes all nodes from the original network.",
  sparse = "A sparse network has relatively few ties for its number of nodes.",
  star = "A star network has one internal, dominating, universal node.",
  strength = "A network's strength is the minimum ratio of removed ties to components created.",
  subgraph = "A subgraph comprises a subset of the nodes and ties in a network.",
  supergraph = "A supergraph is formed by adding nodes, ties, or both to a network.",
  toughness = "A network's strength is the minimum ratio of removed nodes to components created.",
  transitivity = "Triadic closure is where if the connections A-B and A-C exist among three nodes, there is a tendency for B-C also to be formed.",
  threshold = "A threshold is a limit over which behaviour is expected to vary.",
  tour = "A closed trail, a walk without repeated ties.",
  trail = "A walk without repeated ties.",
  transitive = "The transitive closure of a network is the proportion of nodes that are connected by a path of length two that are also connected by a directed tie.",
  triangle = "A cycle of length three in a network.",
  undirected = "An undirected or line network is one in which tie direction is undefined.",
  universal = "A universal, dominating, or apex node is adjacent to every other node in the network.",
  unweighted = "An unweighted network is where the ties have not been assigned weights.",
  volume = "The sum of the degrees of a set of nodes.",
  walk = "A walk is a sequence of ties that joins a sequence of nodes.",
  weighted = "A weighted network is where the ties have been assigned weights."
)


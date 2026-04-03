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
#' @importFrom dplyr as_tibble select tibble
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
#' table_data() |> 
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
                  # components = vapply(datasets, net_components, numeric(1)),
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
  out <- dplyr::bind_rows(out) |> dplyr::arrange(nodes)
  if(!is.null(filter)) out <- dplyr::filter(out, ...)
  # out <- apply(out, 2, function(x) ifelse(is.logical(x), as.logi(x), x))
  out
}


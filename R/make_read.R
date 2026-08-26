# Read ####
# nocov start
#' Making networks from external files
#'
#' @description 
#'   Researchers regularly need to work with a variety of external data formats.
#'   The following functions enable importing from some common external file 
#'   formats into objects that `{manynet}` and other graph/network packages in R 
#'   can work with:
#' 
#'   - `read_matrix()` imports adjacency matrices from Excel/csv files.
#'   - `read_edgelist()` imports edgelists from Excel/csv files.
#'   - `read_nodelist()` imports nodelists from Excel/csv files.
#'   - `read_pajek()` imports Pajek (.net or .paj) files.
#'   - `read_ucinet()` imports UCINET files from the header (.##h).
#'   - `read_dynetml()` imports DyNetML interchange format for rich social network data.
#'   - `read_graphml()` imports GraphML files,
#'   including those exported by Network Canvas.
#'   - `read_gml()` imports GML files.
#'   - `read_gdf()` imports GDF files.
#'   - `read_gexf()` imports GEXF files, such as those exported by Gephi.
#' @param file A character string with the system path to the file to import.
#'   If left unspecified, an OS-specific file picker is opened to help users select it.
#'   Note that in `read_ucinet()` the file path should be to the header file (.##h),
#'   if it exists and that it is currently not possible to import multiple
#'   networks from a single UCINET file. Please convert these one by one.
#' @param sv Allows users to specify whether their csv file is
#'   `"comma"` (English, the default) or `"semi-colon"` (European) separated.
#' @param ... Additional parameters passed to the read/write function.
#' @return `read_edgelist()` and `read_nodelist()` import a list rather than a
#'   network, so they return a tibble,
#'   which can then be coerced or combined into a network from there.
#'
#'   Every other `read_*()` function returns a stocnet object (see
#'   [make_stocnet()]).
#'   This is the class that holds the most of what a file can contain,
#'   such as the network's metadata, more than two modes, several layers,
#'   and the times at which nodes and ties are present.
#'   Note that a network can be coerced into any other format
#'   with `{manynet}`'s `as_` methods.
#' @family makes
#' @details There are a number of repositories for network data
#'   that hold various datasets in different formats. See for example:
#'
#'   - [networkdata](https://schochastics.github.io/networkdata/)
#'   - [GML datasets](http://www-personal.umich.edu/~mejn/netdata/)
#'   - [SNAP Stanford Large Network Dataset Collection](http://snap.stanford.edu/data/)
#'   - [Colorado Index of Complex Networks](https://icon.colorado.edu)
#'
#'   Please let us know if you identify any further repositories
#'   of social or political networks and we would be happy to add them here.
#'
#'   The `_ucinet` functions only work with relatively recent UCINET
#'   file formats, e.g. type 6406 files.
#'   To import earlier UCINET file types, you will need to update them first.
#'   To import multiple matrices packed into a single UCINET file,
#'   you will need to unpack them and convert them one by one.
#'   
#'   `read_graphml()` reads the file itself rather than relying on igraph,
#'   so that keys declared `for="all"` and files holding more than one graph
#'   are read rather than quietly discarded.
#'   Where several graphs are present, they are combined into a single network
#'   and distinguished by a 'graph' node attribute.
#'
#'   Network Canvas exports are recognised by their namespace and read
#'   accordingly.
#'   Each interview session is exported as a separate graph, so the sessions are
#'   combined into one network in which each session is a component.
#'   Since ego is recorded at the level of the graph rather than as a node,
#'   and ego-alter ties are left implicit,
#'   ego is added as a node with ties to each of its alters,
#'   and every tie records the ego that reported it in a 'by' column,
#'   making the result a cognitive social structure (see [is_cognitive()]).
#'   Use `ego = FALSE` to keep just the alters and the ties between them.
#'   Note that node types are reported in a character 'nodeset' column rather
#'   than a logical 'type' column, because ego networks are not two-mode:
#'   ties within a nodeset are exactly what is collected.
#'   Note too that, since alters are particular to a session,
#'   the sessions share no nodes, so `as_matrix()` on such a network returns a
#'   large and very sparse three-dimensional array.
#'
#'   `read_gexf()` reads the node and tie attributes declared in the file,
#'   as well as the visualisation elements Gephi adds,
#'   so that positions are available as 'x' and 'y' node attributes,
#'   and sizes and colours as 'size' and 'color'.
#'   Nodes are named from their labels where the file gives them,
#'   since node ids are required by the format and so are an export artefact.
#'   Dynamic files are read with their 'start' and 'end' times,
#'   which makes the result a dynamic network (see [is_dynamic()]).
#'   Where a directed network contains ties declared undirected or mutual,
#'   those ties are reciprocated, since a network is directed or not as a whole.
#' @source
#' `read_ucinet()` kindly supplied by Christian Steglich,
#' constructed on 18 June 2015.
#' @importFrom utils read.csv read.csv2 read.table
#' @name make_read
#' @seealso [as]
NULL

#' @rdname make_read 
#' @export
read_matrix <- function(file = file.choose(),
                        sv = c("comma", "semi-colon"),
                        ...) {
  if(missing(file)) cli::cli_alert_success("Executing: read_matrix('{file}')")
  sv <- match.arg(sv)
  if(!grepl("\\.csv$|\\.xlsx$|\\.xls$", file)) file <- paste0(file, ".csv")
  if (grepl("csv$", file)) {
    if (sv == "comma") {
      out <- read.csv(file, ...) # For US
    } else {
      out <- read.csv2(file, ...) # For EU
    }
  } else if (grepl("xlsx$|xls$", file)) {
    thisRequires("readxl")
    out <- readxl::read_excel(file, ...)
  }
  if((dim(out)[1]+1) == dim(out)[2])
    out <- out[,-1]
  if(!is.null(colnames(out)) & 
     all(colnames(out) == paste0("X",seq_along(colnames(out)))))
    colnames(out) <- NULL
  if(!is.null(colnames(out)) & is.null(rownames(out)) &
     dim(out)[1] == dim(out)[2])
    rownames(out) <- colnames(out)
  out <- as.matrix(out)
  if(is.null(rownames(out)) && colnames(out)[1] == "V1") colnames(out) <- NULL
  as_stocnet(out)
}

#' @rdname make_read 
#' @export
read_edgelist <- function(file = file.choose(),
                          sv = c("comma", "semi-colon"),
                          ...) {
  if(missing(file)) cli::cli_alert_success("Executing: read_edgelist('{file}')")
  sv <- match.arg(sv)
  if(!grepl("\\.csv$|\\.xlsx$|\\.xls$", file)) file <- paste0(file, ".csv")
  if (grepl("csv$", file)) {
    if (sv == "comma") {
      out <- read.csv(file, header = TRUE, ...) # For US
    } else {
      out <- read.csv2(file, header = TRUE, ...) # For EU
    }
  } else if (grepl("xlsx$|xls$", file)) {
    thisRequires("readxl")
    out <- readxl::read_excel(file, ...)
  }
  out
}

#' @rdname make_read
#' @export
read_nodelist <- function(file = file.choose(),
                          sv = c("comma", "semi-colon"),
                          ...) {
  if(missing(file)) cli::cli_alert_success("Executing: read_nodelist('{file}')")
  sv <- match.arg(sv)
  if(!grepl("\\.csv$|\\.xlsx$|\\.xls$", file)) file <- paste0(file, ".csv")
  if (grepl("csv$", file)) {
    if (sv == "comma") {
      out <- read.csv(file, header = TRUE, ...) # For US
    } else {
      out <- read.csv2(file, header = TRUE, ...) # For EU
    }
  } else if (grepl("xlsx$|xls$", file)) {
    thisRequires("readxl")
    out <- readxl::read_excel(file, ...)
  }
  out
}

#' @rdname make_read
#' @param ties A character string indicating the ties/network,
#'   where the data contains several.
#' @importFrom network read.paj
#' @importFrom utils read.delim
#' @export
read_pajek <- function(file = file.choose(), 
                       ties = NULL,
                       ...) {
  if(missing(file)) cli::cli_alert_success("Executing: read_pajek('{file}')")
  if(!grepl("\\.paj$", file)) file <- paste0(file, ".paj")
  paj <- network::read.paj(file, ...)
  if(!is.network(paj)){
    if(is.null(ties)) 
      snet_abort(paste("This file contains multiple networks/ties.",
                 "Please choose a set of ties for the imported network among:\n",
                 paste0("- '", names(paj$networks), "'", collapse = "\n "),
                 "\n by adding the name as a character string to the `ties = ` argument"))
    out <- paj[[1]][[ties]]
    if("partitions" %in% names(paj)){
      for(x in names(paj$partitions)){
        out <- igraph::set_vertex_attr(out, name = gsub(".clu","",x),
                                       value = paj$partitions[,x])
      }
    }
    out <- as_tidygraph(out)
  } else {
    out <- as_tidygraph(paj)
  }
  # if(grepl("Partition", utils::read.delim(file))){
  #   clus <- strsplit(paste(utils::read.delim(file)), "\\*")[[1]]
  #   clus <- clus[grepl("^Vertices|^Partition", clus)][-1]
  #   if(length(clus) %% 2 != 0) snet_abort("Unexpected .pajek file structure.")
  #   namo <- clus[c(TRUE, FALSE)]
  #   attr <- clus[c(FALSE, TRUE)]
  #   for (i in seq_len(namo)){
  #     vct <- strsplit(attr[i], ",")[[1]][-1]
  #     vct <- gsub("\"", "", vct)
  #     vct <- gsub(" ", "", vct, fixed = TRUE)
  #     vct <- vct[!grepl("^$", vct)]
  #     if(all(grepl("^-?[0-9.]+$", vct))) vct <- as.numeric(vct)
  #     out <- set_vertex_attr(out, name = strsplit(namo[i], " |\\.")[[1]][2],
  #                            value = vct)
  #   }
  # }
  as_stocnet(out)
}

#' @rdname make_read
#' @export
read_ucinet <- function(file = file.choose()) {
  if(missing(file)) cli::cli_alert_success("Executing: read_ucinet('{file}')")
  # Some basic checks of the input file
  # Check if the file is a UCINET header file
  if(!grepl("\\.##h$", file)) file <- paste0(file, ".##h")
  if (!grepl("\\.##h$", file)) {
    snet_abort("Please select the UCINET header file with the
                                  '.##h' extension.")
  } # Continue if header file is selected
  # Check whether there is a data file to be imported in the same folder as the
  # hearder file.
  if (!(file.exists(sub("h$", "d", file)))) snet_abort("UCINET data file not found.
                                                 Please add the '.##d' file in
                                                 the same folder as the header
                                                 file you are trying to
                                                 import. It should also have
                                                 the same name as the header
                                                 file.")
  read_ucinet_header <- function(header_file) {
    UCINET.header <- file(header_file, "rb")
    ignore <- readBin(UCINET.header, what = "int", size = 1)
    headerversion <- paste(
      rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
      rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
      rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
      rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
      rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
      sep = ""
    )
    # Check for correct UCINET version
    if (!(headerversion %in% c("DATE:", "V6404"))) {
      close(UCINET.header)
      snet_abort(paste("Unknown header type; try more recent UCINET file types"))
    }
    # Get ymd and weekday of the UCINET file
    year <- 2000 + readBin(UCINET.header, what = "int", size = 2)
    month <- c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
      "Sep", "Oct", "Nov", "Dec"
    )[readBin(UCINET.header, what = "int", size = 2)]
    day <- readBin(UCINET.header, what = "int", size = 2)
    dow <- c(
      "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
      "Saturday", "Sunday"
    )[readBin(UCINET.header, what = "int", size = 2)]
    labtype <- readBin(UCINET.header, what = "int", size = 2)
    infile.dt <- c(
      "nodt", "bytedt", "booleandt", "shortintdt", "worddt",
      "smallintdt", "longintdt", "singledt", "realdt", "doubledt",
      "compdt", "extendeddt", "labeldt", "setdt", "stringdt", "pointerdt",
      "chardt", "integerdt", "nodelistdt", "sparsedt", "int64dt"
    )[
      readBin(UCINET.header, what = "int", size = 1)
    ]
    # Get the dimensions of the matrix
    ndim <- readBin(UCINET.header, what = "int", size = 2)
    if (headerversion == "V6404") {
      fct <- 2
    } else {
      fct <- 1
    }
    dims <- c(
      readBin(UCINET.header, what = "int", size = 2 * fct),
      readBin(UCINET.header, what = "int", size = 2 * fct)
    )
    if (ndim == 3) {
      dims[3] <- readBin(UCINET.header, what = "int", size = 2 * fct)
    }
    # Check if user tries to import multiple networks at once.
    # This check fails if it is a time series or multilevel network.
    if (!(ndim == 2 | ndim == 3 & dims[3] == 1)) {
      close(UCINET.header)
      snet_abort(paste("UCINET file with", dims[3], "levels; please convert separately"))
    }
    # Extract the title of the UCINET network
    t.length <- readBin(UCINET.header, what = "int", size = 1)
    if (t.length > 0) {
      titl <- vapply(seq_len(t.length), function(i) {
        rawToChar(readBin(UCINET.header, what = "raw", size = 1))
      }, FUN.VALUE = character(1))
      titl <- paste(titl, collapse = "")
    } else {
      titl <- ""
    }
    haslab <- c(
      readBin(UCINET.header, what = "logical", size = 1),
      readBin(UCINET.header, what = "logical", size = 1)
    )
    if (ndim == 3) {
      haslab[3] <- readBin(UCINET.header, what = "logical", size = 1)
    }
    dim.labels <- list()
    for (arr.dim in seq_along(dims)) {
      if (haslab[arr.dim]) {
        dim.labels[[arr.dim]] <- rep(NA, dims[arr.dim])
        for (i in seq_len(dims[arr.dim])) {
          lab <- ""
          lablen <- readBin(UCINET.header, what = "int", size = 2)
          for (let in seq_len(lablen)) {
            lab <- paste(lab,
                         rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
                         sep = ""
            )
          }
          dim.labels[[arr.dim]][i] <- lab
        }
      }
    }
    # Close file connection
    close(UCINET.header)
    if (ndim == 3 & dims[3] == 1) {
      titl <- dim.labels[[3]][1]
      # warning(paste('UCINET file with one level; level name "',
      # 	titl,'" treated as network name',sep=''))
      ndim <- 2
      dims <- dims[1:2]
      haslab <- haslab[1:2]
      dim.labels <- dim.labels[1:2]
    }
    list(
      headerversion = headerversion,
      date = paste(dow, paste(day, month, year, sep = "-")),
      labtype = labtype,
      infile.dt = infile.dt,
      ndim = ndim,
      dims = dims,
      titl = titl,
      haslab = haslab,
      dim.labels = dim.labels
    )
  }
  # Start of main function code:
  header <- read_ucinet_header(file)
  file <- sub(".##h", "", file)
  # Read in the actual data file ".##d"
  UCINET.data <- file(paste(file, ".##d", sep = ""), "rb")
  thedata <- vector()
  for (i in 1:(header$dims[1] * header$dims[2])) {
    thedata[i] <- readBin(UCINET.data,
                          what = "numeric",
                          size = 4,
                          endian = "little"
    )
  }
  close(UCINET.data)
  # Build the adjacency matrix
  mat <- matrix(thedata,
                nrow = header$dims[2],
                ncol = header$dims[1],
                dimnames = header$dim.labels[c(2, 1)],
                byrow = TRUE
  )
  # put additional info from header file on matrix
  if (!(is.null(header$title))) {
    attr(mat, "title") <- header$title
  }
  attr(mat, "date") <- header$date
  # attr(mat,'labtype') <- header$labtype
  # attr(mat,'infile.dt') <- header$infile.dt
  # Convert the adjacency matrix to a stocnet object
  as_stocnet(mat)
}

#' @rdname make_read 
#' @importFrom dplyr bind_rows coalesce filter mutate select everything
#' @export
read_dynetml <- function(file = file.choose()) {
  if(missing(file)) cli::cli_alert_success("Executing: read_dynetml('{file}')")
  if(!grepl("\\.xml$", file, ignore.case = TRUE)) file <- paste0(file, ".xml")
  thisRequires("xml2")
  name <- type <- nodeset <- target <- value <- NULL
  xmlfile <- xml2::read_xml(file)
  xmllist <- xml2::as_list(xmlfile)
  # Getting nodeset
  # to deal with legacy constructions:
  if("MetaMatrix" %in% names(xmllist$DynamicNetwork))
    nodesets <- xmllist$DynamicNetwork$MetaMatrix$nodes else
      nodesets <- xmllist$DynamicNetwork$MetaNetwork$nodes
  nodesets <- dplyr::coalesce(unlist(lapply(nodesets, 
                                            function(x) ifelse(is.null(attr(x, "id")),
                                                               NA_character_, attr(x, "id")))),
                              unlist(lapply(nodesets, 
                                            function(x) ifelse(is.null(attr(x, "type")),
                                                               NA_character_, attr(x, "type")))))
  # to deal with legacy constructions:
  if("MetaMatrix" %in% names(xmllist$DynamicNetwork)){
    nodesets <- unname(rep(nodesets, vapply(xmllist$DynamicNetwork$MetaMatrix$nodes,
                                            function(x) length(x), numeric(1))))
  } else
    nodesets <- unname(rep(nodesets, vapply(xmllist$DynamicNetwork$MetaNetwork$nodes,
                                            function(x) length(x), numeric(1)))) 
  # Getting nodes
  nodes <- xml2::as_list(xml2::xml_find_all(xmlfile, ".//node"))
  nodes <- dplyr::bind_rows(lapply(nodes, function(x){
    values <- sapply(x$properties, function(y) attr(y, "value"))
    attrs <- sapply(x$properties, function(y) attr(y, "name"))
    names(values) <- attrs
    c(name = attr(x, "id"), values)
  }))
  # Add nodeset information if necessary
  if(length(unique(nodesets))==2)
    nodes <- nodes |> dplyr::mutate(type = nodesets == unique(nodesets)[2]) |> 
    dplyr::select(name, type, dplyr::everything()) else if (length(unique(nodesets))>2)
      nodes <- nodes |> dplyr::mutate(nodeset = nodesets) |> 
    dplyr::select(name, nodeset, dplyr::everything())
  
  # Getting edges
  edgelist <- xml2::xml_attrs(xml2::xml_find_all(xmlfile, ".//edge"))
  # to deal with legacy constructions:
  if(length(edgelist)==0) edgelist <- xml2::xml_attrs(xml2::xml_find_all(xmlfile, ".//link"))
  edgelist <- as.data.frame(t(sapply(edgelist, function(x) x, simplify = TRUE)))
  edgelist$type <- NULL
  edgelist$value <- as.numeric(edgelist$value)
  edgelist <- dplyr::filter(edgelist, source %in% nodes$name & target %in% nodes$name)
  edgelist <- dplyr::filter(edgelist, value != 0)
  out <- as_tidygraph(list(nodes = nodes, ties = edgelist))
  net_el <- xml2::xml_find_first(xmlfile, ".//network")
  if(!is.na(xml2::xml_attr(net_el, "isDirected")) &&
     tolower(xml2::xml_attr(net_el, "isDirected")) %in% c("false","0"))
    out <- to_undirected(out)
  as_stocnet(out)
}

#' @rdname make_read
#' @param ego Logical, whether to add ego as a node in ego-centric formats
#'   such as Network Canvas, where ego is otherwise only recorded at the level
#'   of the network. By default TRUE.
#'   Where ego is added, ties from ego to each of its alters are also added,
#'   and every tie gains a 'by' column identifying the ego that reported it,
#'   which makes the network a cognitive social structure (see [is_cognitive()]).
#' @importFrom igraph set_graph_attr
#' @export
read_graphml <- function(file = file.choose(), ego = TRUE) {
  if(missing(file)) snet_success("Executing: read_graphml('{file}')")
  if(!grepl("\\.graphml$", file, ignore.case = TRUE))
    file <- paste0(file, ".graphml")
  thisRequires("xml2")
  xmlfile <- xml2::read_xml(file)
  # Network Canvas is identified by its namespace before it is stripped
  netcanvas <- any(grepl("schema.networkcanvas.com",
                         as.character(xml2::xml_ns(xmlfile)), fixed = TRUE))
  xml2::xml_ns_strip(xmlfile)
  key_map <- .graphml_keys(xmlfile)
  graphs <- xml2::xml_find_all(xmlfile, "/graphml/graph")
  if(length(graphs) == 0) snet_abort("No graphs found in {.file {file}}.")
  parsed <- lapply(graphs, .graphml_parse, key_map = key_map)
  # The builders return an igraph, since they are written against it,
  # and the network is coerced once here, at the boundary of the function.
  if(netcanvas) {
    snet_minor_info("Reading {length(parsed)} Network Canvas session{?s}.")
    as_stocnet(.netcanvas_build(parsed, key_map = key_map, ego = ego))
  } else as_stocnet(.graphml_build(parsed))
}

# Maps each <key> declaration to its human-readable name, type, scope, and
# default. GraphML requires key ids to be NMTOKENs, so exporters such as
# Network Canvas put variable UUIDs in `id` and the readable name in
# `attr.name`; we always report the latter.
.graphml_keys <- function(xmlfile) {
  keys <- xml2::xml_find_all(xmlfile, "/graphml/key")
  if(length(keys) == 0) return(list())
  ids <- xml2::xml_attr(keys, "id")
  nms <- xml2::xml_attr(keys, "attr.name")
  nms[is.na(nms)] <- ids[is.na(nms)]
  typs <- xml2::xml_attr(keys, "attr.type")
  typs[is.na(typs)] <- "string"
  fors <- xml2::xml_attr(keys, "for")
  fors[is.na(fors)] <- "all"
  defs <- vapply(keys, function(k) {
    d <- xml2::xml_find_first(k, "./default")
    if(inherits(d, "xml_missing")) NA_character_ else xml2::xml_text(d)
  }, character(1))
  out <- lapply(seq_along(ids), function(i)
    list(name = nms[i], type = typs[i], scope = fors[i], default = defs[i]))
  stats::setNames(out, ids)
}

.graphml_cast <- function(x, type) {
  switch(type,
         boolean = ,
         bool = ifelse(is.na(x), NA, tolower(trimws(x)) %in% c("true", "1")),
         int = ,
         integer = ,
         long = suppressWarnings(as.integer(x)),
         float = ,
         double = suppressWarnings(as.numeric(x)),
         as.character(x))
}

# Collects the <data> children of a set of elements into a data frame,
# one column per applicable <key>. Note that keys declared for="all" apply to
# both nodes and ties; igraph's reader discards these entirely.
.graphml_data <- function(els, key_map, scope) {
  if(length(els) == 0 || length(key_map) == 0) return(NULL)
  valid <- names(key_map)[vapply(key_map, function(k)
    k$scope %in% c("all", scope), logical(1))]
  if(length(valid) == 0) return(NULL)
  vals <- lapply(els, function(e) {
    d <- xml2::xml_find_all(e, "./data")
    if(length(d) == 0) return(stats::setNames(character(0), character(0)))
    stats::setNames(xml2::xml_text(d), xml2::xml_attr(d, "key"))
  })
  present <- unique(unlist(lapply(vals, names)))
  defaulted <- valid[vapply(key_map[valid], function(k)
    !is.na(k$default), logical(1))]
  used <- union(intersect(valid, present), defaulted)
  if(length(used) == 0) return(NULL)
  cols <- lapply(used, function(k) {
    raw <- vapply(vals, function(v)
      if(k %in% names(v)) v[[k]] else NA_character_, character(1))
    if(!is.na(key_map[[k]]$default)) raw[is.na(raw)] <- key_map[[k]]$default
    .graphml_cast(raw, key_map[[k]]$type)
  })
  names(cols) <- make.unique(vapply(key_map[used], function(k)
    k$name, character(1)))
  data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
}

.graphml_parse <- function(g, key_map) {
  nds <- xml2::xml_find_all(g, "./node")
  eds <- xml2::xml_find_all(g, "./edge")
  list(ids = xml2::xml_attr(nds, "id"),
       nodes = .graphml_data(nds, key_map, "node"),
       ties = .graphml_data(eds, key_map, "edge"),
       from = xml2::xml_attr(eds, "source"),
       to = xml2::xml_attr(eds, "target"),
       info = .graphml_data(list(g), key_map, "graph"),
       meta = xml2::xml_attrs(g),
       directed = !identical(xml2::xml_attr(g, "edgedefault"), "undirected"))
}

# Assembles ordinary (non-Network Canvas) GraphML. Where a file holds several
# graphs, all are retained as components rather than silently dropping all but
# the first, as igraph's reader does.
.graphml_build <- function(parsed) {
  if(length(parsed) > 1) {
    snet_minor_info(paste("Found {length(parsed)} graphs;",
                          "combining them into a single network,",
                          "distinguished by a 'graph' node attribute."))
    for(i in seq_along(parsed)) {
      parsed[[i]]$ids <- paste0(i, ":", parsed[[i]]$ids)
      parsed[[i]]$from <- paste0(i, ":", parsed[[i]]$from)
      parsed[[i]]$to <- paste0(i, ":", parsed[[i]]$to)
      gid <- parsed[[i]]$meta[["id"]] %||% as.character(i)
      parsed[[i]]$nodes <- .bind_col(parsed[[i]]$nodes,
                                     "graph", gid, length(parsed[[i]]$ids))
    }
  }
  ids <- unlist(lapply(parsed, `[[`, "ids"))
  nodes <- dplyr::bind_rows(lapply(parsed, `[[`, "nodes"))
  ties <- dplyr::bind_rows(lapply(parsed, `[[`, "ties"))
  from <- unlist(lapply(parsed, `[[`, "from"))
  to <- unlist(lapply(parsed, `[[`, "to"))
  # Only label the network where the file actually names its nodes;
  # node ids alone are an export artefact, and promoting them to names would
  # spuriously label otherwise unlabelled networks.
  if(is.null(nodes) || ncol(nodes) == 0)
    nodes <- data.frame(row.names = seq_along(ids))
  # Retained for parity with igraph's reader, and so that nodes can be traced
  # back to the file, but only where it says something the names do not.
  if(!"id" %in% names(nodes) && !identical(ids, as.character(seq_along(ids))))
    nodes$id <- ids
  if(any(c("name", "label") %in% names(nodes))) {
    if(!"name" %in% names(nodes)) nodes$name <- nodes$label
    nodes$name <- make.unique(as.character(nodes$name))
    nodes <- nodes[, c("name", setdiff(names(nodes), "name")), drop = FALSE]
    idmap <- stats::setNames(nodes$name, ids)
    el <- data.frame(from = unname(idmap[from]), to = unname(idmap[to]),
                     stringsAsFactors = FALSE)
  } else el <- data.frame(from = match(from, ids), to = match(to, ids))
  if(!is.null(ties) && nrow(ties) == nrow(el))
    el <- cbind(el, ties[, setdiff(names(ties), c("from", "to")), drop = FALSE])
  # Constructed undirected directly rather than via to_undirected(), which
  # collapses ties using igraph's default edge.attr.comb and so would discard
  # every tie attribute read above.
  out <- .graphml_graph(nodes, el,
                        any(vapply(parsed, `[[`, logical(1), "directed")))
  info <- parsed[[1]]$info
  if(!is.null(info)) for(nm in names(info))
    out <- igraph::set_graph_attr(out, nm, info[[1, nm]])
  make_mnet(out)
}

#' @importFrom tidygraph tbl_graph
.graphml_graph <- function(nodes, ties, directed) {
  make_mnet(tidygraph::tbl_graph(nodes = nodes, edges = ties,
                                 directed = directed))
}

.bind_col <- function(df, name, value, n) {
  if(is.null(df)) df <- data.frame(row.names = seq_len(n))
  df[[name]] <- value
  df
}

.rename_col <- function(df, from, to) {
  if(!is.null(df) && from %in% names(df)) names(df)[names(df) == from] <- to
  df
}

# Network Canvas ####

# Network Canvas exports one <graph> per interview session, holding that
# session's alters and the ties between them. Ego is recorded not as a node but
# as <data> on the graph itself, and ego-alter ties are left implicit.
# Since every tie in a session was reported by that session's ego, the sessions
# can be combined into one network in which the reporter of each tie is
# recorded in a 'by' column, i.e. a cognitive social structure.
.netcanvas_build <- function(parsed, key_map, ego = TRUE) {
  sessions <- lapply(seq_along(parsed), function(i)
    .netcanvas_session(parsed[[i]], i, ego))
  nodes <- dplyr::bind_rows(lapply(sessions, `[[`, "nodes"))
  ties <- dplyr::bind_rows(lapply(sessions, `[[`, "ties"))
  # Node ids are only unique within a session, and alters in different sessions
  # are different people even where they share a name, so names are
  # disambiguated before ties are resolved against them.
  nodes$name <- make.unique(as.character(nodes$name))
  ties$from <- nodes$name[match(ties$from, nodes$.key)]
  ties$to <- nodes$name[match(ties$to, nodes$.key)]
  if(ego) {
    ties$by <- match(ties$by, nodes$.key)
    ties <- ties[, c("from", "to", "by",
                     setdiff(names(ties), c("from", "to", "by"))),
                 drop = FALSE]
  }
  nodes$.key <- NULL
  if(!ego) nodes$ego <- NULL
  nodes <- .netcanvas_categories(nodes, key_map)
  nodes <- .netcanvas_layout(nodes)
  nodes <- nodes[, c("name", setdiff(names(nodes), "name")), drop = FALSE]
  out <- .graphml_graph(nodes, ties,
                        any(vapply(parsed, `[[`, logical(1), "directed")))
  if(length(parsed) == 1) {
    meta <- parsed[[1]]$meta
    out <- add_info(out, method = "interview", boundary = "ego",
                    source = "empirical",
                    name = meta[["protocolName"]] %||% NULL)
  }
  out
}

.netcanvas_session <- function(p, i, ego) {
  n <- length(p$ids)
  nodes <- p$nodes
  if(is.null(nodes)) nodes <- data.frame(row.names = seq_len(n))
  nodes$name <- if("name" %in% names(nodes)) as.character(nodes$name) else
    if("label" %in% names(nodes)) as.character(nodes$label) else p$ids
  nodes$label <- NULL
  # Node and tie types are declared for="all" in Network Canvas exports, which
  # is precisely what igraph's reader discards.
  nodes <- .rename_col(nodes, "networkCanvasType", "nodeset")
  nodes <- .rename_col(nodes, "networkCanvasUUID", "uuid")
  nodes$.key <- paste0(i, ":", p$ids)
  nodes$ego <- FALSE
  el <- data.frame(from = paste0(i, ":", p$from), to = paste0(i, ":", p$to),
                   stringsAsFactors = FALSE)
  if(!is.null(p$ties)) {
    p$ties <- .rename_col(p$ties, "networkCanvasType", "type")
    p$ties <- .rename_col(p$ties, "networkCanvasUUID", "uuid")
    el <- cbind(el, p$ties[, setdiff(names(p$ties), c("from", "to")),
                           drop = FALSE])
  }
  if(ego) {
    egokey <- paste0(i, ":ego")
    egorow <- p$info
    if(is.null(egorow)) egorow <- data.frame(row.names = 1L)
    egorow <- .rename_col(egorow, "networkCanvasUUID", "uuid")
    egorow$name <- as.character(egorow$ego_name %||%
                                  p$meta[["caseId"]] %||% paste0("ego", i))
    egorow$ego_name <- NULL
    egorow$.key <- egokey
    egorow$ego <- TRUE
    egorow$nodeset <- "ego"
    nodes <- dplyr::bind_rows(nodes, egorow)
    egoties <- data.frame(from = egokey, to = paste0(i, ":", p$ids),
                          stringsAsFactors = FALSE)
    # Distinguished as their own layer, since eliciting an alter is not the
    # same relation as any of the tie types collected between alters.
    if("type" %in% names(el)) egoties$type <- "ego"
    el <- dplyr::bind_rows(el, egoties)
    el$by <- egokey
  }
  # Session metadata is carried on the nodes so that it survives combination
  # with the other sessions.
  meta <- p$meta[setdiff(names(p$meta), c("edgedefault", "id"))]
  for(nm in names(meta)) nodes[[nm]] <- unname(meta[[nm]])
  list(nodes = nodes, ties = el)
}

# Network Canvas expands every categorical variable into one boolean key per
# option, since its categoricals are natively multi-select. The option groups
# are recovered exactly from the <key> declarations -- ids are
# '<variable uuid>_<hash of option>' while names are '<variable>_<option>' --
# rather than guessed from patterns in the column names. Groups are collapsed to
# a factor only where no case selected more than one option; genuinely
# multi-select variables are left as they are rather than dropped.
.netcanvas_categories <- function(nodes, key_map) {
  ids <- names(key_map)
  cand <- ids[vapply(key_map, function(k)
    identical(k$type, "boolean"), logical(1)) & grepl("_", ids)]
  if(length(cand) < 2) return(nodes)
  groups <- split(cand, sub("_[^_]+$", "", cand))
  groups <- groups[lengths(groups) > 1]
  wide <- character(0)
  for(grp in groups) {
    nms <- vapply(key_map[grp], function(k) k$name, character(1))
    stem <- sub("_+$", "", .common_prefix(nms))
    if(stem == "" || !all(nms %in% names(nodes))) next
    opts <- substring(nms, nchar(stem) + 2L)
    if(any(opts == "")) next
    mat <- as.matrix(nodes[, nms, drop = FALSE]) == TRUE
    mat[is.na(mat)] <- FALSE
    if(any(rowSums(mat) > 1)) {
      wide <- c(wide, stem)
      next
    }
    picked <- apply(mat, 1, function(r)
      if(any(r)) opts[which(r)[1]] else NA_character_)
    nodes[nms] <- NULL
    nodes[[stem]] <- factor(picked, levels = opts)
  }
  if(length(wide) > 0)
    snet_minor_info(paste("Multiple selections found for {.var {wide}};",
                          "left as indicator columns."))
  nodes
}

.common_prefix <- function(x) {
  if(length(x) == 1) return(x)
  chars <- strsplit(x, "", fixed = TRUE)
  k <- 0L
  for(j in seq_len(min(lengths(chars)))) {
    if(length(unique(vapply(chars, `[`, character(1), j))) == 1L) k <- j else break
  }
  substr(x[1], 1L, k)
}

# Network Canvas splits layout variables into '<name>_X' and '<name>_Y' keys.
# These are lowercased to match its CSV export, and a lone layout is exposed as
# plain 'x' and 'y' so that it can be used directly when plotting.
.netcanvas_layout <- function(nodes) {
  xs <- grep("_X$", names(nodes), value = TRUE)
  ys <- grep("_Y$", names(nodes), value = TRUE)
  stems <- intersect(sub("_X$", "", xs), sub("_Y$", "", ys))
  if(length(stems) == 0) return(nodes)
  if(length(stems) == 1 && !any(c("x", "y") %in% names(nodes))) {
    nodes <- .rename_col(nodes, paste0(stems, "_X"), "x")
    nodes <- .rename_col(nodes, paste0(stems, "_Y"), "y")
  } else for(s in stems) {
    nodes <- .rename_col(nodes, paste0(s, "_X"), paste0(s, "_x"))
    nodes <- .rename_col(nodes, paste0(s, "_Y"), paste0(s, "_y"))
  }
  nodes
}

#' @rdname make_read
#' @export
read_gml <- function(file = file.choose()) {
  if(missing(file)) cli::cli_alert_success("Executing: read_gml('{file}')")
  if(!grepl("\\.gml$", file, ignore.case = TRUE)) file <- paste0(file, ".gml")
  as_stocnet(igraph::read_graph(file, format = "gml"))
}

#' @rdname make_read
#' @export
read_gdf <- function(file = file.choose()) {
  if(missing(file)) cli::cli_alert_success("Executing: read_gdf('{file}')")
  if(!grepl("\\.gdf$", file, ignore.case = TRUE)) file <- paste0(file, ".gdf")
  gdf <- readLines(file)
  
  edge_place <- grep("edgedef>", gdf)
  if (length(edge_place) > 0) {
    has_edge_data <- length(length(edge_place):length(gdf)) > 1
    node_data <- gdf[1:(edge_place - 1)]
  } else {
    has_edge_data <- FALSE
    node_data <- gdf
  }
  
  if (has_edge_data) {
    snet_minor_info("Extracting tie data.")
    edge_data <- gdf[edge_place:length(gdf)]
    edge_data[1] <- sub("edgedef>node", "node", edge_data[1])
    edge_data <- read.table(text = edge_data, sep = ",", 
                            header = TRUE, stringsAsFactors = FALSE)
    if(is.numeric(edge_data[,1])) edge_data[,1] <- as.character(edge_data[,1])
    if(is.numeric(edge_data[,2])) edge_data[,2] <- as.character(edge_data[,2])
    names(edge_data) <- c("from","to")
  } else {
    snet_minor_info("No tie data found.")
    edge_data <- data.frame()
  }
  
  snet_minor_info("Extracting node data")
  node_data[1] <- gsub("nodedef>name", "name", node_data[1])
  node_data[1] <- paste0(
    sapply(strsplit(node_data[1], ","),
           function(x) gsub("^(.*) [A-Z]+$", "\\1", x)),
    collapse = ","
  )
  
  ## Some links have commas in them without quotation marks, thus messing
  ## up the fread. We just remove troubled rows, and check n commas in
  ## the header row
  n_sep <- lengths(regmatches(node_data, gregexpr(",", node_data)))
  bad_apples <- which(n_sep != stats::median(n_sep[2:length(n_sep)]))
  bad_apples <- bad_apples[bad_apples != 1]
  if (length(bad_apples) > 0) {
    node_data <- node_data[-bad_apples]
    snet_minor_info("Removed {length(bad_apples)} row{?s} due to comma errors.")
  }
  if (n_sep[1] < stats::median(n_sep)) {
    node_data[1] <- paste0(
      node_data[1],
      paste0(rep(",", (stats::median(n_sep[2:length(n_sep)]) - n_sep[1])),
             collapse = "")
    )
  }
  
  ## The combine and fread, if there is any node data.
  if (length(node_data) > 1) {
    node_data <- data.frame(do.call(rbind, strsplit(node_data, ",")))
    names(node_data) <- node_data[1,]
    node_data <- node_data[-1, , drop = FALSE]
  } else {
    snet_minor_info("No node data found.")
    node_data <- data.frame()
    has_node_data <- FALSE
  }
  
  as_stocnet(as_tidygraph(list(nodes = node_data, ties = edge_data)))
}

# GEXF ####

#' @rdname make_read
#' @importFrom igraph set_graph_attr
#' @export
read_gexf <- function(file = file.choose()) {
  if(missing(file)) snet_success("Executing: read_gexf('{file}')")
  if(!grepl("\\.gexf$", file, ignore.case = TRUE)) file <- paste0(file, ".gexf")
  thisRequires("xml2")
  xmlfile <- xml2::read_xml(file)
  xml2::xml_ns_strip(xmlfile)
  graph <- xml2::xml_find_first(xmlfile, "/gexf/graph")
  if(inherits(graph, "xml_missing"))
    snet_abort("No graph found in {.file {file}}.")
  decl <- .gexf_declarations(graph)
  nds <- xml2::xml_find_all(graph, "./nodes//node")
  eds <- xml2::xml_find_all(graph, "./edges//edge")
  if(length(nds) == 0) snet_abort("No nodes found in {.file {file}}.")
  ids <- xml2::xml_attr(nds, "id")
  nodes <- .gexf_values(nds, decl$node)
  nodes <- .gexf_when(nds, nodes)
  nodes <- .gexf_viz(nds, nodes)
  nodes <- .gexf_parents(nds, nodes)
  nodes <- .gexf_label(nds, nodes, ids)
  # An isolate appears in no tie, so a network whose nodes say nothing else
  # still needs a row for each of them to keep its size.
  if(is.null(nodes) || ncol(nodes) == 0)
    nodes <- data.frame(row.names = seq_along(ids))
  ties <- .gexf_ties(eds, decl$edge, ids, graph)
  out <- .graphml_graph(nodes, ties$el, ties$directed)
  meta <- xml2::xml_find_first(xmlfile, "/gexf/meta")
  if(!inherits(meta, "xml_missing"))
    for(ch in xml2::xml_children(meta))
      out <- igraph::set_graph_attr(out, xml2::xml_name(ch), xml2::xml_text(ch))
  as_stocnet(out)
}

# Maps each <attribute> declaration to its title, type, and default.
# GEXF keys attvalues by the declaration's id, and holds the readable name in
# `title`; we always report the latter. Where the class is not declared,
# the specification takes the declarations to be for nodes.
.gexf_declarations <- function(graph) {
  out <- lapply(c(node = "node", edge = "edge"), function(cls) {
    path <- paste0("./attributes[@class='", cls, "']/attribute")
    if(cls == "node")
      path <- paste0(path, " | ./attributes[not(@class)]/attribute")
    at <- xml2::xml_find_all(graph, path)
    if(length(at) == 0) return(list())
    ids <- xml2::xml_attr(at, "id")
    ttl <- xml2::xml_attr(at, "title")
    ttl[is.na(ttl)] <- ids[is.na(ttl)]
    typ <- xml2::xml_attr(at, "type")
    typ[is.na(typ)] <- "string"
    def <- vapply(at, function(a) {
      d <- xml2::xml_find_first(a, "./default")
      if(inherits(d, "xml_missing")) NA_character_ else xml2::xml_text(d)
    }, character(1))
    stats::setNames(lapply(seq_along(ids), function(i)
      list(name = ttl[i], type = typ[i], default = def[i])), ids)
  })
  out
}

# Collects the <attvalue> children of a set of elements into a data frame,
# one column per declared attribute. A dynamic file records one attvalue per
# spell, so only the first value of each attribute is taken.
.gexf_values <- function(els, decl) {
  if(length(els) == 0 || length(decl) == 0) return(NULL)
  titles <- vapply(decl, function(d) d$name, character(1))
  vals <- lapply(els, function(e) {
    a <- xml2::xml_find_all(e, "./attvalues/attvalue")
    if(length(a) == 0) return(stats::setNames(character(0), character(0)))
    k <- xml2::xml_attr(a, "for")
    # some exporters key each attvalue by title rather than by id
    k[is.na(k)] <- names(decl)[match(xml2::xml_attr(a, "title")[is.na(k)],
                                     titles)]
    v <- xml2::xml_attr(a, "value")
    ok <- !is.na(k) & !duplicated(k)
    stats::setNames(v[ok], k[ok])
  })
  present <- unique(unlist(lapply(vals, names)))
  defaulted <- names(decl)[vapply(decl, function(d)
    !is.na(d$default), logical(1))]
  used <- union(intersect(names(decl), present), defaulted)
  if(length(used) == 0) return(NULL)
  cols <- lapply(used, function(k) {
    raw <- vapply(vals, function(v)
      if(k %in% names(v)) v[[k]] else NA_character_, character(1))
    if(!is.na(decl[[k]]$default)) raw[is.na(raw)] <- decl[[k]]$default
    .graphml_cast(raw, decl[[k]]$type)
  })
  names(cols) <- make.unique(unname(titles[used]))
  data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
}

# Dynamic files time-stamp nodes and ties either on the element itself or in
# one or more <spell> children; the first spell is taken where both are given.
.gexf_when <- function(els, df) {
  n <- length(els)
  if(n == 0) return(df)
  for(w in c("start", "end")) {
    v <- vapply(els, function(e) {
      x <- xml2::xml_attr(e, w)
      if(is.na(x)) {
        s <- xml2::xml_find_first(e, "./spells/spell")
        if(!inherits(s, "xml_missing")) x <- xml2::xml_attr(s, w)
      }
      x
    }, character(1))
    if(all(is.na(v))) next
    num <- suppressWarnings(as.numeric(v))
    df <- .bind_col(df, w, if(identical(is.na(num), is.na(v))) num else v, n)
  }
  df
}

# Gephi records layout, size, and colour in the viz namespace rather than as
# declared attributes. Positions are exposed as 'x' and 'y' so that they can be
# used directly when plotting.
.gexf_viz <- function(els, df) {
  n <- length(els)
  if(n == 0) return(df)
  # Matched by local name, since the viz namespace survives xml_ns_strip(),
  # which strips only the default namespace.
  vizattr <- function(el, val) vapply(els, function(e) {
    p <- xml2::xml_find_first(e, paste0("./*[local-name()='", el, "']"))
    if(inherits(p, "xml_missing")) NA_character_ else xml2::xml_attr(p, val)
  }, character(1))
  for(w in c("x", "y", "z")) {
    v <- suppressWarnings(as.numeric(vizattr("position", w)))
    if(!all(is.na(v))) df <- .bind_col(df, w, v, n)
  }
  size <- suppressWarnings(as.numeric(vizattr("size", "value")))
  if(!all(is.na(size))) df <- .bind_col(df, "size", size, n)
  rgb <- lapply(c("r", "g", "b"), function(w)
    suppressWarnings(as.integer(vizattr("color", w))))
  if(!all(is.na(rgb[[1]]))) {
    col <- sprintf("#%02X%02X%02X", rgb[[1]], rgb[[2]], rgb[[3]])
    col[is.na(rgb[[1]]) | is.na(rgb[[2]]) | is.na(rgb[[3]])] <- NA_character_
    df <- .bind_col(df, "color", col, n)
  }
  df
}

# GEXF nests nodes to record a hierarchy, and also allows a node to point at
# its parent with a 'pid'. Either way the parent is retained as an attribute.
.gexf_parents <- function(els, df) {
  n <- length(els)
  if(n == 0) return(df)
  pid <- xml2::xml_attr(els, "pid")
  nested <- vapply(els, function(e) {
    p <- xml2::xml_parent(xml2::xml_parent(e))
    if(identical(xml2::xml_name(p), "node")) xml2::xml_attr(p, "id") else
      NA_character_
  }, character(1))
  pid[is.na(pid)] <- nested[is.na(pid)]
  if(all(is.na(pid))) return(df)
  .bind_col(df, "parent", pid, n)
}

# Node ids are required by the format, so they are an export artefact rather
# than names; only labels name the nodes, as in read_graphml().
.gexf_label <- function(els, df, ids) {
  labs <- xml2::xml_attr(els, "label")
  if(all(is.na(labs))) return(df)
  labs[is.na(labs)] <- ids[is.na(labs)]
  df <- .bind_col(df, "name", make.unique(labs), length(ids))
  df[, c("name", setdiff(names(df), "name")), drop = FALSE]
}

.gexf_ties <- function(els, decl, ids, graph) {
  n <- length(els)
  defdir <- xml2::xml_attr(graph, "defaultedgetype")
  if(is.na(defdir)) defdir <- "undirected"
  if(n == 0) return(list(el = data.frame(from = integer(0), to = integer(0)),
                         directed = defdir %in% c("directed", "mutual")))
  el <- data.frame(from = match(xml2::xml_attr(els, "source"), ids),
                   to = match(xml2::xml_attr(els, "target"), ids))
  weight <- suppressWarnings(as.numeric(xml2::xml_attr(els, "weight")))
  if(!all(is.na(weight))) {
    weight[is.na(weight)] <- 1
    el$weight <- weight
  }
  vals <- .gexf_values(els, decl)
  if(!is.null(vals))
    el <- cbind(el, vals[, setdiff(names(vals), names(el)), drop = FALSE])
  el <- .gexf_when(els, el)
  types <- xml2::xml_attr(els, "type")
  types[is.na(types)] <- defdir
  keep <- !is.na(el$from) & !is.na(el$to)
  if(any(!keep)) {
    snet_minor_info("Dropped {sum(!keep)} tie{?s} with unmatched endpoints.")
    el <- el[keep, , drop = FALSE]
    types <- types[keep]
  }
  directed <- any(types %in% c("directed", "mutual"))
  # A network is directed or not as a whole, so ties the file declares
  # undirected or mutual are reciprocated where the rest are directed.
  if(directed) {
    both <- types != "directed" & el$from != el$to
    if(any(both)) {
      snet_minor_info(paste("Reciprocating {sum(both)} tie{?s}",
                            "declared undirected or mutual."))
      rec <- el[both, , drop = FALSE]
      rec[c("from", "to")] <- rec[c("to", "from")]
      el <- rbind(el, rec)
    }
  }
  list(el = el, directed = directed)
}

# Write ####

#' Making networks to external files
#'
#' @description 
#'   Researchers may want to save or work with networks outside R.
#'   The following functions offer ways to export to some common external
#'   file formats:
#' 
#'   - `write_matrix()` exports an adjacency matrix to a .csv file.
#'   - `write_edgelist()` exports an edgelist to a .csv file.
#'   - `write_nodelist()` exports a nodelist to a .csv file.
#'   - `write_pajek()` exports Pajek .net files.
#'   - `write_ucinet()` exports a pair of UCINET files in V6404 file format (.##h, .##d).
#'   - `write_dynetml()` exports DyNetML interchange format files.
#'   - `write_graphml()` exports GraphML files.
#'   - `write_gml()` exports GML files.
#'   - `write_gdf()` exports GDF files.
#'   - `write_gexf()` exports GEXF files, for example for use in Gephi.
#' @details
#'   Note that these functions are not as actively maintained as others
#'   in the package, so please let us know if any are not currently working
#'   for you or if there are missing import routines
#'   by [raising an issue on Github](https://github.com/stocnet/manynet/issues).
#'
#'   `write_gexf()` writes the node and tie attributes as declared attributes,
#'   except that 'x', 'y', 'z', 'size', and 'color' are written as the
#'   visualisation elements Gephi reads,
#'   and 'start' and 'end' are written as times, which makes the file dynamic.
#'   Only a hexadecimal colour is written as a visualisation element.
#'   A colour named some other way, such as "red",
#'   is written as an ordinary attribute, which reads back unchanged.
#'   Node names are written as labels, since node ids are required by the format
#'   and so are written as positions in the network.
#' @inheritParams mark_is
#' @param filename Character string filename.
#'   If missing, the files will have the same name as the object
#'   and be saved to the working directory.
#'   An appropriate extension will be added if not included.
#' @param name Character string to name the network internally, e.g. in UCINET.
#'   By default the name will be the same as the object.
#' @param ... Additional parameters passed to the write function.
#' @return The `write_`functions export to different file formats,
#'   depending on the function.
#' @family makes
#' @source 
#' `write_ucinet()` kindly supplied by Christian Steglich, 
#' constructed on 18 June 2015.
#' @importFrom utils write.csv write.csv2
#' @name make_write
#' @seealso [as]
NULL

#' @rdname make_write 
#' @export
write_matrix <- function(.data,
                         filename,
                         ...) {
  if (missing(.data)) {
    Abruzzo <- Campania <- Calabria <- Puglia <- NULL
    Abruzzo <- c(1, 0.76, 0.8, 0.90)
    Campania <- c(0.76, 1, 0.62, 0.69)
    Calabria <- c(0.80, 0.62, 1, 0.85)
    Puglia <- c(0.90, 0.69, 0.85, 1)
    out <- data.frame(Abruzzo, Campania, Calabria, Puglia)
    row.names(out)<- c('Abruzzo','Campania', 'Calabria', 'Puglia')
    out <- as_matrix(out)
    object_name <- "test"
  } else {
    object_name <- deparse(substitute(.data))
    out <- as_matrix(.data)
  }
  if (missing(filename)){
    filename <- paste0(getwd(), "/", object_name, ".csv")
    snet_success("Writing to {.file {filename}}")
  } 
  if(!grepl("\\.csv$", filename)) filename <- paste0(filename, ".csv")
  write.csv(out, file = filename, row.names = FALSE)
}

#' @rdname make_write 
#' @export
write_edgelist <- function(.data,
                           filename,
                           ...) {
  if (missing(.data)) {
    out <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      weight = c(1.1, 11, 110)
    )
    object_name <- "test"
  } else {
    object_name <- deparse(substitute(.data))
    out <- as.data.frame(as_edgelist(.data))
  }
  if (missing(filename)){
    filename <- paste0(getwd(), "/", object_name, "-edges.csv")
    snet_success("Writing to {.file {filename}}")
  }
  if(!grepl("\\.csv$", filename)) filename <- paste0(filename, ".csv")
  write.csv(out, file = filename, row.names = FALSE, ...)
}

#' @rdname make_write
#' @export
write_nodelist <- function(.data,
                           filename,
                           # name,
                           ...) {
  if (missing(.data)) {
    out <- data.frame(
      type = c(FALSE, FALSE, TRUE),
      name = c("A", "B", "C")
    )
    object_name <- "test"
  } else {
    object_name <- deparse(substitute(.data))
    out <- as.data.frame(as_tidygraph(.data))
  }
  if (missing(filename)){
    filename <- paste0(getwd(), "/", object_name, "-nodes.csv")
    snet_success("Writing to {.file {filename}}")
  } 
  if(!grepl("\\.csv$", filename)) filename <- paste0(filename, ".csv")
  write.csv(out, file = filename, row.names = FALSE, ...)
}

#' @rdname make_write 
#' @importFrom igraph write_graph
#' @export
write_pajek <- function(.data,
                        filename,
                        ...) {
  if (missing(filename)) {
    object_name <- deparse(substitute(.data))
    filename <- paste0(getwd(), "/", object_name, ".net")
    snet_success("Writing to {.file {filename}}")
  }
  if(!grepl("\\.paj$", filename)) filename <- paste0(filename, ".paj")
  igraph::write_graph(as_igraph(.data),
                      file = filename,
                      format = "pajek",
                      ...
  )
}

#' @rdname make_write
#' @importFrom utils askYesNo
#' @return A pair of UCINET files in V6404 file format (.##h, .##d)
#' @export
write_ucinet <- function(.data,
                         filename,
                         name) {
  object_name <- deparse(substitute(.data))
  if (missing(filename)) filename <- paste0(getwd(), "/", object_name)
  if (missing(name)) name <- object_name
  # Check to avoid overwriting files by mistake
  if (file.exists(paste(filename, ".##h", sep = ""))) {
    overwrite <- utils::askYesNo(paste("There is already a file called ", 
                                       object_name, 
                                       ".##h here. Do you want to overwrite it?", 
                                       sep = ""))
    if (overwrite == FALSE | is.na(overwrite)) {
      snet_abort("Writing aborted by user.")
    }
  }
  mat <- as_matrix(.data)
  # start with UCINET header file:
  UCINET.header <- file(paste(filename, ".##h", sep = ""), "wb")
  writeBin(5L, UCINET.header, size = 1)
  writeBin(charToRaw("V"), UCINET.header, size = 1)
  writeBin(charToRaw("6"), UCINET.header, size = 1)
  writeBin(charToRaw("4"), UCINET.header, size = 1)
  writeBin(charToRaw("0"), UCINET.header, size = 1)
  writeBin(charToRaw("4"), UCINET.header, size = 1)
  year <- as.integer(substr(Sys.Date(), 3, 4))
  writeBin(year, UCINET.header, size = 2)
  month <- as.integer(substr(Sys.Date(), 6, 7))
  writeBin(month, UCINET.header, size = 2)
  day <- as.integer(substr(Sys.Date(), 9, 10))
  writeBin(day, UCINET.header, size = 2)
  dow <- which(c(
    "Mon",
    "Tue",
    "Wed",
    "Thu",
    "Fri",
    "Sat",
    "Sun"
  ) == substr(date(), 1, 3))
  writeBin(dow, UCINET.header, size = 2)
  writeBin(3L, UCINET.header, size = 2)
  # labtype, unused in V6404 files
  writeBin(7L, UCINET.header, size = 1) # infile.dt = 7 'longintdt'
  writeBin(2L, UCINET.header, size = 2) # ndim = 2 for matrix
  writeBin(ncol(mat), UCINET.header, size = 4) # number of columns of matrix
  writeBin(nrow(mat), UCINET.header, size = 4) # number of rows of matrix
  writeBin(nchar(name), UCINET.header, size = 1) # length of matrix name
  if (nchar(name) > 0) {
    for (i in 1:nchar(name)) {
      writeBin(charToRaw(substr(name, i, i)), UCINET.header, size = 1)
    }
  }
  # Deal with column names of adjacency matrix
  labc <- colnames(mat)
  # Encoding(labc) <- "UTF-8"
  if (!is.null(labc)) {
    if (length(table(labc)) != length(labc)) {
      labc <- NULL
      warning("non-unique column labels, all column labels are dropped")
    }
  }
  writeBin(!is.null(labc), UCINET.header, size = 1)
  # Deal with column names of adjacency matrix
  labr <- rownames(mat)
  # Encoding(labr) <- "UTF-8"
  if (!is.null(labr)) {
    if (length(table(labr)) != length(labr)) {
      labr <- NULL
      warning("non-unique row labels, all row labels are dropped")
    }
  }
  writeBin(!is.null(labr), UCINET.header, size = 1)
  # Write node names of columns
  if (!is.null(labc)) {
    for (i in seq_len(ncol(mat))) {
      writeBin(as.integer(2 * nchar(labc[i])), UCINET.header, size = 2)
      for (let in seq_len(nchar(labc[i]))) {
        writeBin(charToRaw(substr(labc[i], let, let)),
                 UCINET.header,
                 size = 1
        )
        writeBin(raw(1), UCINET.header, size = 1)
      }
    }
  }
  # Write node names of rows
  if (!is.null(labr)) {
    for (i in seq_len(nrow(mat))) {
      writeBin(as.integer(2 * nchar(labr[i])), UCINET.header, size = 2)
      for (let in seq_len(nchar(labr[i]))) {
        writeBin(charToRaw(substr(labr[i], let, let)),
                 UCINET.header,
                 size = 1
        )
        writeBin(raw(1), UCINET.header, size = 1)
      }
    }
  }
  close(UCINET.header)
  # continue with UCINET data file: --> Write the actual matrix
  UCINET.data <- file(paste(filename, ".##d", sep = ""), "wb")
  snet_success("Writing to {.file {filename}}")
  for (i in seq_along(mat)) {
    writeBin(t(mat)[i], UCINET.data, size = 4, endian = "little")
  }
  close(UCINET.data)
}

#' @rdname make_write
#' @importFrom igraph write_graph
#' @export
write_graphml <- function(.data,
                          filename,
                          # name,
                          ...) {
  # if (missing(name)) name <- deparse(substitute(.data))
  if (missing(filename)){
    filename <- paste0(getwd(), "/", deparse(substitute(.data)), ".graphml")
    snet_success("Writing to {.file {filename}}")
  } 
  if(!grepl("\\.graphml$", filename)) filename <- paste0(filename, ".graphml")
  igraph::write_graph(as_igraph(.data),
                      filename,
                      format = "graphml")
}

#' @rdname make_write
#' @importFrom igraph write_graph
#' @export
write_gml <- function(.data,
                      filename,
                      ...) {
  if (missing(filename)){
    filename <- paste0(getwd(), "/", deparse(substitute(.data)), ".gml")
    snet_success("Writing to {.file {filename}}")
  }
  if(!grepl("\\.gml$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".gml")
  g <- as_igraph(.data)
  # The GML format keeps 'directed' for the network's own directedness, which
  # the file already records, so an attribute of that name is dropped here
  # rather than left for igraph's writer to warn that it ignored.
  if("directed" %in% igraph::graph_attr_names(g))
    g <- igraph::delete_graph_attr(g, "directed")
  # igraph's GML writer warns when converting logical attributes to numeric;
  # convert them ourselves first so the export is silent
  for(a in igraph::graph_attr_names(g))
    if(is.logical(igraph::graph_attr(g, a)))
      igraph::graph_attr(g, a) <- as.integer(igraph::graph_attr(g, a))
  for(a in igraph::vertex_attr_names(g))
    if(is.logical(igraph::vertex_attr(g, a)))
      igraph::vertex_attr(g, a) <- as.integer(igraph::vertex_attr(g, a))
  for(a in igraph::edge_attr_names(g))
    if(is.logical(igraph::edge_attr(g, a)))
      igraph::edge_attr(g, a) <- as.integer(igraph::edge_attr(g, a))
  igraph::write_graph(g,
                      filename,
                      format = "gml",
                      id = seq_len(igraph::vcount(g)) - 1)
}

#' @rdname make_write
#' @export
write_gdf <- function(.data,
                      filename,
                      ...) {
  if (missing(filename)){
    filename <- paste0(getwd(), "/", deparse(substitute(.data)), ".gdf")
    snet_success("Writing to {.file {filename}}")
  }
  if(!grepl("\\.gdf$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".gdf")
  g <- as_igraph(.data)
  ids <- node_labels(g)
  nodes <- data.frame(name = ids)
  edges <- as.data.frame(igraph::as_edgelist(g, names = FALSE))
  edges <- data.frame(node1 = ids[edges[,1]],
                      node2 = ids[edges[,2]])
  node_header <- "nodedef>name VARCHAR"
  edge_header <- "edgedef>node1 VARCHAR,node2 VARCHAR"
  writeLines(c(node_header,
              apply(nodes, 1, paste, collapse = ","),
              edge_header,
              apply(edges, 1, paste, collapse = ",")),
            filename)
}

#' @rdname make_write
#' @export
write_gexf <- function(.data,
                       filename,
                       ...) {
  if (missing(filename)){
    filename <- paste0(getwd(), "/", deparse(substitute(.data)), ".gexf")
    snet_success("Writing to {.file {filename}}")
  }
  if(!grepl("\\.gexf$", filename, ignore.case = TRUE))
    filename <- paste0(filename, ".gexf")
  thisRequires("xml2")
  g <- as_igraph(.data)
  ids <- as.character(seq_len(igraph::vcount(g)) - 1)
  el <- igraph::as_edgelist(g, names = FALSE)
  # These are written as visualisation elements and as times instead,
  # so they are not also declared as attributes.
  # An attribute the format cannot hold, such as a colour R does not know,
  # is declared as an ordinary attribute rather than dropped.
  viz <- .gexf_vizable(g)
  nodeatts <- setdiff(igraph::vertex_attr_names(g),
                      c("name", viz, "start", "end"))
  tieatts <- setdiff(igraph::edge_attr_names(g), c("weight", "start", "end"))
  dynamic <- any(c("start", "end") %in% c(igraph::vertex_attr_names(g),
                                          igraph::edge_attr_names(g)))
  doc <- xml2::xml_new_root("gexf", version = "1.2",
                            xmlns = "http://www.gexf.net/1.2draft",
                            "xmlns:viz" = "http://www.gexf.net/1.2draft/viz")
  meta <- xml2::xml_add_child(doc, "meta",
                              lastmodifieddate = format(Sys.Date()))
  xml2::xml_add_child(meta, "creator", "manynet")
  graph <- xml2::xml_add_child(doc, "graph",
                               mode = if(dynamic) "dynamic" else "static",
                               defaultedgetype = if(is_directed(g))
                                 "directed" else "undirected")
  if(dynamic) xml2::xml_set_attr(graph, "timeformat", "double")
  .gexf_declare(graph, g, nodeatts, "node")
  .gexf_declare(graph, g, tieatts, "edge")
  nodesxml <- xml2::xml_add_child(graph, "nodes")
  nodevals <- .gexf_strings(g, nodeatts, "node")
  labs <- if(is_labelled(g)) node_labels(g) else NULL
  for (i in seq_along(ids)) {
    nd <- xml2::xml_add_child(nodesxml, "node", id = ids[i])
    if(!is.null(labs)) xml2::xml_set_attr(nd, "label", labs[i])
    .gexf_write_when(nd, g, i, "node")
    .gexf_write_values(nd, nodevals, i)
    .gexf_write_viz(nd, g, i, viz)
  }
  edgesxml <- xml2::xml_add_child(graph, "edges")
  tievals <- .gexf_strings(g, tieatts, "edge")
  weights <- if(is_weighted(g))
    as.character(igraph::edge_attr(g, "weight")) else NULL
  for (i in seq_len(nrow(el))) {
    ed <- xml2::xml_add_child(edgesxml, "edge", id = as.character(i - 1),
                              source = ids[el[i, 1]], target = ids[el[i, 2]])
    if(!is.null(weights)) xml2::xml_set_attr(ed, "weight", weights[i])
    .gexf_write_when(ed, g, i, "edge")
    .gexf_write_values(ed, tievals, i)
  }
  xml2::write_xml(doc, filename)
}

.gexf_attr <- function(g, class, a) {
  if(class == "node") igraph::vertex_attr(g, a) else igraph::edge_attr(g, a)
}

.gexf_declare <- function(graph, g, atts, class) {
  if(length(atts) == 0) return(invisible(NULL))
  blk <- xml2::xml_add_child(graph, "attributes", class = class)
  for (a in atts) {
    v <- .gexf_attr(g, class, a)
    xml2::xml_add_child(blk, "attribute", id = a, title = a,
                        type = if(is.logical(v)) "boolean" else
                          if(is.integer(v)) "integer" else
                            if(is.numeric(v)) "double" else "string")
  }
}

.gexf_strings <- function(g, atts, class) {
  out <- lapply(atts, function(a) {
    v <- .gexf_attr(g, class, a)
    if(is.logical(v)) tolower(as.character(v)) else as.character(v)
  })
  stats::setNames(out, atts)
}

.gexf_write_values <- function(el, vals, i) {
  used <- names(vals)[vapply(vals, function(v) !is.na(v[i]), logical(1))]
  if(length(used) == 0) return(invisible(NULL))
  av <- xml2::xml_add_child(el, "attvalues")
  for (a in used)
    xml2::xml_add_child(av, "attvalue", "for" = a, value = vals[[a]][i])
}

.gexf_write_when <- function(el, g, i, class) {
  for (w in c("start", "end")) {
    v <- .gexf_attr(g, class, w)
    if(is.null(v) || is.na(v[i])) next
    xml2::xml_set_attr(el, w, as.character(v[i]))
  }
}

# The red, green, and blue values a colour gives, or NA where it gives none.
# Only hexadecimal colours are read, since these are what the format holds and
# what `read_gexf()` returns. A colour named some other way is written as an
# ordinary attribute instead, which reads back unchanged.
# An eight digit colour is left to that path too, since a `<viz:color>` holds
# no alpha value and so would drop it.
.hex2rgb <- function(x) {
  out <- matrix(NA_integer_, nrow = 3, ncol = length(x))
  ok <- !is.na(x) & grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$", x)
  if(any(ok)) {
    hex <- sub("^#", "", x[ok])
    short <- nchar(hex) == 3L
    if(any(short)) hex[short] <- paste0(substr(hex[short], 1, 1),
                                        substr(hex[short], 1, 1),
                                        substr(hex[short], 2, 2),
                                        substr(hex[short], 2, 2),
                                        substr(hex[short], 3, 3),
                                        substr(hex[short], 3, 3))
    out[, ok] <- vapply(hex, function(h)
      strtoi(substring(h, c(1, 3, 5), c(2, 4, 6)), 16L), integer(3))
  }
  out
}

# The attributes the visualisation elements can hold: positions and sizes must
# be numeric, and colours must be hexadecimal. Any other attribute of the same
# name is left to be declared as an ordinary attribute instead.
.gexf_vizable <- function(g) {
  out <- character(0)
  for (w in c("x", "y", "z", "size")) {
    v <- igraph::vertex_attr(g, w)
    if(!is.null(v) && is.numeric(v)) out <- c(out, w)
  }
  col <- igraph::vertex_attr(g, "color")
  if(!is.null(col) && !anyNA(.hex2rgb(col[!is.na(col)])[1, ]))
    out <- c(out, "color")
  out
}

.gexf_write_viz <- function(el, g, i, viz) {
  pos <- lapply(c("x", "y", "z"), function(w)
    if(w %in% viz) igraph::vertex_attr(g, w)[i] else numeric(0))
  names(pos) <- c("x", "y", "z")
  if(!all(vapply(pos, function(v) length(v) == 0 || is.na(v), logical(1)))) {
    # The format expects a position to give both x and y,
    # so a missing coordinate is written as zero rather than left out.
    if(length(pos$z) == 0 || is.na(pos$z)) pos$z <- NULL
    pos <- lapply(pos, function(v)
      if(length(v) == 0 || is.na(v)) "0" else as.character(v))
    do.call(xml2::xml_add_child, c(list(el, "viz:position"), pos))
  }
  size <- if("size" %in% viz) igraph::vertex_attr(g, "size") else NULL
  if(!is.null(size) && !is.na(size[i]))
    xml2::xml_add_child(el, "viz:size", value = as.character(size[i]))
  col <- if("color" %in% viz) igraph::vertex_attr(g, "color") else NULL
  if(!is.null(col) && !is.na(col[i])) {
    rgb <- .hex2rgb(col[i])
    if(!is.na(rgb[1]))
      xml2::xml_add_child(el, "viz:color", r = as.character(rgb[1]),
                          g = as.character(rgb[2]), b = as.character(rgb[3]))
  }
}

#' @rdname make_write
#' @export
write_dynetml <- function(.data,
                          filename,
                          ...) {
  if (missing(filename)){
    filename <- paste0(getwd(), "/", deparse(substitute(.data)), ".xml")
    snet_success("Writing to {.file {filename}}")
  }
  if(!grepl("\\.xml$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".xml")
  thisRequires("xml2")
  g <- as_igraph(.data)
  ids <- node_labels(g)
  el <- igraph::as_edgelist(g, names = FALSE)
  doc <- xml2::xml_new_root("DynamicNetwork")
  metanetwork <- xml2::xml_add_child(doc, "MetaNetwork")
  nodesxml <- xml2::xml_add_child(metanetwork, "nodes")
  nodeclass <- xml2::xml_add_child(nodesxml, "nodeclass",
                                   type = "agent", id = "agent")
  for (id in ids) xml2::xml_add_child(nodeclass, "node", id = id)
  networksxml <- xml2::xml_add_child(metanetwork, "networks")
  networkxml <- xml2::xml_add_child(networksxml, "network",
                                    id = "network",
                                    sourceType = "agent", targetType = "agent",
                                    isDirected = tolower(as.character(is_directed(g))))
  for (i in seq_len(nrow(el))) {
    xml2::xml_add_child(networkxml, "edge",
                        source = ids[el[i, 1]],
                        target = ids[el[i, 2]],
                        value = "1")
  }
  xml2::write_xml(doc, filename)
}

# nocov end
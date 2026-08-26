#' Marking networks change formats
#' @name mark_format_change
#' @description
#'   These functions implement logical tests for various network properties.
#'   All `is_*()` functions return a logical scalar (TRUE or FALSE).
#'   
#'   - `is_longitudinal()` marks networks TRUE if they are panels, i.e. if each
#'   moment they record re-observes the whole network.
#'   - `is_dynamic()` marks networks TRUE if they record a stream of events,
#'   either as increments to the ties or as the interval each tie lasts over.
#'   
#'   A network records time in one way or the other, so the two marks do not
#'   overlap. See the Time section of [to_time()] for what separates them.
#'   - `is_changing()` marks networks TRUE if they contain any nodal changes.
#' @template param_data
#' @eval detail_avail("is_(longitudinal|dynamic|changing)")
#' @family marks
#' @family changes
NULL

#' @rdname mark_format_change
#' @examples
#' is_longitudinal(create_tree(5, 3))
#' @export
is_longitudinal <- function(.data) UseMethod("is_longitudinal")

#' @export
is_longitudinal.default <- function(.data) {
  is_longitudinal(as_igraph(.data))
}

#' @export
is_longitudinal.igraph <- function(.data) {
  # A panel network re-observes the whole network at each moment, so each
  # moment replaces the one before it. See `.time_rule()`.
  # It re-observes the ties too, so the ties carry the stamp. A network that
  # records only nodal changes, such as a diffusion on a static network,
  # is therefore not a panel.
  identical(.time_rule(.data), "replace") && !is.null(.time_moments(.data)) &&
    any(c("time", "wave", "panel") %in% net_tie_attributes(.data))
}

#' @export
is_longitudinal.list <- function(.data) {
  if(is_list(.data)){
    all(lapply(.data, net_nodes)==net_nodes(.data[[1]]))
  } else FALSE
}

# A stocnet object is a list, so it needs a method of its own to keep it from
# being marked by the list method above.
#' @export
is_longitudinal.stocnet <- is_longitudinal.igraph

#' @rdname mark_format_change
#' @examples 
#' is_dynamic(create_tree(3))
#' @export
is_dynamic <- function(.data) UseMethod("is_dynamic")

#' @export
is_dynamic.default <- function(.data) {
  is_dynamic(as_igraph(.data))
}

#' @export
is_dynamic.igraph <- function(.data) {
  # A dynamic network records a stream of events, either as increments to the
  # ties or as the intervals over which each tie lasts. Either way it is not a
  # panel, so the two marks do not overlap. See `.time_rule()`.
  .time_rule(.data) %in% c("increment", "interval")
}

#' @export
is_dynamic.stocnet <- is_dynamic.igraph

#' @rdname mark_format_change
#' @examples 
#' is_changing(fict_starwars)
#' @export
is_changing <- function(.data) UseMethod("is_changing")

#' @export
is_changing.default <- function(.data) {
  is_changing(as_igraph(.data))
}

#' @export
is_changing.igraph <- function(.data) {
  "changes" %in% igraph::graph_attr_names(.data)
}

#' @export
is_changing.stocnet <- function(.data) {
  "changes" %in% names(.data) && !is.null(.data$changes)
}

#' @export
is_changing.diff_model <- function(.data) {
  is_changing.igraph(as_igraph(.data))
}


# Time ####

# A network records time in its ties along two independent axes. How a moment
# is represented is structural: a point, in a 'time' column, or an interval,
# in 'begin' and 'end' columns. How a moment relates to the one before it is
# not, and is declared in `info$update`: "replace", where each moment re-states
# the ties, or "increment", where each row is a delta. This returns which rule
# scoping a network to a moment must follow.
# An interval tie carries its own lifespan, so `update` says nothing about it
# and is not consulted; the representation is tested first for that reason.
.time_rule <- function(.data){
  atts <- tryCatch(net_tie_attributes(.data), error = function(e) character(0))
  # A tie that begins and never ends is right-censored, not point-stamped, so
  # a beginning alone marks an interval network. GEXF spells one this way.
  if(any(c("begin", "beg", "start") %in% atts)) return("interval")
  update <- tryCatch(as_infolist(.data)$update, error = function(e) NULL)
  if("increment" %in% atts || isTRUE(any(update == "increment")))
    return("increment")
  if("replace" %in% atts || any(c("time", "wave", "panel") %in% atts) ||
     is_changing(.data)) return("replace")
  "none"
}

# The moments a network records, in order, or NULL where it records none.
# What counts as a moment depends on how they are represented: an interval
# network changes at every tie beginning and ending, and a point-stamped one
# at every stamp. Nodal changes are moments too, since a network whose
# composition changes is a different network from one moment to the next, but
# a caller counting the waves of a panel asks for `changes = FALSE`, since a
# change recorded after the last wave does not add a wave.
.time_moments <- function(.data, changes = TRUE){
  atts <- tryCatch(net_tie_attributes(.data), error = function(e) character(0))
  moments <- if(identical(.time_rule(.data), "interval")){
    begin <- intersect(c("begin", "beg", "start"), atts)[1]
    ends <- if("end" %in% atts) .bare_time(tie_attribute(.data, "end")) else NULL
    c(.bare_time(tie_attribute(.data, begin)), ends)
  } else {
    stamp <- intersect(c("time", "wave", "panel"), atts)[1]
    if(is.na(stamp)) NULL else .bare_time(tie_attribute(.data, stamp))
  }
  if(isTRUE(changes) && is_changing(.data)){
    chg <- .bare_time(as_changelist(.data)$time)
    # A changelist moment on a different scale from the tie stamps, a date
    # against a wave number, cannot be put in the same order as them, so it
    # only joins them where the two are on the same scale.
    if(is.null(moments) || .same_scale(chg, moments))
      moments <- c(moments, chg)
  }
  moments <- sort(unique(moments[!is.na(moments)]))
  if(!length(moments)) NULL else moments
}

# A moment is a moment and not a measurement of one, but `tie_attribute()`
# returns a 'tie_measure' where the column is numeric, which does not combine
# with a moment from another source.
.bare_time <- function(x){
  if(is.null(x)) return(NULL)
  if(inherits(x, "tie_measure")) x <- as.vector(x)
  x
}

# Whether two sets of moments count time the same way, and so can be ordered
# against each other. A wave numbered 3 and a wave numbered 3L are the same
# moment; a wave numbered 3 and the third of March are not comparable at all.
.same_scale <- function(x, y){
  if(is.numeric(x) && is.numeric(y)) return(TRUE)
  identical(class(x), class(y))
}

# The layers to carry into every moment: those a network declares
# cross-sectional, and, where it declares nothing, those recorded at a single
# moment while another layer spans several. Such a layer states something that
# holds throughout, a constant dyadic covariate, so scoping the network to one
# moment should not drop it.
.invariant_layers <- function(.data){
  layers <- tryCatch(layer_names(.data), error = function(e) NULL)
  if(is.null(layers)) return(character(0))
  observation <- tryCatch(as_infolist(.data)$observation, error = function(e) NULL)
  if(!is.null(names(observation)))
    return(intersect(layers, names(observation)[observation == "cross-sectional"]))
  atts <- tryCatch(net_tie_attributes(.data), error = function(e) character(0))
  stamp <- intersect(c("time", "wave", "panel"), atts)[1]
  layer <- intersect(c("layer", "type"), atts)[1]
  if(is.na(stamp) || is.na(layer)) return(character(0))
  spans <- tapply(.bare_time(tie_attribute(.data, stamp)),
                  as.character(tie_attribute(.data, layer)),
                  function(x) length(unique(x)))
  spans <- spans[!is.na(spans)]
  # Where no layer spans more than one moment, none of them stands out as
  # holding throughout while the others vary.
  if(!length(spans) || max(spans) < 2) return(character(0))
  intersect(layers, names(spans)[spans == 1])
}

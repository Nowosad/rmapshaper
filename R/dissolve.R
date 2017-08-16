#' Aggregate shapes in a polygon or point layer.
#'
#' Aggregates using specified field, or all shapes if no field is given. For point layers,
#' replaces a group of points with their centroid.
#'
#' @param input spatial object to dissolve. One of:
#' \itemize{
#'  \item \code{geo_json} or \code{character} points or polygons;
#'  \item \code{geo_list} points or polygons;
#'  \item \code{SpatialPolygons}, or \code{SpatialPoints}
#'  }
#' @param field the field to dissolve on
#' @param sum_fields fields to sum
#' @param copy_fields fields to copy. The first instance of each field will be
#'   copied to the aggregated feature.
#' @param weight Name of an attribute field for generating weighted centroids (points only).
#' @param snap Snap together vertices within a small distance threshold to fix
#'   small coordinate misalignment in adjacent polygons. Default \code{TRUE}.
#' @param force_FC should the output be forced to be a \code{FeatureCollection} even
#' if there are no attributes? Default \code{TRUE}.
#'  \code{FeatureCollections} are more compatible with \code{rgdal::readOGR} and
#'  \code{geojsonio::geojson_sp}. If \code{FALSE} and there are no attributes associated with
#'  the geometries, a \code{GeometryCollection} will be output. Ignored for \code{Spatial}
#'  objects, as the output is always the same class as the input.
#' @param snap_interval Specify snapping distance in source units, must be a
#'   numeric. Default \code{NULL}
#' @param merge_overlaps Whether or not to dissolve overlapping polygons as well as adjacent.
#'   If \code{TRUE}, uses mapshaper's \code{dissolve2} command.
#'
#' @return the same class as the input
#'
#' @examples
#' library(geojsonio)
#' library(sp)
#'
#' poly <- structure('{"type":"FeatureCollection",
#'   "features":[
#'   {"type":"Feature",
#'   "properties":{"a": 1, "b": 2},
#'   "geometry":{"type":"Polygon","coordinates":[[
#'   [102,2],[102,3],[103,3],[103,2],[102,2]
#'   ]]}}
#'   ,{"type":"Feature",
#'   "properties":{"a": 5, "b": 3},
#'   "geometry":{"type":"Polygon","coordinates":[[
#'   [100,0],[100,1],[101,1],[101,0],[100,0]
#'   ]]}}]}', class = c("json", "geo_json"))
#' poly <- geojson_sp(poly)
#' plot(poly)
#' length(poly)
#' poly@data
#'
#' # Dissolve the polygon
#' out <- ms_dissolve(poly)
#' plot(out)
#' length(out)
#' out@data
#'
#' # Dissolve and summing columns
#' out <- ms_dissolve(poly, sum_fields = c("a", "b"))
#' plot(out)
#' out@data
#'
#' @export
ms_dissolve <- function(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
                        weight = NULL, snap = TRUE, snap_interval = NULL,
                        merge_overlaps = FALSE, force_FC = TRUE) {
  UseMethod("ms_dissolve")
}

#' @export
ms_dissolve.character <- function(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
                                  weight = NULL, snap = TRUE, snap_interval = NULL,
                                  merge_overlaps = FALSE, force_FC = TRUE) {
  input <- check_character_input(input)

  call <- make_dissolve_call(field = field, sum_fields = sum_fields, weight = weight,
                             copy_fields = copy_fields, snap = snap,
                             snap_interval = snap_interval, merge_overlaps = merge_overlaps)

  apply_mapshaper_commands(data = input, command = call, force_FC = force_FC)

}

#' @export
ms_dissolve.geo_json <- function(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
                                 weight = NULL, snap = TRUE, snap_interval = NULL,
                                 merge_overlaps = FALSE, force_FC = TRUE) {

  call <- make_dissolve_call(field = field, sum_fields = sum_fields, weight = weight,
                             copy_fields = copy_fields, snap = snap,
                             snap_interval = snap_interval, merge_overlaps = merge_overlaps)

  apply_mapshaper_commands(data = input, command = call, force_FC = force_FC)
}

#' @export
ms_dissolve.geo_list <- function(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
                                 weight = NULL, snap = TRUE, snap_interval = NULL,
                                 merge_overlaps = FALSE, force_FC = TRUE) {

  call <- make_dissolve_call(field = field, sum_fields = sum_fields, weight = weight,
                             copy_fields = copy_fields, snap = snap,
                             snap_interval = snap_interval, merge_overlaps = merge_overlaps)

  geojson <- geojsonio::geojson_json(input)

  ret <- apply_mapshaper_commands(data = geojson, command = call, force_FC = force_FC)

  geojsonio::geojson_list(ret)
}

#' @export
ms_dissolve.SpatialPolygons <- function(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
                                        weight = NULL, snap = TRUE, snap_interval = NULL,
                                        merge_overlaps = FALSE, force_FC = TRUE) {
 dissolve_sp(input = input, field = field, sum_fields = sum_fields, copy_fields = copy_fields,
             weight = weight, snap = snap,
             snap_interval = snap_interval, merge_overlaps = merge_overlaps)
}

#' @export
ms_dissolve.SpatialPoints <- function(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
                                      weight = NULL, snap = TRUE, snap_interval = NULL,
                                      merge_overlaps = FALSE, force_FC = TRUE) {
  dissolve_sp(input = input, field = field, sum_fields = sum_fields, copy_fields = copy_fields,
              weight = weight, snap = snap,
              snap_interval = snap_interval, merge_overlaps = merge_overlaps)
}

#' @export
ms_dissolve.sf <- function(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
                           weight = NULL, snap = TRUE, snap_interval = NULL,
                           merge_overlaps = FALSE, force_FC = TRUE) {
  if (!is.null(weight) && !(weight %in% names(input))) {
    stop("specified 'weight' column not present in input data", call. = FALSE)
  }

  dissolve_sf(input = input, field = field, sum_fields = sum_fields, copy_fields = copy_fields,
              weight = weight, snap = snap,
              snap_interval = snap_interval, merge_overlaps = merge_overlaps)

}

#' @export
ms_dissolve.sfc <- function(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
                            weight = NULL, snap = TRUE, snap_interval = NULL,
                            merge_overlaps = FALSE, force_FC = TRUE) {
  if (!is.null(weight)) {
    warning("'weight' cannot be used with sfc objects. Ignoring it and proceeding...")
  }

  dissolve_sf(input = input, field = field, sum_fields = sum_fields, copy_fields = copy_fields,
              weight = NULL, snap = snap,
              snap_interval = snap_interval, merge_overlaps = merge_overlaps)
}

make_dissolve_call <- function(field, sum_fields, copy_fields, weight, snap, snap_interval,
                               merge_overlaps) {

  if (!is.null(snap_interval)) {
    if (!is.numeric(snap_interval)) stop("snap_interval must be a numeric")
    if (snap_interval < 0) stop("snap_interval must be >= 0")
  }

  if (is.null(sum_fields)) {
    sum_fields_string <- NULL
  } else {
    sum_fields_string <- paste0("sum-fields=", paste0(sum_fields, collapse = ","))
  }

  if (is.null(copy_fields)) {
    copy_fields_string <- NULL
  } else {
    copy_fields_string <- paste0("copy-fields=", paste0(copy_fields, collapse = ","))
  }

  if (is.null(weight)) {
    weight_string <- NULL
  } else {
    weight_string <- paste0("weight=", weight)
  }

  if (snap && !is.null(snap_interval)) snap_interval <- paste0("snap-interval=", snap_interval)
  if (snap) snap <- "snap" else snap <- NULL

  dissolve_type <- ifelse(merge_overlaps, "-dissolve2", "-dissolve")

  call <- list(snap, snap_interval, dissolve_type, field, sum_fields_string, copy_fields_string, weight_string)

  call
}

dissolve_sp <- function(input, field, sum_fields, copy_fields, weight, snap,
                        snap_interval, merge_overlaps) {

  if (!inherits(input, "SpatialPointsDataFrame") && !is.null(weight)) {
    stop("weight arguments only applies to points with attributes", call. = FALSE)
  }

  if (!is.null(weight) && !(weight %in% names(input))) {
    stop("specified 'weight' column not present in input data", call. = FALSE)
  }

  call <- make_dissolve_call(field = field, sum_fields = sum_fields, copy_fields = copy_fields,
                             weight = weight, snap = snap,
                             snap_interval = snap_interval, merge_overlaps = merge_overlaps)

  ms_sp(input = input, call = call)
}

dissolve_sf <- function(input, field, sum_fields, copy_fields, weight, snap,
                        snap_interval, merge_overlaps) {

  if (!all(sf::st_is(input, c("POINT", "MULTIPOINT", "POLYGON", "MULTIPOLYGON")))) {
    stop("ms_dissolve only works with (MULTI)POINT or (MULTI)POLYGON", call. = FALSE)
  }

  if (!all(sf::st_is(input, c("POINT", "MULTIPOINT"))) && !is.null(weight)) {
    stop("weights arguments only applies to points", call. = FALSE)
  }

  call <- make_dissolve_call(field = field, sum_fields = sum_fields, copy_fields = copy_fields,
                             weight = weight, snap = snap,
                             snap_interval = snap_interval, merge_overlaps = merge_overlaps)

  ms_sf(input = input, call = call)
}

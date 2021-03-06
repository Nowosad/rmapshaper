% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dissolve.R
\name{ms_dissolve}
\alias{ms_dissolve}
\title{Aggregate shapes in a polygon or point layer.}
\usage{
ms_dissolve(input, field = NULL, sum_fields = NULL, copy_fields = NULL,
  weight = NULL, snap = TRUE, force_FC = TRUE, sys = FALSE)
}
\arguments{
\item{input}{spatial object to dissolve. One of:
\itemize{
\item \code{geo_json} or \code{character} points or polygons;
\item \code{geo_list} points or polygons;
\item \code{SpatialPolygons}, or \code{SpatialPoints}
}}

\item{field}{the field to dissolve on}

\item{sum_fields}{fields to sum}

\item{copy_fields}{fields to copy. The first instance of each field will be
copied to the aggregated feature.}

\item{weight}{Name of an attribute field for generating weighted centroids (points only).}

\item{snap}{Snap together vertices within a small distance threshold to fix
small coordinate misalignment in adjacent polygons. Default \code{TRUE}.}

\item{force_FC}{should the output be forced to be a \code{FeatureCollection} even
if there are no attributes? Default \code{TRUE}.
\code{FeatureCollections} are more compatible with \code{rgdal::readOGR} and
\code{geojsonio::geojson_sp}. If \code{FALSE} and there are no attributes associated with
the geometries, a \code{GeometryCollection} will be output. Ignored for \code{Spatial}
objects, as the output is always the same class as the input.}

\item{sys}{Should the system mapshaper be used instead of the bundled mapshaper? Gives
better performance on large files. Requires the mapshapr node package to be installed
and on the PATH.}
}
\value{
the same class as the input
}
\description{
Aggregates using specified field, or all shapes if no field is given. For point layers,
replaces a group of points with their centroid.
}
\examples{
library(geojsonio)
library(sp)

poly <- structure('{"type":"FeatureCollection",
  "features":[
  {"type":"Feature",
  "properties":{"a": 1, "b": 2},
  "geometry":{"type":"Polygon","coordinates":[[
  [102,2],[102,3],[103,3],[103,2],[102,2]
  ]]}}
  ,{"type":"Feature",
  "properties":{"a": 5, "b": 3},
  "geometry":{"type":"Polygon","coordinates":[[
  [100,0],[100,1],[101,1],[101,0],[100,0]
  ]]}}]}', class = c("json", "geo_json"))
poly <- geojson_sp(poly)
plot(poly)
length(poly)
poly@data

# Dissolve the polygon
out <- ms_dissolve(poly)
plot(out)
length(out)
out@data

# Dissolve and summing columns
out <- ms_dissolve(poly, sum_fields = c("a", "b"))
plot(out)
out@data

}

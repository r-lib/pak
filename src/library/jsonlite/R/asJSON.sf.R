# For 'sf' geometry columns; use same structure as GeoJSON
setMethod("asJSON", "sf", function(x, sf = c("dataframe", "features", "geojson"), ...) {
  sf <- match.arg(sf)
  if (sf == 'dataframe') {
    callNextMethod()
  } else {
    sf_column <- attr(x, 'sf_column')
    if (!length(sf_column)) sf_column <- 'geometry'
    geometry <- x[[sf_column]]
    input <- as.data.frame(x)
    features <- data.frame(type = rep('Feature', nrow(input)), stringsAsFactors = FALSE)
    features$properties = input[names(input) != sf_column]
    features$geometry = geometry
    if (sf == 'features') {
      asJSON(features, sf = sf, ...)
    } else {
      output <- list(
        type = unbox('FeatureCollection'),
        name = unbox('sfdata'),
        features = features
      )
      asJSON(output, sf = sf, ...)
    }
  }
})

setMethod("asJSON", "sfc", function(x, ...) {
  y <- lapply(unclass(x), geom_to_geojson)
  asJSON(y, ...)
})

geom_to_geojson <- function(x) {
  val <- list(
    type = unbox(sf_to_titlecase(class(x)[2])) # see: sf::st_geometry_type
  )
  if (inherits(x, "GEOMETRYCOLLECTION")) {
    val$geometries = lapply(x, geom_to_geojson)
  } else {
    val$coordinates = unclass(x)
  }
  return(val)
}

# See sf::sf.tp
sf_to_titlecase <- function(x) {
  sf_types <-
    c("Point", "LineString", "Polygon", "MultiPoint", "MultiLineString", "MultiPolygon", "GeometryCollection", "CircularString", "CompoundCurve", "CurvePolygon", "MultiCurve", "MultiSurface", "Curve", "Surface", "PolyhedralSurface", "TIN", "Triangle")
  matches <- match(as.character(x), toupper(sf_types))
  sf_types[matches]
}

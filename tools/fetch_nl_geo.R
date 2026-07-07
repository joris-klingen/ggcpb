# fetch_nl_geo.R ----
#
# Build-time script (not shipped in the package tarball; tools/ is in
# .Rbuildignore): downloads the generalised Dutch administrative
# boundaries and converts them to the flat polygon table bundled as
# inst/extdata/nl_geo.rds, which cpb_nl_geo() and cpb_map() read.
#
# Source: https://cartomap.github.io/nl/ -- generalised CBS/Kadaster
# boundaries (statcode/statnaam per feature), served as GeoJSON per
# year and level. Coordinates are Rijksdriehoek (EPSG:28992) metres, so
# maps render with coord_fixed(1) and no projection step is needed.
#
# Re-run (from the package root) to bump the boundary year:
#   Rscript tools/fetch_nl_geo.R

YEAR <- 2025
LEVELS <- c(gemeente = "gemeente", corop = "coropgebied", provincie = "provincie")

# flatten one GeoJSON feature collection into a data.frame with one row
# per vertex: code/name identify the region, part is the polygon-part
# id (ggplot2 `group`), ring the ring id within the part (ggplot2
# `subgroup`; rings after the first are holes)
flatten_geojson <- function(g) {
  rows <- lapply(seq_along(g$features), function(i) {
    f <- g$features[[i]]
    polys <- if (f$geometry$type == "Polygon") {
      list(f$geometry$coordinates)
    } else if (f$geometry$type == "MultiPolygon") {
      f$geometry$coordinates
    } else {
      stop("unexpected geometry type: ", f$geometry$type)
    }
    do.call(rbind, lapply(seq_along(polys), function(pi) {
      do.call(rbind, lapply(seq_along(polys[[pi]]), function(ri) {
        m <- do.call(rbind, lapply(polys[[pi]][[ri]], function(xy) {
          c(as.numeric(xy[[1]]), as.numeric(xy[[2]]))
        }))
        data.frame(
          code = f$properties$statcode,
          name = f$properties$statnaam,
          part = sprintf("%s.%d", f$properties$statcode, pi),
          ring = sprintf("%s.%d.%d", f$properties$statcode, pi, ri),
          x = m[, 1], y = m[, 2]
        )
      }))
    }))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

geo <- lapply(LEVELS, function(lvl) {
  url <- sprintf("https://cartomap.github.io/nl/rd/%s_%d.geojson", lvl, YEAR)
  message("fetching ", url)
  flatten_geojson(jsonlite::read_json(url))
})
attr(geo, "year") <- YEAR
attr(geo, "source") <- "cartomap.github.io/nl (generalised CBS/Kadaster boundaries), EPSG:28992"

dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(geo, "inst/extdata/nl_geo.rds", compress = "xz")
message("wrote inst/extdata/nl_geo.rds (",
        round(file.size("inst/extdata/nl_geo.rds") / 1024), " KB)")

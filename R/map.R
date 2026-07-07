# map.R ----
#
# Choropleth maps of the Netherlands on the bundled generalised
# CBS/Kadaster boundaries (via cartomap.github.io/nl, EPSG:28992;
# rebuilt with tools/fetch_nl_geo.R). Regions are separated by thin
# seams in the plot background colour, and the legend sits inside the
# panel at top-left, in the empty North Sea corner of the country.

.ggcpb_geo_env <- new.env(parent = emptyenv())

#' Boundaries of the Netherlands at three administrative levels
#'
#' Returns the bundled generalised boundaries (source: CBS/Kadaster via
#' cartomap, 2025; Rijksdriehoek coordinates, EPSG:28992) as a flat
#' polygon table ready for [ggplot2::geom_polygon()]: one row per
#' vertex with the region `code` (CBS "statcode", e.g. `"GM0014"`,
#' `"CR01"`, `"PV20"`) and `name`, plus `part` (polygon-part id, use as
#' `group`) and `ring` (ring id within the part, use as `subgroup`;
#' rings after the first are holes).
#'
#' @param level One of `"gemeente"` (municipalities, default),
#'   `"corop"` (COROP regions) or `"provincie"` (provinces).
#' @return A data.frame with columns `code`, `name`, `part`, `ring`,
#'   `x`, `y`.
#' @examples
#' nl <- cpb_nl_geo("provincie")
#' head(nl)
#' @importFrom rlang .data
#' @export
cpb_nl_geo <- function(level = c("gemeente", "corop", "provincie")) {
  level <- match.arg(level)
  if (is.null(.ggcpb_geo_env$geo)) {
    path <- system.file("extdata", "nl_geo.rds", package = "ggcpb")
    if (!nzchar(path)) {
      stop("the bundled boundary data (inst/extdata/nl_geo.rds) is missing; ",
           "rebuild it with tools/fetch_nl_geo.R.", call. = FALSE)
    }
    .ggcpb_geo_env$geo <- readRDS(path)
  }
  .ggcpb_geo_env$geo[[level]]
}

#' A CPB-styled choropleth map of the Netherlands
#'
#' Draws one value per Dutch municipality, COROP region or province on
#' the bundled generalised boundaries. Regions are joined to `data` by
#' CBS code (`"GM0014"`, `"CR01"`, `"PV20"`) or by name -- whichever
#' matches the `region` column best -- and filled with the CPB
#' sequential gradient (numeric values) or a discrete CPB palette.
#' Regions are separated by thin background-colour seams, and the
#' legend sits inside the panel at top-left (the empty North Sea
#' corner). Returns a real ggplot object that can be extended with `+`.
#'
#' @param data A data.frame or data.table with one row per region.
#' @param region Column (tidy eval) identifying the region, as CBS
#'   statcode or as name.
#' @param value Column (tidy eval) with the value to fill by. Numeric
#'   values get [scale_fill_cpb_c()]; discrete values get the discrete
#'   CPB palette.
#' @param level One of `"gemeente"` (default), `"corop"` or
#'   `"provincie"`; must match the regions in `data`.
#' @param border_colour Border colour between regions; defaults to the
#'   CPB background colour, a soft seam (on a classed or continuous
#'   fill neighbouring classes already differ, so this is enough). Pass
#'   `"white"` for more contrast, e.g. on a two-class discrete map.
#' @param border_linewidth Border line width; defaults to `0.15`, a
#'   thin seam that keeps adjacent regions legible without outlines.
#' @param palette CPB palette for a *discrete* `value` column; one of
#'   `"qualitative"`, `"discr"`, `"sequential"` (pink ramp), or
#'   `"blues"` (blue ramp -- the usual choice for classed maps).
#' @param index Optional integer vector of palette positions for a
#'   discrete `value` column, forwarded to [scale_fill_cpb_manual()].
#' @param reverse For a numeric `value` column: reverse the sequential
#'   gradient (passed to [scale_fill_cpb_c()]).
#' @param na_fill Fill for regions without a value in `data`; defaults
#'   to the CPB missing-value grey.
#' @param legend Legend placement. `"topleft"` (default) puts the
#'   legend inside the panel at top-left -- in the empty North Sea
#'   corner of the Netherlands -- as a vertical block. Any other value
#'   (e.g. `"bottom"`, `"none"`) is forwarded to [theme_cpb()].
#' @param flush_legend Anchor a `"bottom"` legend flush bottom-left, as
#'   in the other wrappers. Ignored when `legend = "topleft"`.
#' @param title,subtitle Plot title/subtitle. As elsewhere, `ylab` does
#'   not exist here: use `subtitle` for the unit caption.
#' @param filllab Legend title; defaults to `NULL` (no title).
#' @param ... Further arguments passed to [ggplot2::geom_polygon()].
#' @return A `ggplot` object.
#' @examples
#' df <- data.frame(prov = c("Groningen", "Friesland", "Drenthe"),
#'                  waarde = c(1.2, 3.4, 2.1))
#' cpb_map(df, region = prov, value = waarde, level = "provincie",
#'         title = "Voorbeeld", subtitle = "waarde")
#' @export
cpb_map <- function(data, region, value,
                    level = c("gemeente", "corop", "provincie"),
                    border_colour = NULL,
                    border_linewidth = 0.15,
                    palette = "sequential",
                    index = NULL,
                    reverse = FALSE,
                    na_fill = NULL,
                    legend = "topleft",
                    flush_legend = TRUE,
                    title = NULL,
                    subtitle = NULL,
                    filllab = NULL,
                    ...) {
  level <- match.arg(level)
  region <- rlang::enquo(region)
  value <- rlang::enquo(value)

  geo <- cpb_nl_geo(level)
  keys <- as.character(rlang::eval_tidy(region, data))
  vals <- rlang::eval_tidy(value, data)

  # join by code or by name, whichever matches more regions
  by_code <- mean(keys %in% geo$code)
  by_name <- mean(keys %in% geo$name)
  geo_key <- if (by_code >= by_name) geo$code else geo$name
  unmatched <- setdiff(keys, geo_key)
  if (length(unmatched)) {
    warning("ggcpb: ", length(unmatched), " region(s) in `data` not found on the ",
            level, " map (", paste(utils::head(unmatched, 5), collapse = ", "),
            if (length(unmatched) > 5) ", ..." else "",
            "); check the level and the region codes/names.", call. = FALSE)
  }
  if (anyDuplicated(keys)) {
    stop("`data` must have exactly one row per region; duplicated: ",
         paste(utils::head(unique(keys[duplicated(keys)]), 5), collapse = ", "),
         call. = FALSE)
  }
  geo$cpb__value <- vals[match(geo_key, keys)]

  tokens <- cpb_tokens()
  # thin seams in the plot background colour separate the regions; on a
  # classed/continuous fill neighbouring classes already differ, so a
  # soft seam is enough. Pass border_colour = "white" for more contrast.
  if (is.null(border_colour)) border_colour <- tokens$bg
  if (is.null(na_fill)) na_fill <- tokens$na

  # the legend sits inside the panel, top-left, by default -- the empty
  # North Sea corner of the Netherlands leaves room for it there
  inside <- identical(legend, "topleft")

  p <- ggplot2::ggplot(geo, ggplot2::aes(
    x = .data$x, y = .data$y, group = .data$part, subgroup = .data$ring,
    fill = .data$cpb__value
  )) +
    ggplot2::geom_polygon(colour = border_colour,
                          linewidth = border_linewidth, ...) +
    # Rijksdriehoek coordinates are metres: equal scales, no projection
    ggplot2::coord_fixed(ratio = 1)

  p <- p + if (is.numeric(vals)) {
    scale_fill_cpb_c(reverse = reverse, na.value = na_fill)
  } else if (!is.null(index)) {
    scale_fill_cpb_manual(index = index, palette = palette, na.value = na_fill)
  } else {
    scale_fill_cpb_d(palette = palette, na.value = na_fill)
  }
  if (is.numeric(vals)) {
    # a compact colourbar, thin like the legend keys; vertical when the
    # legend sits inside top-left, horizontal along the bottom otherwise
    p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(
      direction = if (inside) "vertical" else "horizontal",
      theme = ggplot2::theme(
        legend.key.height = grid::unit(if (inside) 2.4 else 0.25, "cm"),
        legend.key.width  = grid::unit(if (inside) 0.30 else 2.8, "cm")
      )
    ))
  }

  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, fill = filllab) +
    theme_cpb(grid = "none", ticks = FALSE,
              legend = if (inside) "bottom" else legend,
              flush_legend = if (inside) FALSE else flush_legend) +
    ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  if (inside) {
    p <- p + ggplot2::theme(
      legend.position        = "inside",
      legend.position.inside = c(0, 0.98),
      legend.justification   = c(0, 1),
      legend.direction       = "vertical"
    )
  }
  p
}

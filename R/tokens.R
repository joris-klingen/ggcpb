# tokens.R ----
#
# Canonical CPB design tokens: the single source of truth for the CPB
# house style, stored as internal package data -- they are not exported
# directly. Use the exported accessors cpb_tokens(), cpb_pal() and
# cpb_cols() to read them from outside the package.

# qualitative / categorical (primary discrete palette) ----
cpb_colors <- c(
  "#F596AF", "#e6006e", "#820050", "#d7c8c8", "#87d2ff",
  "#005faf", "#193c69", "#96827d", "#64504b", "lightgrey"
)

# alternate discrete ordering (blue/pink lead) ----
cpb_colors_discr <- c(
  "#eb0073", "#005795", "#fad1e8", "#b7e4ff",
  "#820050", "#97cafb", "#00a5ff", "lightgrey"
)

# sequential (light pink -> dark) ----
cpb_colors_scale <- c(
  "#fff1f8", "#fad1e8", "#f08bb8", "#e93a8d", "#a81256", "#4f0a2a", "lightgrey"
)

# structural ----
cpb_bg   <- "#eef8ff" # plot background
cpb_grid <- "#c9d1da" # gridline colour
cpb_na   <- "lightgrey" # NA fill/colour -- always the last entry above

# table accents (exposed for completeness; used by other CPB tooling) ----
cpb_table_header <- "#a81256"
cpb_table_total  <- "#f08bb8"

#' Look up a named CPB palette, excluding the trailing NA colour
#'
#' Internal helper used by [cpb_pal()], [cpb_cols()] and the CPB scales.
#' The last entry of each raw token vector is always the NA colour
#' (`"lightgrey"`), not a data colour, so it is dropped here.
#'
#' @param palette One of `"qualitative"`, `"discr"`, `"sequential"`.
#' @return An unnamed character vector of hex colours (data colours only).
#' @noRd
cpb_palette_colours <- function(palette = c("qualitative", "discr", "sequential")) {
  palette <- match.arg(palette)
  raw <- switch(
    palette,
    qualitative = cpb_colors,
    discr       = cpb_colors_discr,
    sequential  = cpb_colors_scale
  )
  # drop the trailing NA colour ("lightgrey") -- it is never a data colour
  utils::head(raw, -1L)
}

#' Read-only access to the CPB design tokens
#'
#' Returns the canonical CPB colour tokens as a plain list. Use this to inspect the raw hex values, e.g. when
#' building a custom scale that the package does not provide directly.
#' The palette vectors are returned without their trailing NA colour;
#' the NA colour is available separately as `na`.
#'
#' @return A named list with elements `colors`, `colors_discr`,
#'   `colors_scale` (character vectors of data colours), `bg`, `grid`,
#'   `na`, `table_header`, `table_total` (single hex strings).
#' @examples
#' cpb_tokens()$colors
#' cpb_tokens()$bg
#' @export
cpb_tokens <- function() {
  list(
    colors       = cpb_palette_colours("qualitative"),
    colors_discr = cpb_palette_colours("discr"),
    colors_scale = cpb_palette_colours("sequential"),
    bg           = cpb_bg,
    grid         = cpb_grid,
    na           = cpb_na,
    table_header = cpb_table_header,
    table_total  = cpb_table_total
  )
}

# formatters.R ----
#
# Thin wrappers over scales::label_*() with Dutch-locale defaults
# ("." as the thousands separator, "," as the decimal mark).

#' Euro-formatted labels, Dutch locale
#'
#' A thin wrapper around [scales::label_currency()] with the euro sign and
#' configurable grouping/decimal punctuation for Dutch or English styles.
#'
#' @param accuracy Passed to [scales::label_currency()]; `NULL`
#'   (default) lets `scales` pick a sensible accuracy from the data.
#' @param style Formatting style: `"dutch"` (default, `.` thousands, `,` decimal)
#'   or `"english"` (`,` thousands, `.` decimal).
#' @param ... Further arguments passed to [scales::label_currency()].
#' @return A labelling function suitable for `scale_*(labels = ...)`.
#' @examples
#' label_euro_nl()(1234.5)
#' label_euro_nl(style = "english")(1234.5)
#' @export
label_euro_nl <- function(accuracy = NULL, style = c("dutch", "english"), ...) {
  style <- match.arg(style)
  m <- number_format_style(style)
  scales::label_currency(
    prefix       = "\u20ac", # euro sign, as an escape for locale-independent parsing
    big.mark     = m$big.mark,
    decimal.mark = m$decimal.mark,
    accuracy     = accuracy,
    ...
  )
}

#' Percent-formatted labels, Dutch locale
#'
#' A thin wrapper around [scales::label_percent()] with Dutch or English grouping
#' and decimal marks. Note the default `scale = 1`: CPB data is
#' typically already expressed in percentage points (e.g. `45` meaning
#' 45%), unlike `scales::label_percent()`'s own default of `scale =
#' 100`, which expects a proportion (e.g. `0.45`).
#'
#' @param scale Multiplier applied before formatting; `1` (default)
#'   assumes the values are already percentage points. Use `100` for
#'   proportions in `[0, 1]`.
#' @param accuracy Rounding accuracy; `1` (default) rounds to whole
#'   percentage points.
#' @param style Formatting style: `"dutch"` (default, `.` thousands, `,` decimal)
#'   or `"english"` (`,` thousands, `.` decimal).
#' @param ... Further arguments passed to [scales::label_percent()].
#' @return A labelling function suitable for `scale_*(labels = ...)`.
#' @examples
#' label_pct_nl()(c(4.5, 12, 100))
#' label_pct_nl(style = "english")(c(4.5, 12, 100))
#' @export
label_pct_nl <- function(scale = 1, accuracy = 1, style = c("dutch", "english"), ...) {
  style <- match.arg(style)
  m <- number_format_style(style)
  scales::label_percent(
    scale        = scale,
    accuracy     = accuracy,
    big.mark     = m$big.mark,
    decimal.mark = m$decimal.mark,
    ...
  )
}

#' Plain number labels, Dutch locale
#'
#' A thin wrapper around [scales::label_number()] with Dutch or English grouping
#' (`.`) and decimal (`,`) marks.
#'
#' @param style Formatting style: `"dutch"` (default, `.` thousands, `,` decimal)
#'   or `"english"` (`,` thousands, `.` decimal).
#' @param ... Further arguments passed to [scales::label_number()].
#' @return A labelling function suitable for `scale_*(labels = ...)`.
#' @examples
#' label_number_nl()(1234567.8)
#' label_number_nl(style = "english")(1234567.8)
#' @export
label_number_nl <- function(style = c("dutch", "english"), ...) {
  style <- match.arg(style)
  m <- number_format_style(style)
  scales::label_number(
    big.mark     = m$big.mark,
    decimal.mark = m$decimal.mark,
    ...
  )
}

#' Helper function for thousands and decimal marks for Dutch or English styles
#' @noRd
number_format_style <- function(style = c("dutch", "english")) {
  style <- match.arg(style)
  if (style == "english") {
    list(big.mark = ",", decimal.mark = ".")
  } else {
    list(big.mark = ".", decimal.mark = ",")
  }
}

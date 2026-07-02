# formatters.R ----
#
# Thin wrappers over scales::label_*() with Dutch-locale defaults
# ("." as the thousands separator, "," as the decimal mark).

#' Euro-formatted labels, Dutch locale
#'
#' A thin wrapper around [scales::label_currency()] with the euro sign,
#' Dutch thousands separator (`.`) and decimal mark (`,`).
#'
#' @param accuracy Passed to [scales::label_currency()]; `NULL`
#'   (default) lets `scales` pick a sensible accuracy from the data.
#' @param ... Further arguments passed to [scales::label_currency()].
#' @return A labelling function suitable for `scale_*(labels = ...)`.
#' @examples
#' label_euro_nl()(1234.5)
#' label_euro_nl(accuracy = 1)(c(1000, 25000))
#' @export
label_euro_nl <- function(accuracy = NULL, ...) {
  scales::label_currency(
    prefix       = "\u20ac", # euro sign, as an escape for locale-independent parsing
    big.mark     = ".",
    decimal.mark = ",",
    accuracy     = accuracy,
    ...
  )
}

#' Percent-formatted labels, Dutch locale
#'
#' A thin wrapper around [scales::label_percent()] with Dutch grouping
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
#' @param ... Further arguments passed to [scales::label_percent()].
#' @return A labelling function suitable for `scale_*(labels = ...)`.
#' @examples
#' label_pct_nl()(c(4.5, 12, 100))
#' label_pct_nl(scale = 100)(c(0.045, 0.12))
#' @export
label_pct_nl <- function(scale = 1, accuracy = 1, ...) {
  scales::label_percent(
    scale        = scale,
    accuracy     = accuracy,
    big.mark     = ".",
    decimal.mark = ",",
    ...
  )
}

#' Plain number labels, Dutch locale
#'
#' A thin wrapper around [scales::label_number()] with Dutch grouping
#' (`.`) and decimal (`,`) marks.
#'
#' @param ... Further arguments passed to [scales::label_number()].
#' @return A labelling function suitable for `scale_*(labels = ...)`.
#' @examples
#' label_number_nl()(1234567.8)
#' @export
label_number_nl <- function(...) {
  scales::label_number(
    big.mark     = ".",
    decimal.mark = ",",
    ...
  )
}

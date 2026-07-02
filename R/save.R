# save.R ----
#
# Figure export helper. Width is strict and tied to the CPB page format
# (half or full page); height defaults to the CPB report height but has
# a "presentation" preset and can always be overridden explicitly.

#' Save a plot at CPB page dimensions
#'
#' A wrapper around [ggplot2::ggsave()] that enforces the CPB page
#' widths and renders with the `ragg` device by default (needed for the
#' bundled `RijksoverheidSansText` font to render correctly).
#'
#' Width is strict: it is set by `page`, not free-form. `page = "half"`
#' gives a width of 2.98 in; `page = "full"` gives 5.96 in. An explicit
#' `width` is only an escape hatch and is validated against these two
#' values -- any other width errors, so a stray `width = 8` fails
#' loudly rather than silently producing an off-spec figure.
#'
#' Height defaults to 2.98 in (the `"report"` preset). Pass
#' `preset = "presentation"` for the 2.5 in presentation height, or set
#' `height` explicitly for anything else (e.g. a tall stacked-facet
#' export) -- an explicit `height` always wins over `preset`.
#'
#' @param filename Path to write to; passed to [ggplot2::ggsave()].
#' @param plot The plot to save; defaults to [ggplot2::last_plot()].
#' @param page Either `"half"` (default, 2.98 in wide) or `"full"`
#'   (5.96 in wide). Ignored if `width` is supplied explicitly.
#' @param preset Either `"report"` (default, 2.98 in tall) or
#'   `"presentation"` (2.5 in tall). Ignored if `height` is supplied
#'   explicitly.
#' @param height Explicit height in inches. `NULL` (default) uses
#'   `preset` to determine the height.
#' @param width Explicit width in inches; must be `2.98` or `5.96`.
#'   `NULL` (default) uses `page` to determine the width.
#' @param dpi Resolution in dots per inch; defaults to `300`. CPB tall
#'   exports commonly use `dpi = 800`.
#' @param device Graphics device passed to [ggplot2::ggsave()];
#'   defaults to [ragg::agg_png()] so the bundled CPB font renders
#'   correctly.
#' @param bg Output background colour; defaults to the CPB background
#'   colour so it matches `theme_cpb()`'s on-plot background. Use
#'   `bg = NA` for a transparent background.
#' @param ... Further arguments passed to [ggplot2::ggsave()].
#' @return Invisibly, the `filename` that was written.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl))) +
#'   geom_bar() +
#'   theme_cpb()
#' save_cpb("cyl_bar.png", p, page = "half")
#' save_cpb("cyl_bar_full.png", p, page = "full", preset = "presentation")
#' }
#' @export
save_cpb <- function(filename,
                      plot = ggplot2::last_plot(),
                      page = c("half", "full"),
                      preset = c("report", "presentation"),
                      height = NULL,
                      width = NULL,
                      dpi = 300,
                      device = ragg::agg_png,
                      bg = cpb_bg,
                      ...) {
  page <- match.arg(page)
  preset <- match.arg(preset)

  page_widths <- c(half = 2.98, full = 5.96)
  allowed_widths <- unname(page_widths)

  if (is.null(width)) {
    width <- unname(page_widths[[page]])
  } else if (!any(abs(width - allowed_widths) < 1e-6)) {
    stop(
      "save_cpb(): `width` must be one of the CPB page widths (2.98 or ",
      "5.96 inches); got ", width, ". Use `page = \"half\"` or ",
      "`page = \"full\"` instead, or pass an explicit width matching one ",
      "of these two values.",
      call. = FALSE
    )
  }

  if (is.null(height)) {
    height <- if (preset == "presentation") 2.5 else 2.98
  }

  ggplot2::ggsave(
    filename = filename,
    plot     = plot,
    width    = width,
    height   = height,
    units    = "in",
    dpi      = dpi,
    device   = device,
    bg       = bg,
    ...
  )

  tcat("ggcpb: wrote ", filename, " (", width, " x ", height, " in, ", dpi, " dpi)")

  invisible(filename)
}

# cut.R ----
#
# cpb_cut(): bin a numeric vector into an ordered factor with tidy Dutch
# class labels ("lager dan 20%", "20% - 30%", ..., "60% en hoger"), the
# labelling convention of the classed CPB figures (choropleths, stacked
# distributional bars). A thin, house-styled wrapper around cut() in the
# spirit of the OS Amsterdam os_cut() helper, but formatting the numbers
# through the package's Dutch-locale formatters instead of raw prefixes.

#' Bin a numeric vector into ordered Dutch class labels
#'
#' Cuts `x` at `breaks` into an ordered factor whose levels read the way
#' classed CPB figures label them: an open first class
#' (`"lager dan 20%"`), closed middle classes (`"20% - 30%"`) and an
#' open last class (`"60% en hoger"`). Numbers are formatted with a
#' Dutch-locale formatter, so decimal commas, thousands separators, `%`
#' and `€` come out right. Pair with `palette = "blues"` in the CPB
#' scales (or [cpb_map()]) for the classed light-to-dark fill.
#'
#' @param x A numeric vector to bin.
#' @param breaks A numeric vector of cut points including the outer
#'   bounds, e.g. `c(0, 20, 30, 40, 50, 60, Inf)`. As with [cut()], `n`
#'   breaks give `n - 1` classes. A single integer is passed to [cut()]
#'   to request that many equal-width bins (labelled the same way).
#' @param labeller A labelling function applied to the break values,
#'   one of the package formatters ([label_number_nl()] (default),
#'   [label_pct_nl()], [label_euro_nl()]) or any `function(x)` returning
#'   a character vector. Only the *finite* interior breaks are labelled;
#'   the open ends use `start_label`/`end_label`.
#' @param start_label,end_label,sep Text around the numbers:
#'   `start_label` precedes the first class's upper bound
#'   (`"lager dan "`), `end_label` follows the last class's lower bound
#'   (`" en hoger"`), and `sep` joins the two bounds of a middle class
#'   (`" - "`).
#' @param first,last Optional overrides for the first/last class label
#'   (e.g. `first = "geen"`); `NULL` (default) uses the constructed
#'   open-ended labels.
#' @param ordered If `TRUE` (default), return an ordered factor.
#' @return An (ordered) factor the same length as `x`.
#' @examples
#' aandeel <- c(8, 24, 33, 47, 61, 72)
#' cpb_cut(aandeel, breaks = c(0, 20, 30, 40, 50, 60, Inf),
#'         labeller = label_pct_nl())
#' # -> lager dan 20% / 20% - 30% / ... / 60% en hoger
#' @export
cpb_cut <- function(x,
                    breaks,
                    labeller = label_number_nl(),
                    start_label = "lager dan ",
                    end_label = " en hoger",
                    sep = " - ",
                    first = NULL,
                    last = NULL,
                    ordered = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }
  if (!is.function(labeller)) {
    stop("`labeller` must be a function, e.g. label_number_nl().", call. = FALSE)
  }

  # a single integer means "this many equal-width bins": let cut() pick
  # the breaks, then relabel them in house style
  if (length(breaks) == 1L && is.numeric(breaks) && breaks >= 1) {
    tmp <- cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)
    breaks <- as.numeric(unique(c(
      sub("^[\\[(]([^,]+),.*$", "\\1", levels(tmp)[1]),
      sub("^.*,([^]\\)]+)[]\\)]$", "\\1", levels(tmp))
    )))
  }

  breaks <- sort(unique(breaks))
  if (length(breaks) < 2) {
    stop("`breaks` must give at least two cut points (one class).", call. = FALSE)
  }
  n_class <- length(breaks) - 1L

  # label only the finite interior breaks; open ends are worded
  interior <- breaks[-c(1L, length(breaks))]
  lab <- function(v) vapply(v, function(b) {
    if (is.finite(b)) labeller(b) else as.character(b)
  }, character(1))
  ib <- lab(interior)

  if (n_class == 1L) {
    labels <- paste0(start_label, ib[1])       # degenerate: one open class
  } else {
    lo_open <- paste0(start_label, ib[1])
    hi_open <- paste0(ib[length(ib)], end_label)
    mids <- character(0)
    if (n_class > 2L) {
      mids <- paste0(ib[-length(ib)], sep, ib[-1L])
    }
    labels <- c(lo_open, mids, hi_open)
  }

  if (!is.null(first)) labels[1] <- first
  if (!is.null(last))  labels[length(labels)] <- last

  cut(x, breaks = breaks, labels = labels,
      ordered_result = ordered, include.lowest = TRUE, right = TRUE)
}

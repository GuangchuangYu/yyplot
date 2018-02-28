##' setting graphic parameter
##'
##' setting graphic parameter via `par` for base plot and apply it to ggplot2. Currently only works for par(mar).
##' @title ggpar
##' @param ... parameters pass to par
##' @return NULL
##' @author Guangchuang Yu
##' @importFrom ggplot2 theme_set
##' @importFrom ggplot2 theme_get
##' @importFrom ggplot2 theme
##' @importFrom grid unit
##' @importFrom graphics par
##' @export
ggpar <- function(...) {
    par(...)
    theme_set(theme_get() +
              theme(plot.margin=unit(par('mar'), 'lines')))
}

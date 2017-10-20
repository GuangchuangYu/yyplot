##' setting font for ggplot (axis text, label, title, etc.)
##'
##'
##' @title set_font
##' @param p ggplot object
##' @param family font fammily
##' @param fontface font face
##' @param size font size
##' @param color font color
##' @return TableGrob object
##' @importFrom grid editGrob
##' @importFrom grid grid.ls
##' @importFrom grid grid.force
##' @importFrom grid gPath
##' @importFrom grid gpar
##' @importFrom ggplot2 ggplotGrob
##' @importFrom ggplot2 .pt
##' @export
##' @examples
##' library(grid)
##' library(ggplot2)
##' d <- data.frame(x=rnorm(10), y=rnorm(10), lab=LETTERS[1:10])
##' p <- ggplot(d, aes(x, y)) + geom_text(aes(label=lab), size=5)
##' g <- set_font(p, family="Times", fontface="italic", color='firebrick')
##' grid.draw(g)
##' @author guangchuang yu
set_font <- function(p, family="sans", fontface=NULL, size=NULL, color=NULL) {
    if (!is.null(size))
        size <- size  * .pt
    par <- list(fontfamily = family, fontface = fontface, fontsize = size, col = color)
    par <- par[!sapply(par, is.null)]
    gp <- do.call(gpar, par)
    g <- ggplotGrob(p)
    ng <- grid.ls(grid.force(g), print=FALSE)$name
    txt <- ng[which(grepl("text", ng))]

    for (i in seq_along(txt)) {
        g <- editGrob(grid.force(g), gPath(txt[i]),
                      grep = TRUE, gp = gp)
    }
    return(g)
}

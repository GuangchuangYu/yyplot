##' convert pdf slides to one single png figure
##'
##'
##' @title slides2png
##' @param pdf pdf file
##' @param outfile output png file
##' @param scale scale of the width & height
##' @param page which pages to be converted
##' @param add_page whether add page number
##' @return gtable
##' @export
##' @importFrom grid rasterGrob
##' @importFrom grid textGrob
##' @importFrom grid gpar
##' @importFrom grid grobTree
##' @importFrom grid unit
##' @importFrom grid grid.draw
##' @importFrom ggplot2 ggsave
##' @importFrom magick image_read
##' @importFrom magick image_info
##' @importFrom gtable gtable_matrix
##' @importFrom grDevices png
##' @importFrom grDevices dev.off
##' @author guangchuang yu
slides2png <- function(pdf, outfile = "slides.png", scale=1, page="all", add_page=TRUE) {
    x <- image_read(pdf)
    if (is.character(page) && page == "all")
        page <- 1:length(x)
    if (!is.numeric(page))
        stop("page should be numeric vector")


    width <- image_info(x[page])$width[page]
    height <- image_info(x[page])$height[page]

    if (add_page) {
        grobs <- lapply(page, function(i)
            grobTree(rasterGrob(x[i]),
                     textGrob(x=.9, y=.9, page[i],
                              gp=gpar(col='steelblue', cex=2, fontfamily='Impact'))))
    } else {
        grobs <- lapply(page, function(i) rasterGrob(x[i]))
    }

    mat <- matrix(grobs, nrow=length(page))
    w <- max(width) * scale
    h <- w * height/width
    g <- gtable_matrix(name = "demo", grobs = mat,
                       widths = unit(w, "points"),
                       heights = unit(h, "points"))

    png(outfile, width=w, height = sum(h))
    grid.draw(g)
    dev.off()
    ## ggsave(g, filename=outfile, width=px2in(w), height = px2in(sum(h)), limitsize=FALSE)
    invisible(g)
}


px2in <- getFromNamespace("px2in", "meme")

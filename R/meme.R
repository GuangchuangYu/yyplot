##' create meme
##'
##'
##' @title meme
##' @param img path or url
##' @param upper upper text
##' @param lower lower text
##' @param size size of text
##' @param color color of text
##' @param font font family of text
##' @param plot boolean, one of TRUE or FALSE
##' @return grob object
##' @importFrom magick image_read
##' @importFrom magick image_info
##' @importFrom grid textGrob
##' @importFrom grid rasterGrob
##' @importFrom grid gpar
##' @importFrom grid viewport
##' @importFrom grid grid.draw
##' @importFrom grid gList
##' @importFrom grDevices dev.new
##' @export
##' @author guangchuang yu
meme <- function(img, upper="", lower="", size="auto", color="white", font="Helvetica", plot=TRUE) {
    x <- image_read(img)
    info <- image_info(x)

    if (size == "auto") {
        size <- info$height/250
    }

    gp <- gpar(col = color, fontfamily = font, cex = size)
    upperGrob <- textGrob(toupper(upper), gp = gp, vp = viewport(y=.8))
    lowerGrob <- textGrob(toupper(lower), gp = gp, vp = viewport(y=.1))

    meme <- gList(rasterGrob(x), upperGrob, lowerGrob)

    if (plot) {
        dev.new(width=7, height=7*info$height/info$width)
        grid.draw(meme)
    }

    res <- list(gList=meme, width = info$width, height = info$height)
    invisible(res)
}

##' save meme plot
##'
##'
##' @title meme_save
##' @param meme meme output
##' @param file output file
##' @param device graphic device
##' @param dpi dpi of the figure
##' @return NULL
##' @export
##' @author guangchuang yu
meme_save <- function(meme, file="meme.png", device=NULL, dpi = 300) {
    dev <- plot_dev(device, file, dpi = dpi)
    dev(file = file, width = meme$width * 0.010417, height = meme$height * 0.010417)
    on.exit(utils::capture.output(grDevices::dev.off()))
    grid.draw(meme$gList)
    invisible()
}

plot_dev <- getFromNamespace("plot_dev", "ggplot2")

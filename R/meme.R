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

    scale <- 250
    if (size == "auto") {
        ## size <- info$height / 10
        size <- info$height/scale
    }
    ## m <- (info$width - size)/2
    ## u <- paste0("+", m, "+", info$height * .1)
    ## l <- paste0("+", m, "+", info$height * .9)
    ## x <- image_annotate(x, toupper(upper), size=size, location = u, color = color, font = font)
    ## x <- image_annotate(x, toupper(lower), size=size, location = l, color = color, font = font)

    upperGrob <- textGrob(toupper(upper), gp = gpar(col = color, fontfamily = font, cex = size), vp = viewport(y=.8))
    lowerGrob <- textGrob(toupper(lower), gp = gpar(col = color, fontfamily = font, cex = size), vp = viewport(y=.1))

    res <- gList(rasterGrob(x), upperGrob, lowerGrob)

    if (plot) {
        dev.new(width=info$width/scale, height=info$height/scale)
        grid.draw(res)
    }
    invisible(res)
}


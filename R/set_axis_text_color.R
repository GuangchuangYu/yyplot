##' color axis text by specifying color and add legend based on mapping variable
##'
##'
##' @title set_axis_text_color
##' @param p ggplot object
##' @param colors named vector of colors
##' @param mapping mapping variable for generating legend
##' @param fake_aes fake aes that use to add a invisible layer for generating legend
##' @param axis one of 'x', 'y', or 'xy'
##' @return ggplot object
##' @importFrom ggplot2 guide_legend
##' @importFrom ggplot2 guides
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 element_text
##' @export
##' @author guangchuang yu
##' @examples
##' require(ggplot2)
##' set.seed(2017-07-04)
##' d <- data.frame(x = letters[1:5], y = rnorm(5),
##'		group = sample(c("Control", "Treatment"), 5, replace=TRUE),
##'		type = sample(LETTERS[1:2], 5, replace=TRUE)
##')
##' cols <- c(A = "red", B = "blue")
##' p <- ggplot(d, aes(x,y)) + geom_col(aes(fill=group))
##' set_axis_text_color(p, colors=cols, mapping=~type, fake_aes='color')
set_axis_text_color <- function(p, colors, mapping, fake_aes='color', axis="x") {
    if (is.null(names(colors)) || class(colors) != "character") {
        stop("colors should be a named vector...")
    }

    leg <- guide_legend(override.aes=list(color=colors, alpha=1))
    gleg <- list(1)
    names(gleg) <- fake_aes
    gleg[[1]] <- leg
    gleg <- do.call(guides, gleg)

    m <- list(mapping, mapping)
    names(m) <- c(fake_aes, 'label')
    aes_mapping <- do.call(aes_, m)

    p <- p + geom_text(aes_mapping, alpha=0) + gleg
    axis_color <- element_text(color=colors[p$data$type])
    if (axis == "x") {
        p + theme(axis.text.x=axis_color)
    } else if (axis == "y") {
        p + theme(axis.text.y=axis_color)
    } else {
        p + theme(axis.text=axis_color)
    }
    return(p)
}


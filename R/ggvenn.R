##' venn plot using ggplot2
##'
##'
##' @title ggvenn
##' @param x data
##' @param alpha transparency of color
##' @return ggplot object
##' @importFrom rvcheck get_fun_from_pkg
## @importFrom venneuler venneuler
##' @importFrom ggforce geom_circle
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 coord_fixed
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @export
##' @author guangchuang yu
##' @examples
##' \dontrun{
##' set.seed(2017-11-08)
##' x <- matrix(sample(0:4, 40, TRUE, c(.5, .1, .1, .1, .1)), ncol=4)
##' colnames(x) <- LETTERS[1:4]
##' ggvenn(x)
##' }
ggvenn <- function(x, alpha = 0.5) {
    ## the venneuler depends on rJava
    ## maybe wrapping VennDiagram (output gList) as geom layer
    venneuler <- get_fun_from_pkg("venneuler", "venneuler") ## for easy installation, as many users have issue of rJava installation
    y <- venneuler(x)
    d <- data.frame(y$centers,
                    diameters = y$diameters,
                    labels = y$labels,
                    stringsAsFactors = FALSE)
    ggplot(d) +
        geom_circle(aes_(x0 = ~x, y0 = ~y,
                         r = ~diameters/2, fill = ~labels),
                    alpha = alpha) +
        geom_text(aes_(x = ~x, y = ~y, label = ~labels)) +
        coord_fixed()
}



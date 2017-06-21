##' generate qrcode by ggplot
##'
##'
##' @title ggqrcode
##' @param text text string
##' @param color color
##' @param alpha [0, 1] for transparency
##' @return ggplot object
##' @importFrom qrcode qrcode_gen
##' @importFrom tidyr gather_
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 scale_fill_gradient
##' @importFrom ggplot2 theme_void
##' @importFrom ggplot2 theme
##' @export
##' @author guangchuang yu
ggqrcode <- function(text, color="black", alpha=1) {
    pkg <- "qrcode"
    require(pkg, character.only = TRUE)
    x <- qrcode_gen(text, plotQRcode=F, dataOutput=T)
    x <- as.data.frame(x)

    y <- x
    y$id <- rownames(y)
    y <- gather_(y, "key", "value", colnames(y)[-ncol(y)])
    y$key = factor(y$key, levels=rev(colnames(x)))
    y$id = factor(y$id, levels=rev(rownames(x)))

    ggplot(y, aes_(x=~id, y=~key)) + geom_tile(aes_(fill=~value), alpha=alpha) +
        scale_fill_gradient(low="white", high=color) +
        theme_void() + theme(legend.position='none')
}

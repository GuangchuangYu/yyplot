##' generate qrcode by ggplot
##'
##'
##' @title ggqrcode
##' @param text text string
##' @param color color
##' @param alpha [0, 1] for transparency
##' @return ggplot object
##' @importFrom tidyr gather
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 scale_fill_manual
##' @importFrom ggplot2 theme_void
##' @importFrom ggplot2 theme
##' @export
##' @author guangchuang yu
ggqrcode <- function(text, color="black", alpha=1) {
    qr_code <- get_fun_from_pkg(fun = 'qr_code', pkg = "qrcode")
    x <- qr_code(text)
    x <- as.data.frame(x)

    y <- x
    y$id <- rownames(y)
    y <- gather(y, "key", "value", colnames(y)[-ncol(y)])
    y$key = factor(y$key, levels=rev(colnames(x)))
    y$id = factor(y$id, levels=rev(rownames(x)))

    ggplot(y, aes_(x=~id, y=~key)) + geom_tile(aes_(fill=~value), alpha=alpha) +
        scale_fill_manual(values=c("white", color)) + 
        # scale_fill_gradient(low="white", high=color) +
        theme_void() + theme(legend.position='none')
}

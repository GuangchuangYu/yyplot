##' msa plot
##'
##' convert the msa::msaPrettyPrint output to a ggplot object
##' @title msaPrettyPlot
##' @param x an instance of MultipleAlignment
##' @param ... additional parameters passed to msaPrettyPrint
##' @param density resolution for parsing temp PDF file
##' @return ggplot object
##' @importFrom ggimage image_read2
##' @importFrom ggplotify as.ggplot
##' @export
##' @author guangchuang yu
msaPrettyPlot <- function(x, ..., density=300) {
    msaPrettyPrint <- get_fun_from_pkg("msa", "msaPrettyPrint")
    wd <- getwd()
    setwd(tempdir())
    msaPrettyPrint(x, output = "pdf", ...)
    img <- ggimage::image_read2("x.pdf", density = density)
    setwd(wd)
    ggplotify::as.ggplot(img)
}

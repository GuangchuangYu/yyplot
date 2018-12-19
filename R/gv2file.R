##' convert gv (grViz object, output of DiagrammeR) to image file
##'
##' 
##' @title gv2file
##' @param gv grViz object
##' @param file output file
##' @param width width
##' @param height height
##' @return raw
##' @author Guangchuang
##' @importFrom DiagrammeRsvg export_svg
##' @importFrom rsvg rsvg_pdf
##' @importFrom rsvg rsvg_png
##' @importFrom rsvg rsvg_svg
##' @importFrom tools file_ext
##' @export
gv2file <- function(gv, file, width=NULL, height=NULL) {
    ext <- tools::file_ext(file)

    if (! tolower(ext) %in% c('pdf', 'tiff', 'png', 'jpg')) {
        stop("file extension should be one of 'pdf', 'svg' or 'png'.")
    }

    svg <- DiagrammeRsvg::export_svg(gv) %>% charToRaw
    if (ext == 'pdf') {
        rsvg::rsvg_pdf(svg, file=file, width=width, height=height)
    } else if (ext == 'svg') {
        rsvg::rsvg_svg(svg, file=file, width=width, height=height)
    } else {
        rsvg::rsvg_png(svg, file=file, width=width, height=height)
    }
    invisible(svg)
}

##' @importFrom grid grid.draw
##' @method grid.draw expression
##' @export
grid.draw.expression <- function(x, recording = TRUE) {
    eval(x)
}

##' @method grid.draw formula
##' @export
grid.draw.formula <- function(x, recording = TRUE) {
    xx = as.character(x)[2]
    eval(parse(text=xx))
}

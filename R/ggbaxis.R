



##' prototype of using facet_grid to mimic axis break
##'
##'
##' @title ggbaxis
##' @param p ggplot object
##' @param axis one of 'x' or 'y'
##' @param breaks break interval
##' @return ggplot object
##' @importFrom rvcheck get_aes_var
##' @importFrom ggplot2 facet_grid
##' @export
##' @examples
##' \dontrun{
##' d = data.frame(x=c(rnorm(5)+4,rnorm(5)+20,rnorm(5)+5,rnorm(5)+22), y=1:20)
##' mymin = function(x) ifelse(x <= 7, 0, 17)
##' p <- ggplot(d, aes(x, y)) + geom_rect(aes(xmin=mymin(x), xmax=x, ymin=y-.4, ymax=y+.4))
##' ggbaxis(p, axis="x", breaks=c(7, 17))
##' }
##' @author guangchuang yu
ggbaxis <- function(p, axis="x", breaks) {

    d = p$data
    if (axis == "x") {
        v = get_aes_var(p$mapping, "x")
    } else {
        v = get_aes_var(p$mapping, "y")
    }
    d$.type = NA
    d$.type[d[[v]] < breaks[1]] = "p1"
    d$.type[d[[v]] > breaks[2]] = "p2"
    d <- d[!is.na(d$.type),]
    d2 <- d[d$.type == "p2",]

    d2[[v]] = breaks[1]
    d2$.type = "p1"
    d = rbind(d, d2)
    p$data = d
    p + facet_grid(.~.type, scales = "free") +
        theme(strip.text=element_blank())

}



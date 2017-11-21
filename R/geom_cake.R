##' @importFrom grid gList
##' @importFrom grid rectGrob
##' @importFrom grid polygonGrob
##' @importFrom grid gpar
candleGrob <- function(x, y, color.candle = "orange", color.fire = "red", vp=NULL) {
    width <- 0.02
    height <- 0.2

    xx = c(x+.005,x-.01,x+.01,x+.03,x+.015,x+0.005)
    yy = c(y+.2,y+.23,y+.26,y+.23,y+.2,y+.2)

    gList(
        rectGrob(x+width/2, y+height/2, width = width, height = height, gp = gpar(fill=color.candle), vp=vp),
        polygonGrob(xx, yy, gp = gpar(fill = color.fire), vp=vp)
    )
}

ellipseGrob <- function(x, y, a, b, gp=gpar(), vp=NULL) {
    t <- seq(0, 2*pi, length.out=100)
    xx <- x + a * cos(t)
    yy <- y + b * sin(t)
    polygonGrob(xx, yy, gp = gp, vp=vp)
}

##' @importFrom grid segmentsGrob
cakeGrob <- function(x=.5, y=.5, a=.4, b=.14, A=.44, B=.17, height=.3, gp=gpar(), vp=NULL) {
    gp2 <- gp
    if (!is.null(gp$fill)) {
        gp2$col <- gp2$fill
    }
    gList(
        ellipseGrob(x, y-height, A, B, gp=gp, vp=vp),
        ellipseGrob(x, y-height, a, b, gp=gp, vp=vp),
        rectGrob(x, y-height/2, a*2, height, gp=gp2, vp=vp),
        segmentsGrob(x-a, y-height, x-a, y, gp=gp, vp=vp),
        segmentsGrob(x+a, y-height, x+a, y, gp=gp, vp=vp),
        ellipseGrob(x, y, a, b, gp=gp, vp=vp))
}

##' @importFrom grid gTree
cakeCandleGrob <- function(color.cake = "pink", color.candle="orange", color.fire="red", vp=NULL, name=NULL) {
    grobs <- gList(cakeGrob(x=.5, y=.5, a=.4, b=.14, A=.44, B=.17, height=.3, gp=gpar(fill=color.cake)),
                   candleGrob(.25,.45, color.candle, color.fire),
                   candleGrob(.3,.5, color.candle, color.fire),
                   candleGrob(.4, .45,color.candle, color.fire),
                   candleGrob(.5,.5, color.candle, color.fire),
                   candleGrob(.6, .45, color.candle, color.fire),
                   candleGrob(.7, .52, color.candle, color.fire)
                   )
    gTree(children=grobs, name=name, vp=vp)
}


##' ggplot2 layer of birthday cake
##'
##'
##' @title geom_cake
##' @param mapping aes mapping
##' @param data data
##' @param ... additional parameters
##' @return ggplot2 layer
##' @importFrom ggplot2 layer
##' @export
##' @author guangchuang yu
geom_cake <- function(mapping = NULL, data = NULL, ...) {
    layer(
        data = data,
        mapping = mapping,
        geom = GeomCake,
        stat = "identity",
        position = "identity",
        params = list(...),
        check.aes = FALSE
    )
}

##' @importFrom grid viewport
##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 Geom
##' @importFrom ggplot2 draw_key_blank
##' @importFrom ggplot2 aes
GeomCake <- ggproto("GeomCake", Geom,
                  draw_panel = function(data, panel_scales, coord) {
                      data <- coord$transform(data, panel_scales)

                      grobs <- lapply(1:nrow(data), function(i) {
                          vp <- viewport(x=data$x[i], y=data$y[i],
                                         width=data$size[i], height=data$size[i],
                                         just = c("center", "center"),
                                         default.units = "native")
                          cakeCandleGrob(data$color.cake[i], data$color.candle[i], data$color.fire[i], vp=vp, name=i)
                      })
                      class(grobs) <- "gList"
                      ggplot2:::ggname("geom_cake",
                                       gTree(children = grobs))
                  },
                  non_missing_aes = c("x", "y", "size", "color.cake", "color.candle", "color.fire"),
                  default_aes = aes(size=.1, color.cake="#FF3399", color.candle = "orange", color.fire="red"),
                  draw_key = draw_key_blank
                  )



##' @importFrom ggplot2 geom_rect
##' @importFrom ggplot2 geom_polygon
geom_candle <- function(x, y) {
    d <- data.frame(
        x = x,
        y = y)

    d2 <- data.frame(
        x = c(x+.05,x-.1,x+.1,x+.3,x+.15,x+0.05),
        y = c(y+2,y+2.3,y+2.6,y+2.3,y+2,y+2))

    list(
        geom_rect(aes_(xmin=~x, ymin=~y,
                      xmax=~x+.2, ymax=~y+2
                      ),
                  data = d, fill = "red", inherit.aes = FALSE),
        geom_polygon(aes_(x=~x, y=~y), data=d2, fill = "orange", inherit.aes = FALSE)
    )
}

geom_ellipse <- function(x, y, a, b, fill, color="black") {
    t <- seq(0, 2*pi, length.out=100)
    d <- data.frame(x = x + a * cos(t),
                    y = y + b * sin(t))
    geom_polygon(aes_(x=~x, y=~y), data=d, fill=fill, color=color)
}


##' draw a cake using ggplot2
##'
##'
##' @title ggcake
##' @param cake_color color of cake
##' @return ggplot object
##' @importFrom ggplot2 annotate
##' @importFrom ggplot2 theme_void
##' @export
##' @references \url{https://www.r-bloggers.com/fun-with-r-graphics-a-raptor-and-a-cake/}
##' @author guangchuang yu
ggcake <- function(cake_color="#FF3399") {
    d <- data.frame(x = c(0, 10), y = c(0, 10))
    ggplot(d, aes_(x = ~x, y = ~y)) +
        geom_ellipse(5, 2, 4.4, 1.7, fill = cake_color) +
        geom_ellipse(5,2, 4, 1.4, fill = cake_color) +
        annotate("rect", xmin=1, ymin=2, xmax=9, ymax=5, fill=cake_color, color=cake_color) +
        annotate("segment", x=1, y=2, xend=1, yend=5) +
        annotate("segment", x=9, y=2, xend=9, yend=5) +
        geom_ellipse(5,5, 4, 1.4, fill = cake_color) +
        geom_candle(2.5,4.5) +
        geom_candle(3,5) +
        geom_candle(4,4.5) +
        geom_candle(5,5) +
        geom_candle(6,4.5) +
        geom_candle(7,5.2) + theme_void()
}




gglegend <- function(mapping, data, geom, p) {
    xvar <- get_aes_var(p$mapping, 'x')
    yvar <- get_aes_var(p$mapping, 'y')

    dd <- cbind(data, p$data[1:nrow(data), c(xvar, yvar)])
    m <- names(mapping)

    var <- data[[m]]
    names(var) <- data[[get_aes_var(mapping,m)]]

    modifyList(mapping, p$mapping[c('x', 'y')]) -> mapping

    a <- list(var,  alpha = 1)
    names(a)[1] <- m
    leg <- guide_legend(override.aes=a)
    gleg <- list(1)
    names(gleg) <- m
    gleg[[1]] <- leg
    gleg <- do.call(guides, gleg)

    ## if geom-text
    mapping <- modifyList(mapping, aes_string(label=m))
    p + geom(mapping, dd, alpha=0)  + gleg
}



## p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()

## data <- data.frame(colour = c("red",  "blue"), VALUE = c("A", "B"))

## gglegend(aes(colour = VALUE), data, geom_text, p)

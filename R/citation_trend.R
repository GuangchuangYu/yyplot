##' plot citation trend
##'
##'
##' @title plot_citation_trend
##' @param id google scholar ID
##' @param pubid publication id
##' @return ggplot object
##' @importFrom scholar get_article_cite_history
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 geom_point
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @export
##' @author guangchuang yu
plot_citation_trend <- function(id, pubid) {
    df <- get_article_cite_history(id, pubid)
    df$year <- as.factor(df$year)
    title <- paste('Cited by', sum(df$cites))
    p <- ggplot(df, aes_(~year, ~cites)) + geom_segment(aes_(xend=~year, yend=0)) + geom_point(size=3, color='firebrick') +
        theme_minimal() + xlab(NULL) + ylab(NULL) + labs(title=title, caption="data from Google Scholar")
    return(p)
}



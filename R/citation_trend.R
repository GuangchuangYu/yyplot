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
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 ylim
##' @importFrom ggplot2 element_blank
##' @export
##' @author guangchuang yu
plot_citation_trend <- function(id, pubid) {
    df <- get_article_cite_history(id, pubid)
    df$year <- as.factor(df$year)
    title <- paste('Cited by', sum(df$cites))
    ggplot(df, aes_(~year, ~cites)) +
        geom_segment(aes_(xend=~year, yend=0), size=1, color='grey') +
        geom_point(size=8, color="#96B56C") +
    geom_text(aes(label=~cites), size=3) +
        ylim(NA, round(max(df$cites) * 1.1)) +
        xlab(NULL) +
        ylab(NULL) +
        labs(title=title, caption="data from Google Scholar") +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank())
}



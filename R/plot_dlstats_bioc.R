##' plot download stats of Bioconductor package
##'
##'
##' @title plot_dlstats_bioc
##' @param pkg package name
##' @return ggplot object
##' @export
##' @importFrom dlstats bioc_stats
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 labs
##' @author guangchuang yu
plot_dlstats_bioc <- function(pkg) {
    x <- dlstats::bioc_stats(pkg)
    x <- x[-nrow(x), ] # last month data is not complete
    ## txtplot(ggtree::Date2decimal(x$end), x$Nb_of_downloads, width=100, height=25)
    ggplot(x, aes_(~end, ~Nb_of_distinct_IPs)) + geom_point() + geom_line() +
        theme_minimal() +xlab(NULL) + ylab(NULL) +
        labs(title="Monthly download stats", subtitle="by distinct IPs", caption="data from Bioconductor")
}

##' query pubmed record numbers of search term through years
##'
##'
##' @title pubmed_trend
##' @param searchTerm search term
##' @param year year vector
##' @param verbose whether print message
##' @return data.frame
##' @author guangchuang yu
##' @importFrom plyr ldply
##' @importFrom dplyr rename_
##' @export
##' @examples
##' \dontrun{
##' pubmed_trend("Yu Guangchuang[Full Author Name]", 2010:2016)
##' }
pubmed_trend <- function(searchTerm, year, verbose=TRUE) {
    if (length(searchTerm) == 1) {
        res.df <- pubmed_trend.internal(searchTerm, year)
    } else {
        res <- lapply(searchTerm, pubmed_trend.internal, year=year)
        names(res) <- searchTerm
        res.df <- ldply(res)
        colnames(res.df)[1] <- "TERM"
    }

    bg <- pubmed_trend.internal("", year, verbose = FALSE)
    bg <- rename_(bg, ALL=~number)
    res.df <- merge(res.df, bg, by="year", all.x=TRUE)

    class(res.df) <- c("pubmedTrend", "data.frame")
    return(res.df)
}

##' @importFrom RISmed EUtilsSummary
##' @importFrom RISmed QueryCount
pubmed_trend.internal <- function(searchTerm, year, verbose=TRUE) {
    num <- array()
    x <- 1
    if (verbose)
        message('search term: ', searchTerm)

    for (i in year){
        Sys.sleep(1)
        if (verbose)
            message("-> querying year ", i)
        r <- EUtilsSummary(searchTerm, type='esearch', db='pubmed', mindate=i, maxdate=i)
        num[x] <- QueryCount(r)
        x <- x + 1
    }
    res <- data.frame(year=year, number=num)
    return(res)
}

##' @method plot pubmedTrend
##' @export
##' @importFrom ggplot2 geom_line
plot.pubmedTrend <- function(x, y, ...) {
    if ('TERM' %in% colnames(x)) {
        mapping <- aes_(x = ~year, y = ~number/ALL, group = ~TERM, color = ~TERM)
    } else {
        mapping <- aes_(x = ~year, y = ~number/ALL)
    }
    ggplot(x, mapping) + geom_line() + geom_point()
}

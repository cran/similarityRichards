`simPlot` <-
function (simFits = NULL, ...) 
{
    Names <- unique(unlist(lapply(simFits, function(i) names(i))))
    Result <- NULL
    for (name in Names) {
        item <- list(simPlotOne(simFits, name, ...))
        names(item) <- name
        Result <- append(Result, item)
    }
    Result
}

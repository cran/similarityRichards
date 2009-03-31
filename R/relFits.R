setClass("relFits", contains = "list")

`relFits` <-
function (object, FUN, applyFUN2X = function(X, FUN, ...) FUN(X, 
    ...), refName = "Ref", ...) 
{
    return(new("relFits", 
    lapply(object, function(i) {
        index.Ref.relFits <- which(names(i) == refName)
        if (is.numeric(refName)) 
            index.Ref.relFits <- refName
        # print(index.Ref.relFits)
        if (length(index.Ref.relFits) != 1) {
            message("Invalid 'refName' in 'relFits'")
            return(NULL)
        } else {
            fitRef <- applyFUN2X(i[index.Ref.relFits][[1]], FUN, ...)
            lapply(i[-index.Ref.relFits], function(X)
                                       relFit(X, fit = fitRef, ...))}
    })))
}

setClass("simFits", contains = "list")

`simFits` <-
function (object, FUN,
          applyFUN2X = function(X, FUN, ...) FUN(X, ...),
          refName = "Ref", ...) 
{
    return(new("simFits", 
    lapply(object, function(i) {
        index.Ref.simFits <- which(names(i) == refName)
        if (is.numeric(refName))
            index.Ref.simFits <- refName
        # print(index.Ref.simFits)
        if (length(index.Ref.simFits) != 1) {
            message("Invalid 'refName' in 'simFits'")
            return(NULL)
        } else {
            fitRef <- applyFUN2X(i[index.Ref.simFits][[1]], FUN, ...)
            lapply(i[-index.Ref.simFits], function(X)
                simFit(Smp = X, fitRef = fitRef, 
                       FUN = FUN, applyFUN2X = applyFUN2X, ...)) }
    })))
}

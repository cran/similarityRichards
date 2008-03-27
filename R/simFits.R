`simFits` <-
function (list, FUN,
          applyFUN2X = function(X, FUN, ...) FUN(X, ...),
          refName = "Ref", ...) 
{
    lapply(list, function(i) {
        index.Ref <- which(names(i) == refName)
        fitRef <- applyFUN2X(i[index.Ref][[1]], FUN, ...)
        lapply(i[-index.Ref], function(X) simFit(Smp = X, fitRef = fitRef, 
            FUN = FUN, applyFUN2X = applyFUN2X, ...))
    })
}

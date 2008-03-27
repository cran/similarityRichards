`sub2simFits` <-
function (list, fits, refName = "Ref", ...) 
{
    n <- length(list)
    if (!(n == length(fits))) 
        stop("Mismatch: Lists not of same length in 'sub2simFits'")
    if (!all(names(list) == names(fits))) 
        warning("Possible mismatch in 'sub2simFits'")
    result <- NULL
    index.Ref <- which(names(fits) == refName)
    fitRef <- fits[index.Ref][[1]]
    for (i in (1:n)[-index.Ref]) {
        result <- append(result, list(simFit(list[[i]], fitRef = fitRef, 
            fit = fits[[i]], ...)))
    }
    names(result) <- names(list)[-index.Ref]
    result
}

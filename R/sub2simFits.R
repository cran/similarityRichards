`sub2simFits` <-
function (list, fits, refName = "Ref", ...) 
{
    n <- length(list)
    if (!(n == length(fits))) 
        stop("Mismatch: Lists not of same length in 'sub2simFits'")
    if (!all(names(list) == names(fits))) 
        warning("Possible mismatch in 'sub2simFits'")
    result <- NULL
    index.Ref.sim <- which(names(fits) == refName)
    if (is.numeric(refName))
        index.Ref.sim <- refName
    if (length(index.Ref.sim) != 1)
        message("Invalid 'refName' in 'sub2simFits'")
    else {
        fitRef <- fits[index.Ref.sim][[1]]
        for (i in (1:n)[-index.Ref.sim]) {
            result <- append(result, list(simFit(list[[i]], fitRef = fitRef, 
                                                 fit = fits[[i]], ...)))
        }
    }
    if (length(result) > 0)
        names(result) <- names(list)[-index.Ref.sim]
    result
}

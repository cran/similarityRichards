`sub2relFits` <-
function (list, fits, refName = "Ref", ...) 
{
    n <- length(list)
    if (!(n == length(fits))) 
        stop("Mismatch: Lists not of same length in 'sub2relFits'")
    if (!all(names(list) == names(fits))) 
        warning("Possible mismatch in 'sub2relFits'")
    result <- NULL
    index.Ref.rel <- which(names(fits) == refName)
    if (is.numeric(refName)) 
        index.Ref.rel <- refName
    if (length(index.Ref.rel) != 1) 
        message("Invalid 'refName' in 'sub2relFits'")
    else {
        fitRef <- fits[index.Ref.rel][[1]]
        for (i in (1:n)[-index.Ref.rel]) {
            result <- append(result,
                             list(relFit(list[[i]], fit = fitRef, ...)))
        }
    }
    if (length(result) > 0)
        names(result) <- names(list)[-index.Ref.rel]
    result
}

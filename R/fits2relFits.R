`fits2relFits` <-
function (list, fits, refName = "Ref", ...) 
{
    n <- length(list)
    if (!(n == length(fits))) 
        stop("Mismatch: Lists not of same length in 'fits2relFits'")
    if (!all(names(list) == names(fits))) 
        warning("Possible mismatch in 'fits2relFits'")
    result <- NULL
    for (i in 1:n) result <- append(result, list(sub2relFits(list[[i]], 
        fit = fits[[i]], refName = refName, ...)))
    names(result) <- names(list)
    return(new("relFits", result))
}

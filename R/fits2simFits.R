`fits2simFits` <-
function (list, fits, refName = "Ref", ...) 
{
    n <- length(list)
    if (!(n == length(fits))) 
        stop("Mismatch: Lists not of same length in 'fits2simFits'")
    if (!all(names(list) == names(fits))) 
        warning("Possible mismatch in 'fits2simFits'")
    result <- NULL
    for (i in 1:n) result <- append(result, list(sub2simFits(list[[i]], 
        fit = fits[[i]], ...)))
    names(result) <- names(list)
    result
}

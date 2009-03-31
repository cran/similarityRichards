`repValues` <-
function (object, ...) 
{
    X <- lapply(object, function(X) unlist(lapply(X, repValue)))
    sampleNames <- unique(sort(unlist(lapply(X, names))))
    if ((diff(range(lapply(X, length))) == 0) & 
        (max(range(lapply(X, length))) == length(sampleNames) )) {
        plates <- names(X)
        Y <- matrix(unlist(X), nrow = length(plates), byrow = TRUE)
        dimnames(Y) <- list(plates, names(X[[1]]))
        return(Y)
    } else  {
        Y <- NULL
        for (n in sampleNames) 
           Y <- cbind(Y, unlist(lapply(X, function(i) i[n])))
        dimnames(Y)[[2]] <- sampleNames
        return(Y)
    }
}

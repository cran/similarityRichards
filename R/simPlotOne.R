`simPlotOne` <-
function (fits, name, xt = function(x) -x, ylim = c(min(Z), max(Z)),
    Conf = simConf, main = paste("Backfitted values: ", name), 
    sub = paste("Above / below / NA: ", paste("", c(length(which(Z > 
        ylim[2])), length(which(Z < ylim[1])), length(which(is.na(Z)))), 
        collapse = " /")), xlab = "~ Log(concentration) [Dilution step]", 
    ylab = "Relative backfitted concentration", ...) 
{
    Relative <- lapply(fits, function(i) {
        index.Sample <- which(names(i) == name)
        Sample <- i[index.Sample][[1]]
        Sample[, "Relative"]
    })
    lengths <- unlist(lapply(Relative, length))
    if (min(lengths) == max(lengths)) {
      Z <- matrix(unlist(Relative), ncol = length(Relative))
      dimnames(Z) <- list(names(Relative[[1]]), names(Relative))
    } else {
      warning("Not equal lenghts: This code is not tested!")
      .x <- lapply(fits, function(i) {
          index.Sample <- which(names(i) == name)
          Sample <- i[index.Sample][[1]]
          Sample[, "x"]
      })
      xv <- unique(sort(unlist(.x)))
      A <- xv
      lapply(fits, function(i) {
                     index.Sample <- which(names(i) == name)
                     Sample <- i[index.Sample][[1]]
                     idx <- match(Sample[, "x"], xv)
                     row <- rep(NA, length(xv))
                     row[idx] <- Sample[, "Relative"]
                     A <<- cbind(A, row)
                   })
      Z <- A[, -1]
      dimnames(Z) <- list(xv, names(Relative))
    }
    suppressWarnings(matplot(xt(as.real(dimnames(Z)[[1]])), Z,
        ylim = ylim, main = main, sub = sub,
        xlab = xlab, ylab = ylab, ...))
    Conf(Z, x = xt(as.real(dimnames(Z)[[1]])), ylim = ylim, ...)
    Z
}

`simPlotOne` <-
function (fits, name, indep2conc = function(x) x, ylim = c(min(Z), max(Z)), 
    Conf = simConf, main = paste("Backfitted values: ", name), 
    sub = paste("Above / below / NA: ",
    paste("", c(length(which(Z > ylim[2])), 
                length(which(Z < ylim[1])), 
                length(which(is.na(Z)))), collapse = " /")), 
    xlab = "~ Log(concentration) [Dilution step]", 
    ylab = "Relative backfitted concentrations", useFitNames = FALSE, 
    doPlot = TRUE, ...) 
{
    extract <- function(col) lapply(fits, function(i) {
        Sample <- NULL
        index.Sample <- which(names(i) == name)
        if (length(index.Sample) == 1) {
            Sample <- i[index.Sample][[1]]
            Sample[, col]
        }
        else NULL
    })
    x.values <- unique(sort(unlist(extract("x"))))
    Relative <- extract("Relative")
    lengths <- unlist(lapply(Relative, length))
    if ((min(lengths) == max(lengths)) & 
        (length(x.values) == max(lengths))) {
        if (length(Relative) > 1) {
            Z <- matrix(unlist(Relative), ncol = length(Relative))
            if (useFitNames) 
                dimnames(Z) <- list(names(Relative[[1]]), names(Relative))
            else dimnames(Z) <- list(as.character(x.values), names(Relative))
        } else {
            Z <- Relative[[1]]
            if (!useFitNames) 
                names(Z) <- as.character(x.values)
        }
    }
    else {
        A <- x.values
        lapply(fits, function(i) {
            Sample <- NULL
            index.Sample <- which(names(i) == name)
            if (length(index.Sample) == 1) {
                Sample <- i[index.Sample][[1]]
                idx <- match(Sample[, "x"], x.values)
                row <- rep(NA, length(x.values))
                row[idx] <- Sample[, "Relative"]
                A <<- cbind(A, row)
            }
        })
        Z <- A[, -1]
# browser()
        notNull <- unlist(lapply(Relative, function(x) !is.null(x)))
        if (is.matrix(Z))
            if (useFitNames) 
                dimnames(Z) <- list(names(Relative[[min(which(notNull))]]), 
                    names(Relative)[notNull])
            else dimnames(Z) <- list(as.character(x.values), 
                                     names(Relative)[notNull])
        else
            if (useFitNames) 
                names(Z) <- names(Relative[[min(which(notNull))]])
            else 
                names(Z) <- as.character(x.values)
    }
    if (doPlot) {
        if (is.matrix(Z)) {
            x <- as.real(dimnames(Z)[[1]])
        } else
            x <- as.real(names(Z))
        suppressWarnings(matplot(indep2conc(x), Z, 
            ylim = ylim, main = main, sub = sub,
            xlab = xlab, ylab = ylab,  ...))
        if (is.matrix(Z))
            Conf(Z, x = indep2conc(x), ylim = ylim, ...)
        }
    attr(Z, which = "sampleName") <- name
    attr(Z, which = "label") <- sub
    Z
}

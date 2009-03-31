setClass("simFit", contains = "matrix")

`simFit` <-
function (Smp = NULL, fitSmp = NULL,
          Ref = NULL, fitRef = NULL, 
          ny = 0, Sample = TRUE, atSample = TRUE, indepName = "ARGX", 
          minlogx = min(log(Smp[, indepName]) / log(g)),
          maxlogx = max(log(Smp[, indepName]) / log(g)), 
          g = exp(1), length.out = 11,
          LogX = seq(minlogx, maxlogx, length.out = length.out),
          FUN = NULL, applyFUN2X = function(X, FUN, ...) FUN(X, ...), ...) 
{
    P <- function(estimates) {
        .a <- estimates["a"]
        .d <- estimates["d"]
        if (is.element("scal", names(estimates)))
          .b <- 1 / estimates["scal"]
        else
          .b <- estimates["b"]
        if (is.element("g", names(estimates)))
          .ny <- 1 / estimates["g"]
        else
          .ny <- estimates["ny"]
        if (is.na(.ny)) 
            .ny <- ny
        if (is.element("scal", names(estimates)))
          .x50 <- exp(estimates["xmid"])
        else if (is.element("e", names(estimates)))
          .x50 <- exp(estimates["e"]) * ((2^(.ny) - 1)/.ny)^(1/.b)
        else
          .x50 <- estimates["x50"]
        return(list(a = .a, d = .d, b = .b, x50 = .x50, ny = .ny))
    }
    Richards <- function(x, P)
        richards(x, a = P$a, d = P$d, x50 = P$x50, b = P$b, ny = P$ny)
    Richards.inv <- function(y, P)
        richards.inv(y, a = P$a, d = P$d, x50 = P$x50, b = P$b, ny = P$ny)
    invalidFit <- function(fit)
        is.null(fit) | any(class(fit) == c("try-error", "data-error"))

    if (length(which(dimnames(Smp)[[2]] == indepName)) != 1)
        warning("Invalid 'indepName' in 'simFit'")
    if (is.null(fitRef)) 
        fitRef <- applyFUN2X(Ref, FUN, ...)
    if (is.null(fitSmp)) 
        fitSmp <- applyFUN2X(Smp, FUN, ...)
    x <- exp(LogX * log(g))
    names(x) <- LogX
    # The above lines could also be move to aguments.
    if (invalidFit(fitSmp) | invalidFit(fitRef)) {
        message(paste("No fit (simFit): ", class(fitSmp),
                      "/", class(fitRef), "!"))
        y <- rep(NA, length(x))
        result <- cbind(x = x, y = y, BackSmp = y, BackRef = y, Relative = y)
    }
    else {
        estimates <- P(summary(fitSmp)$parameters[, "Estimate"])
        Estimates <- P(summary(fitRef)$parameters[, "Estimate"])
        xs <- estimates$x50
        XR <- Estimates$x50
        if (atSample) 
             XR <- Richards.inv((estimates$a + estimates$d)/2, Estimates)
        else xs <- Richards.inv((Estimates$A + Estimates$D)/2, estimates)
        if (Sample) 
             y <- Richards(x, estimates)
        else y <- Richards(x, Estimates)
        names(y) <- LogX
        BackSmp <- Richards.inv(y, estimates)
        BackRef <- Richards.inv(y, Estimates)
        result <- cbind(x = x, y = y, BackSmp = BackSmp, BackRef = BackRef, 
                        Relative = 100 * (BackRef * xs) / (BackSmp * XR))
    }
    return(new("simFit", result))
}

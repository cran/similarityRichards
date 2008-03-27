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
    if (is.null(fitRef)) 
        fitRef <- applyFUN2X(Ref, FUN, ...)
    if (is.null(fitSmp)) 
        fitSmp <- applyFUN2X(Smp, FUN, ...)
    x <- exp(LogX * log(g))
    names(x) <- LogX
    # The above lines could also be move to aguments.
    if (is.null(fitSmp) | class(fitSmp) == "try-error" |
        is.null(fitRef) | class(fitRef) == "try-error") {
        warning("No fit: try-error!")
        y <- rep(NA, length(x))
        cbind(x = x, y = y, BackSmp = y, BackRef = y, Relative = y)
    }
    else {
        estimates <- summary(fitSmp)$parameters[, "Estimate"]
        .a <- estimates["a"]
        .d <- estimates["d"]
        .b <- estimates["b"]
        .x50 <- estimates["x50"]
        .ny <- estimates["ny"]
        if (is.na(.ny)) 
            .ny <- ny
        Estimates <- summary(fitRef)$parameters[, "Estimate"]
        .A <- Estimates["a"]
        .D <- Estimates["d"]
        .B <- Estimates["b"]
        .X50 <- Estimates["x50"]
        .Ny <- Estimates["ny"]
        if (is.na(.Ny)) 
            .Ny <- ny
        .xs <- .x50
        .XR <- .X50
        if (atSample) 
             .XR <- richards.inv(
                      (.a + .d)/2, a = .A, d = .D, x50 = .X50, b = .B, ny = .Ny)
        else .xs <- richards.inv(
                      (.A + .D)/2, a = .a, d = .d, x50 = .x50, b = .b, ny = .ny)
        if (Sample) 
             y <- richards(x, a = .a, d = .d, x50 = .x50, b = .b, ny = .ny)
        else y <- richards(x, a = .A, d = .D, x50 = .X50, b = .B, ny = .Ny)
        names(y) <- LogX
        BackSmp <- richards.inv(y, a = .a, d = .d, x50 = .x50, b = .b, ny = .ny)
        BackRef <- richards.inv(y, a = .A, d = .D, x50 = .X50, b = .B, ny = .Ny)
        cbind(x = x, y = y, BackSmp = BackSmp, BackRef = BackRef, 
              Relative = 100 * (BackRef * .xs) / (BackSmp * .XR))
    }
}

`repValue` <-
function (X) 
exp(mean(log(X[X[, "ok.lb"] == 1 & X[, "ok.ub"] == 1, "RecovPct"])))

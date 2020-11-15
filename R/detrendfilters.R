
## Filter proposed by Hamilton (2018), doi:10.1162/rest_a_00706, to separate trend and cycle components of a series

H_filter <- function(y, h, p) {
  # y = univariate time series indexed as 1,..,T
  # h = horizon of the linear projection
  # p = number of lags (including zero lag), p >= d
  # The first p + h - 1 values are required to obtain the cycle component at time p + h (= first time point of the series)
  
  # Each row X is x_t', x_t=(y_{t},...,y_{t-p+1}, 1), first p values needed for the regressors, the last t+h is for the last observation
  X <- t(vapply(p:(length(y) - h), function(t1) c(1, y[t1:(t1 - p + 1)]), numeric(p + 1)))
  y_tplush <- y[-(1:(p - 1 + h))] # (p+h, p+h+1, ..., T-1, T)
  
  # The OLS estimate of is then (X'X)^{-1}X'y_tplush - QR factorization is used to reduce numerical error in inverting X'X
  qr_X <- qr(X)
  beta <- c(backsolve(qr.R(qr_X), crossprod(qr.Q(qr_X), y_tplush))) # qr.solve(X, y_tplush), solve(crossprod(X))%*%crossprod(X, y_tplush)
  hat_mat <- tcrossprod(qr.Q(qr_X))
  y_hat <- hat_mat%*%y_tplush
  list(cycle=y_tplush - y_hat, # starting date is p + h - 1 :th observation of the original series y, for all series returned
       trend=y_hat,
       total=y_tplush,
       beta=beta) # OLS coefficients for the regressors (y_{t},...,y_{t-p+1}, 1)
}

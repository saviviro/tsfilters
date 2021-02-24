#' @title Filter proposed by Hamilton (2018, doi:10.1162/rest_a_00706) to separate trend
#'  and cycle components of a series.
#'
#' @description \code{hfilter} separates trend and cycle components of univariate time series
#'  using the method proposed by Hamilton (2018, doi:10.1162/rest_a_00706) based on linear
#'  projections.
#'
#' @param y univariate time series indexed as \code{1,..,T}. Can be either a numeric vector
#'  or matrix, or a class \code{ts} object in which case the dates are correctly passed to
#'  the returned components.
#' @param h an integer value giving the horizon of the linear projection. Hamilton recommends
#'  two years, i.e., 8 for quarterly data and 24 for monthly data.
#' @param p the number of lags (including the zero lag) used in the linear projection. Should be
#'  chosen so that \code{p > d} where \code{d} is the order of integration of the series. Hamilton
#'  recommends \code{p} to be a multiplicative of one year for seasonal data, i.e., for instance
#'  \code{p=4} for quarterly data and \code{p=12} monthly data.
#' @details
#'  The first \code{p + h - 1} observations of the series are required to obtain the cycle component
#'  at time \code{p + h} (= first time point of the series). For more details, see the paper
#'  by Hamilton (2018).
#' @return Returns a class \code{'hfilter'} object containing the following:
#'  \describe{
#'    \item{\code{$cycle}:}{the cyclical component of the series}
#'    \item{\code{$trend}:}{the trend component of the series}
#'    \item{\code{$total}:}{trend + cyclical (shorter than the original seris)}
#'    \item{\code{$beta}:}{the OLS coefficients}
#'    \item{\code{$y}:}{the original series}
#'    \item{\code{$h}:}{the horizon used}
#'    \item{\code{$p}:}{the number of lags (including the zero lag) used}
#'  }
#'  If the provided series \code{y} is of class \code{ts}, the dates of the detrended
#'  series will be set accordingly.
#' @references
#'  \itemize{
#'    \item J.D. Hamilton. 2018. WHY YOU SHOULD NEVER USE THE HODRICK-PRESCOTT FILTER.
#'     \emph{The Review of Economics and Statistics}, 100(5): 831-843.
#'  }
#' @examples
#'  data(INDPRO, package="tsfilters")
#'  IP_filtered <- hfilter(log(INDPRO), h=24, p=12)
#'  IP_filtered
#'  plot(IP_filtered)
#' @export

hfilter <- function(y, h=24, p=12) {
  stopifnot(h %% 1 == 0 && p %% 1 == 0)
  if(!is.ts(y)) {
    y <- ts(as.vector(y), start=1, frequency=1)
  }

  # Store properties of the original series
  y_start <- start(y)
  y_freq <- frequency(y)
  y <- as.vector(y)

  # Calculate the new start time for the trend and cyclical components
  steps_forward <- h + p - 1
  new_start <- get_new_start(y_start=y_start, y_freq=y_freq, steps_forward=steps_forward)

  # Each row X is x_t', x_t=(y_{t},...,y_{t-p+1}, 1), first p values needed for the regressors,
  # the last t+h is for the last observation
  X <- t(vapply(p:(length(y) - h), function(t1) c(1, y[t1:(t1 - p + 1)]), numeric(p + 1)))
  y_tplush <- y[-(1:(p - 1 + h))] # (p+h, p+h+1, ..., T-1, T)

  # The OLS estimate of is then (X'X)^{-1}X'y_tplush - QR factorization is used to reduce numerical
  # error in inverting X'X
  qr_X <- qr(X)
  beta <- c(backsolve(qr.R(qr_X), crossprod(qr.Q(qr_X), y_tplush))) # qr.solve(X, y_tplush), solve(crossprod(X))%*%crossprod(X, y_tplush)
  hat_mat <- tcrossprod(qr.Q(qr_X)) # Hat matrix
  y_hat <- hat_mat%*%y_tplush

  make_ts <- function(a) ts(a, start=new_start, frequency=y_freq)

  structure(list(cycle=make_ts(y_tplush - y_hat), # starting date is p + h - 1 :th observation of the original series y, for all series returned
                 trend=make_ts(y_hat),
                 total=make_ts(y_tplush),
                 beta=beta, # OLS coefficients for the regressors (y_{t},...,y_{t-p+1}, 1)
                 y=y,
                 h=h,
                 p=p),
            class="hfilter")
}


#' @title Take logarithm and then first differences of a time series.
#'
#' @description \code{logdiff} logarithmizes and then takes first differences of a
#'  time series.
#'
#' @inheritParams hfilter
#' @details
#'  The first observation of the series are required as the initial value for the
#'  the first differences. The second observation will thereby be the first observation
#'  of the detrended series.
#' @return Returns a class \code{'ts'} object containing the log-differenced series.
#'  If the provided series \code{y} is of class \code{ts}, the dates of the detrended
#'  series will be set accordingly.
#' @examples
#'  data(INDPRO, package="tsfilters")
#'  IP_logdiff <- logdiff(INDPRO)
#'  start(INDPRO)
#'  start(IP_logdiff)
#'  plot(IP_logdiff)
#' @export

logdiff <- function(y) {
  if(!is.ts(y)) {
    y <- ts(as.vector(y), start=1, frequency=1)
  }

  # Store properties of the original series
  y_start <- start(y)
  y_freq <- frequency(y)
  y <- as.vector(y)

  # Start time of the detrended series:
  new_start <- get_new_start(y_start=y_start, y_freq=y_freq, steps_forward=1)

  # Calculate and return the log-differenced series:
  ts(diff(log(y), lag=1, differences=1), start=new_start, frequency=y_freq)
}


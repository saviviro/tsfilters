
#' @describeIn hfilter plot method
#' @param ... arguments passed to \code{plot.ts}
#' @export

plot.hfilter <- function(x, ...) {
  plot(with(x, cbind(total, cycle, trend)),
       main="Separated cycle and trend", ...)
}


#' @describeIn hpfilter plot method
#' @param ... arguments passed to \code{plot.ts}
#' @export

plot.hpfilter <- function(x, ...) {
  plot(with(x, cbind(total, cycle, trend)),
       main="Separated cycle and trend", ...)
}

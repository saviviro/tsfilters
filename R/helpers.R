#' @title Get the new starting time of series that is forwarded some number of steps
#'
#' @description \code{get_new_start} calculates the new starting time of series
#'   that is forwarded some number of steps
#'
#' @param y_start original starting time of the series
#' @param y_freq frequency of the series
#' @param steps_forward how many steps the series should be forwarded?
#' @return Returns a length two numeric vector with the "year" (or "major")
#'   time point in the first element the "quarter/month/week/day" (or "minor")
#'   time in the second element for a series that is forwarded from \code{y_start}
#'   \code{steps_forward} steps forward.
#' @examples
#'  get_new_start(y_start=c(1999, 12), y_freq=12, steps_forward=1)
#'  get_new_start(y_start=c(1999, 12), y_freq=52, steps_forward=2)
#' @export

get_new_start <- function(y_start, y_freq, steps_forward) {
  majors_forward <- steps_forward %/% y_freq
  minors_forward <- steps_forward %% y_freq
  new_start <- y_start + c(majors_forward, minors_forward)
  if(new_start[2] > y_freq) {
    new_start <- c(new_start[1] + 1, new_start[2] %% y_freq)
  }
  new_start
}

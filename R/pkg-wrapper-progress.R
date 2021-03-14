#' @title Create a progress bar object
#'
#' @param total Numeric value. Total number of ticks.
#' @param format Character value. Denotes the format.
#' @param clear Logical value.
#' @param width Numeric value
#'
#' @return A progress bar object.
#' @export

create_progress_bar <- function(total,
                                format = "Progress: [:bar] :percent eta: :eta",
                                clear = FALSE,
                                width = 100,
                                ...){

  progress::progress_bar$new(
    format = format,
    total = total,
    clear = clear,
    width = width,
    ...)


}

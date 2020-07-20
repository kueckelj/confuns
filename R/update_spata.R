#' Get newest version of SPATA
#'
#' @export
#'

update_spata <- function(){

  library(SPATA)

  base::detach("package:SPATA", unload = TRUE)
  utils::remove.packages("SPATA")
  devtools::install_github("kueckelj/SPATA")

  library(SPATA)

}



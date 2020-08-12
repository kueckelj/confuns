#' Get newest version of SPATA
#'
#' @export
#'

update_spata <- function(){

  if("SPATA" %in% as.data.frame(utils::installed.packages())$Package){

    library(SPATA)
    base::detach("package:SPATA", unload = TRUE)
    utils::remove.packages("SPATA")

  }

  devtools::install_github("kueckelj/SPATA")

  library(SPATA)

}



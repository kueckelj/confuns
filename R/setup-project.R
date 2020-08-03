#' @title Set up the project folder
#'
#' @description Creates input-, meta-, output- and graphics directories relative
#' to the working directory.
#'
#' @param project.name The projects name as a character string used to validate
#' the current working directory.
#'
#' @export
#'

setup_project <- function(project.name){

  # validate the current wd
  wd <- base::getwd()
  pattern <- stringr::str_c(project.name, "$", sep = "")

  if(!stringr::str_detect(string = wd, pattern = pattern)){

    base::stop("Current wd does not lead to the projects folder.")

  }

  # create folders if they do not already exist
  if(!base::dir.exists(path = "input")){

    base::dir.create(path = "input")

  } else {

    base::message("'input' folder already exists.")

  }

  if(!base::dir.exists(path = "meta")){

    base::dir.create(path = "meta")

  } else {

    base::message("'meta' folder already exists.")

  }

  if(!base::dir.exists(path = "output")){

    base::dir.create(path = "output")

  } else {

    base::message("'output' folder already exists.")

  }

  if(!base::dir.exists(path = "graphics")){

    base::dir.create(path = "graphics")

  } else {

    base::message("'graphics' folder already exists.")

  }

}



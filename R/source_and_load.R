#' @title Load in all projects
#'
#' @description Sources all R-scripts from folder 'R' that end with
#' ..._function.R or ..._functions.R.
#'
#' @param folder The absolute or relative to the folder of interest.
#' @param recursive Logical. If set to TRUE the listing recurs into subsequent
#' folders.
#' @param verbose Logical. If set to TRUE informative messages will be printed.
#'
#' (Warnings will always be printed.)
#'
#' @return Sourced in functions as a side effect and an invisible TRUE.
#' @export
#'

source_functions <- function(folder = "R", recursive = FALSE, verbose = TRUE){

  funs <-
    base::list.files(path = folder, recursive = recursive, full.names = TRUE) %>%
    stringr::str_subset(pattern = "_function.R|_functions.R")

  if(base::length(funs) == 0){

    base::stop("Could not find any function.R/functions.R - scripts under directory /R.")

  }

  for(i in base::seq_along(funs)){

    if(base::isTRUE(verbose)){

      base::print(stringr::str_c("Reading:", funs[i], sep = " "))

    }

    base::source(funs[i])

  }

  base::message(stringr::str_c("Sourced in", base::length(funs), "script(s).", sep = " "))

  return(base::invisible(TRUE))

}



#' @title Load in all files at once
#'
#' @description Loads or reads in all files of the respective folder according to the
#' parameters input.
#'
#' @param folder The absolute or relative to the folder of interest.
#' @param file.types The file types you want to load in specified as a character
#' vector. Elements must start with '.' ('.csv', NOT 'csv')!
#' @param file.subset A regular expression that match the files you want to load
#' in specified as a character vector.
#' @param file.ignore A regular expression that match the files you want to be
#' ignored specified as a character vector.
#' @param read.funs A named list providing the functions that are supposed to be
#' used to load the respective file type. Names need to match the respective elements
#' of argument \code{file.types} exactly.
#'
#' @inherit source_functions params
#'
#' @return Loaded files  in the global environment as a side effect and returns
#' an invisible logical value: TRUE.
#' @export
#'

load_files <- function(folder,
                       file.types = c(".csv", ".xls", ".rds", ".xlsx", ".RData"),
                       file.subset = character(),
                       file.ignore = character(),
                       read.funs = list(),
                       recursive = FALSE,
                       verbose = TRUE){

  # check file.types
  if(base::any(!stringr::str_detect(file.types, pattern = "^\\."))){

    base::stop("All specified file extensions in argument 'file.types' must start with a dot.")

  }

  # default functions
  read_funs <- list(
    ".csv" = utils::read.csv,
    ".xls" = readxl::read_xls,
    ".xlsx" = readxl::read_xls,
    ".rds" = base::readRDS,
    ".RData" = base::load)

  # complement read_funs
  if(base::is.list(read.funs) &&
     !base::is.data.frame(read.funs) &&
     base::length(read.funs) != 0 &&
     !base::is.null(base::names(read.funs))){

    # drop all elements that are not functions
    for(name in base::names(read.funs)){

      if(!base::is.function(read.funs[[name]])){

        read.funs[[name]] <- NULL

        base::warning(stringr::str_c("Element", name, "of 'read.funs' is not a function."))

      }

    }

    # overwrite and complement default functions
    read_funs <- join_lists(lst.1 = read_funs,
                            lst.2 = read.funs,
                            drop.unnamed = TRUE)

  }

  # .RData files have to be loaded with 'base::load'
  funs_names <- base::names(read_funs)

  # first adjustment of file.types
  if(base::length(file.types) == 0){

    base::stop("Please provide at least one file type that's to be loaded.")

  } else if(base::any(!file.types %in% funs_names)){

    no_funs <- file.types[!file.types %in% funs_names]

    file.types <- file.types[file.types %in% funs_names]

    if(base::length(no_funs) >= 1){

      no_funs_string <- stringr::str_c(no_funs, collapse = ", ")

      base::warning(stringr::str_c("No matching function for file types: ", no_funs_string))

    }

  }


  # save file names
  files <-
    base::list.files(path = folder,
                     recursive = recursive,
                     full.names = TRUE)

  if(base::length(files) == 0){

    base::stop(stringr::str_c("Could not find any files under directory '/", folder, "'."))

  }

  # subset desired files
  if(base::length(file.subset) != 0 && base::is.character(file.subset)){

    files <- stringr::str_subset(string = files,
                                 pattern = stringr::str_c(file.subset, collapse = "|"))

    if(base::length(files) == 0){

      base::stop("Specification of argument 'file.subset' results in zero files.")

    }

  }

  if(base::length(file.ignore) != 0 && base::is.character(file.ignore)){

    files <- files[!stringr::str_detect(string = files,
                                        pattern = stringr::str_c(file.ignore, collapse = "|"))]

    if(base::length(files) == 0){

      base::stop("Specification of argument 'file.ignore' results in zero files.")

    }

  }

  # save respective object names
  object_names <-
    stringr::str_remove_all(string = files, pattern = "^meta/|.rds$|.csv$|.xls$") %>%
    stringi::stri_extract_last_words() %>%
    base::unlist()

  # save unique file types
  all_file_types <-
    stringr::str_extract_all(string = files, pattern = "\\..{1,}$") %>%
    base::unlist()

  base::names(files) <- all_file_types

  unq_file_types <- base::unique(all_file_types)


  # second adjustment of file.types
  if(base::any(!unq_file_types %in% file.types)){

    cant_read <- unq_file_types[!unq_file_types %in% file.types]

    if(base::all(unq_file_types %in% cant_read)){

      base::stop("Specification of argument 'file.types' results in zero files.")

    }

  }

  if(base::any(!file.types %in% unq_file_types)){

    # use only file.type read in that needs to be used
    file.types <- file.types[file.types %in% unq_file_types]

  }


  # read and store all in a for loop
  for(ft in file.types){

    if(base::isTRUE(verbose)){

      base::message(stringr::str_c("Loading ", ft, "-files:", sep = ""))

    }

    fun_to_apply <- read_funs[[ft]]

    for(f in base::seq_along(files)){

      if(base::names(files[f]) == ft){

        if(ft == ".RData"){

          object_name <- base::load(file = files[f])

          base::assign(x = object_name,
                       value = base::eval(base::str2expression(text = object_name)),
                       envir = .GlobalEnv)

          if(base::isTRUE(verbose)){

            file <-
              stringi::stri_extract_last_words(str = files[f]) %>%
              stringr::str_c("~/", ., sep = "")

            base::message(stringr::str_c("Saved file: '", file, "' under '", object_name,
                                         "' in global environment.",
                                         sep = ""))

          }

        } else {

          base::assign(
            x = object_names[f],
            value = fun_to_apply(files[f]),
            envir = .GlobalEnv
          )

          if(base::isTRUE(verbose)){

            file <-
              stringi::stri_extract_last_words(str = files[f]) %>%
              stringr::str_c("~/", ., sep = "")

            base::message(stringr::str_c("Saved file '", file, "' under '",
                                         object_names[f] , "' in global environment.",
                                         sep = "" ))

          }

        }

      }

    }

  }

}


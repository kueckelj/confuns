
#' @title Load in all files at once
#'
#' @description Loads or reads in all files of the respective folder according to the
#' parameters input.
#'
#' @param folder The absolute or relative directory to the folder of interest.
#' @param file.type.subset The file types you want to load in specified as a character
#' vector. Elements must start with '.' ('.csv', NOT: 'csv')!
#' @param file.type.ignore The file types you don't want to load in specified as a
#' character vector. Elements must start with '.' ('.csv', NOT: 'csv')!
#' @param file.subset A regular expression that match the file names you want to load
#' in specified as a character vector.
#' @param file.ignore A regular expression that match the file names you want to be
#' ignored specified as a character vector.
#' @param read.funs A named list providing the functions that are supposed to be
#' used to load the respective file type. Names need to match the respective elements
#' of argument \code{file.type.subset} exactly.
#'
#' @inherit source_functions params
#'
#' @details This function accesses the folder to which the directory leads. Via \code{base::list.files()} it extracts
#' the directories to all files this folder contains and - if \code{recursive} is set to TRUE - as well to all files all subsequent
#' folders contain.
#'
#' The resulting vector of file directories will be processed according to the \code{.subset} and/or the \code{.ignore} arguments.
#'
#' \code{.subset}: Only the file directories that match one or more of the provided regular expressions will be read in.
#'
#' \code{.ignore}: All file directories that match one or more of the provided regular expressions will be discarded and not read in.
#'
#' After the filtering step every remaining file directory will be given to a specific function in order to be read in. These functions
#' have to be provided in a named list as input of the \code{read.funs}-argument. Default functions are:
#'
#' \itemize{
#'  \item utils::read.csv for files of type \emph{.csv}
#'  \item readxl::read.xls for files of type \emph{.xls}
#'  \item readxl::read.xls for files of type \emph{.xlsx}
#'  \item base::readRDS for files of type \emph{.rds}
#'  \item base::load for files of type \emph{.RData} - can not be overwritten
#'  }
#'
#' Remaining file directories ending with abbreviations that are not specified within the default functions or within \code{read.funs}
#' will be discarded and not read in. In order to add new functions for new file types or overwrite default functions specify those in
#' a named list in \code{read.funs} (e.g. list(".tif" = magick::image_read)).
#'
#' Via \code{base::assign()} every read in object will be stored in the global environment whereby it's object name is the file name without
#' the file type specifiying abbreviation.
#'
#'
#' @return Loaded files  in the global environment as a side effect and returns
#' an invisible logical value: TRUE.
#' @export
#'

load_files <- function(folder,
                       file.type.subset = character(),
                       file.type.ignore = character(),
                       file.subset = character(),
                       file.ignore = character(),
                       read.funs = list(),
                       recursive = FALSE,
                       verbose = TRUE){

  stopifnot(base::is.character(folder) | base::length(folder) == 1)
  folder <- stringr::str_replace_all(string = folder, pattern = "\\\\", replacement = "/")

  # check file.type.
  if(base::any(!stringr::str_detect(file.type.subset, pattern = "^\\."))){

    base::stop("All specified file extensions in argument 'file.type.subset' must start with a dot.")

  }
  if(base::any(!stringr::str_detect(file.type.ignore, pattern = "^\\."))){

    base::stop("All specified file extensions in argument 'file.type.ignore' must start with a dot.")

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

      # make sure that named elements are functions in order not to overwrite with anything else
      if(!base::is.function(read.funs[[name]])){

        read.funs[[name]] <- NULL

        base::warning(stringr::str_c("Element", name, "of 'read.funs' is not a function.", sep = " "))

      }

    }

    # overwrite and complement default functions
    read_funs <- join_lists(lst.1 = read_funs,
                            lst.2 = read.funs,
                            drop.unnamed = TRUE)

  }

  # .RData files have to be loaded with 'base::load'
  read_funs[[".RData"]] <- base::load

  funs_names <- base::names(read_funs)

  if(base::any(funs_names == ".R")){

    read_funs[[".R"]] <- NULL

    base::warning("Use 'confuns::load_R_scripts' or 'confuns::load_functions' in order to source R scripts.")

  }

  # save file names
  files <-
    base::list.files(path = folder,
                     recursive = recursive,
                     full.names = TRUE)

  if(base::length(files) == 0){

    base::stop(stringr::str_c("Could not find any files under directory '/", folder, "'."))

  }
  # first adjustment of file.type.subset
  # if length zero consider all file types in files
  if(base::length(file.type.subset) == 0){

    file.type.subset <-
      stringr::str_extract(string = files, pattern = "\\..+$") %>%
      base::unique()

  }

  # discard file types that are to be ignored
  if(base::length(file.type.ignore) != 0){

    file.type.subset <- file.type.subset[!file.type.subset %in% file.type.ignore]

  }

  # make sure that there are functions provided for all file types
  if(base::any(!file.type.subset %in% funs_names)){

    no_funs <- file.type.subset[!file.type.subset %in% funs_names]

    file.type.subset <- file.type.subset[file.type.subset %in% funs_names]

    if(base::length(no_funs) >= 1){

      no_funs_string <- stringr::str_c(no_funs, collapse = "', '")

      base::warning(stringr::str_c("Ignoring files of type: '", no_funs_string, "' No matching functions specified."))

    }

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
  remove_pattern <- stringr::str_c(c(folder, file.type.subset), collapse = "|")

  object_names <-
    stringr::str_remove_all(string = files, pattern = remove_pattern) %>%
    stringi::stri_extract_last_words() %>%
    base::unlist()

  # save unique file types
  all_file_types <-
    stringr::str_extract_all(string = files, pattern = "\\..{1,}$") %>%
    base::unlist()

  base::names(files) <- all_file_types

  unq_file_types <- base::unique(all_file_types)


  # second adjustment of file.type.subset
  if(base::any(!unq_file_types %in% file.type.subset)){

    cant_read <- unq_file_types[!unq_file_types %in% file.type.subset]

    if(base::all(unq_file_types %in% cant_read)){

      base::stop("Specification of argument 'file.type.subset' results in zero files.")

    }

  }

  if(base::any(!file.type.subset %in% unq_file_types)){

    # use only file.type.subset that needs to be used
    file.type.subset <- file.type.subset[file.type.subset %in% unq_file_types]

  }


  # read and store all in a for loop
  for(ft in file.type.subset){

    if(base::isTRUE(verbose)){

      base::message(stringr::str_c("Loading ", ft, "-files:", sep = ""))

    }

    fun_to_apply <- read_funs[[ft]]

    for(f in base::seq_along(files)){

      if(base::names(files[f]) == ft){

        file <-
          stringi::stri_extract_last_words(str = files[f]) %>%
          stringr::str_c("~/", ., sep = "")

        # loading .RData files with base::load
        if(ft == ".RData"){

          object_name <- base::load(file = files[f])

          base::assign(x = object_name,
                       value = base::eval(base::str2expression(text = object_name)),
                       envir = .GlobalEnv)

          # loading successful?
          if(base::exists(x = object_name, where = .GlobalEnv) && base::isTRUE(verbose)){

            base::message(stringr::str_c("Saved file: '", file, "' under '", object_name,
                                         "' in global environment.",
                                         sep = ""))

          } else if(!base::exists(x = object_name, where = .GlobalEnv)){

            base::warning(stringr::str_c("Could not read in file: '", file, "'. Did you specify an appropriate function?"))

          }

        # load every other file with the specified function
        } else {

          base::assign(
            x = object_names[f],
            value = fun_to_apply(files[f]),
            envir = .GlobalEnv
          )

          # loading successful?
          if(base::exists(x = object_names[f], where = .GlobalEnv) && base::isTRUE(verbose)){

            base::message(stringr::str_c("Saved file: '", file, "' under '", object_names[f],
                                         "' in global environment.",
                                         sep = ""))

          } else if(!base::exists(x = object_names[f], where = .GlobalEnv)){

            base::warning(stringr::str_c("Could not read in file: '", file, "'. Did you specify an appropriate function?"))

          }

        }

      }

    }

  }

}


#' @title Load in all functions
#'
#' @description Sources all R-scripts from specified folder via \code{base::source()} that end with
#' \emph{_function.R} or \emph{_functions.R}.
#'
#' @param folder The absolute or relative to the folder of interest.
#' @param recursive Logical. If set to TRUE the file listing recurs into subsequent
#' folders (starting with the folder specified in \code{folder}).
#' @param verbose Logical. If set to TRUE informative messages will be printed.
#'
#' (Warnings will always be printed.)
#'
#' @return Sourced in functions as a side effect and an invisible TRUE.
#' @export
#'

load_functions <- function(folder = "R",
                           recursive = FALSE,
                           verbose = TRUE){

  stopifnot(base::is.character(folder) | base::length(folder) == 1)

  funs <-
    base::list.files(path = folder, recursive = recursive, full.names = TRUE) %>%
    stringr::str_subset(pattern = "_function.R$|_functions.R$")

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


#' @title Load in all R scripts
#'
#' @description Sources all R-scripts from specified folder via \code{base::source()}.
#'
#' @param folder The absolute or relative to the folder of interest.
#' @param script.subset A regular expression that match the file names you want to load
#' in specified as a character vector.
#' @param recursive Logical. If set to TRUE the file listing recurs into subsequent
#' folders (starting with the folder specified in \code{folder}).
#' @param verbose Logical. If set to TRUE informative messages will be printed.
#'
#' (Warnings will always be printed.)
#'
#' @return Sourced in functions as a side effect and an invisible TRUE.
#' @export
#'

load_scripts <- function(folder = "R",
                           script.subset = NULL,
                           recursive = FALSE,
                           verbose = TRUE){

  stopifnot(base::is.character(folder) | base::length(folder) == 1)

  scripts <-
    base::list.files(path = folder, recursive = recursive, full.names = TRUE) %>%
    stringr::str_subset(pattern = "\\.R$")

  if(base::length(scripts) == 0){

    base::stop(stringr::str_c("Could not find any R scripts under directory:", folder, sep = ""))

  }

  if(base::is.character(script.subset) && !base::length(script.subset) != 0){

    subset_pattern <- stringr::str_c(script.subset, collapse = "|")

    scripts <- scripts[stringr::str_detect(string = scripts, pattern = subset_pattern)]

  }
  if(base::length(scripts) == 0){

    base::stop("Specification of argument 'scrip.subset' results in zero files.")

  }


  for(i in base::seq_along(scripts)){

    if(base::isTRUE(verbose)){

      base::print(stringr::str_c("Reading:", scripts[i], sep = " "))

    }

    base::source(scripts[i])

  }

  if(base::isTRUE(verbose)){
    base::message(stringr::str_c("Sourced in", base::length(scripts), "script(s).", sep = " "))
  }

  return(base::invisible(TRUE))

}


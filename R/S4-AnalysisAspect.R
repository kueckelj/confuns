#' @include S4-generics.R
NULL

#' @title The \code{AnalysisAspect}-class
#'
#' @description S4-class that provides the basic slots for any analysis
#' aspect such as clustering, dimensional reduction, outlier detection etc.
#'
#' @slot data data.frame. The data on which the analysis bases on.
#' @slot key_name character. The name of the variable that is used to identify
#' each observation uniquely.
#' @slot meta data.frame. Data that was part of the input data but is not supposed
#' to be included in analysis steps.
#' @slot methods list. A list of objects of S4-classes depending on the analysis
#' aspect.
#' @slot variables_grouping character. The names of all grouping variables
#' of the input data - variables of class character or factor. (Does not include
#' variable of slot @@key_name)
#' @slot variables_logical character. The names of all logical variables of
#' the input data.
#' @slot variables_numeric character. The names of all numeric variables
#' based on which outlier detection is conducted.
#'
#' @export
AnalysisAspect <- setClass(Class = "AnalysisAspect",
                           slots = list(
                             data = "data.frame",
                             key_name = "character",
                             meta = "data.frame",
                             methods = "list",
                             variables_grouping = "character",
                             variables_logical = "character",
                             variables_numeric = "character",
                             version = "list"
                           ))

version_analysis_aspect <- list(major = 0, minor = 1, patch = 0)





# r-objects ---------------------------------------------------------------

#' @export
valid_analysis_aspects <- c("AnalysisAspect", "Clustering", "Correlation", "DimRed","OutlierDetection")

# -----


# functions ---------------------------------------------------------------

#' @title Initiate analysis
#'
#' @description Sets up an object of class \code{AnalysisAspect}.
#'
#' @param data Data.frame containing the data to be analyzed.
#' @param key_name The key variable. If NULL, the key variable is created
#' either by using the rownames and - if rownames are invalid - by combining
#' input for argument \code{key_prefix} with the rownumbers.
#' @param key_prefix Character value. The prefix for the artificial
#' key variable.
#' @param meta_names Names of the data.frame of \code{data} that are supposed
#' to be treated as meta data. Meta data is not integrated in any form
#' of analysis.
#' @param analysis_aspect The actual analysis aspect. Use \code{validAnalysisAspects()}
#' to obtain all valid input options.
#' @param verbose
#'
#' @return An object of class specified in \code{analysis_aspect}.
#'
#' @export
#'
initiateAnalysisAspect <- function(data,
                                   key_name,
                                   key_prefix = "ID",
                                   meta_names = character(0),
                                   analysis_aspect = "AnalysisAspect",
                                   verbose = TRUE){

  # input check
  is_value(x = key_name, mode = "character", skip.allow = TRUE, skip.val = NULL)

  check_one_of(
    input = analysis_aspect,
    against = valid_analysis_aspects
  )

  df <-
    base::as.data.frame(data) %>%
    dplyr::select(-dplyr::all_of(meta_names))

  variables_grouping <-
    dplyr::select(df, -{{key_name}}) %>%
    dplyr::select_if(.predicate = ~ base::is.character(.x) | base::is.factor(.x)) %>%
    base::colnames()

  variables_logical <-
    dplyr::select(df, -{{key_name}}) %>%
    dplyr::select_if(.predicate = ~ base::is.logical(.x)) %>%
    base::colnames()

  variables_numeric <-
    dplyr::select_if(df, .predicate = base::is.numeric) %>%
    base::colnames()

  object <-
    methods::new(
      Class = analysis_aspect,
      variables_grouping = variables_grouping,
      variables_logical = variables_logical,
      variables_numeric = variables_numeric
    )

  object <-
    setData(
      object = object,
      data = data,
      key_name = key_name,
      key_prefix = key_prefix,
      verbose = verbose
    )

}






#' @rdname validInput
#' @export
validAnalysisAspects <- function(){

  return(valid_analysis_aspects)

}


# -----


# own generics ------------------------------------------------------------





# -----



# methods for own generics ------------------------------------------------


# -----



# methods for external generics -------------------------------------------



#' @param grouping,logical,numeric Logical value. Indicate if the respective variable
#' types should be part of the output data.frame.
#' @param meta Logical value. Indicates if the data.frame in slot @@meta should
#' be joined to the output data.frame.
#'
#' @rdname getDf
#' @export
setMethod(
  f = "getDf",
  signature = "AnalysisAspect",
  definition = function(object,
                        complete = TRUE,
                        grouping = FALSE,
                        logical = FALSE,
                        numeric = FALSE,
                        meta = FALSE){

  if(base::any(c(grouping, logical, numeric, meta))){

    complete <- FALSE

  }

  if(base::isTRUE(complete)){

    grouping <- TRUE
    logical <- TRUE
    numeric <- TRUE
    meta <- TRUE

  }

  var_names <- object@key_name

  if(base::isTRUE(grouping)){

    var_names <- c(var_names, object@variables_grouping)

  }

  if(base::isTRUE(logical)){

    var_names <- c(var_names, object@variables_logical)

  }

  if(base::isTRUE(numeric)){

    var_names <- c(var_names, object@variables_numeric)

  }

  df_out <-
    dplyr::select(object@data, dplyr::all_of(x = var_names)) %>%
    tibble::as_tibble()

  if(base::isTRUE(meta)){

    df_out <- dplyr::left_join(x = df_out, y = object@meta, by = object@key_name)

  }

  return(df_out)

})


#' @rdname getMtr
#' @export
setMethod(
  f = "getMtr",
  signature = "AnalysisAspect",
  definition = function(object, scale = FALSE){

    mtr <-
      getDf(object, numeric = TRUE) %>%
      tibble::column_to_rownames(var = object@key_name) %>%
      base::as.matrix()

    if(base::isTRUE(scale)){

      mtr <-
        base::apply(
          X = mtr,
          MARGIN = 2,
          FUN = normalize_zscore, na.rm = TRUE
          )

    }

    return(mtr)

  }
)

#' @rdname getResults
#' @export
setMethod(
  f = "getResults",
  signature = "AnalysisAspect",
  definition = function(object, method){

    object_class <- base::class(object)

    if(object_class == "AnalysisAspect"){

      out <- NULL

    } else {

      valid_methods <-
        stringr::str_c("validMethods", object_class, "()") %>%
        base::parse(text = .) %>%
        base::eval()

      check_one_of(
        input = method,
        against = valid_methods
      )

      method_obj <- object@methods[[method]]

      if(base::is.null(method_obj)){

        stop(
          glue::glue(
            "No results found for method '{method}' in this object of class '{object_class}'."
          )
        )

      } else {

        out <- method_obj

      }

    }

    return(method_obj)

  }
)


#' @rdname setData
#' @export
setMethod(
  f = "setData",
  signature = "AnalysisAspect",
  definition = function(object,
                        data,
                        key_name = NULL,
                        key_prefix = "id",
                        meta_names = character(0),
                        verbose = TRUE){

    object <-
      set_data_hlpr(
        object = object,
        data = data,
        key.name = key_name,
        key.prefix = key_prefix,
        meta.names = meta_names,
        slot.data = "data",
        slot.key.name = "key_name",
        slot.meta = "meta",
        verbose = verbose
      )

    return(object)

  })


# -----





# g -----------------------------------------------------------------------



#' @title Obtain object data
#'
#' @description Extracts the objects data as a data.frame.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame with subclass \code{tibble}.
#' @export
#'

setGeneric(name = "getDf", def = function(object){

  standardGeneric(f = "getDf")

})



#' @title Obtain analysis results
#'
#' @description Generic extractor for results mainly used for
#' programming purpose as it provides informative error
#' messages if the requested content is missing.
#'
#' @inherit argument_dummy params
#' @param ...
#'
#' @return Depends on the objects class.
#' @export
#'

setGeneric(name = "getResults", def = function(object, ...){

  standardGeneric(f = "getResults")

})





# s -----------------------------------------------------------------------

#' @title Set data and key variables
#'
#' @description Sets the objects slot @@data and slot @@key_name and makes
#' sure that the key variable exists and is valid.
#'
#' @inherit argument_dummy params
#'
#' @details If \code{key_name} is NULL the key variable is constructed
#' as a combination of the input data.frames rownames and the string
#' provided with \code{key_prefix}. If the the datas rownames are NULL
#' or contain \emph{""} the rownumbers are used instead. The newly constructed
#' key variable is named \emph{data_ids}.
#'
#'
#'
#' @return A data.frame with subclass \code{tibble}.
#' @export
#'
setGeneric(name = "setData", def = function(object,
                                            data,
                                            key_name = "id",
                                            key_prefix = "",
                                            verbose = TRUE){

  standardGeneric(f = "setData")

})



#' @title Set data helper
#'
#' @description Helper for generic function \code{setData()}.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
set_data_hlpr <- function(object,
                          data,
                          key.name,
                          key.prefix,
                          slot.key.name = "key_name",
                          slot.data = "data",
                          verbose = TRUE){

  if(base::is.character(key.name)){

    check_data_frame(
      df = data,
      var.class = purrr::set_names(x = list(c("character", "factor")), nm = key.name)
    )

    is_key_variable(df = data, key.name = key.name, stop.if.false = TRUE)

    data[[key.name]] <- base::as.character(data[[key.name]])

    base::rownames(data) <- NULL

  } else if(base::is.null(key.name)){

    give_feedback(
      msg = "Constructing new key variable named 'data_ids'.",
      verbose = verbose
    )

    key_var <- base::rownames(data)

    if(base::is.null(key_var) | base::any(key_var == "")){

      give_feedback(
        msg = "Rownames are empty or incomplete. Using rownumbers and prefix '{key.prefix}' to construct key variable.",
        verbose = verbose
      )

      rnums <- 1:base::nrow(data)

      key_var <- stringr::str_c(key.prefix, rnums, sep = "")

    } else {

      key_var <- stringr::str_c(key.prefix, key_var)

    }

    key.name <- "data_ids"

    data[[key.name]] <- key_var

  }

  methods::slot(object, name = slot.data) <- base::as.data.frame(data)

  methods::slot(object, name = slot.key.name) <- key.name

  return(object)

}

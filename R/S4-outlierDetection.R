

#' @include S4-generics.R
NULL

# S4-classes --------------------------------------------------------------

#' @title The \code{OutlierDetection}-class
#'
#' @description S4-class that contains data and the corresponding outlier detection
#' results of several methods.
#'
#' @slot data data.frame. The data based on which outlier detection is conducted.
#' @slot key_name character. The name of the variable that is used to identify
#' each observation uniquely.
#' @slot methods list. A list of objects of S4-class \code{OutlierDetectionMethods}.
#' @slot variables_grouping character. The names of all grouping variables
#' of the input data - variables of class character or factor. (Does not include
#' variable of slot @@key_name)
#' @slot variables_numeric character. The names of all numeric variables
#' based on which outlier detection is conducted.
#'
#' @export

OutlierDetection <- setClass(Class = "OutlierDetection",
                             slots = list(
                               data = "data.frame",
                               key_name = "character",
                               methods = "list",
                               variables_grouping = "character",
                               variables_numeric = "character"
                             ))

#' @title The \code{OutlierDetectionMethod}-class
#'
#' @description S4-class that contains results from the outlier detection
#' algorithm it stands for.
#'
#' @slot key_name character. The name of the variable that is used to identify
#' each observation uniquely.
#' @slot method character. The name of the method. Is additionally encoded
#' as a suffix in the S4-class that inherits class \code{OutlierDetectionMethod}
#'
#' @export
#'
OutlierDetectionMethod <- setClass(Class = "OutlierDetectionMethod",
                                   slots = list(
                                     key_name = "character",
                                     method = "character"
                                   ))


#' @title The \code{OutlierDetectionIQR}-class
#'
#' @description S4-class that contains results from outlier detection
#' algorithm \emph{Interquartile Range}.
#'
#' @slot results list. Named list of IDs that were identified as outliers.
#' IDs are extracted from the key variable.
#' @slot results_across list (nested list). Named list of lists as described
#' in slot @@results. Named according to the grouping variables across which
#' outlier detection has been conducted.
#' @export

OutlierDetectionIQR <- setClass(Class = "OutlierDetectionIQR",
                                slots = list(
                                  results = "list",
                                  results_across = "list"
                                ),
                                contains = "OutlierDetectionMethod"
)


#' @title The \code{OutlierDetectionMahalanobis}-class
#'
#' @description S4-class that contains results from outlier detection
#' algorithm \emph{Mahalanobis}.
#'
#' @slot results data.frame. A data.frame with columns \emph{mahal}, \emph{pval} and
#' \emph{<key_name>}.
#'
#' @slot results_across list. Named list of data.frames as described
#' in slot @@results. Named according to the grouping variables across which
#' outlier detection has been conducted.
#'
#' @export
OutlierDetectionMahalanobis <- setClass(Class = "OutlierDetectionMahalanobis",
                                        slots = list(
                                          results = "data.frame",
                                          results_across = "list"
                                        ),
                                        contains = "OutlierDetectionMethod"
)

# -----


# r-objects ---------------------------------------------------------------

outlier_var_classes <-
  list(
    IQR = c("numeric"),
    Mahalanobis = c("numeric")
  )

valid_methods_outlier_detection <- base::names(outlier_var_classes)

# -----


# functions ---------------------------------------------------------------

#' @title Set up \code{OutlierDetection} object
#'
#' @description Sets up an object of class \code{OutlierDetection}.
#'
#' @param data A data.frame. (Rownames are dropped.)
#' @inherit argument_dummy params
#'
#' @inherit setData details
#'
#' @return An object of S4-class \code{OutlierDetection}.
#' @export
#'
initiateOutlierDetection <- function(data, key_name, key_prefix = "ID", verbose = TRUE){

  # input check
  is_value(x = key_name, mode = "character", skip.allow = TRUE, skip.val = NULL)

  df <- base::as.data.frame(data)

  variables_numeric <-
    dplyr::select_if(df, .predicate = base::is.numeric) %>%
    base::colnames()

  variables_grouping <-
    dplyr::select(df, -{{key_name}}) %>%
    dplyr::select_if(.predicate = ~ base::is.character(.x) | base::is.factor(.x)) %>%
    base::colnames()

  object <-
    OutlierDetection(
      variables_grouping = variables_grouping,
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

  return(object)

}

#' @title Detect outliers with IQR
#'
#' @description Uses IQR to detect outliers in a data.frame
#'
#' @param df A data.frame.
#' @param key.name Character value. The variable that identifies each observation uniquely
#' with its value.
#' @param var.name Character value. The numeric variable according to which
#' outliers are detected.
#'
#' @return Character vector. The values from variable \code{key.name} that
#' were identified as outliers. If not outliers exist the function returns
#' a character vector of length 0.
#' @export
#'

detect_outliers_iqr <- function(df, key.name, var.name){

  variable <- base::as.numeric(df[[var.name]])

  iqr_res <- grDevices::boxplot.stats(variable)

  outlier_values <- iqr_res$out

  outlier_positions <- base::which(variable %in% outlier_values)

  outlier_ids <- dplyr::pull(df[outlier_positions,], var = {{key.name}})

  return(outlier_ids)

}

#' @title Detect outliers with Mahalanobis
#'
#' @description Uses mahalanobis distance to detect outliers in a data.frame
#'
#' @param df A data.frame.
#' @param key.name Character value. The variable that identifies each observation uniquely
#' with its value.
#' @param var.names Character vector. The numeric variables according to which
#' outliers are detected. If NULL, all numeric variable are chosen
#'
#' @return A data.frame with columns \emph{mahal}, \emph{pval} and
#' \emph{<key.name>}.
#'
#' @export
#'
detect_outliers_mahalanobis <- function(df, key.name, var.names = NULL){

  is_vec(x = var.names, mode = "character", skip.allow = TRUE, skip.val = NULL)

  is_key_variable(df = df, key.name = key.name, stop.if.false = TRUE)

  if(base::is.character(var.names)){

    check_data_frame(
      df = df,
      var.class =
        purrr::map(var.names, .f = ~ "numeric") %>%
        purrr::set_names(nm = var.names)
    )

  } else {

    var.names <-
      dplyr::select_if(df, .predicate = base::is.numeric) %>%
      base::colnames()

  }

  df <-
    dplyr::select(df, {{key.name}}, dplyr::all_of(var.names)) %>%
    tibble::column_to_rownames(var = {{key.name}})

  df$mahal <-
    stats::mahalanobis(
      x = df,
      center = base::colMeans(df),
      cov = stats::cov(df)
    )

  df$pval <-
    stats::pchisq(
      q = df$mahal,
      df = base::length(var.names)-1,
      lower.tail = FALSE
    )

  out <-
    dplyr::select(df, mahal, pval) %>%
    tibble::rownames_to_column(var = {{key.name}}) %>%
    tibble::as_tibble() %>%
    magrittr::set_attr(which = "var.names", value = var.names)

  return(out)

}


#' @title Detect outliers
#'
#' @description Runs outlier detection based on the the suffixed method.
#'
#' @inherit argument_dummy params
#' @param object An object of S4-class \code{OutlierDetection}.
#' @param across Character value or NULL. If character,
#' the data is splittd into the groups the variable \code{across}
#' contains and outlier detection is conducted separately for
#' each group.
#'
#' @return The input object.
#' @export
#'
detectOutliersIQR <- function(object, across = NULL, verbose = TRUE){

  df <- getDf(object)

  key_name <- object@key_name
  var_names <- object@variables_numeric

  iqr_results <- object@methods[["IQR"]]

  if(base::is.null(iqr_results)){

    give_feedback(
      msg = "Creating new object of class 'OutlierDetectionIQR'.",
      verbose = verbose
    )

    iqr_results <- OutlierDetectionIQR(key_name = key_name)

  }

  # detect outliers
  if(base::is.character(across)){

    check_one_of(
      input = across,
      against = object@variables_grouping
    )

    give_feedback(
      msg = glue::glue("Detecting outliers across '{across}' with method 'IQR'."),
      verbose = verbose
    )

    groups <- base::unique(df[[across]])

    iqr_results@results_across[[across]] <-
      purrr::map(.x = groups, .f = function(group){

        flt_df <- dplyr::filter(df, !!rlang::sym(across) == {{group}})

        out_list <-
          purrr::map(.x = var_names, .f = function(var_name){

            ids <-
              detect_outliers_iqr(
                df = flt_df,
                key.name = key_name,
                var.name = var_name
              )

            return(ids)

          }) %>%
          purrr::set_names(nm = var_names)

      }) %>%
      purrr::set_names(nm = groups)

  } else {

    give_feedback(
      msg = "Detecting outliers with method 'IQR'.",
      verbose = verbose
    )

    iqr_results@results <-
      purrr::map(.x = var_names, .f = function(var_name){

        ids <-
          detect_outliers_iqr(
            df = df,
            key.name = key_name,
            var.name = var_name
          )

        return(ids)

      }) %>%
      purrr::set_names(nm = var_names)

  }

  object@methods[["IQR"]] <- iqr_results

  give_feedback(msg = "Done.", verbose = verbose)

  return(object)

}

#' @rdname detectOutliersIQR
#' @export
detectOutliersMahalanobis <- function(object, across = NULL, verbose = TRUE){

  df <- getDf(object)

  key_name <- object@key_name
  var_names <- object@variables_numeric

  mahal_results <- object@methods[["Mahalanobis"]]

  if(base::is.null(mahal_results)){

    give_feedback(
      msg = "Creating new object of class 'OutlierDetectionMahalanobis'.",
      verbose = verbose
    )

    mahal_results <- OutlierDetectionMahalanobis(key_name = key_name)

  }

  # detect outliers
  if(base::is.character(across)){

    check_one_of(
      input = across,
      against = object@variables_grouping
    )

    give_feedback(
      msg = glue::glue("Detecting outliers across '{across}' with method 'Mahalanobis'."),
      verbose = verbose
    )

    groups <- base::unique(df[[across]])

    mahal_results@results_across[[across]] <-
      purrr::map(.x = groups, .f = function(group){

        flt_df <- dplyr::filter(df, !!rlang::sym(across) == {{group}})

        ids <-
          detect_outliers_mahalanobis(
            df = flt_df,
            key.name = key_name,
            var.names = var_names
          )

        return(ids)

      }) %>%
      purrr::set_names(nm = groups)

  } else {

    give_feedback(
      msg = "Detecting outliers with method 'Mahalanobis'.",
      verbose = verbose
    )

    mahal_results@results <-
      detect_outliers_mahalanobis(
        df = df,
        key.name = key_name,
        var.names = var_names
      )

  }

  object@methods[["Mahalanobis"]] <- mahal_results

  give_feedback(msg = "Done.", verbose = verbose)

  return(object)


}


# -----


# own generics ------------------------------------------------------------

#' @title Detect outliers
#'
#' @description Runs outlier detection according to the method
#' denoted in argument \code{method}.
#'
#' @inherit argument_dummy params
#' @param across Character value or NULL. If character, denotes
#' the grouping variable of interest.
#' @param method Character value. The outlier detection method. Valid
#' input options are \emph{'IQR'} and \emph{'Mahalanobis'}.
#'
#' @return The input object.
#' @export
#'

setGeneric(name = "detectOutliers", def = function(object, ...){

  standardGeneric(f = "detectOutliers")

})



#' @title Obtain outlier results
#'
#' @description Extracts outlier results from the object.
#'
#' @inherit detectOutliers params
#' @inherit argument_dummy params
#'
#' @return  If \code{across} is NULL the return value is a data.frame if
#' \code{method} = \emph{'Mahalanobis'} or a list if \code{method} = \emph{'IQR'}.
#' In this list each slot is a vector of IDs that were identified
#' as outliers within the variable denoted by the name of the list's slot.
#'
#' If \code{across} is a character value the returned value is a list
#' named according to the groups of the grouping variable \code{across} each containing
#' output as described above.
#'
setGeneric(name = "getOutlierResults", def = function(object, method = "IQR", across = NULL, verbose = TRUE, ...){

  standardGeneric(f = "getOutlierResults")

})

# -----


# methods for own generics ------------------------------------------------

#' @rdname detectOutliers
#' @export
setMethod(
  f = "detectOutliers",
  signature = "OutlierDetection",
  definition = function(object, method, across = NULL, verbose = TRUE){

    check_one_of(
      input = method,
      against = valid_methods_outlier_detection
    )

    if(method == "IQR"){

      object <-
        detectOutliersIQR(
          object = object,
          across = across,
          verbose = verbose
        )

    } else if(method == "Mahalanobis"){

      object <-
        detectOutliersMahalanobis(
          object = object,
          across = across,
          verbose = verbose
        )

    }

    return(object)

  })


#' @rdname getOutlierResults
#' @export
setMethod(
  f = "getOutlierResults",
  signature = "OutlierDetection",
  definition = function(object, method = "IQR", across = NULL, verbose = TRUE){

    method_obj <- object@methods[[method]]

    if(base::is.null(method_obj)){

      stop(glue::glue("No outlier detection results found for method '{method}'."))

    }

    if(base::is.character(across)){

      check_one_of(
        input = across,
        against = object@variables_grouping
      )

      out <- method_obj@results_across[[across]]

      if(base::is.null(out)){

        stop(glue::glue("No outlier detection results found for method '{method}' across variable '{across}'."))

      }

    } else {

      out <- method_obj@results

      if(rlang::is_empty(out)){

        stop(glue::glue("No outlier detection results found for method '{method}'."))

      }

    }

    return(out)

  })

# -----


# methods for external generics -------------------------------------------



#' @rdname getDf
#' @export
setMethod(f = "getDf", signature = "OutlierDetection", definition = function(object){

  tibble::as_tibble(object@data)

})


#' @rdname setData
#' @export
setMethod(
  f = "setData",
  signature = "OutlierDetection",
  definition = function(object,
                        data,
                        key_name = NULL,
                        key_prefix = "id"){

    object <-
      set_data_hlpr(
        object = object,
        data = data,
        key.name = key_name,
        key.prefix = key_prefix,
        slot.data = "data",
        slot.key.name = "key_name"
      )

    return(object)

  })

# -----

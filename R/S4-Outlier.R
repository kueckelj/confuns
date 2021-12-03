#' @include S4-AnalysisAspect.R
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
#' @slot meta data.frame. Data that was part of the input data but is not supposed
#' to be included in analysis steps.
#' @slot variables_grouping character. The names of all grouping variables
#' of the input data - variables of class character or factor. (Does not include
#' variable of slot @@key_name)
#' @slot variables_numeric character. The names of all numeric variables
#' based on which outlier detection is conducted.
#'
#' @export

OutlierDetection <- setClass(Class = "OutlierDetection",
                             slots = list(),
                             contains = "AnalysisAspect"
                             )

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
initiateOutlierDetection <- function(data,
                                     key_name,
                                     key_prefix = NULL,
                                     lgl_to_group = TRUE,
                                     meta_names = character(0),
                                     verbose = TRUE
                                     ){

  object <-
    initiateAnalysisAspect(
      data = data,
      key_name = key_name,
      key_prefix = key_prefix,
      meta_names = meta_names,
      lgl_to_group = lgl_to_group,
      verbose = verbose,
      analysis_aspect = "OutlierDetection",
    )

  return(object)

}

#' @title Detect outliers with IQR
#'
#' @description Uses IQR to detect outliers in a data.frame
#'
#' @param df A data.frame.
#' @param key.name Character value. The variable that identifies each observation uniquely.
#' @param var.name Character value. The numeric variable based on which outliers are detected.
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

    iqr_results <- OutlierDetectionIQR(key_name = key_name, method = "IQR")

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

    mahal_results <- OutlierDetectionMahalanobis(key_name = key_name, method = "Mahalanobis")

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


#' @rdname validInput
#' @export
validMethodsOutlierDetection <- function(){

  return(valid_methods_outlier_detection)

}

# -----


# own generics ------------------------------------------------------------


# d

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

# g

#' @title Obtain outlier ids
#'
#' @description Extracts IDs that identify the observations
#' that were determined as outliers.
#'
#' @inherit argument_dummy params
#' @param flatten Logical value. If \code{across} is specified
#' \code{flatten} can be set to TRUE in order to force the
#' flatten teh list into a character vector of IDs.
#'
#' @return Character vector if \code{across} = NULL. List of character
#' vectors if \code{across} is a character value.
#'
#' @export

setGeneric(name = "getOutlierIDs", def = function(object, ...){

  standardGeneric(f = "getOutlierIDs")

})


#' @title Obtain outlier results
#'
#' @description Extracts outlier results from the object.
#'
#' @inherit detectOutliers params
#' @inherit argument_dummy params
#'
#' @return The return value depends on the method chosen:
#'
#' \itemize{
#'  \item{\code{method} = \emph{'IQR'}:}{
#'  A named list in which each slot is a vector of IDs which were identified
#'  as outliers within a numeric variable denoted by the name of the list's slot.
#'  }
#'  \item{\code{method} = \emph{'Mahalanobis'}:}{
#'   A data.frame  with column names \emph{<key.name>}, \emph{mahal} and \emph{pval}.
#'  }
#' }
#'
#' If \code{across} is a character value the returned value is a list
#' named according to the groups of the grouping variable \code{across} each containing
#' output as described above.
#'
setGeneric(name = "getOutlierResults", def = function(object, ...){

  standardGeneric(f = "getOutlierResults")

})


# -----


# methods for own generics ------------------------------------------------

#' @rdname detectOutliers
#' @export
setMethod(
  f = "detectOutliers",
  signature = "OutlierDetection",
  definition = function(object, method, across = NULL, verbose = TRUE, ...){

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


#' @rdname getOutlierIDs
#' @export
setMethod(
  f = "getOutlierIDs",
  signature = "OutlierDetectionIQR",
  definition = function(object,
                        variables = NULL,
                        across = NULL,
                        across_subset = NULL,
                        flatten = FALSE,
                        ...){

    if(base::is.null(across)){

      out <- getResults(object = object, across = across)

      if(base::is.character(variables)){

        check_one_of(
          input = variables,
          against = base::names(out),
          fdb.opt = 2,
          ref.opt.2 = "variables for which outliers have been detected"
        )

        out <- lselect(lst = out, all_of(variables))

      }

      out <- purrr::flatten_chr(out) %>% base::unique()

    } else if(base::is.character(across)){

      out <- getResults(object, across = across)

      if(base::is.character(across_subset)){

        check_one_of(
          input = across_subset,
          against = base::names(out),
          fdb.opt = 2,
          ref.opt.2 = glue::glue("groups of grouping variable '{across}'")
        )

        out <- lselect(lst = out, all_of(across_subset))

      }

      if(base::is.character(variables)){

        all_vars <-
          purrr::map(.x = out, .f = base::names) %>%
          purrr::flatten_chr() %>%
          base::unique()

        check_one_of(
          input = variables,
          against = all_vars,
          ref.input = "variable input",
          fdb.opt = 2,
          ref.opt.2 = "variables for which outliers have been detected"
        )

        out <- purrr::map(.x = out, .f = ~ lselect(lst = .x, all_of(variables)))

      }

      out <-
        purrr::map(
          .x = out,
          .f = ~ purrr::flatten_chr(.x) %>% base::unique()
        )

      if(base::isTRUE(flatten)){

        out <-
          purrr::flatten_chr(out) %>%
          base::unique()

      }

    }

    return(out)

  })


#' @rdname getOutlierIDs
#' @export
setMethod(
  f = "getOutlierIDs",
  signature = "OutlierDetection",
  definition = function(object,
                        method = "IQR",
                        variables = NULL,
                        across = NULL,
                        across_subset = NULL,
                        flatten = FALSE){

    method_obj <- getResults(object, method = method)

    if(method == "IQR"){

      out <-
        getOutlierIDs(
          object = method_obj,
          variables = variables,
          across = across,
          across_subset = across_subset,
          flatten = flatten
        )

    }

    return(out)

  }
)





#' @rdname getOutlierResults
#' @export
setMethod(
  f = "getOutlierResults",
  signature = "OutlierDetection",
  definition = function(object, method = "IQR", across = NULL, verbose = TRUE){

    method_obj <- getResults(object = object, method = method)

    if(base::is.character(across)){

      check_one_of(
        input = across,
        against = object@variables_grouping
      )

      out <- getResults(object = method_obj, across = across)


    } else {

      out <- getResults(object = method_obj, across = across)

    }

    return(out)

  })

# -----


# methods for external generics -------------------------------------------



#' @rdname getResults
#' @export
setMethod(f = "getResults", signature = "OutlierDetection", definition = function(object, method){

  out <- object@methods[[method]]

  if(base::is.null(out)){

    stop(glue::glue("No outlier detection results found for method '{method}'."))

  }

  return(out)

})

#' @rdname getResults
#' @export



# -----

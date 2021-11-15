#' @include S4-AnalysisAspect.R
NULL




# S4-classes --------------------------------------------------------------

#' @title The \code{Correlation}-class
#'
#' @description S4-class for convenient correlation analysis.
#'
#' @slot data data.frame. The data on which the analysis bases on.
#' @slot key_name character. The name of the variable that is used to identify
#' each observation uniquely.
#' @slot meta data.frame. Data that was part of the input data but is not supposed
#' to be included in analysis steps.
#' @slot methods list. A list of objects of S4-class \code{ClusteringMethod}.
#' @slot variables_grouping character. The names of all grouping variables
#' of the input data - variables of class character or factor. (Does not include
#' variable of slot @@key_name)
#' @slot variables_logical character. The names of all logical variables of
#' the input data.
#' @slot variables_numeric character. The names of all numeric variables
#' based on which outlier detection is conducted.

Correlation <- setClass(Class = "Correlation",
                        slots = list(),
                        contains = "AnalysisAspect"
                        )


CorrelationMethod <- setClass(Class = "CorrelationMethod",
                              slots = list(
                                key_name = "character",
                                method = "character",
                                results = "list",
                                results_across = "list"
                              ))


CorrelationPearson <- setClass(Class = "CorrelationPearson",
                               slots = list(),
                               contains = "CorrelationMethod"
                               )

CorrelationSpearman <- setClass(Class = "CorrelationSpearman",
                                slots = list(),
                                contains = "CorrelationMethod"
                                )


# -----



# r-objects ---------------------------------------------------------------

valid_methods_corr <- c("pearson", "spearman")

valid_plot_types_corr <- c("lower", "upper", "complete")

valid_shapes_corr <- c("circle", "rect", "tile")

# -----


# functions ---------------------------------------------------------------

#' @rdname initiateAnalysisAspect
#' @export
initiateCorrelation <- function(data,
                                key_name,
                                key_prefix = "ID",
                                meta_names = character(0),
                                verbose = TRUE){

  object <-
    initiateAnalysisAspect(
      data = data,
      key_name = key_name,
      key_prefix = key_prefix,
      meta_names = meta_names,
      verbose = verbose,
      analysis_aspect = "Correlation"
    )

  return(object)

}


#' @rdname validInput
#' @export
validMethodsCorr <- function(){

  return(valid_methods_corr)

}


# -----




# methods for external generics -------------------------------------------

#' @rdname computeCorrelation
setMethod(
  f = "computeCorrelation",
  signature = "Correlation",
  definition = function(object,
                        across = NULL,
                        methods_corr = "pearson",
                        verbose = TRUE
                        ){

    check_one_of(
      input = methods_corr,
      against = validMethodsCorr()
    )

    is_value(x = across, mode = "character", skip.allow = TRUE, skip.val = NULL)

    for(method_corr in methods_corr){

      corr_obj <- object@methods[[method_corr]]

      if(base::is.null(corr_obj)){

        class_name <-
          stringr::str_c("Correlation",  make_capital_letters(method_corr))

        give_feedback(
          msg = glue::glue("Creating new object of class {class_name}."),
          verbose = verbose
          )

        corr_obj <-
          methods::new(
            Class = class_name,
            key_name = object@key_name,
            method = method_corr
          )

      }

      give_feedback(
        msg = glue::glue("Computing correlation according to method '{method_corr}'."),
        verbose = verbose
        )

      if(base::is.null(across)){

        mtr <- getMtr(object)

        corr_obj@results <-
          Hmisc::rcorr(x = mtr, type = method_corr) %>%
          magrittr::set_attr(which = "class", value = "list")

      } else {

        check_one_of(
          input = across,
          against = object@variables_grouping
        )

        give_feedback(msg = glue::glue("Correlating across '{across}'."), verbose = verbose)

        df <- getDf(object, numeric = TRUE, grouping = TRUE)

        groups <- levels(df[[across]])

        corr_obj@results_across[[across]] <-
          purrr::map(.x = groups, .f = function(group){

            mtr <-
              dplyr::filter(df, !!rlang::sym(across) == {{group}}) %>%
              tibble::column_to_rownames(var = object@key_name) %>%
              dplyr::select_if(.predicate = base::is.numeric) %>%
              base::as.matrix()

            out <-
              Hmisc::rcorr(x = mtr, type = method_corr) %>%
              magrittr::set_attr(which = "class", value = "list")

            return(out)

          })

      }

      object@methods[[method_corr]] <- corr_obj

    }

    give_feedback(msg = "Done.", verbose = verbose)

    return(object)

  })

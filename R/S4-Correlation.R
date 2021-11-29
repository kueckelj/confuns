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

valid_types_corr <- c("lower", "upper", "complete")

valid_shapes_corr <- c("circle", "rect", "tile")

# -----


# functions ---------------------------------------------------------------

#' @title Adjust correlation matrix
#'
#' @description
#'
#' @param mtr Correlation or p-value matrix.
#' @inherit corr_dummy params
#'
#' @return Adjusted input matrix.
#' @export
#'

adjust_corr_mtr <- function(mtr, type = "complete", diagonal = TRUE){

  if(type == "lower"){

    mtr[base::upper.tri(mtr, diag = !diagonal)] <- NA

  } else if(type == "upper"){

    mtr[base::lower.tri(mtr, diag = !diagonal)] <- NA

  }

  return(mtr)

}

distinct_corr_df <- function(corr_df){

  if(base::names(corr_df)[1] != "var1"){

    stop("Input data.frame must not contain data across groups.")

  }

  vars <- base::unique(corr_df$var1) %>% base::as.character()

  comb_df <-
    utils::combn(x = vars, m = 2) %>%
    base:::t() %>%
    base::as.data.frame() %>%
    magrittr::set_colnames(value = c("var1", "var2"))

  out <-
    dplyr::left_join(x = comb_df, y = corr_df, by = c("var1", "var2")) %>%
    tibble::as_tibble()

  return(out)

}

#' @rdname initiateAnalysisAspect
#' @export
initiateCorrelation <- function(data,
                                key_name,
                                key_prefix = "ID",
                                lgl_to_group = TRUE,
                                meta_names = character(0),
                                verbose = TRUE){

  object <-
    initiateAnalysisAspect(
      data = data,
      key_name = key_name,
      key_prefix = key_prefix,
      meta_names = meta_names,
      lgl_to_group = lgl_to_group,
      verbose = verbose,
      analysis_aspect = "Correlation"
    )

  return(object)

}


#' @title Melt \code{rcorr}
#'
#' @description Melts object of class \code{rcorr} to a data.frame.
#'
#' @param rcorr_obj An object of class \code{rcorr}.
#' @inherit corr_dummy params
#'
#' @return A data.frame with the following columns:
#'
#' \itemize{
#'  \item{\emph{var1}:}{ Factor. First variable of the correlated variable pair.}
#'  \item{\emph{var2}:}{ Factor. Second variable of the correlated variable pair.},
#'  \item{\emph{corr}:}{ Numeric. The correlation vaule.},
#'  \item{\emph{pval}:}{ Numeric. The corresponding p-value.},
#'  }
#'
#' @export
#'
melt_rcorr <- function(rcorr_obj, type = "complete", diagonal = TRUE, distinct = FALSE){

  pval_df <-
    adjust_corr_mtr(mtr = base::as.matrix(rcorr_obj$P), type = type, diagonal = diagonal) %>%
    reshape2::melt(
      data = .,
      varnames = c("var1", "var2"),
      value.name = "pval"
    )

  corr_df <-
    adjust_corr_mtr(mtr = base::as.matrix(rcorr_obj$r), type = type, diagonal = diagonal) %>%
    reshape2::melt(
      varnames = c("var1", "var2"),
      value.name = "corr"
    ) %>%
    tibble::as_tibble() %>%
    dplyr::left_join(x = ., y= pval_df, by = c("var1", "var2"))

  if(base::isTRUE(distinct)){

    corr_df <- distinct_corr_df(corr_df)

  }

  return(corr_df)

}


#' @rdname validInput
#' @export
validMethodsCorrelation <- function(){

  return(valid_methods_corr)

}

#' @rdname validInput
#' @export
validShapesCorrelation <- function(){

  return(valid_shapes_corr)

}

#' @rdname validInput
#' @export
validTypesCorrelation <- function(){

  return(valid_types_corr)

}

# -----




# methods for external generics -------------------------------------------

methods::setOldClass(Classes = "corr_df")


#' @rdname computeCorrelation
setMethod(
  f = "computeCorrelation",
  signature = "Correlation",
  definition = function(object,
                        across = NULL,
                        methods_corr = "pearson",
                        verbose = TRUE,
                        ...
                        ){

    check_one_of(
      input = methods_corr,
      against = validMethodsCorrelation()
    )

    n_vars <- base::length(object@variables_numeric)

    is_vec(x = across, mode = "character", skip.allow = TRUE, skip.val = NULL)

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
        msg = glue::glue("Correlating {n_vars} variables according to method '{method_corr}'."),
        verbose = verbose
        )

      if(base::is.null(across)){

        mtr <- getMtr(object)

        corr_obj@results <-
          Hmisc::rcorr(x = mtr, type = method_corr, ...) %>%
          magrittr::set_attr(which = "class", value = "list")

      } else {

        all_across <- base::unique(across)

        base::rm(across)

        for(across in all_across){

          check_one_of(
            input = across,
            against = object@variables_grouping
          )

          give_feedback(msg = glue::glue("Correlating {n_vars} variables across '{across}'."), verbose = verbose)

          df <- getDf(object, numeric = TRUE, grouping = TRUE)

          groups <- base::levels(df[[across]])

          corr_obj@results_across[[across]] <-
            purrr::map(.x = groups, .f = function(group){

              mtr <-
                dplyr::filter(df, !!rlang::sym(across) == {{group}}) %>%
                tibble::column_to_rownames(var = object@key_name) %>%
                dplyr::select_if(.predicate = base::is.numeric) %>%
                base::as.matrix()

              out <-
                tryCatch({

                  res <-
                    Hmisc::rcorr(x = mtr, type = method_corr) %>%
                    magrittr::set_attr(which = "class", value = "list")

                  res

                },
                error = function(error){

                  give_feedback(
                    msg = glue::glue(
                      "The following error occured in group '{group}': ",
                      "'{error$message}.'"
                    ),
                    verbose = TRUE
                  )

                })

              return(out)

            }) %>%
            purrr::set_names(nm = groups)

        }



      }

      object@methods[[method_corr]] <- corr_obj

    }

    give_feedback(msg = "Done.", verbose = verbose)

    return(object)

  })


#' @rdname getCorrDf
#' @export
setMethod(
  f = "getCorrDf",
  signature = "Correlation",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        pval_threshold = 0.05,
                        type = "complete",
                        diagonal = TRUE,
                        distinct = FALSE,
                        digits = 2,
                        verbose = TRUE,
                        sep = " & ",
                        ...){

    is_value(sep, mode = "character")
    is_value(digits, mode = "numeric")

    check_one_of(
      input = type,
      against = validTypesCorrelation()
    )

    if(base::is.null(across)){

      corr_res <- getRcorr(object, method_corr = method_corr)

      corr_df <- melt_rcorr(corr_res, type = type, diagonal = diagonal, distinct = distinct)

    } else {

      corr_df <-
        getRcorr(
          object = object,
          method_corr = method_corr,
          across = across,
          across_subset = across_subset
        ) %>%
        purrr::imap_dfr(
          .x = .,
          .f = function(corr_res, group){

            if(base::is.character(corr_res)){

              msg <-
                glue::glue(
                  "No correlation results for group '{group}' due to error: ",
                  "{corr_res}"
                )

              give_feedback(msg = msg, verbose = verbose, with.time = FALSE)

              out <- NULL

            } else {

              out <-
                melt_rcorr(rcorr_obj = corr_res, type = type, diagonal = diagonal, distinct = distinct) %>%
                dplyr::mutate({{across}} := {{group}})

            }

            return(out)

          }
        ) %>%
        dplyr::mutate({{across}} := base::as.factor(x = !!rlang::sym(across)))

    }

    corr_df <-
      dplyr::mutate(
        .data = corr_df,
        var1 = base::as.factor(var1),
        var2 = base::as.factor(var2),
        var_pair = stringr::str_c(var1, var2, sep = sep) %>% base::as.factor(),
        corr = base::round(corr, digits = digits),
        pval_threshold = {{pval_threshold}},
        signif = pval < pval_threshold,
        signif = tidyr::replace_na(signif, replace = TRUE),
        method_corr = {{method_corr}}
      ) %>%
      dplyr::select(
        dplyr::any_of(across), var1, var2, var_pair, corr, pval, pval_threshold, signif, method_corr
      )

    base::class(corr_df) <-
      c("corr_df", base::class(corr_df))

    return(corr_df)

  }
)

#' @inherit corr_dummy params
#' @rdname getCorrMtr
#' @export
setMethod(
  f = "getCorrMtr",
  signature = "Correlation",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        type = "complete",
                        diagonal = TRUE,
                        flatten = TRUE){

    if(base::is.null(across)){

      out <-
        getRcorr(object = object, method_corr = method_corr, as_list = TRUE)[["r"]] %>%
        adjust_corr_mtr(type = type, diagonal = diagonal)

    } else {

      out <-
        getRcorr(
          object = object,
          method_corr = method_corr,
          across = across,
          across_subset = across_subset,
          as_list = TRUE
        ) %>%
        purrr::map(
          .f = ~ adjust_corr_mtr(mtr = .x[["r"]], type = type, diagonal = diagonal)
          )

      if(base::length(out) == 1 & base::isTRUE(flatten)){

        out <- out[[1]]

      }

    }

    return(out)

  }
)

#' @rdname getRcorr
#' @export
setMethod(
  f = "getRcorr",
  signature = "Correlation",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        as_list = FALSE,
                        flatten = TRUE){

    check_one_of(
      input = method_corr,
      against = validMethodsCorrelation()
    )

    corr_obj <- getResults(object, method = method_corr)

    out <-
      getRcorr(
        object = corr_obj,
        method_corr = method_corr,
        across = across,
        across_subset = across_subset,
        as_list = as_list
      )

  }
)


#' @rdname getRcorr
#' @export
setMethod(
  f = "getRcorr",
  signature = "CorrelationMethod",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        as_list = FALSE,
                        stop_if_null = TRUE){

    check_one_of(
      input = method_corr,
      against = validMethodsCorrelation()
    )

    if(base::is.null(across)){

      out <- object@results

      if(base::is.null(out) & base::isTRUE(stop_if_null)){

        stop(
          glue::glue(
            "No rcorr object found for method '{method_corr}'."
          )
        )

      }

      if(base::isFALSE(as_list)){

        out <- magrittr::set_attr(x = out, which = "class", value = "rcorr")

      }


    } else {

      out <- object@results_across[[across]]

      if(base::is.null(out)){

        stop(
          glue::glue(
            "No results found for method '{method_corr}' across '{across}'."
          )
        )

      }

      if(base::is.null(across_subset)){

        across_subset <- base::names(out)

      } else {

        check_one_of(
          input = across_subset,
          against = base::names(out)
        )

      }

      if(base::isFALSE(as_list)){

        out <-
          purrr::map(
            .x = out[across_subset],
            .f = ~ magrittr::set_attr(.x, which = "class", value = "rcorr")
          )

      }


      if(base::length(out) == 1 & base::isTRUE(flatten)){

        out <- out[[1]]

      }

    }

    return(out)

  }
)

#' @rdname plotCorrplot
#' @export
setMethod(
  f = "plotCorrplot",
  signature = "Correlation",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        variables_subset = NULL,
                        pval_threshold = NULL,
                        type = "lower",
                        diagonal = TRUE,
                        color_low = "darkred",
                        color_high = "steelblue",
                        shape = "tile",
                        size_by_corr = TRUE,
                        size_max = 15,
                        display_values = TRUE,
                        values_alpha = 0.9,
                        values_color = "black",
                        values_digits = 2,
                        values_size = 4,
                        display_grid = TRUE,
                        grid_color = "grey",
                        grid_size = 0.5,
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE){


    check_one_of(
      input = shape,
      against = validShapesCorrelation()
    )

    check_one_of(
      input = type,
      against = validTypesCorrelation()
    )

    corr_df <-
      getCorrDf(
        object = object,
        method_corr = method_corr,
        across = across,
        type = type,
        diagonal = diagonal,
        digits = values_digits,
        pval_threshold = base::ifelse(test = base::is.null(pval_threshold), 0.5, pval_threshold),
        verbose = verbose
      )

    if(base::is.character(variables_subset)){

      corr_df <-
        check_across_subset(
          df = corr_df,
          across = "var1",
          across_subset = variables_subset
        ) %>%
        check_across_subset(
          df = .,
          across = "var2",
          across_subset = variables_subset
        )

    }

    # baseline plot
    p <-
      ggplot2::ggplot(data = corr_df, mapping = ggplot2::aes(x = var1, y = var2)) +
      ggplot2::scale_color_gradient2(midpoint = 0, low = color_low, high = color_high, na.value = "white") +
      ggplot2::scale_size_area(max_size = size_max) +
      ggplot2::theme_void() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(),
        axis.text.x = ggplot2::element_text(angle = 90)
      ) +
      ggplot2::guides(size = FALSE) +
      ggplot2::coord_fixed()

    # add facets
    if(base::is.character(across)){

      facet_by <- rlang::sym(across)

      facet_add_on <- ggplot2::facet_wrap(facets = facet_by, nrow = nrow, ncol = ncol)

    } else {

      facet_add_on <- NULL

    }

    if(base::is.numeric(pval_threshold)){

      insignif_df <- dplyr::filter(corr_df, base::isFALSE(signif))

      insignif_add_on <-
        ggplot2::geom_point(
          data = insignif_df,
          size = size_max,
          color = "black"
        )

    } else {

      insignif_add_on <- NULL

    }

    # create aes mapping
    if(base::isTRUE(size_by_corr)){

      mapping <- ggplot2::aes(x = var1, y = var2, color = corr, size = corr_abs)

    } else {

      mapping <- ggplot2::aes(x = var1, y = var2, color = corr)

    }

    # add geometry

    # turns negative correlation values into positives
    # for display by size
    shape_df <-
      dplyr::filter(corr_df, !base::is.na(corr)) %>%
      dplyr::mutate(corr_abs = base::abs(corr))

    if(shape == "circle"){

      shape_num <- 19

      if(base::isTRUE(size_by_corr)){

        geom_add_on <- ggplot2::geom_point(data = shape_df, shape = shape_num, mapping = mapping)

      } else {

        geom_add_on <- ggplot2::geom_point(data = shape_df, shape = shape_num, mapping = mapping, size = size_max)

      }

    } else if(shape == "rect"){

      shape_num <- 15

      if(base::isTRUE(size_by_corr)){

        geom_add_on <- ggplot2::geom_point(data = shape_df, shape = shape_num, mapping = mapping)

      } else {

        geom_add_on <- ggplot2::geom_point(data = shape_df, shape = shape_num, mapping = mapping, size = size_max)

      }

    } else if(shape == "tile"){

      geom_add_on <-
        list(
          ggplot2::geom_tile(mapping = ggplot2::aes(x = var1, y = var2, fill = corr)),
          ggplot2::scale_fill_gradient2(midpoint = 0, low = color_low, high = color_high, na.value = "white")
        )

    }

    # add grid
    if(base::isTRUE(display_grid)){

      grid_add_on <-
        ggplot2::geom_tile(
          fill = NA, color = grid_color, size = grid_size,
          data = shape_df
        )

    } else {

      grid_add_on <- NULL

    }

    # add values
    if(base::isTRUE(display_values)){

      value_df <- dplyr::filter(corr_df, !base::is.na(corr))

      if(base::is.numeric(pval_threshold)){

        value_df <- dplyr::filter(value_df, signif)

      }

      values_add_on <-
        list(
          ggplot2::geom_text(
            mapping = ggplot2::aes(label = corr),
            alpha = values_alpha,
            color = values_color,
            size = values_size,
            data = value_df),
          ggplot2::guides(fill = FALSE)
        )

    } else {

      values_add_on <- NULL

    }

    p +
      facet_add_on +
      grid_add_on +
      geom_add_on +
      values_add_on +
      insignif_add_on


  }
)

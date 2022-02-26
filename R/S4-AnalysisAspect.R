#' @include S4-generics.R
NULL

#' @title The \code{AnalysisAspect}-class
#'
#' @description S4-class that provides the basic slots for any analysis
#' aspect such as clustering, dimensional reduction, outlier detection etc.
#'
#' @slot data data.frame. The data on which the analysis bases on.
#' @slot data_scaled data.frame The numeric data scaled via z-score.
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
                             data_scaled = "data.frame",
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
                                   key_prefix = NULL,
                                   meta_names = character(0),
                                   lgl_to_group = TRUE,
                                   analysis_aspect = "AnalysisAspect",
                                   verbose = TRUE){

  # input check
  is_value(x = key_name, mode = "character", skip.allow = TRUE, skip.val = NULL)

  check_one_of(
    input = analysis_aspect,
    against = valid_analysis_aspects
  )

  data <- base::as.data.frame(data)

  if(base::isTRUE(lgl_to_group)){

    data <- logical_to_group(data, skip = meta_names)

  }

  df <-
    base::as.data.frame(data) %>%
    dplyr::select(-dplyr::all_of(meta_names))

  variables_grouping <-
    dplyr::select(df, -dplyr::any_of(key_name)) %>%
    dplyr::select_if(.predicate = ~ base::is.character(.x) | base::is.factor(.x)) %>%
    base::colnames()

  variables_logical <-
    dplyr::select(df, -dplyr::any_of(key_name)) %>%
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
      meta_names = meta_names,
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


#' @rdname getKeyDf
#' @export
setMethod(
  f = "getKeyDf",
  signature = "AnalysisAspect",
  definition = function(object, ...){

    object@data[object@key_name] %>%
      tibble::as_tibble()

  }
)

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


#' @rdname getScaledDf
#' @export
setMethod(
  f = "getScaledDf",
  signature = "AnalysisAspect",
  definition = function(object, grouping = FALSE, logical = FALSE){

    sdf <- object@data_scaled

    if(purrr::is_empty(x = sdf)){

      stop(glue::glue("No scaled data in this object of class {base::class(object)}"))

    }

    df <- getDf(object, grouping = grouping, logical = logical, complete = FALSE)

    out <- dplyr::left_join(x = sdf, y = df, by = object@key_name)

    return(out)

  })

#' @rdname getScaledMtr
#' @export
setMethod(
  f = "getScaledMtr",
  signature = "AnalysisAspect",
  definition = function(object, ...){

    out <-
      getScaledDf(object) %>%
      tibble::column_to_rownames(var = object@key_name) %>%
      base::as.matrix()

    return(out)

  }
)

#' @rdname getVariableNames
#' @export
setMethod(
  f = "getVariableNames",
  signature = "AnalysisAspect",
  definition = function(object, types = c("key", "numeric", "grouping", "logical", "meta"), unname = FALSE){

    grouping_vars <-
      object@variables_grouping %>%
      purrr::set_names(nm = base::rep("grouping", base::length(.)))

    numeric_vars <-
      object@variables_numeric %>%
      purrr::set_names(nm = base::rep("numeric", base::length(.)))

    logical_vars <-
      object@variables_logical %>%
      purrr::set_names(nm = base::rep("logical", base::length(.)))

    key <- object@key_name %>% purrr::set_names(nm = "key")

    meta_vars <-
      object@meta %>%
      dplyr::select(-key) %>%
      base::colnames() %>%
      purrr::set_names(nm = base::rep("meta", base::length(.)))

    out <- c(grouping_vars, numeric_vars, logical_vars, key, meta_vars)

    if(base::is.character(types)){

      out <- out[base::names(out) %in% types]

    }

    if(base::isTRUE(unname)){

      out <- base::unname(out)

    }

    return(out)
  }
)


#' @rdname renameKeyVariable
#' @export
setMethod(
  f = "renameKeyVariable",
  signature = "AnalysisAspect",
  definition = function(object, new_key_name){

    is_value(x = new_key_name, mode = "character")

    check_none_of(
      input = new_key_name,
      against = getVariableNames(object, unname = TRUE),
      ref.against = "variable names"
    )

    old_key_name <- object@key_name

    object@key_name <- new_key_name

    object@data <- dplyr::rename(object@data, {{new_key_name}} := {{old_key_name}})

    object@data_scaled <- dplyr::rename(object@data_scaled, {{new_key_name}} := {{old_key_name}})

    object@methods <-
      purrr::map(.x = object@methods, .f = function(slot){

        slot@key_name <- new_key_name

        return(slot)

      })

    give_feedback(msg = glue::glue("Key name changed to '{new_key_name}'"))

    return(object)

  }
)

#' @rdname plotScatterplot
#' @export
setMethod(
  f = "plotScatterplot",
  signature = "AnalysisAspect",
  definition = function(object,
                        x,
                        y,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        ncol = NULL,
                        nrow = NULL,
                        scales = "fixed",
                        space = "fixed",
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_clrp = "milo",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1.5,
                        color_aes = "color",
                        color_by = NULL,
                        color_trans = "identity",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        order_by = NULL,
                        order_desc = FALSE,
                        shape_by = NULL,
                        size_by = NULL,
                        display_smooth = FALSE,
                        smooth_alpha = 0.9,
                        smooth_color = "blue",
                        smooth_method = "lm",
                        smooth_se = FALSE,
                        smooth_size = 1,
                        display_corr = FALSE,
                        corr_method = "pearson",
                        corr_p_min = 0.00005,
                        corr_pos_x = NULL,
                        corr_pos_y = NULL,
                        corr_text_sep = "\n",
                        corr_text_size = 1,
                        transform_with = NULL,
                        ...){

    df <-
      getDf(object, numeric = TRUE, grouping = TRUE)

    plot_scatterplot(
      df = df,
      x = x,
      y = y,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      ncol = ncol,
      nrow = nrow,
      scales = scales,
      space = space,
      pt.alpha = pt_alpha,
      pt.color = pt_color,
      pt.clrp = pt_clrp,
      pt.fill = pt_fill,
      pt.shape = pt_shape,
      pt.size = pt_size,
      color.aes = color_aes,
      color.by = color_by,
      color.trans = color_trans,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      clrsp = clrsp,
      order.by = order_by,
      order.desc = order_desc,
      shape.by = shape_by,
      size.by = size_by,
      display.smooth = display_smooth,
      smooth.alpha = smooth_alpha,
      smooth.color = smooth_color,
      smooth.method = smooth_method,
      smooth.se = smooth_se,
      smooth.size = smooth_size,
      display.corr = display_corr,
      corr.method = corr_method,
      corr.p.min = corr_p_min,
      corr.pos.x = corr_pos_x,
      corr.pos.y = corr_pos_y,
      corr.text.sep = "\n",
      corr.text.size = corr_text_size,
      transform.with = transform_with,
      ...
    )

  }
)

#' @rdname scaleData
#' @export
setMethod(
  f = "scaleData",
  signature = "AnalysisAspect",
  definition = function(object, na_rm = TRUE, verbose = TRUE, ...){

  give_feedback(msg = "Scaling data.", verbose = verbose)

  object@data_scaled <-
    getDf(object, numeric = TRUE, complete = FALSE) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(x = object@variables_numeric),
        .fns = normalize_zscore,
        na.rm = na_rm
      )
    )

  return(object)

})


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


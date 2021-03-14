

#' @title Plot distribution and results of statistical tests
#'
#' @inherit argument_dummy params
#' @param plot_type Character value. Denotes the function to call. Must
#' be one of \emph{'violin', 'boxplot', 'density', 'histogram', 'ridgeplot'}.
#' @param ... Arguments given to the called function.
#'
#' @inherit ggplot2_dummy return
#'
#' @return
#' @export
#'

plot_statistics <- function(df, plot_type = "violin", ...){

  default_list <-
    list(df = df, ...)

  call_flexibly(
    fn = stringr::str_c("plot", plot_type, sep = "_"),
    fn.ns = "confuns",
    default = default_list
  )

}



#' @title Plot distribution and results of statistical tests
#'
#' @description These functions visualize the distribution of numerical variables via box-
#' and violinplots while simultaneously allowing for statistical tests. See details
#' for more.
#'
#' @param display.facets Logical value. Only relevant if \code{across} is set
#' to NULL. Denotes if a subplot for each variable is supposed to be created.
#' @param step.increase Numeric value. Denotes the increase in fraction of total
#' height for every additional comparison to minimize overlap.
#' @param vjust Numeric value. Denotes the relative, vertical position of the results of
#' the test denoted in \code{test.groupwise}. Negative input highers, positive
#' input lowers the position.
#' @param ... Additional arguments given to the respective \code{ggplot2::geom_<plot_type>()}
#' function.
#'
#' @inherit argument_dummy params
#' @inherit scale_color_add_on params
#' @inherit ggplot2_dummy return
#'
#' @details
#' Argument \code{variables} accepts only values that refer to numerical
#' variables. Use \code{vjust} and \code{step.increase} to move the results of statistical
#' tests in order to keep the plot aesthetically pleasing.
#'
#' @export
#'
#' @examples #Not run:
#'
#' library(tidyerse)
#'
#' df <- mtcars
#'
#' df$cluster_kmeans <-
#'   stats::kmeans(x = mtcars, centers = 4)$cluster %>%
#'   base::as.factor()
#'
#' plot_violin(df)
#'
#' plot_violin(df, variables = c("qsec", "wt", "hp"))
#'
#' plot_violin(df,
#'             variables = c("qsec", "wt", "hp"),
#'             display.facets = FALSE)
#'
#' plot_violin(df,
#'             variables = c("qsec", "wt", "hp"),
#'             across = "cluster_kmeans",
#'             ncol = 1)

plot_violin <- function(df,
                        variables = NULL,
                        across = NULL,
                        across.subset = NULL,
                        relevel = TRUE,
                        test.pairwise = NULL,
                        test.groupwise = NULL,
                        ref.group = NULL,
                        step.increase = 0.1,
                        vjust = 0,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display.facets = TRUE,
                        display.points = FALSE,
                        pt.alpha = 0.8,
                        pt.color = "black",
                        pt.num = 100,
                        pt.shape = 19,
                        pt.size = 1.5,
                        clrp = "milo",
                        clrp.adjust = NULL,
                        verbose = TRUE,
                        ...){

  make_available(...)

  # 1. Control --------------------------------------------------------------

  are_values(c("across", "ref.group"),
             mode = "character",
             skip.allow = TRUE,
             skip.value = NULL)

  are_vectors(c("variables", "across.subset"),
              mode = "character",
              min.length  = 1,
              skip.allow = TRUE,
              skip.val = NULL)

  # 2. Data processing ------------------------------------------------------

  keep <-
    purrr::keep(.x = pt.shape, .p = ~ is_any_of(.x, "character"))

  df_shifted <-
    process_and_shift_df(
      df = df,
      keep = keep,
      variables = variables,
      valid.classes = "numeric",
      across = across,
      across.subset = across.subset,
      relevel = relevel,
      verbose = verbose
    )


  # if across is not NULL set the information to the value of 'across'
  # otherwise set to "variables"

  aes_x <-
    across_or(across, "variables")

  aes_y <- "values"

  aes_fill <-
    across_or(across, "variables")

  # 3. Create ggplot add ons -----------------------------------------------

  # facet add on
  facet_add_on <-
    statistics_facet_wrap(
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display.facets
      )

  # jitter add on
  jitter_add_on <-
    statistics_geom_jitter(
      df_shifted = df_shifted,
      across = across,
      aes_x = aes_x,
      aes_y = aes_y,
      display.points = display.points,
      pt.alpha = pt.alpha,
      pt.color = pt.color,
      pt.num = pt.num,
      pt.shape = pt.shape,
      pt.size = pt.size
    )

  # tests add on
  tests_add_on <-
    statistics_tests(
      df_shifted = df_shifted,
      across = across,
      aes_y = aes_y,
      ref.group = ref.group,
      test.pairwise = test.pairwise,
      test.groupwise = test.groupwise,
      step.increase = step.increase,
      vjust = vjust
    )

  # legend add on
  legend_add_on <-
    base::ifelse(
      test = base::is.null(across) & base::is.numeric(pt.shape),
      yes = list(legend_none()),
      no = list()
    )


  # 4. Assemble final plot output -------------------------------------------

  ggplot2::ggplot(data = df_shifted, ggplot2::aes(x = .data[[aes_x]], .data[[aes_y]])) +
    ggplot2::geom_violin(ggplot2::aes(fill = .data[[aes_fill]]), ...) +
    theme_statistics() +
    facet_add_on +
    tests_add_on +
    jitter_add_on +
    scale_color_add_on(
      aes = "fill", variable = df_shifted[[aes_fill]],
      clrp = clrp, clrp.adjust = clrp.adjust
      ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(x = NULL, y = NULL) +
    legend_add_on


}


#' @rdname plot_violin
#' @export
plot_boxplot <- function(df,
                         variables = NULL,
                         across = NULL,
                         across.subset = NULL,
                         relevel = TRUE,
                         test.pairwise = NULL,
                         test.groupwise = NULL,
                         ref.group = NULL,
                         step.increase = 0.1,
                         vjust = 0,
                         scales = "free",
                         nrow = NULL,
                         ncol = NULL,
                         display.facets = TRUE,
                         display.points = FALSE,
                         pt.alpha = 0.8,
                         pt.color = "black",
                         pt.num = 100,
                         pt.shape = 19,
                         pt.size = 1.5,
                         clrp = "milo",
                         clrp.adjust = NULL,
                         verbose = TRUE,
                         ...){

  make_available(...)

  # 1. Control --------------------------------------------------------------

  are_values(c("across", "ref.group"),
             mode = "character",
             skip.allow = TRUE,
             skip.value = NULL)

  are_vectors(c("variables", "across.subset"),
              mode = "character",
              min.length  = 1,
              skip.allow = TRUE,
              skip.val = NULL)


  # 2. Data processing ------------------------------------------------------

  keep <-
    purrr::keep(.x = pt.shape, .p = ~ is_any_of(.x, "character"))

  df_shifted <-
    process_and_shift_df(
      df = df,
      keep = keep,
      variables = variables,
      valid.classes = "numeric",
      across = across,
      across.subset = across.subset,
      relevel = relevel,
      verbose = verbose
    )


  # if across is not NULL set the information to the value of 'across'
  # otherwise set to "variables"

  aes_x <-
    across_or(across, "variables")

  aes_y <- "values"

  aes_fill <-
    across_or(across, "variables")

  # 3. Create ggplot add ons -----------------------------------------------

  # facet add on
  facet_add_on <-
    statistics_facet_wrap(
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display.facets
    )

  # jitter add on
  jitter_add_on <-
    statistics_geom_jitter(
      df_shifted = df_shifted,
      across = across,
      aes_x = aes_x,
      aes_y = aes_y,
      display.points = display.points,
      pt.alpha = pt.alpha,
      pt.color = pt.color,
      pt.num = pt.num,
      pt.shape = pt.shape,
      pt.size = pt.size
    )

  # tests add on
  tests_add_on <-
    statistics_tests(
      df_shifted = df_shifted,
      across = across,
      aes_y = aes_y,
      ref.group = ref.group,
      test.pairwise = test.pairwise,
      test.groupwise = test.groupwise,
      step.increase = step.increase,
      vjust = vjust
    )

  # legend add on
  legend_add_on <-
    base::ifelse(
      test = base::is.null(across) & base::is.numeric(pt.shape),
      yes = list(legend_none()),
      no = list()
    )


  # 4. Assemble final plot output -------------------------------------------

  ggplot2::ggplot(data = df_shifted, ggplot2::aes(x = .data[[aes_x]], .data[[aes_y]])) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = .data[[aes_fill]]), ...) +
    theme_statistics() +
    facet_add_on +
    tests_add_on +
    jitter_add_on +
    scale_color_add_on(
      aes = "fill", variable = df_shifted[[aes_fill]],
      clrp = clrp, clrp.adjust = clrp.adjust
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(x = NULL, y = NULL) +
    legend_add_on

}




#' @title Plot distribution and results of statistical tests
#'
#' @description These functions visualize the distribution of numerical variables via
#' histograms, density- and ridgeplots. Argument \code{variables} accepts
#' only values that refer to numerical variables.
#'
#' @inherit plot_violin params return
#' @inherit argument_dummy params return
#'
#' @return
#' @export
#'

plot_density <- function(df,
                         variables = NULL,
                         across = NULL,
                         across.subset = NULL,
                         relevel = TRUE,
                         display.facets = TRUE,
                         scales = "free",
                         nrow = NULL,
                         ncol = NULL,
                         clrp = "milo",
                         clrp.adjust = NULL,
                         verbose = TRUE,
                         ...){

  make_available(...)

  # 1. Control --------------------------------------------------------------

  are_values(c("across"),
             mode = "character",
             skip.allow = TRUE,
             skip.value = NULL)

  are_vectors(c("variables", "across.subset"),
              mode = "character",
              min.length  = 1,
              skip.allow = TRUE,
              skip.val = NULL)

  # 2. Data processing ------------------------------------------------------

  df_shifted <-
    process_and_shift_df(
      df = df,
      variables = variables,
      valid.classes = "numeric",
      across = across,
      across.subset = across.subset,
      relevel = relevel,
      verbose = verbose
    )

  # if across is not NULL set the information to the value of 'across'
  # otherwise set to "variables"
  aes_y <- across_or(across, "variables")
  aes_fill <- across_or(across, "variables")

  # 3. Create ggplot add ons -----------------------------------------------

  # facet add on
  facet_add_on <-
    statistics_facet_wrap(
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display.facets
    )

  # legend add on
  legend_add_on <-
    base::ifelse(
      test = base::is.null(across),
      yes = list(legend_none()),
      no = list()
    )

  # 4. Assemble final plot output -------------------------------------------

  ggplot2::ggplot(data = df_shifted, ggplot2::aes(x = .data[["values"]])) +
    ggplot2::geom_density(ggplot2::aes(fill = .data[[aes_fill]]), ...) +
    theme_statistics() +
    facet_add_on +
    scale_color_add_on(
      aes = "fill", variable = df_shifted[[aes_fill]],
      clrp = clrp, clrp.adjust = clrp.adjust
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    legend_add_on


}


#' @rdname plot_density
#' @export
plot_histogram <- function(df,
                           variables = NULL,
                           across = NULL,
                           across.subset = NULL,
                           relevel = TRUE,
                           scales = "free",
                           nrow = NULL,
                           ncol = NULL,
                           clrp = "milo",
                           clrp.adjust = NULL,
                           verbose = TRUE,
                           ...){

  make_available(...)

  # 1. Control --------------------------------------------------------------

  are_values(c("across"),
             mode = "character",
             skip.allow = TRUE,
             skip.value = NULL)

  are_vectors(c("variables", "across.subset"),
              mode = "character",
              min.length  = 1,
              skip.allow = TRUE,
              skip.val = NULL)

  # 2. Data processing ------------------------------------------------------

  df_shifted <-
    process_and_shift_df(
      df = df,
      variables = variables,
      valid.classes = "numeric",
      across = across,
      across.subset = across.subset,
      relevel = relevel,
      verbose = verbose
    )

  # if across is not NULL set the information to the value of 'across'
  # otherwise set to "variables"
  aes_y <- across_or(across, "variables")
  aes_fill <- across_or(across, "variables")

  # 3. Create ggplot add ons -----------------------------------------------

  # facet add on
  facet_add_on <-
    statistics_facet_wrap(scales = scales, nrow = nrow, ncol = ncol)

  # legend add on
  legend_add_on <-
    base::ifelse(
      test = base::is.null(across),
      yes = list(legend_none()),
      no = list()
    )

  # 4. Assemble final plot output -------------------------------------------

  ggplot2::ggplot(data = df_shifted, ggplot2::aes(x = .data[["values"]])) +
    ggplot2::geom_histogram(
      ggplot2::aes(fill = .data[[aes_fill]]),
      color = "black", ...
      ) +
    theme_statistics() +
    facet_add_on +
    scale_color_add_on(
      aes = "fill", variable = df_shifted[[aes_fill]],
      clrp = clrp, clrp.adjust = clrp.adjust
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    legend_add_on


}

#' @rdname plot_density
#' @export
plot_ridgeplot <- function(df,
                           variables = NULL,
                           across = NULL,
                           across.subset = NULL,
                           relevel = TRUE,
                           display.facets = TRUE,
                           scales = "free",
                           nrow = NULL,
                           ncol = NULL,
                           alpha = 0.85,
                           clrp = "milo",
                           clrp.adjust = NULL,
                           verbose = TRUE,
                           ...){

  make_available(...)

  # 1. Control --------------------------------------------------------------

  are_values(c("across"),
             mode = "character",
             skip.allow = TRUE,
             skip.value = NULL)

  are_vectors(c("variables", "across.subset"),
              mode = "character",
              min.length  = 1,
              skip.allow = TRUE,
              skip.val = NULL)


  # 2. Data processing ------------------------------------------------------

  df_shifted <-
    process_and_shift_df(
      df = df,
      variables = variables,
      valid.classes = "numeric",
      across = across,
      across.subset = across.subset,
      relevel = relevel,
      verbose = verbose
    )

  # if across is not NULL set the information to the value of 'across'
  # otherwise set to "variables"
  aes_y <- across_or(across, "variables")
  aes_fill <- across_or(across, "variables")


  # 3. Create ggplot add ons ------------------------------------------------

  # facet add on
  facet_add_on <-
    statistics_facet_wrap(
      display.facets = base::ifelse(base::is.null(across), FALSE, display.facets),
      scales = scales,
      nrow = nrow,
      ncol = ncol)

  # 4. Assemble final ggplot output -----------------------------------------

  ggplot2::ggplot(data = df_shifted, mapping = ggplot2::aes(.data[["values"]], .data[[aes_y]])) +
    ggridges::geom_density_ridges(
      mapping = ggplot2::aes(fill = .data[[aes_fill]]),
      color = "black", alpha = alpha, ...
      ) +
    theme_statistics() +
    facet_add_on +
    scale_color_add_on(
      aes = "fill", variable = df_shifted[[aes_fill]],
      clrp = clrp, clrp.adjust = clrp.adjust
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    legend_none()

}


#' @title Plot distribution of discrete/categorical variables
#'
#' @description This function visualizes the distribution of discrete
#' variable - argument \code{variables} accepts only values that refer
#' to discrete variables.
#'
#' @inherit plot_violin params return
#' @inherit argument_dummy params return
#'
#' @return
#' @export
#'

plot_barplot <- function(df,
                         variables = NULL,
                         across = NULL,
                         across.subset = NULL,
                         relevel = TRUE,
                         display.facets = TRUE,
                         nrow = NULL,
                         ncol = NULL,
                         clrp = "milo",
                         clrp.adjust = NULL,
                         position = "dodge",
                         ...){

  # 1. Control --------------------------------------------------------------

  base::stopifnot(is.data.frame(df))

  are_values(c("across"),
             mode = "character",
             skip.allow = TRUE,
             skip.value = NULL)

  are_vectors(c("variables", "across.subset"),
              mode = "character",
              min.length  = 1,
              skip.allow = TRUE,
              skip.val = NULL)

  check_no_overlap(
    x = variables,
    y = across
  )

  # 2. Data processing ------------------------------------------------------

  df_shifted <-
    process_and_shift_df(
      df = df,
      variables = variables,
      valid.classes = c("character", "factor"),
      across = across,
      across.subset = across.subset,
      relevel = relevel,
      verbose = verbose
    )

  # if across is not NULL set the information to the value of 'across'
  # otherwise set to "variables"
  aes_fill <-
    across_or(
      across = across,
      otherwise = "variables",
      variables = base::unique(df_shifted[["variables"]]),
      "values")

  # 3. Create ggplot add ons ------------------------------------------------

  facet_add_on <-
    statistics_facet_wrap(
      scales = "free",
      nrow = nrow,
      ncol = ncol,
      display.facets = display.facets
    )

  legend_add_on <-
    base::ifelse(
      test = base::is.null(across),
      yes = list(legend_none()),
      no = list()
      )

  scales_add_on <-
    base::ifelse(
      test = position == "fill",
      yes = list(ggplot2::scale_y_continuous(labels = scales::percent)),
      no = list()
      )

  # 4. Assemble final ggplot ------------------------------------------------

  ggplot2::ggplot(data = df_shifted, mapping = ggplot2::aes(x = .data[["values"]])) +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(fill = .data[[aes_fill]]),
      color = "black",
      position = position, ...
      ) +
    theme_statistics() +
    scale_color_add_on(
      aes = "fill", variable = df_shifted[[aes_fill]],
      clrp = clrp, clrp.adjust = clrp.adjust
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    facet_add_on +
    legend_add_on +
    scales_add_on

}



# Helper functions --------------------------------------------------------

across_or <- function(across, otherwise = "variables", variables = NULL, ...){

  res <- base::ifelse(base::is.null(across), yes = otherwise, no = across)

  if(base::is.null(across) && !base::is.null(variables) && base::length(variables) == 1){

    res <- c(...)

  }

  base::return(res)

}














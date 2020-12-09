
#' @title Plot descriptive statistics
#'
#' @description This function allows for a variety of different plots concerning
#' descriptive statistics.
#'
#' @inherit check_object params
#' @param variables Character vector. The names of the numeric variables of interest.
#' @inherit check_across_subset params
#' @inherit phase_single params
#' @param plot.type Character value. One of \emph{'histogram', 'ridgeplot', 'boxplot',
#' 'violinplot'} or \emph{'density'} to visualize the value distribution of those
#' variables specified via the \code{variables} argument.
#'
#' If set to \emph{'boxplot'} or \emph{'violinplot'} and only one variable is
#' specified statistical test can be performed.
#'
#' @param binwidth Numeric value. Only relevant if \code{plot_type} is set to \emph{'histogram'}.
#' @param display.points Logical. If set to TRUE the value distribution of \code{n.cells} is additionally
#' displayed by points.
#' @inherit pt.args params
#' @inherit n.cells params
#' @param shape.to Character value. Denotes the discrete variable to which the shape of the
#' points are mapped. (Ignored if \code{display.points} is set to FALSE.)
#' @param test.pairwise Character value. One of \emph{'none', 't.test', 'wilcox.test'}.
#' @param test.groupwise Character value. One of \emph{'none', 'anova', 'kruskal.test'}.
#' @param ref.group Character value. Denotes the reference group for the statistical tests. Must
#' be one value of the variable specified in \code{across}.
#' @inherit colors params
#' @param ... Additional arguments given to \code{ggplot2::facet_wrap()}.
#' @inherit verbose params
#'
#' @export
#'

plot_descriptive_statistics <- function(df,
                                        variables = "all",
                                        across,
                                        across.subset = NULL,
                                        plot.type = "boxplot",
                                        binwidth,
                                        test.pairwise = "none",
                                        test.groupwise = "none",
                                        display.points = FALSE,
                                        pt.size = 1.2,
                                        pt.alpha = 0.8,
                                        pt.color = "black",
                                        n.obs = 100,
                                        shape.to = "none",
                                        ref.group,
                                        clrp = "milo",
                                        ... ,
                                        verbose = TRUE){


  make_available(..., verbose = verbose)

  # 1. Control --------------------------------------------------------------

  df <- dplyr::ungroup(x = df)

  if(!plot.type %in% c("histogram", "density", "ridgeplot", "boxplot", "violinplot")){

    base::stop("Argument 'plot.type' needs to be one of 'histogram', 'density', 'ridgeplot', 'boxplot', 'violinplot'.")

  }

  if(plot.type %in% c("violinplot", "ridgeplot", "boxplot")){

    max_length = 10

  } else {

    max_length = 25

  }

  confuns::is_value(clrp, "character", "clrp")

  # check across input
  confuns::is_value(across, "character", "across")
  confuns::check_data_frame(
    df = df,
    var.class = list(c("character", "factor")) %>% magrittr::set_names(across),
    ref = "df"
  )

  # check variable input
  confuns::is_vec(variables, "character", "variables")

  if(base::all(variables == "all")){

    if(base::isTRUE(verbose)){base::message("Argument 'variables' set to 'all'. Extracting all valid, numeric variables.")}

    variables <- base::colnames(dplyr::select_if(.tbl = df, .predicate = base::is.numeric))

  } else {

    check_list <-
      purrr::map(variables, function(i){c("numeric", "integer")}) %>%
      magrittr::set_names(value = variables)

    confuns::check_data_frame(
      df = df,
      var.class = check_list,
      ref = "df"
    )

    if(base::isTRUE(verbose)){"All specified variables found."}

  }


  # prevent naming issues
  if(base::any(c("variables", "values") %in% c(variables, across))){

    call_flexibly(fn = "give_feedback",
                  fn.ns = "",
                  default = list(msg = "Arguments 'across' and 'variables' must not contain/be equal to 'variables' and/or 'values'.",
                                 type = "error",
                                 fdb.fn = "stop"),
                  v.fail = base::stop("See error-message above.")
                  )

  }


  df <- check_across_subset(df = df, across = across, across.subset = across.subset)

  # -----

  # 2. Data extraction ------------------------------------------------------

  df <-
    tidyr::pivot_longer(
      data = df,
      cols = dplyr::all_of(x = variables),
      names_to = "variables",
      values_to = "values"
    )

  df <- check_across_subset(df, across, across.subset)

  reverse <- FALSE

  # -----

  # 3. Display add on -------------------------------------------------------

  # ggplot main
  if(plot.type %in% c("density","histogram")){

    ggplot_main <-
      ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = values))

  } else if(plot.type == "ridgeplot"){

    ggplot_main <-
      ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = values, y = .data[[across]]))

  } else if(plot.type %in% c("violinplot", "boxplot")){

    ggplot_main <-
      ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = .data[[across]], y = values))

  }

  # ggplot geom
  if(plot.type == "histogram"){

    display_add_on <-
      list(
        ggplot2::geom_histogram(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                                color = "black", binwidth = binwidth,
                                data = df),
        ggplot2::labs(y = NULL)
      )

  } else if(plot.type == "density"){

    display_add_on <-
      list(
        ggplot2::geom_density(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                              color = "black", data = df,alpha = 0.825),
        ggplot2::labs(y = "Density")
      )

  } else if(plot.type == "ridgeplot"){

    reverse <- TRUE

    display_add_on <-
      list(
        ggridges::geom_density_ridges(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                                      color = "black", data = df, alpha = 0.825),
        ggplot2::labs(y = across, x = NULL)

      )

  } else if(plot.type == "violinplot"){

    display_add_on <-
      list(
        ggplot2::geom_violin(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                             color = "black", data = df),
        ggplot2::labs(y = NULL, x = across)
      )

  } else if(plot.type == "boxplot"){

    display_add_on <-
      list(
        ggplot2::geom_boxplot(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                              color = "black", data = df),
        ggplot2::labs(y = NULL, x = across)
      )

  }

  # -----


  # 4. Statistic add on -----------------------------------------------------

  max_value <- base::max(df[["values"]], na.rm = TRUE)
  labels_y <- NULL
  n_variables <- dplyr::n_distinct(df[["variables"]])


  # --- pairwise statistics
  if(n_variables == 1 & plot.type %in% testable_plottypes){

    if(test.pairwise %in% c("t.test", "wilcox.test")){

      comparison_list <-
        ggpubr_comparison_list(ref.group = ref.group, groups = base::levels(df[[across]]))

      labels_y <- ggpubr_y_labels(input.list = comparison_list, max.value = max_value)

      pairwise_add_on <- list(
        ggpubr::stat_compare_means(
          method = test.pairwise,
          comparisons = comparison_list,
          label.y = labels_y,
          data = df
        )
      )


    } else if(test.pairwise == "none") {

      if(base::isTRUE(verbose)){base::message("Skip pairwise testing.")}

      pairwise_add_on <- list()

    } else if(base::is.character(test.pairwise)){

      base::warning("Invalid input for argument 'test.pairwise'.")

    }

    # --- groupwise statistic
    if(test.groupwise %in% c("anova", "kruskal.test")){

      if(base::is.null(labels_y)){

        label_y <- max_value*1.1

      } else if(base::is.numeric(labels_y)){

        label_y <- base::max(labels_y, na.rm = TRUE)*1.1

      }

      groupwise_add_on <- list(
        ggpubr::stat_compare_means(
          method = test.groupwise,
          label.y = label_y,
          data = df
        )
      )

    } else if(test.groupwise == "none"){

      if(base::isTRUE(verbose)){base::message("Skip groupwise testing.")}

      groupwise_add_on <- list()

    } else {

      base::warning("Invalid input for argument 'test.groupwise'.")

      groupwise_add_on <- list()

    }

  } else {

    pairwise_add_on <- list()
    groupwise_add_on <- list()

   # base::warning("set up confuns warnings")

  }

  # -----


  # 5. Jitter add on  -------------------------------------------------------

  if(base::isTRUE(display.points) & plot.type %in% testable_plottypes){

    jitter_data <-
      dplyr::group_by(.data = df, !!rlang::sym(across)) %>%
      dplyr::slice_sample(n = n.obs)

    if(shape.to != "none"){

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_data, size = pt.size, alpha = pt.alpha,
          color = pt.color, mapping = ggplot2::aes(shape = .data[[shape.to]])
        )

    } else {

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_data, size = pt.size, alpha = pt.alpha,
          color = pt.color, height = 0.25, width = 0.25
        )
    }


  } else {

    jitter_add_on <- list()

  }

  # -----


  # 6. Plotting -------------------------------------------------------------

  facet_add_on <-
    call_flexibly(fn = "facet_wrap",
                  fn.ns = "ggplot2",
                  default = list(facets = stats::as.formula(. ~ variables),
                                 scales = "free"),
                  v.fail = ggplot2::facet_wrap(facets = . ~ variables, scales = "free"))

  ggplot_main +
    display_add_on +
    facet_add_on +
    confuns::scale_color_add_on(aes = "fill", variable = "discrete",
                                clrp = clrp, guide = ggplot2::guide_legend(reverse = reverse)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(color = "black"),
      axis.text.x = ggplot2::element_text(color = "black"),
      strip.text.y = ggplot2::element_text(angle = 0, face = "italic", size = 14),
      strip.placement = "outside",
      strip.background = ggplot2::element_rect(color = "white", fill = "white"),
      panel.spacing.y = ggplot2::unit(10, "pt")
    ) +
    ggplot2::labs(x = NULL) +
    pairwise_add_on +
    groupwise_add_on +
    jitter_add_on

}

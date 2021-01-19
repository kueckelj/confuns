

#' @title Facet wrap add on
#'

statistics_facet_wrap <- function(display.facets = TRUE, scales, ...){

  if(base::isTRUE(display.facets)){

    facet_formula <- stats::as.formula(. ~ variables)

    default_list <- list(facets = facet_formula, scales = scales, ...)

    facet_add_on <-
      call_flexibly(
        fn = "facet_wrap", fn.ns = "ggplot2",
        default = default_list,
        v.fail = ggplot2::facet_wrap(facets = facet_formula, scales = scales)
      )

  } else {

    facet_add_on <- NULL

  }

}



#' @title Geom jitter add on
#'

statistics_geom_jitter <- function(df_shifted,
                                   across,
                                   aes_x,
                                   aes_y,
                                   display.points,
                                   pt.alpha,
                                   pt.color,
                                   pt.num,
                                   pt.shape,
                                   pt.size){


  if(!base::isTRUE(display.points)){

    base::return(NULL)

  } else if(base::isTRUE(display.points)){

    # sample data.frame

    group <- "variables"

    if(base::is.character(across)){

      jitter_df <-
        dplyr::group_by(.data = df_shifted, !!rlang::sym(group), !!rlang::sym(across)) %>%
        dplyr::slice_sample(n = pt.num)

    } else {

      jitter_df <-
        dplyr::group_by(.data = df_shifted, !!rlang::sym(group)) %>%
        dplyr::slice_sample(n = pt.num)

    }


    if(base::is.numeric(pt.shape)){

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_df,
          alpha = pt.alpha,
          color = pt.color,
          shape = pt.shape,
          size = pt.size,
          mapping = ggplot2::aes(x = .data[[aes_x]],
                                 y = .data[[aes_y]]),
          height = 0.25, width = 0.25
        )

    } else if(base::is.character(pt.shape)){

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_df,
          alpha = pt.alpha,
          color = pt.color,
          size = pt.size,
          mapping = ggplot2::aes(x = .data[[aes_x]],
                                 y = .data[[aes_y]],
                                 shape = .data[[pt.shape]]),
          height = 0.25, width = 0.25
        )

    }

    base::return(jitter_add_on)

  }

}


#' @title Stat compare means
#'

statistics_tests <- function(df_shifted,
                             across,
                             aes_y,
                             ref.group,
                             test.pairwise,
                             test.groupwise,
                             step.increase,
                             vjust){

  pairwise_add_on <- list()
  groupwise_add_on <- list()

  if(base::is.character(across)){

    # if across refers to character convert to factor
    if(!base::is.factor(df_shifted[[across]])){

      df_shifted[[across]] <-
        base::factor(df_shifted[[across]])

    } else {

      # if across refers to factor drop unused levels for statistical tests
      df_shifted[[across]] <-
        base::droplevels(df_shifted[[across]])

    }

    # pairwise tests
    if(!base::is.null(test.pairwise)){

      check_one_of(
        input = test.pairwise,
        against = pairwise_tests,
        fdb.fn = "stop"
      )

      groups <- base::levels(df_shifted[[across]])

      # check ref.group input
      if(base::is.null(ref.group)){

        ref.group <- groups[1]

      } else {

        check_one_of(
          input = ref.group,
          against = groups,
          fdb.fn = "stop"
        )

      }

      comparison_list <-
        ggpubr_comparison_list(ref.group = ref.group, groups = groups)

      pairwise_add_on <- list(
        ggpubr::stat_compare_means(
          comparisons = comparison_list,
          data = df_shifted,
          mapping = ggplot2::aes(x = .data[[across]], y = .data[[aes_y]]),
          method = test.pairwise,
          step.increase = step.increase
        )
      )

    }

    # groupwise tests
    if(!base::is.null(test.groupwise)){

      check_one_of(
        input = test.groupwise,
        against = groupwise_tests,
        fdb.fn = "stop"
      )

      groupwise_add_on <- list(
        ggpubr::stat_compare_means(
          method = test.groupwise,
          data = df_shifted,
          vjust = vjust,
          mapping = ggplot2::aes(x = .data[[across]], y = .data[[aes_y]])
        )
      )

    }

  }

  statistics_add_on <-
    list(
      pairwise_add_on ,
      groupwise_add_on
    )

  base::return(statistics_add_on)

}

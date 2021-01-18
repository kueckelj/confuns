

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
    jitter_df <-
      dplyr::group_by(.data = df_shifted, variables, !!rlang::sym(across)) %>%
      dplyr::slice_sample(n = pt.num)

    if(base::is.numeric(pt.shape)){

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_df,
          alpha = pt.alpha,
          color = pt.color,
          shape = pt.shape,
          size = pt.size,
          mapping = ggplot2::aes(x = .data[[aes_x]],
                                 y = .data[["values"]]),
          height = 0.25, width = 0.25
        )

    } else if(base::is.null(pt.shape)){

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_df,
          alpha = pt.alpha,
          color = pt.color,
          size = pt.size,
          mapping = ggplot2::aes(x = .data[[aes_x]],
                                 y = .data[["values"]],
                                 shape = .data[[aes_x]]),
          height = 0.25, width = 0.25
        )

    } else if(base::is.character(pt.shape)){

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_df,
          alpha = pt.alpha,
          color = pt.color,
          size = pt.size,
          mapping = ggplot2::aes(x = .data[[across]],
                                 y = .data[["values"]],
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
                             ref.group,
                             test.pairwise,
                             test.groupwise,
                             step.increase,
                             vjust){

  pairwise_add_on <- list()
  groupwise_add_on <- list()

  if(base::is.character(across)){

    # pairwise tests
    if(!base::is.null(test.pairwise)){

      groups <- base::levels(df_shifted[[across]])

      if(base::is.null(ref.group)){

        ref.group <- groups[1]

      }

      check_one_of(
        input = ref.group,
        against = groups,
        fdb.fn = "stop"
      )

      check_one_of(
        input = test.pairwise,
        against = pairwise_tests,
        fdb.fn = "stop"
      )

      comparison_list <-
        ggpubr_comparison_list(ref.group = ref.group, groups = groups)

      pairwise_add_on <- list(
        ggpubr::stat_compare_means(
          comparisons = comparison_list,
          data = df_shifted,
          mapping = ggplot2::aes(x = .data[[across]], y = .data[["values"]]),
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
          mapping = ggplot2::aes(x = .data[[across]], y = .data[["values"]])
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

#' @title Prepare input for \code{ggpubr::stat_compare_means()}

ggpubr_comparison_list <- function(ref.group, groups){

  utils::combn(x = groups, m = 2) %>%
    base::as.data.frame() %>%
    purrr::map(.f = ~ .x) %>%
    purrr::keep(.p = ~ ref.group %in% .x)

}


#' @rdname ggpubr_comparison_list

ggpubr_y_labels <- function(lst, max.value){

  base::seq(max.value*1.1, max.value*1.25, length.out = base::length(lst))

}


ggpubr_y_labels2 <- function(lst, max.values){

  purrr::map(
    .x = max.values,
    .f = ~ ggpubr_y_labels(max.value = .x, lst = comparison_list)
    )

}

variables
max.values <- max_values

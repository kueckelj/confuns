#' @title Prepare input for \code{ggpubr::stat_compare_means()}

ggpubr_comparison_list <- function(ref.group, groups){

  utils::combn(x = groups, m = 2) %>%
    base::as.data.frame() %>%
    purrr::map(.f = ~ .x) %>%
    purrr::keep(.p = ~ ref.group %in% .x)

}


#' @rdname ggpubr_comparison_list

ggpubr_y_labels <- function(input.list, max.value){

  base::seq(max.value*1.1, max.value*1.35, length.out = base::length(input.list))

}

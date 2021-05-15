


#' Title
#'
#' @param old.df Baseline data.frame to which new variables are to be joined.
#' @param new.df Data.frame containing new variables.
#' @param variable.names Character vector of variable names to be joined.
#' @param by Character value. Denotes key variable.
#' @param overwrite Logical value.
#' @param verbose Logical value.
#'
#' @return Joined data.frame.
#' @export
#'
#' @examples
join_safely <- function(old.df, new.df, ref.new.df, variable.names, by,  valid.classes = "any", overwrite = FALSE, verbose = TRUE){

  confuns::check_data_frame(
    df = new.df,
    var.class = purrr::map(variable.names, .f = function(var){ base::return(valid.classes)}) %>% purrr::set_names(variable.names),
    ref = ref.new.df
  )

  old_names <- base::colnames(old.df)

  variable.names_dupl <- variable.names[variable.names %in% old_names]

  if(base::length(variable.names_dupl) >= 1){

    ref_dupl_variables <- glue::glue_collapse(x = variable.names_dupl, sep = "', '", last = "' and '")

    ref1 <- confuns::adapt_reference(variable.names_dupl, "Variable", "Variables")
    ref2 <- confuns::adapt_reference(variable.names_dupl, "exists", "exist")

    if(!base::isTRUE(overwrite)){

      msg <- glue::glue("{ref1} '{ref_dupl_variables}' already {ref2}. Set argument 'overwrite' to TRUE in order to allow overwriting.")

      confuns::give_feedback(msg = msg, fdb.fn = "stop")

    } else {

      ref2 <- confuns::adapt_reference(variable.names_dupl, "is", "are")

      msg <- glue::glue("{ref1} '{ref_dupl_variables}' {ref2} being overwritten.")

      confuns::give_feedback(msg = msg, verbose = verbose)

      old.df <- dplyr::select(old.df, -dplyr::all_of(variable.names_dupl))

    }

  }

  new.df <- dplyr::select(new.df, dplyr::all_of(c(variable.names, by)))

  joined_df <- dplyr::left_join(x = old.df, y = new.df, by = by)

  base::return(joined_df)

}

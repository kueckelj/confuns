
#' @title Assemble list of objects
#' @export
assemble_list <- function(obj_names, env){

  out_list <- list()

  for(name in obj_names){

    out_list[[name]] <-
      base::parse(text = name) %>%
      base::eval(envir = env)

  }

  return(out_list)

}

#' @title Adjust ggplot parameters
#' @export
adjust_ggplot_params <- function(params){

  # get environment of plotting function
  cenv <- rlang::caller_env()

  # get plotting function
  cfn <- rlang::caller_fn()

  # get names of the arguments of the plotting function
  args_all_names <- rlang::fn_fmls_names(fn = cfn)

  # assemble list that contains the input of the arguments
  # of the plotting function
  args_all <- assemble_list(obj_names = args_all_names, env = cenv)

  # vector of names of the arguments that assign a variable
  # to an aesthetic that could otherwise be defined as a set parameter
  aes_names <-
    base::names(params) %>%
    stringr::str_c(., "by", sep = "_")

  # list of argument input of only the aesthetic assignment
  # arguments
  aes_input <- args_all[args_all_names %in% aes_names]

  # figure out for which aesthetic a variable was assigned
  # then extract the names of the corresponding parameter argument
  assigned_aes <-
    purrr::discard(.x = aes_input, .p = base::is.null) %>%
    base::names() %>%
    stringr::str_remove(pattern = "_by")

  # remove the parameter arguments for which a variable has
  # been assigned
  params_out <- params[!base::names(params) %in% assigned_aes]

  return(params_out)

}

#' Set Up Project Directories with Optional Python Environment
#'
#' Initializes a new project directory structure in the current working directory.
#' Verifies if the working directory's name matches the provided project name.
#' If the names do not match, the execution is stopped with an error message.
#' Optionally sets up a Python virtual environment and installs specified packages.
#'
#' @param project_name The name of the project which should match the end of the
#'        current working directory's path.
#' @param python Logical, if `TRUE`, sets up a Python virtual environment and
#'        script directories. Defaults to `TRUE`.
#' @param python_version The version of Python to use for the virtual environment.
#'        If `NULL`, uses the system's default Python version.
#' @param python_pkgs A character vector of additional Python packages to install
#'        in the virtual environment, beyond the defaults (matplotlib, numpy, pandas).
#'
#' @return NULL This function is used for its side effect of creating directories
#'         and optionally a Python virtual environment. It does not return any value.
#'
#' @details
#' The directories created are as follows:
#' \itemize{
#'   \item{`data`: Main directory for all data-related storage.
#'     \itemize{
#'       \item{`raw`: For storing raw, unmodified data.}
#'       \item{`proc`: For storing processed data, which is derived from raw data.}
#'       \item{`out`: For storing output data, such as results or final datasets.}
#'     }
#'   }
#'   \item{`docs`: For documentation files related to the project.}
#'   \item{`figures`: For figures (.ai, .svg, ...).}
#'   \item{`plots`: For storing graphical representations generated from the data.}
#'   \item{`R_markdown`: For storing R Markdown documents used for reports or presentations.}
#'   \item{`R_scripts`: For storing R scripts that are part of the project.}
#'   \item{`py_scripts` and `py_venv`: Created only if `python` is `TRUE`. The `py_venv` directory
#'         contains a Python virtual environment named `venv_main` with specified Python packages installed.}
#' }
#'
#' @examples
#' # Set up a project named "MyProject" with a Python virtual environment
#' set_up_project("MyProject", python = TRUE, python_version = "3.8", python_pkgs = c("scipy", "seaborn"))
#'
#' # Set up a project without a Python environment
#' set_up_project("MyProject", python = FALSE)
#'
#' @export
#'
set_up_project <- function(project_name, python = TRUE, python_version = NULL, python_pkgs = NULL){

  wd <- getwd()

  if(!stringr::str_detect(wd, pattern = stringr::str_c(project_name, "$"))){

    stop(glue::glue("Project name {project_name} and working directory {wd} do not match."))

  }

  # create folders
  dir.create(path = "data")
  for(d in c("raw", "proc", "out")){
    dir.create(path = file.path("data", d))
  }

  dir.create("docs")
  dir.create("figures")
  dir.create("plots")

  if(isTRUE(python)){
    dir.create("py_scripts")
    dir.create("py_venv")

    pkgs <-
      c("matplotlib", "numpy", "pandas", python_pkgs) %>%
      base::unique()

    reticulate::virtualenv_create(
      envname = file.path(wd, "py_venv", "venv_main"),
      packages = pkgs,
      version = python_version
    )
  }

  dir.create("R_markdown")
  dir.create("R_scripts")

  message("Project set up.")
}




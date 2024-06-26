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
#' proj_setup("MyProject", python = TRUE, python_version = "3.8", python_pkgs = c("scipy", "seaborn"))
#'
#' # Set up a project without a Python environment
#' proj_setup("MyProject", python = FALSE)
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
proj_setup <- function(project_name, python = TRUE, python_version = NULL, python_pkgs = NULL){

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

#' Install Python Packages into a Virtual Environment
#'
#' This function installs the specified Python packages into the given virtual environment.
#' It attempts to install each package and catches any errors during the installation process.
#'
#' @param pkgs A vector of package names to install.
#' @param venv The name of the virtual environment where packages will be installed.
#'             Default is 'venv_main'.
#' @param ... Additional arguments passed to `virtualenv_install`.
#'
#' @return NULL This function attempts to install packages and handles errors silently,
#'         returning no value.
#'
#' @examples
#' proj_py_install_pkgs(c("numpy", "pandas"), "venv_main")
#'
#' @export
proj_py_install_pkgs <- function(pkgs, venv = "venv_main", ...){
  for(pkg in pkgs){
    tryCatch({
      reticulate::virtualenv_install(
        envname = file.path(getwd(), "py_venv", venv),
        packages = pkg,
        ...
      )
    })
  }
}





#' Check if a Python Package is Installed
#'
#' Checks if a specified package, and optionally a specific version, is installed in the active
#' Python environment. The function retrieves a list of installed packages and searches for the specified package.
#'
#' @param pkg The name of the package to check.
#' @param version Optional specific version to check for. If provided, checks for the package
#'                and the specific version.
#'
#' @return Logical indicating whether the specified package (and version) is installed.
#'
#' @examples
#' proj_py_pkg_installed("numpy")
#' proj_py_pkg_installed("pandas", "1.1.5")
#'
#' @export
proj_py_pkg_installed <- function(pkg, version = NULL){

  pp <- proj_py_pkgs()

  if(is.character(version)){

    pp <- dplyr::filter(pp, version %in% {{version}})

  }

  pkg %in% pp$pkg

}


#' List Installed Python Packages
#'
#' Retrieves a list of installed Python packages in the active virtual environment using `pip list`.
#' The output is returned as a tibble with columns for package names and versions.
#'
#' @return A tibble with two columns: 'pkg' for package names and 'version' for package versions.
#'
#' @examples
#' proj_py_pkgs()
#'
#' @export
proj_py_pkgs <- function(){
  python_code <- "import subprocess
result = subprocess.run(['pip', 'list'], capture_output=True, text=True).stdout
result"

  prel_out <-
    py_run_string(python_code)$result %>%
    stringr::str_replace_all("\\n", "|") %>%
    strsplit(split = "\\|")

  prel_out <- prel_out[[1]][-c(1:4)]

  out <-
    tibble::tibble(x = prel_out) %>%
    dplyr::transmute(
      pkg = stringr::str_extract(x, pattern = "^[A-Za-z0-9]*"),
      version = stringr::str_extract(x, pattern = "(\\d|\\.)*$")
    )

  return(out)
}


#' Use a Specific Python Virtual Environment
#'
#' This function sets the specified Python virtual environment as active for the current R session.
#' It checks if the virtual environment exists at the specified path; if not, it throws an error.
#'
#' @param venv The name of the virtual environment to activate. Default is 'venv_main'.
#'
#' @return NULL This function is used for its side effect of activating a Python virtual
#'         environment and does not return any value.
#'
#' @examples
#' proj_py_use_venv("venv_main")
#'
#' @export
proj_py_use_venv <- function(venv = "venv_main"){
  venv_path <- file.path(getwd(), "py_venv", venv)

  if(!dir.exists(venv_path)){
    stop(glue::glue("Virtual environment {venv} not found."))
  }

  reticulate::use_virtualenv(venv_path, required = TRUE)
}



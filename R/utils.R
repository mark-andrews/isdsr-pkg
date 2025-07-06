#' Install the packages listed in isdsr's *Suggests*
#'
#' The core functionality of isdsr works after you install the package
#' itself and any packages in its *Imports* field.
#' However, throughout the accompanying textbook, we will use some packages
#' that are not *Imports* dependencies.
#' These are not required for isdsr code to work, but are used or are recommended
#' in the book.
#' This function installs all these extra packages.
#'
#' `install_extras()` checks which of those suggested packages are not
#' currently installed and installs just the missing ones.  Dependencies of
#' each extra package (their own *Depends*, *Imports*, and *LinkingTo*)
#' are handled automatically by \code{\link[utils]{install.packages}}.
#'
#' The function never installs packages silently: it prints the list of names
#' it is about to install, or a friendly message if everything is already
#' present.
#'
#' @return Called for its side effect of installing packages; invisibly returns
#'   the character vector of packages that were requested for installation.
#'
#' @examples
#' \dontrun{
#' # After installing isdsr:
#' isdsr::install_extras()
#' }
#'
#' @seealso \code{\link[utils]{install.packages}},
#'   \code{\link{installed.packages}}
#' @export
install_extras <- function() {
  extras <- setdiff(
    tools::package_dependencies(
      "isdsr",
      which     = "Suggests", # look only at Suggests
      recursive = FALSE # do not chase their own Suggests
    )$isdsr,
    rownames(installed.packages())
  )

  if (length(extras)) {
    message("Installing: ", paste(extras, collapse = ", "))
    install.packages(extras)
  } else {
    message("All suggested packages are already installed.")
  }

  invisible(extras)
}


#' Copy a file from *inst/extdata* into the working directory
#'
#' This helper lets users experiment with the small demo files that ship
#' with **isdsr** (or any other package) without asking them to locate the
#' installation directory.  It looks for `filename` inside the package’s
#' *inst/extdata* folder, checks a few safety conditions, then copies the
#' file to `getwd()`.
#'
#' The call aborts with an informative error when
#' * the requested file is not present in *extdata*, or
#' * a file of the same name already exists in the working directory and
#'   `overwrite = FALSE`.
#'
#' @param filename A single character string giving the exact path to the
#'   resource within *inst/extdata*.  Sub-directories are allowed, e.g.
#'   `"csv/demo.csv"` or `"images/logo.png"`.
#' @param overwrite Should an existing target file be replaced?  Defaults to
#'   `FALSE`, in which case the function stops rather than overwrite data.
#'
#' @return Invisibly returns a logical scalar: `TRUE` when the copy succeeded
#'   and `FALSE` otherwise.  The function is called for its side effect of
#'   writing a file; on success it prints a short message, on failure it raises
#'   an error or warning.
#'
#' @section See also:
#' * [system.file()] for locating files inside installed packages.
#' * [base::file.copy()] which does the low-level transfer.
#'
#' @examples
#' ## copy the example CSV that ships with the package -----------------
#' tmp <- tempdir()
#' old <- setwd(tmp)
#'
#' copy_extdata("survey.csv") # writes survey.csv into tempdir()
#' readr::read_csv("survey.csv") # inspect its contents
#'
#' unlink("survey.csv") # clean up
#' setwd(old)
#'
#' ## overwrite protection --------------------------------------------
#' copy_extdata("survey.csv") # first copy
#' copy_extdata("survey.csv", overwrite = TRUE) # replace it silently
#'
#' @export
copy_extdata <- function(filename, overwrite = FALSE) {
  # Get the current package name automatically
  pkg_name <- utils::packageName()

  # Find the source file in extdata
  source_file <- system.file("extdata", filename, package = pkg_name)

  # Check if file exists
  if (source_file == "" || !file.exists(source_file)) {
    stop("File '", filename, "' not found in package extdata")
  }

  # Get destination path
  dest_file <- file.path(getwd(), filename)

  # Check if destination exists and overwrite is FALSE
  if (file.exists(dest_file) && !overwrite) {
    stop("File '", filename, "' already exists. Use overwrite = TRUE to replace it.")
  }

  # Copy to working directory
  success <- file.copy(source_file, dest_file, overwrite = overwrite)

  if (success) {
    message("File '", filename, "' copied to: ", dest_file)
  } else {
    warning("Failed to copy file '", filename, "'")
  }

  return(invisible(success))
}

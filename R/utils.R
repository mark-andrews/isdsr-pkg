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

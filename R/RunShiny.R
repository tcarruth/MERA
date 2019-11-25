#' Run a Shiny Application
#'
#' \code{Shiny} runs one of the Shiny Applications that are included in the package
#'
#' @param app The name of the Shiny application to run.
#' @references Modified from Deal Attali's code: \url{http://deanattali.com/2015/04/21/r-package-shiny-app/}
#' @importFrom utils install.packages installed.packages
#' @export
Shiny <- function(app) {
  temp <- try(class(app), silent=TRUE)

  if (class(temp) == "try-error") app <- deparse(substitute(app))
  Apps <- list.files(system.file("shiny_apps", package = "MERA"))
  validAppMsg <- paste0("Valid examples are:\n '", paste(Apps, collapse = "', '"), "'")
  if (missing(app) || !nzchar(app) || !app %in% Apps) {
    stop(
      'Please run `Shiny()` with a valid Shiny app',
      validAppMsg,
      call. = FALSE)
  }
  appDir <- system.file("shiny_apps", app, package = "MERA")
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}


PKGENVIR <- new.env(parent=emptyenv()) 

#' Run the MERA Shiny App
#'
#' @param skin Character. Skin to use to present results
#'
#' @return Nothing. Opens the MERA app in a web browser
#' @export
MERA <- function(skin="Generic") {
  if (class(skin) !="character") stop("skin must be character")
  skins <- list.files(file.path(system.file(package = 'MERA'), "shiny_apps/MERA/Source/Skins"))
  skins <- tools::file_path_sans_ext(skins)
  if (!skin %in% skins) stop(skin , ' is not a valid skin. Options are: ', paste(skins, collapse=", "))
  
  PKGENVIR$skin <- skin
  appDir <- system.file("shiny_apps/MERA", package = "MERA")
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}
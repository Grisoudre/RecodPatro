#' @export
Patro <- function() {
  appDir <- system.file("AppPatro", "myapp", package = "RecodPatro")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `RecodPatro`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

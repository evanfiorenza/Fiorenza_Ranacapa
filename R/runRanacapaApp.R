# ' Run the shiny app!
# ' @export
runRanacapaApp <- function() {
  port <- Sys.getenv("RANACAPA_PORT")
  port <- as.numeric(port)
  
  appDir <- system.file("explore-anacapa-output", package = "ranacapa")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ranacapa`.", call. = FALSE)
  }
  if (is.null(port) || is.na(port) || port == "")
  {
    shiny::runApp(appDir, display.mode = "normal")

  }
  else
  {
    shiny::runApp(appDir, port = port, display.mode = "normal")
  }
}
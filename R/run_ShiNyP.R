#' Launch the ShiNyP Shiny app
#'
#' @export
run_ShiNyP = function() {
  shiny::shinyApp(ui = ShiNyP_ui(), server = ShiNyP_server)
}

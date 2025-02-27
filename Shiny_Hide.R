cat(file = stderr(), "Shiny_Hide.R", "\n")

hide_enable <- function(session, input, output, params) {
  cat(file = stderr(), "Function - hide_enable...", "\n")

  observe({
    if (params$plate_number > 1) {
      shinyjs::show("norm_select")
      shinyjs::show("excel_samples_norm")
      updateRadioButtons(session, "norm_option", choices = list("Raw" = 1, "Normalized" = 2), selected = 1)
    }else{
      shinyjs::hide("norm_select")
      updateRadioButtons(session, "norm_option", choices = list("Raw" = 1), selected = 1)
      params$norm_select <<- "None"
      shinyjs::hide("excel_samples_norm")
      updateCheckboxInput(session, "excel_samples_norm", value = FALSE)
    }
  })
  

  cat(file = stderr(), "Function - hide_enable...end", "\n")
}
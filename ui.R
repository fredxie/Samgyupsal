library(shiny)


shinyUI(
  fluidPage(
    headerPanel("Period Prevalence Tool"),
  
    sidebarPanel(
      h3("Please input required parameters"),
      dateRangeInput('dateRange',
                     label = 'Date range input: dd-mm-yyyy',
                     format = "dd/mm/yyyy",
                     start = Sys.Date(), 
                     end = Sys.Date(),
                     separator = " - "
      ),
      textInput("icd", "inclusion_icd9", value = ""),
      textInput("ndc", "inclusion_ndc", value = ""),
      textInput("proc", "inclusion_proc", value = ""),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      h4("Raw Prevalence: "),
      verbatimTextOutput("rawPrevalence"),
      h4("Adjusted Prevalence: "),
      verbatimTextOutput("adjustedPrevalence")
    )
  )
)
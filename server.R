library(shiny)

source("prevalence.R")

shinyServer(
  function(input, output) {
    calculatedPrevalence <- NULL
    
    IsCalculationDone <- function() {
      return (is.null(calculatedPrevalence));
    }
    
    DoCalculation <- function(dateLow, dateHigh, icd, ndc, proc) {
      calculatedPrevalence <- calculatePrevalence(dateLow, dateHigh, icd, ndc, proc);
    }
    
    DoCalculationIfNotDone <- function(dateLow, dateHigh, icd, ndc, proc) {
      if (!IsCalculationDone()) {
        DoCalculation();
      }
    }
    
    output$value <- renderText({
      as.character(input$dateRange[1])
    })
    
    GetRawPrevalence <- eventReactive(input$calculate, {
      DoCalculationIfNotDone(
        as.character(input$dateRange[1]), 
        as.character(input$dateRange[2]), 
        input$icd, 
        input$ndc, 
        input$proc);
      return (calculatedPrevalence[1]);
    })
    
    GetAdjustedPrevalence <- eventReactive(input$calculate, {
      DoCalculationIfNotDone(
        as.character(input$dateRange[1]), 
        as.character(input$dateRange[2]), 
        input$icd, 
        input$ndc, 
        input$proc);
      return (calculatedPrevalence[2]);
    })
    
    output$rawPrevalence <- renderText({GetRawPrevalence()})
    output$adjustedPrevalence <- renderText({GetAdjustedPrevalence()})
  }
)
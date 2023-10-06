#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Caminalcule and Pollard Simulation"),
  mainPanel(
    numericInput("camiPop", "Initial Caminalcule Population:", 5, 1, 100),
    numericInput("pollPop", "Initial Pollard Population:", 5, 1, 75),
    numericInput("camiPopMultiply", "Caminalcule Population Multiplier:", 3, 1, 10),
    numericInput("numGenerations", "Number of Generations:", 1, 1, 100),
    actionButton("simulateBtn", "Simulate"),
    DTOutput("simulationOutput")
  )
)

server <- function(input, output, session) {
  resultData <- reactiveVal(character(0))
  
  observeEvent(input$simulateBtn, {
    camiPop <- reactiveVal(input$camiPop)
    pollPop <- reactiveVal(input$pollPop)
    
    resultsData <- c(sprintf("Gen: 0 Starting Caminalcule Pop: %d Starting Pollard Pop: %d", camiPop(), pollPop()))
    
    biome <- matrix(FALSE, nrow = 10, ncol = 10)
    chosenXY <- matrix(FALSE, nrow = 10, ncol = 10)
    
    for (index in 1:input$numGenerations) {
      found <- 0
      
      for (cami in 1:camiPop()) {
        x <- sample(1:10, 1)
        y <- sample(1:10, 1)
        
        if (!biome[x, y]) {
          biome[x, y] <- TRUE
        } else {
          while (biome[x, y]) {
            x <- sample(1:10, 1)
            y <- sample(1:10, 1)
          }
          biome[x, y] <- TRUE
        }
      }
      
      for (polly in 1:pollPop()) {
        x <- sample(1:10, 1)
        y <- sample(1:10, 1)
        
        while (chosenXY[x, y]) {
          x <- sample(1:10, 1)
          y <- sample(1:10, 1)
        }
        
        if (biome[x, y] && !chosenXY[x, y]) {
          camiPop(camiPop() - 1)
          found <- found + 1
          chosenXY[x, y] <- TRUE
        } else {
          chosenXY[x, y] <- TRUE
        }
      }
      
      camiPop(camiPop() * input$camiPopMultiply)
      pollPop(pollPop() + found - floor(pollPop() / 3) + 1)
      
      result_string <- sprintf("Gen: %d Starting Caminalcule Pop: %d Ending Pollard Pop: %d || found: %d", index, camiPop(), pollPop(), found)
      resultsData <- c(resultsData(), result_string)  # Call resultsData() as a function to update the reactive value
    }
    
    if (length(resultsData()) < input$numGenerations + 1) {
      resultsData <- c(resultsData(), "Simulation ended after reaching at least thirty generations.")
    }
    
    resultData(resultsData())  # Call resultData() as a function to update the reactive value
  })
  
  output$simulationOutput <- renderDT({
    DT::datatable(data.frame(Results = resultData()), options = list(pageLength = 20))
  })
}

shinyApp(ui, server)

















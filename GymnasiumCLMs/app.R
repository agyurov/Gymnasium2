#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ordinal)
library(rsconnect)
library(shinyjs)
library(xtable)
load(".RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("26-05-2017"),
  
  # Sidebar with a slider input for number of bins 
  # Show a plot of the generated distribution
  mainPanel(
    selectInput(inputId = "response", label = "Response variable",
                choices = names(dfu)[-1]),
    uiOutput("predictors"),
    checkboxInput("interactions", "Second order interactions", value = FALSE, width = NULL),
    selectInput(inputId = "link", label = "Link functions",
                choices = c("logit","probit","cloglog","cauchit")),
    submitButton("Fit model", icon("play")),
    textOutput("message"),
    uiOutput("model"),
    textOutput("predaccu"),
    uiOutput("prediction")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  message = reactiveValues()
  mdl = reactiveValues()

  output$predictors = renderUI({
    predopts = names(dfu)[!names(dfu) %in% input$response]
    selectInput(inputId = "predictors", label = "Predictor variables",
                choices = predopts,
                multiple = TRUE)
  })
  output$model = renderTable({
    if(is.null(input$predictors)){
      return()
    }
    if(!input$interactions){
      model = model.listZ(pred = dfu[input$predictors],
                          resp = dfu[input$response],
                          link = input$link,
                          exclude.warnings = F)
    }
    if(input$interactions){
      model = model.listZ2(pred = dfu[input$predictors],
                           resp = dfu[input$response],
                           link = input$link,
                           exclude.warnings = F)
    }
    message$message = model[[1]]$message
    mdl$model = model[[1]]
    if(is.null(model[[1]]$message)){
      message$message = ""
    }
    if(length(model) != 0){
      x = summary(model[[1]])
      x = x$coefficients[-(1:length(x$alpha)),]
      x
    }
    
  },rownames=T)
  
  output$message = renderText({
    message$message
  })
  
  output$prediction = renderTable({
    if(length(mdl$model) <1) return()
    xtable(class.pred(mdl$model)$table)
  })
  output$predaccu = renderText({
    if(length(mdl$model) <1) return()
    "Prediction accuracy"
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


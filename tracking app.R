library(shiny)


fieldsAll <- c("date", "posted_blog", "time_spent", "category")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}


humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

fieldsMandatory <- c("time_spent", "category")

shinyApp(
  
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Time Tracker"),
    
    div(
      id = "form",
      
      textInput("date", "Date:"),
      checkboxInput("posted_blog", "Blog Posted", FALSE),
      sliderInput("time_spent", "In minutes", 0, 60, 2, ticks = FALSE),
      selectInput("category", "category of work",
                  c("",  "Get REAL", "Blog", "Meditation")),
      actionButton("submit", "Submit", class = "btn-primary")
      
      
    ), #id
    
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
    )
    
  ),#ui
  
  
  server = function(input, output, session) {
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })  
    
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })
    
    saveData <- function(data) {
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(data))
      
      write.csv(x = data, file = file.path(responsesDir, fileName),
                row.names = FALSE, quote = TRUE)
    }
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })    
    
    
  }
  
  
  
) #shinyapp


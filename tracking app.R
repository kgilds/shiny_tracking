library(shiny)
library(DT)


fieldsAll <- c("date", "posted_blog", "time_spent", "category")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}


humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

fieldsMandatory <- c("time_spent", "category")


loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- dplyr::bind_rows(data)
  data <- dplyr::group_by(data,category)
  data <- dplyr::summarise(data, avg_time = mean(time_spent))
  data
}

options(DT.options = list(pageLength = 5, language = list(search = 'Filter:')))

shinyApp(
  
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Time Tracker"),
    
    
      DT::dataTableOutput("responsesTable"),
   
    
    div(
      id = "form",
      
      dateInput("date", "Date:"),
      checkboxInput("posted_blog", "Blog Posted", FALSE),
      numericInput("time_spent", "In minutes",0),
      selectInput("category", "category of work",
                  c("",  "Get REAL", "Blog", "Meditation", "R4DS", "Coding")),
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
  
    
    output$responsesTable <- DT::renderDataTable(
      loadData(),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )   
    
  }
  
  
  
) #shinyapp


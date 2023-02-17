
######## gutenbergR method UI & server ############

# For documentation:
# https://docs.ropensci.org/gutenbergr/

gutenbergRUI <- function(id, label = "Enter the Project Gutenberg text ID:"){
  ns <- NS(id)
  tagList(
    
    h2(textOutput(ns("title1"))),
    p(textOutput(ns("subtitle1"))),
    
    # Textbox to enter gutenbergR ID
    textInput(inputId = "gutenbergRID", 
              label = label,
              value = " "),
    
    # Sumbit button to submit url
    actionButton(inputId = "submitID", label = "Submit"),
    
    # Output of what user has entered
    hr(), 
    # p(ns("urlInput")),
    # HTML("<em style='color:blue;'>  </em>"),
    
    h2(textOutput(ns("title2"))),
    em(textOutput(ns("subtitle2"))),
    # tableOutput(ns("files")),
    # tableOutput(ns("contents")),
    
    # Source
    hr(),
    h5("About Project Gutenberg"),
    
  )
}

gutenbergRServer <- function(id, rv = rv){
  moduleServer(
    id, 
    function(input, output, session, rv){
      output$title1 <- renderText({
        paste("Project Gutenberg")
      })
      output$subtitle1 <- renderText({
        paste("Access a public domain of texts through Project Gutenberg to use for your analysis.")
      })
      
      title2 <- reactive({
        "Text Summary"
      })
      output$title2 <- renderText({
        paste(title2())
      })
      
      subtitle2 <- reactive({
        if(is.null(input$tweetInput)){
          "No text uploaded."
        } else {
          "...Below is a summary of text..."
        }
      })
      output$subtitle2 <- renderText({
        paste(subtitle2())
      })
      
    }
  )
}
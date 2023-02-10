######## rvest method UI & server ############
rvestUI <- function(id, label = "Enter a webpage link:"){
  ns <- NS(id)
  tagList(
    
    h1(textOutput(ns("title1"))),
    p(textOutput(ns("subtitle1"))),
    
    # Widget for user to input URL
    textInput(inputId = "urlInput", 
              label = label,
              value = " "),
    # Sumbit button to submit url
    actionButton(inputId = "submitURL", label = "Submit URL"),
    
    p(textOutput(ns("url_print"))),
    
    hr(), 
    # h2("Text Summary"),
    # em("Below is a summary of what has been uploaded..."),
  
    # text summary title
    h2(textOutput(ns("title2"))),
    em(textOutput(ns("subtitle2"))),
    
    # tableOutput(ns("files")),
    # tableOutput(ns("contents"))
    
    p(textOutput(ns("urlPrint"))),
    # HTML("<em style='color:blue;'>  </em>"),
    
    
    hr(),
    em(textOutput(ns("source")))
  )
}

rvestServer <- function(id, rv = rv){
  moduleServer(
    id, 
    function(input, output, session, rv){
      
      output$title1 <- renderText({
        paste("Web Scraping")
      })
      output$subtitle1 <- renderText({
        paste("Scrape web pages for data with rvest. Provide a link to the webpage you wish to harvest data from, and ....")
      })
      
      url <- reactive({
        input$urlInput
      })
      
      # output$urlPrint <- renderText({
      #   paste(url())
      # })
      
      # Display uploaded files header when files uploaded
      title2 <- reactive({
        if(is.null(url())){
          return(NULL)
        }
        "URL Details"
      })
      # creating uploaded files header
      output$title2 <- renderText({
        paste(title2())
      })

      subtitle2 <- reactive({
        if(is.null(input$urlInput)){
          "No URL entered"
        } else {
          "...Below is a summary of what has been entered..."
        }
      })
      output$subtitle2 <- renderText({
        paste(subtitle2())
      })
      
      # observeEvent(input$submitURL, {
      #   
      # })

      # Output for text entry
      # output$contents <- renderTable({
      #   files <- input$fileUpload
      #   ext <- tools::file_ext(files$datapath)
      #   req(files) 
      #   validate(need(ext == "txt", "Please upload only .txt files, or choose a different upload method."))
      #   if(is.null(files)){
      #     return(NULL)
      #   }
      #   read_file(files$datapath)
      # }, 
      # striped = T, 
      # bordered = T,
      # colnames = F # could change so col name says Files Uploaded
      #)
      
      output$source <- renderText({
        paste("Source: Wickham H (2022). rvest: Easily Harvest (Scrape) Web Pages. https://rvest.tidyverse.org/, https://github.com/tidyverse/rvest.")
      })
    }
  )
}
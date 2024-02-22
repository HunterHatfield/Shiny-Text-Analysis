
# Outer UI for text selector page

# Text selector UI
textSelectorUI <- function(id, label = "Choose file(s):"){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        h1("01 | Import"), 
        p("Import files containing text for analysis in the form of .txt, .csv, and/or .tsv files."),
        p("Select the file type you would like to import (.txt or .csv) below,  then use the file browser to choose your file(s). Use the Submit button to create a dataset from the imported files once you are ready."),
        hr(),

        # Add ns() around method & paste0 stln used in reporting
        # later on
        h3("Primary data"),
        radioButtons(inputId = "method",
                     label = "Choose a file type to import:",
                     choiceNames = list(
                       'Text file import', '.csv/.tsv file upload' # ,
                       # 'Project Gutenberg'
                     ),
                     choiceValues = list(
                       "upload", "csv" #, "gutenbergR"
                       )
        ),
        hr(),
        
        # Secondary uploads UI
        secondaryUI(ns("secondary")),
        
        
          
      ), # end side bar panel

      mainPanel(

        fluidRow(
          
          box(title = NULL, 
            status = "primary", 
            solidHeader = T,
            collapsible = T,
            width = 12,
            
           # inner UI inserted depending on selecting upload method 
            conditionalPanel(
              condition = "input.method == 'upload'",
              uploadUI(ns("upload"))
            ),
            conditionalPanel(
              condition = "input.method == 'csv'", 
              csvUI(ns("csv"))
            ),
            conditionalPanel(
              condition="input.method=='gutenbergR'", 
              gutenbergRUI(ns("gutenbergR"))
            )

          ), # end box
          
          box(title = NULL, 
              status = "success", 
              solidHeader = T,
              collapsible = T,
              width = 12,
              
              h3("Imported data"),
              em(textOutput(ns("num_submitted_subtitle"))),
              
              p(),
              wellPanel(
                em(textOutput(ns("submitted_subtitle"))),
                DT::dataTableOutput(ns("content_display")), 
                hr(class = 'hr-blank'),
                h4(textOutput(ns("selected_title"))),
                em(textOutput(ns("selectedRowContent")))
              ),
              
          )
          
        ) # end fluid row
      ) # end main panel
  ) # end side bar layout
  ) # end taglist
}


############## SERVER LOGIC ##########################################

textSelectorServer <- function(id, rv = NULL, session = session){
  moduleServer(id, function(input, output, session){ 

    # Inner modules for file uploads, csv uploads and secondary uploads
    uploadServerAttempt <- try(uploadServer("upload", 
                                            rv = rv, parent = session))
    if("try-error" %in% class(uploadServerAttempt)){
      shinyalert(
        title = "The file upload server call returned a fatal error.",
        text = "Reload the app and ensure all files sourced properly.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    
    csvServerAttempt <- try(csvServer("csv", rv = rv))
    if("try-error" %in% class(csvServerAttempt)){
      shinyalert(
        title = "The CSV upload server call returned a fatal error.",
         text = "Reload the app and ensure all files sourced properly.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    }
    
    secondaryServerAttempt <- try(secondaryServer("secondary", rv = rv))
    if("try-error" %in% class(secondaryServerAttempt)){
      shinyalert(
        title = "The secondary upload server call returned a fatal error.",
        text = "Reload the app and ensure all files sourced properly.",
        size = "xs", 
        closeOnEsc = TRUE, closeOnClickOutside = TRUE,
        html = FALSE, type = "info",
        showConfirmButton = TRUE, showCancelButton = FALSE,
        confirmButtonText = "Dismiss",
        confirmButtonCol = "#4169E1",
        timer = 0, imageUrl = "", animation = TRUE
      )
      return()
    } 
      
    # old gutenberg server
    # gutenbergRServer("gutenbergR", rv = rv)
      

      #### Subtitles ####
      output$selected_title <- renderText({
        if(is.null(input$content_display_rows_selected)){
          return(NULL)
        } 
        "Document contents preview:"
      })
      
      output$submitted_subtitle <- renderText({
        validate(need(rv$content_primary$data, "No files found."))
        return(NULL)
      })
      
      # Subtitle for how many files were uploaded 
      output$num_submitted_subtitle <- renderText({
        if(!is.null(nrow(rv$content_primary$data)) ){
          return(paste(c("You have uploaded: ", nrow(rv$content_primary$data),
                         "text document(s)."), sep = ","))
        }
        return(NULL)
      })
      
      #### Table output for the resulting content tibble ####
      
      # JS callback function to display grey NA values instead of blank spaces
      rowCallback <- c(
        "function(row, data){",
        "  for(var i=0; i<data.length; i++){",
        "    if(data[i] === null){",
        "      $('td:eq('+i+')', row).html('NA')",
        "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
        "    }",
        "  }",
        "}"  
      )
      
      output$content_display <- DT::renderDataTable(
        rv$content_primary$data,
        options = list(
          paging = TRUE, pageLength = 5,
          scrollX = TRUE, scrollY = TRUE,
          dom = 'frtip',
          rowCallback = JS(rowCallback),
          columnDefs =
            list(#list(targets = '_all', className = 'dt-left'),
                 list(targets = 1,
                      render = JS("function(data, type, row, meta) {",
                                  "return type === 'display' && data.length > 100 ?",
                                  "'<span title=\"' + data + '\">' + data
                          .substr(0, 100) + '...</span>' : data;","}"
                        )
                      )
                )
        ),
        selection = 'single',
        rownames = FALSE
      )
      

      #### Row selection viewer ####
      # If no row selected, nothing returned.Else the content of the row is displayed
      selectedRows <- reactive({
        if(length(input$content_display_rows_selected) == 0){
          return(NULL)
        }
        rv$content_primary$data %>%
          filter(row_number() == input$content_display_rows_selected)
      })
      
      # Creating text for the number of rows
      output$selectedRowContent <- renderText({
        if(length(input$content_display_rows_selected) == 0){
          return(NULL)
        }
        
        paste0(substr(selectedRows()$Contents, start = 1, stop = 1000), "...")
      })
      
    } # end module function
  )
}
  
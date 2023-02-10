
#### Module for R Shiny apps which takes a tibble and defines functionalities to upload a second 
# .csv or .tsv, and adjoin the secondary file to the first based on a selected column. 

secondaryUI <- function(id){
  ns <- NS(id)
  tagList(
    
    # Secondary uploads UI. renderUI() function is used to renderUI based on whether
    # or not a user has completed primary uploads. This means secondary upload functionality
    # is only available once primary upload is complete. 
    # Render UI for secondary upload once !is.null(rv$content) && nrow(rv$content) > 0
    uiOutput(ns("secondary_snippet")),
    
    DT::dataTableOutput(ns("file_tibble_second"))
    
  )
} # end secondary UI



###### SERVER ######
# Takes in id and a list of reactive values which is assumed to contain a tibble of the file contents called 'content' and thus accessed by the function as rv$content.
# Also takes in a list of reactive values (can be empty)
secondaryServer <- function(id, rv = rv, primary = rv$content, parent = session){
  moduleServer(id, 
    function(input, output, session = parent){
      # req(primary) # require the primary file to exist
      
      #### Creating ui for secondary uploads ####
      output$secondary_snippet <- renderUI({
        ns <- NS(id)
        true_UI <- tagList(
          
          h3("Secondary upload"),
          p("Upload a secondary .csv/.tsv file to adjoin."),
          
          shinyFilesButton(ns("csvtsvUpload2"),
                           label = "Select a .csv/.tsv file",
                           title = "Choose a .csv or .tsv file...",
                           multiple = FALSE,
                           class = "btn-primary")
        )
        false_UI <- tagList(
          em("Note: to adjoin a secondary table, submit your primary upload first.",
             style = "color: grey")
        )
        
        # if there is a primary upload present, render secondary UI
        # otherwise UI is rendered to let user know to complete primary upload
        if(!is.null(rv$content) && nrow(rv$content) > 0){
          return(true_UI)
        } 
        return(false_UI)
        
      }) # end render secondary UI
      
      volumes <- c(Home = fs::path_home(),
                   getVolumes()())
      
      shinyFileChoose(input, "csvtsvUpload2",
                      roots = volumes,
                      filetypes = c('csv', 'tsv'))
      
      #### Secondary upload processing ####
      # extracting and parsing uploaded files from shiny files input
      secondary_file <- reactive({
        req(input$csvtsvUpload2)
        
        if(is.null(input$csvtsvUpload2)){
          return(NULL)
        }
        
        parseFilePaths(roots = volumes, input$csvtsvUpload2)
      })
      
      # ensuring secondary file is saved as reactive value in list
      observe({
        rv$secondary_file <- secondary_file()
      })
      

      # Cleaning & mutating dataframe object created by shinyFiles
      file_tibble_second <- reactive({
        req(rv$secondary_file)
        rv$secondary_file %>%
          rename('File' = name,
                 "Size (KB)" = size,
                 "Datapath" = datapath) %>%
          select(-type)
      })
      
      # Creating DT to display file
      output$file_tibble_second <- DT::renderDataTable(
        file_tibble_second(), 
        options = list(
          paging = FALSE, 
          scrollX = TRUE, 
          scrollY = TRUE, 
          dom = 'rti' # only want processing, table and info around the DT.
        ),
        escape = FALSE,
        selection = 'none', 
        rownames = FALSE 
      )
      
    }
  )
} # end secondary server logic
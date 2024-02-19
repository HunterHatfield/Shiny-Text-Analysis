

######## CSV upload method UI & server ###########


csvUI <- function(id, label = "Choose text file(s):"){
  ns <- NS(id)
  tagList(

    h1("Import a table"),
    p("<<Import a file containing comma separated values (.csv) or tab separated values (.tsv) for text mining.>>"),
    em("Note: individual file size limited to 30MB."),
    hr(),
    
    # File uploading
    fileInput(ns("csvtsvUpload"), label = NULL, 
              multiple = FALSE, accept = c(".csv", ".tsv"), 
              placeholder = "No file(s) selected."),
    
    hr(),
    
    wellPanel(
      em(textOutput(ns("subtitle3"))),
      DT::dataTableOutput(ns("file_tibble")) %>%
        withSpinner(),
    ),
    
    fluidRow(
      column(6, {
        actionButton(ns("submit_csvtsv"), 
                     label = "Submit file(s)", 
                     class = "btn-success")
      }),
      column(6, { # , offset = 1
        actionButton(ns("clear_csvtsv"), label = "Clear file(s)", 
                     class = "btn-danger")
      }),
    ),
  )
}

csvServer <- function(id, rv = NULL){
  moduleServer(
    id, 
    function(input, output, session){
      
      
      # saving files() to a reactive value
      observe({
        req(input$csvtsvUpload)
        rv$csvtsv_file <- input$csvtsvUpload
      })
      
      # if no files found, print none found.
      output$subtitle3 <- renderText({
        validate(
          need(file_tibble(), "No files found.")
        )
        return(NULL)
      })
      
      # populate the type column with the extension of the files
      file_tibble <- reactive({
        if(!is.null(rv$csvtsv_file)){
          rv$csvtsv_file %>%
            mutate(type = tools::file_ext(datapath)) %>%
            rename('File Name' = name,
                   "File Type" = type,
                   "Size (bytes)" = size,
                   "Local datapath" = datapath)
        }
      })
      
      # Creating DT to display file
      output$file_tibble <- DT::renderDataTable(
        file_tibble(), 
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
      
      # Saving type for ensuring right read method used on submission
      type <- reactive({
        tools::file_ext(rv$csvtsv_file$datapath)
      })
      
      # When submit clicked...
      observeEvent(input$submit_csvtsv, {
        
        req(nrow(file_tibble()) > 0)
        req(rv$csvtsv_file) # require something uploaded or get fatal error 
        
        # Clean up - select first 2 cols, rename first col to ID, second to content
        csvtsv_contents <- reactive({
          
          # if file uploaded is a .csv file
          if(type() == 'csv'){
            
            temp <- read_csv(rv$csvtsv_file$datapath) # create tibble from file
            colnames(temp)[c(1,2)] <- c("ID", "Contents")
            
            # Is this conversion to chars needed?
            temp <- temp %>%
              mutate(ID = as.character(ID), 
                     Contents = as.character(Contents))
            return(temp)
            
          } else { # else the file is tsv
            
            temp <- read_tsv(rv$csvtsv_file$datapath)
            # Renaming just first two rows to required names
            colnames(temp)[c(1,2)] <- c("ID", "Contents")
            
            temp <- temp %>%
              mutate(ID = as.character(ID), 
                     Contents = as.character(Contents))
            return(temp)
          }
        })
        
        rv$csvtsv_contents <- csvtsv_contents()
        rv$content <- rv$csvtsv_contents
        
        # Creating list for dataset created and its characteristics
        content_primary <- shiny::reactiveValues(data = csvtsv_contents(),
                                                 is_stop_rm = FALSE, 
                                                 is_tokenised = FALSE, 
                                                 is_filtered = FALSE,
                                                 is_mutated = FALSE,
                                                 content_prepared = 
                                                   csvtsv_contents(),
                                                 content_edited = 
                                                   csvtsv_contents(),
                                                 content_primary_tf_idf = NULL)
        

        
        # Saving content_primary list containing data & characteristics in main rv list
        rv$content_primary <- content_primary
        rv$content_to_visualise <- NULL
        rv$content_stats <- NULL

        # rv$numFiles <- nrow(rv$content_primary$data)
        
        # Notification for when files successfully uploaded
        # When submit button clicked
        if(nrow(rv$content_primary$data) > 0){
          showNotification(paste("Table submitted successfully."), 
                           duration = 8, 
                           type = "message")
        }
        
      }) # end submit csv
      
      # Clear files
      observeEvent(input$clear_csvtsv, {
        clear_reactives()
        shinyjs::reset("csvtsvUpload") # used to reset any input object
      }) # end observe event clear
      
      
      # Num files always updating to number of rows in content
      # observe({
      #   rv$numFiles <- nrow(rv$content_primary$data)
      # })
      
    }
  )
}
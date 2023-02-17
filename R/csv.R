######## File upload method UI & server ###########
csvUI <- function(id, label = "Choose text file(s):"){
  ns <- NS(id)
  tagList(

    h1("Upload your table"),
    p("Upload a file containing comma separated values (.csv) or tab separated values (.tsv) for primary text analysis."),
    em("Note: file upload size is limited to 10MB"),
    hr(),
    
    # File uploading
    shinyFilesButton(ns("csvtsvUpload"),
                     label = "Select a .csv/.tsv file",
                     title = "Choose a .csv or .tsv file...",
                     multiple = FALSE, 
                     class = "btn-primary"),
    
    hr(),
    
    wellPanel(
      em(textOutput(ns("subtitle3"))),
      DT::dataTableOutput(ns("file_tibble")),
    ),
    
    fluidRow(
      column(3, {
        actionButton(ns("submit_csvtsv"), 
                     label = "Submit file", 
                     class = "btn-success")
      }),
      column(3, offset = 1, {
        actionButton(ns("clear_csvtsv"), label = "Clear file", 
                     class = "btn-danger")
      }),
      column(6, {
        p()
      })
      
    ),
  )
}

csvServer <- function(id, rv = rv){
  moduleServer(
    id, 
    function(input, output, session){
      
      volumes <- c(Home = fs::path_home(),
                   "R Installation" = R.home(),
                   getVolumes()())
      
      shinyFileChoose(input, "csvtsvUpload",
                      roots = volumes,
                      filetypes = c('csv', 'tsv'),
                      session = session)
      
      # extracting uploaded files from shiny files input
      csvtsv_file <- reactive({
        req(input$csvtsvUpload)
        
        if(is.null(input$csvtsvUpload)){
          return(NULL)
        }
        
        parseFilePaths(roots = volumes, input$csvtsvUpload)
      })
      
      observe({
        rv$csvtsv_file <- csvtsv_file()
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
                   "Size (KB)" = size,
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
          
          # Different handlers for diff file type
          
          # if file uploaded is a .csv file
          if(type() == 'csv'){
            
            # create tibble from file
            temp <- read_csv(rv$csvtsv_file$datapath) %>% 
              dplyr::select(1:2)
            
            colnames(temp) <- c("ID", "Contents")
            
            temp <- temp %>%
              mutate(ID = as.character(ID), 
                     Contents = as.character(Contents))
            
            return(temp)
            
          } else { # else the file is tsv
            
            # create tibble from file
            temp <- read_tsv(rv$csvtsv_file$datapath) %>% 
              dplyr::select(1:2)
            
            colnames(temp) <- c("ID", "Contents")
            
            temp <- temp %>%
              mutate(ID = as.character(ID), 
                     Contents = as.character(Contents))
            
            return(temp)
            
          }
          
        })
        
        # Could just have one 
        rv$csvtsv_contents <- csvtsv_contents() 
          
        rv$content <- rv$csvtsv_contents
        rv$numFiles <- nrow(rv$content)
        
        # Notification for when files successfully uploaded
        # When submit button clicked
        if(rv$numFiles > 0){
          showNotification(paste("Table submitted successfully."), 
                           duration = 5, 
                           type = "message")
        }
        
        
      }) # end submit csv
      
      # Clear files
      observeEvent(input$clear_csvtsv, {
        
        clear_reactives()

      }) # end observe event clear
      
      
      # Num files always updating to number of rows in content
      observe({
        rv$numFiles <- nrow(rv$content)
      })
      
    }
  )
}
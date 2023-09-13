######## File upload method UI & server ###########
# v2

# Create a unique file name
# fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))

# assign to global env
# assign('data',data2,envir=.GlobalEnv)

outputDir <- "R"


####### UI for text file upload section ########################

uploadUI <- function(id, label = "Choose file(s):"){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    h1("File Upload"),
    p("Upload text files (.txt) to extract text data from."),
    em("Note: file upload size is limited to 10MB."),
    hr(),
    
    fluidRow(
      column(12, {
        # shinyFilesButton(ns("fileUpload"),
        #              label = "Select file(s)",
        #              title = "Choose files to upload for analysis...",
        #              multiple = TRUE, 
        #              class = "btn-primary")
        
        # Implementation with fileInput - as shinyFiles only allows user 
        # to browse the file system of the shiny host
        fileInput(ns("fileUpload2"), label = NULL, 
                  multiple = TRUE, accept = c(".txt"), 
                  placeholder = "No file(s) selected.")
        }
      )
    ),
    
    wellPanel(
      em(textOutput(ns("selected_subtitle"))),
      DT::dataTableOutput(ns("file_display")),
    ),
    
    fluidRow(
      
        column(3,{
          actionButton(ns("submitFiles"), 
                       label = "Submit file(s)", 
                       class ="btn-success")
        }),
        column(3, {
          actionButton(ns("appendFiles"), 
                       label = "Append file(s)", 
                       class ="btn-warning")
        }),
        column(3, {
          actionButton(ns("clearFiles"), 
                       label = "Clear file(s)", 
                       class ="btn-danger")
        }), 
        column(3, {
          p()
        })
    ),
  )
}

###### SERVER ######
uploadServer <- function(id, rv, parent){
  moduleServer(
    id, 
    function(input, output, session){
      
      # shinyFiles implementation
      # volumes <- c(Home = fs::path_home(),
      #              "R Installation" = R.home(),
      #              getVolumes()())
      
      # shinyFileChoose(input, "fileUpload",
      #                 roots = volumes,
      #                 filetypes = c('txt'),
      #                 session = session)
      
      
      # extracting uploaded files from shiny files input
      # files <- reactive({
      #   # req(input$fileUpload) # require files to be uploaded before running further
      #   req(input$fileUpload2) # require files to be uploaded before running further
      #   # 
      #   # if(is.null(input$fileUpload)){
      #   #   return(NULL)
      #   # } 
      #   
      #   print("Printing input$fileUpload from fileInput")
      #   print(input$fileUpload2)
      #   
      #   # parseFilePaths(roots = volumes, input$fileUpload)
      #   parseFilePaths(roots = volumes, input$fileUpload2)
      # })
      
      # saving files() to a reactive value
      observe({
        req(input$fileUpload2)
        rv$files <- input$fileUpload2
      })
      
      # if no files found, print none found.
      output$selected_subtitle <- renderText({
        validate(
          need(rv$files, "No files found.")
        )
        return(NULL)
      })
      
      # populate the type column with the extension of the files
      # Using if(!is.null()) means that when the files are cleared the
      # wellPanel which they were in is reduced to its original size.
      # using req() breaks that for some reason.
      file_tibble <- reactive({
        req(rv$files)
        rv$files %>%
          mutate(type = tools::file_ext(datapath)) %>%
          rename('File Name' = name,
                 "File Type" = type,
                 "Size (KB)" = size,
                 "Local datapath" = datapath)
      })
      
      # Using datatable from DT package for file output table
      output$file_display <- DT::renderDataTable(
        file_tibble(), 
        # rv$file_tibble,
        options = list(
           paging = TRUE, 
           title = "test",
           pageLength = 5, 
           scrollX = TRUE, 
           scrollY = TRUE, 
           dom = 'frtipB',
           buttons = 'none', # no butons
           columnDefs = list(
             list(targets = '_all', className = 'dt-left'),
               list(targets = 1,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 100 ?",
                      "'<span title=\"' + data + '\">' + 
                      data.substr(0, 100) + '...</span>' : data;","}"))
                     )
             ),
        escape = FALSE,
        # select a single column, accessed through 
        # input$tableID_rows_selected in the server
        selection = 'none', 
        rownames = FALSE  # don't show row numbers
      )
      
#############################################################################
      
      # When submit button is clicked:
      observeEvent(input$submitFiles, {
        
        # Taking the datapaths of files, applying read file to read in and 
        # create tibble called content.
        content <- reactive({
          rv$files$datapath %>%
            map_chr(~read_file(.)) %>%
            tibble(ID = rv$files$name, 
                   `Contents` = .)
        })
        
        # Creating list for dataset created and its characteristics
        content_primary <- shiny::reactiveValues(data = content(),
                                                 is_stop_rm = FALSE, 
                                                 is_tokenised = FALSE, 
                                                 is_filtered = FALSE,
                                                 is_mutated = FALSE,
                                                 content_primary_tf_idf = NULL)
        
        # Old method of singular data assignment w/ no characteristics
        rv$content <- content()
        
        # Saving content_primary list containing data & characteristics in main rv list
        rv$content_primary <- content_primary
        print("Assigned content_primary list to rv$content_primary:")
        print(rv$content_primary)
        
        # calculating number of rows in content()
        rv$numFiles <- nrow(rv$content_primary$data)

        output$title2 <- renderText({
          if(is.null(rv$files)){
            return(NULL)
          }
          "Selected file(s)"
        })
        
        # Notification for when files successfully uploaded
        # When submit button clicked
        if(nrow(rv$content_primary$data) > 0){
        showNotification(paste("File(s) submitted successfully."), 
                         duration = 5, 
                         type = "message")
        }
        
        # To indicate data has not been tokenised yet, for when 
        # new data is uploaded
        rv$is_tokenised <- FALSE
        
      }) # end observe event submit files
#############################################################################
      
#############################################################################
      # When append button clicked:
      # - read all files to create tibble of added content
      # - create updated content tibble with  
      observeEvent(input$appendFiles, {

        # Taking the datapaths of files, applying read file to read in and
        # create tibble called content.
        added_content <- reactive({
          rv$files$datapath %>%
            map_chr(~read_file(.)) %>%
            tibble(ID = rv$files$name,
                   `Contents` = .)
        })
      
        # Take added content and append to initial content
        updated_content <- reactive({
          # rbind(rv$content, added_content())
          print("Attempting to rbind rows")
          
          try(
            rbind(rv$content_primary$data, added_content())
          )
          
        })
        
        # If appending could not be performed produce error
        if('try-error' %in% class(updated_content())){
          shinyalert(
            title = "Append failed: differing columns",
            text = "These documents could not be appended as they produce a different number of columns to the already submitted data. \n \n Try joining two datasets by a common column with the secondary upload functionality.",
            size = "xs", 
            closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "info",
            showConfirmButton = TRUE, showCancelButton = FALSE,
            confirmButtonText = "Dismiss",
            confirmButtonCol = "#4169E1",
            timer = 0, imageUrl = "", animation = TRUE
          )
          
          
        } else {
          
          rv$content_primary$data <- updated_content()
          rv$content_primary$is_stop_rm <- FALSE
          rv$content_primary$is_tokenised <- FALSE
          rv$content_primary$is_filtered <- FALSE
          rv$content_primary$is_mutated <- FALSE
          print(c("Updated rv$content_primary$is_tokenised:", rv$content_primary$data))
          
          # calculating number of rows in content
          rv$numFiles <- nrow(rv$content_primary$data)
          
          # Notification for when files successfully uploaded
          # When append button clicked
          if(nrow(rv$content_primary$data) > 0){
            showNotification(paste("File(s) appended successfully."), 
                             duration = 5, 
                             type = "message")
          }
          
        }
        
        output$title2 <- renderText({
          if(is.null(rv$files)){
            return(NULL)
          } 
          "Selected file(s)"
        })
        

      }) # end observe event append files

      
      # Clear files
      observeEvent(input$clearFiles, {
        
        clear_reactives()
        
        # shinyjs::reset("ns(fileUpload)") # used to reset any input object
        # shinyjs::refresh() # refreshes the page
        
      }) # end observe event clear files
      
      
      # Using onStop
      onStop(function() {
        cat("Session has stopped")
        
      }) # end onStop function
      
    }
  )
} # end upload server

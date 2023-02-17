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
        shinyFilesButton(ns("fileUpload"),
                     label = "Select file(s)",
                     title = "Choose files to upload for analysis...",
                     multiple = TRUE, 
                     class = "btn-primary")
        }
      )
    ),
    hr(),
    
    wellPanel(
      # h3(textOutput(ns("title2"))), # looks better without
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
      volumes <- c(Home = fs::path_home(),
                   "R Installation" = R.home(),
                   getVolumes()())
      
      shinyFileChoose(input, "fileUpload",
                      roots = volumes,
                      filetypes = c('txt'),
                      session = session)
      
      # extracting uploaded files from shiny files input
      files <- reactive({
        req(input$fileUpload) # require files to be uploaded before running further
        
        if(is.null(input$fileUpload)){
          return(NULL)
        } 
        
        parseFilePaths(roots = volumes, input$fileUpload)
      })
      
      # saving files() to a reactive value
      observe({
        rv$files <- files()
      })
      
      # output$title2 <- renderText({
      #   if(is.null(rv$files)){
      #     return(NULL)
      #   } 
      #   "Selected file(s)"
      # })
      
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
        if(!is.null(rv$files)){
        #req(rv$files)
        rv$files %>%
          mutate(type = tools::file_ext(datapath)) %>%
          rename('File Name' = name,
                 "File Type" = type,
                 "Size (KB)" = size,
                 "Local datapath" = datapath)
        }
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
      
      # When submit button is clicked: save the data as an .xlsx file, calculate number of rows calculated and render content display data table.
      observeEvent(input$submitFiles, {
        
        # Taking the datapaths of files, applying read file to read in and 
        # create tibble called content.
        content <- reactive({
          rv$files$datapath %>%
            map_chr(~read_file(.)) %>%
            tibble(ID = rv$files$name, 
                   `Contents` = .)
        })
        
        rv$content <- content()
        
        # calculating number of rows in content()
        rv$numFiles <- nrow(rv$content)

        output$title2 <- renderText({
          if(is.null(rv$files)){
            return(NULL)
          } 
          "Selected file(s)"
        })
        
        # Notification for when files successfully uploaded
        # When submit button clicked
        if(rv$numFiles > 0){
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
          rbind(rv$content, added_content())
        })
        
        rv$content <- updated_content()

        # calculating number of rows in content
        rv$numFiles <- nrow(rv$content)
        
        output$title2 <- renderText({
          if(is.null(rv$files)){
            return(NULL)
          } 
          "Selected file(s)"
        })
        
        # Notification for when files successfully uploaded
        # When append button clicked
        if(rv$numFiles > 0){
          showNotification(paste("File(s) appended successfully."), 
                           duration = 5, 
                           type = "message")
        }
        
        rv$test <- rv$test + 1

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
      
      rv$test <- 0
      
    }
  )
} # end upload server

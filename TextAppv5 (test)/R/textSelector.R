
# Outer UI for text selector page

# Text selector UI
textSelectorUI <- function(id, label = "Choose file(s):"){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h1("01 | Select"), 
        em("Upload text for analysis by choosing your own files, scraping webpages, or through a public domain of texts provided by Project Gutenberg."),
        hr(),

        # Add ns() around method & paste0 stln used in reporting
        # later on
        h3("Primary upload"),
        radioButtons(inputId = "method",
                     label = "Choose a method:",
                     choiceNames = list(
                       'Text file upload', '.csv/.tsv file upload',
                       'Web scraping', 'Twitter scraping', 
                       'Project Gutenberg'
                     ),
                     choiceValues = list(
                       "upload", "csv", "rvest", 
                       "twitteR", "gutenbergR")
        ),
        hr(),
        
        # Secondary uploads UI. renderUI() function is used to renderUI based on whether
        # or not a user has completed primary uploads. This means secondary upload functionality
        # is only available once primary upload is complete.
        # Render UI for secondary upload once !is.null(rv$content) && nrow(rv$content) > 0
        # secondaryUI(ns("secondary")) # Using module functionality breaks shinyFiles. 
        uiOutput(ns("secondaryUploadUI")),
        uiOutput(ns("secondaryJoinUI")),
        hr(),
          
      ), # end side bar panel
      
      mainPanel(

        fluidRow(
          
          box(title = " ", 
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
              condition = "input.method == 'rvest'", 
              rvestUI(ns("rvest"))
            ),
            conditionalPanel(
              condition="input.method == 'twitteR'", 
              twitteRUI(ns("twitteR"))
            ),
            conditionalPanel(
              condition="input.method=='gutenbergR'", 
              gutenbergRUI(ns("gutenbergR"))
            )

          ), # end box
          
          box(title = " ", 
              status = "success", 
              solidHeader = T,
              collapsible = T,
              width = 12,
              
              h2("Submitted Files"),
              em(textOutput(ns("num_submitted_subtitle"))),
              
              p(),
              wellPanel(
                em(textOutput(ns("submitted_subtitle"))),
                DT::dataTableOutput(ns("content_display")), 
                h2(textOutput(ns("selected_title"))),
                em(textOutput(ns("selectedRowContent")))
              ),
              
          )
          
        ) # end fluid row
      ) # end main panel
  ) # end side bar layout
  ) # end taglist
}


############## SERVER LOGIC ##########################################

textSelectorServer <- function(id, rv = rv, session = session){
  moduleServer(
    id, 
    function(input, output, session){ 

      uploadServer("upload", rv = rv, parent = session)

      twitteRServer("twitteR", rv = rv)
      gutenbergRServer("gutenbergR", rv = rv)
      csvServer("csv", rv = rv)
      rvestServer("rvest", rv = rv)
      
      # primary_upload <- observe(rv$content)
      # secondaryServer("secondary", rv = rv, primary = primary_upload, parent = session)
      
      #### Subtitles ####
      output$selected_title <- renderText({
        if(is.null(input$content_display_rows_selected)){
          return(NULL)
        } 
        "Selected file contents:"
      })
      
      output$submitted_subtitle <- renderText({
        validate(
          need(rv$content, "No files found.")
        )
        return(NULL)
      })
      
      # Subtitle for how many files were uploaded 
      output$num_submitted_subtitle <- renderText({
        if(!is.null(rv$numFiles) ){
          return(paste(c("You have uploaded: ", rv$numFiles,
                         "file(s)."), sep = ","))
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
        rv$content,
        options = list(
          paging = TRUE,
          pageLength = 5,
          scrollX = TRUE,
          scrollY = TRUE,
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
        rv$content %>%
          filter(row_number() == input$content_display_rows_selected)
      })
      
      # Creating text for the number of rows
      output$selectedRowContent <- renderText({
        if(length(input$content_display_rows_selected) == 0){
          return(NULL) 
        }
        selectedRows()$Contents
      })
      
      
      
      #### Creating ui for secondary uploads ####
      output$secondaryUploadUI <- renderUI({
        ns <- NS(id)
        true_UI <- tagList(

          h3("Secondary upload"),
          p("Upload a secondary .csv/.tsv file to adjoin."),

          shinyFilesButton(ns("csvtsvUpload2"),
                           label = "Select a .csv/.tsv file...",
                           title = "Choose a .csv or .tsv file...",
                           multiple = FALSE,
                           class = "btn-primary"),
          hr(),
          
          # Display uploaded files
          DT::dataTableOutput(ns("file_tibble_secondary")),
          hr(),
        )
              
        false_UI <- tagList(
          em("Note: to adjoin a secondary table, submit your primary upload 
             first.", style = "color: grey")
        )

        # if there is a primary upload present, render secondary UI
        # otherwise UI is rendered to let user know to complete primary upload
        if(!is.null(rv$content) && nrow(rv$content) > 0){
          return(true_UI)
        }
        return(false_UI)

      }) # end render secondary UI ##
      
      
      #### Creating ui for secondary joins ####
      # This UI goes under the secondary upload section and is rendered only
      # when secondary content is uploaded.
      output$secondaryJoinUI <- renderUI({
        
        req(rv$content_secondary)
        
        ns <- NS(id)
        tagList(
          
          h3("Join datasets"),
          
          # Options for type of join and what to join on
          p("Select a column from each dataset to join on:"),
          
          column(width = 6, 
                 
                 # Input for which column to join on in primary
                 selectInput(ns("col_primary"), 
                             label = "Primary data", 
                             choices = list("N/A" ="na")
                 ),
          ), 
          
          column(width = 6, 
                 
                 # Input for which column to join on in secondary
                 selectInput(ns("col_secondary"), 
                             label = "Secondary data", 
                             choices = list("N/A" ="na")
                 ),
          ),
          
          # Join type drop down menu
          fluidRow(
            column(width = 12, 
              selectInput(ns("join_type"), 
                          label = "Choose a join type:", 
                          choices = 
                            list("Inner (natural)" = "inner", 
                                 "Outer (full)" = "full", 
                                 "Left" = "left", 
                                 "Right" = "right")
              ),
            ),
          ),
          
          # Join and undo buttons
          fluidRow(
            column(width = 6, 
                   actionButton(ns("join_datasets"), 
                                label = "Join", 
                                class ="btn-primary"),
            ),
            column(width = 6, 
                   actionButton(ns("undo_join"), 
                                label = "Undo", 
                                class ="btn-danger"),
            ),
          ),
          
          hr(),
         
          # Using row & column layout for consistency in padding aesthetics
          fluidRow(
            column(12, 
                   em(textOutput(ns("join_results"))), 
                   tags$br(),
                   actionButton(ns("confirm_join"), 
                                label = "Confirm", 
                                class = "btn-success")
             )
          )
        )
        
  
      }) # end render secondary join UI

      #### Secondary upload processing ####
      volumes <- c(Home = fs::path_home(),
                   getVolumes()())

      shinyFileChoose(input, "csvtsvUpload2",
                      roots = volumes,
                      filetypes = c('csv', 'tsv'),
                      session = session)

      # extracting and parsing uploaded files from shiny files input
      # are both of these req() and if() statements necessary???
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
      file_tibble_secondary <- reactive({
        req(rv$secondary_file)
        rv$secondary_file %>%
          rename('File' = name,
                 "Size (KB)" = size,
                 "Datapath" = datapath) %>%
          dplyr::select(-type)
      })

      # Creating DT to display file
      output$file_tibble_secondary <- DT::renderDataTable(
        file_tibble_secondary(),
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
      
      # Text mining secondary file
      # Using if else to apply correct read function depending on format
      content_secondary <- reactive({
        
        req(rv$secondary_file$datapath)
        
        # if file uploaded is a .csv file
        if(tools::file_ext(rv$secondary_file$datapath) == 'csv'){
          
          read_csv(rv$secondary_file$datapath)
          
        } else { # else the file is tsv
          read_tsv(rv$secondary_file$datapath)
        }
      })
      
      # Saving secondary content as reactive value
      observe({
        rv$content_secondary <- content_secondary()
      })

      #### Updating select inputs based on columns in data ####
      primary_cols <- reactive({
        req(rv$content)
        colnames(rv$content)
      })
      secondary_cols <-reactive({
        req(rv$secondary_file)
        colnames(rv$content_secondary)
      })
      
      # To generate the list of columns in each data set
      observe({
        
        rv$primary_cols <- primary_cols()
        rv$secondary_cols <- secondary_cols()
        
        updateSelectInput(session, "col_primary",
                          choices = rv$primary_cols, 
                          selected= rv$primary_cols[1])
        updateSelectInput(session, "col_secondary",
                          choices = rv$secondary_cols, 
                          selected= rv$secondary_cols[1])
      })
      
      
      # When the Join button clicked, first text mining secondary file, 
      # then joining based on selected columns
      observeEvent(input$join_datasets, {
        
        req(rv$content)
        req(rv$content_secondary) # require secondary content first
        
        # Duplicating content so now have rv$primary_content
        # and rv$content_secondary ready to join
        rv$content_primary <- rv$content
        
        # Saving the selected columns to join on 
        rv$join_col_primary <- input$col_primary
        rv$join_col_secondary <- input$col_secondary
        
        # Saving type of join
        rv$join_type <- input$join_type
        
        # Performing join using join_secondary function outlined in 
        # utils.R - basically joins tibbles based on col names and join
        # type.
        content_joined <- reactive({
          join_secondary(rv$content_primary, # isolate(rv$content_primary),
                         rv$content_secondary, #isolate(rv$content_secondary),
                         rv$join_col_primary, rv$join_col_secondary,
                         rv$join_type)
        })
        
        rv$content_joined <- content_joined()
        
        output$join_results <- renderText({
          req(rv$content_joined)
          paste(c("Resulting dataset will contain ", ncol(rv$content_joined), " column(s) and ", nrow(rv$content_joined), " row(s)."), sep = ",")
        })
        
      }) # end observe event
      
      
      # On submitting joined dataset, assign content as content_joined
      observeEvent(input$confirm_join, {
        
        req(rv$content_joined)
        
        # Assigning joined content to content
        rv$content <- rv$content_joined # content_joined()
        
        rv$is_stop_removed <- FALSE
        rv$is_tokenised <- FALSE
        
      }) # end submit join observe event
      
      # When revert join button clicked, reset rv$content to 
      # rv$content_primary if rv$content_joined exists (as a proxy for
      # joining has occurred)

      observeEvent(input$undo_join, {
        
        req(rv$content_joined)
        
        rv$content <- rv$content_primary
        
        output$join_results <- renderText(NULL)
        
      })
      
      
    } # end module function
    #### ####
  ) 
}
  
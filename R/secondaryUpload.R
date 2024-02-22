
#### Module for R Shiny apps which takes a tibble and defines functionalities to upload a second 
# .csv or .tsv, and adjoin the secondary file to the first based on a selected column. 

secondaryUI <- function(id){
  ns <- NS(id)
  tagList(
    
    # Secondary uploads UI. renderUI() function is used to renderUI based on whether
    # or not a user has completed primary uploads. This means secondary upload functionality
    # is only available once primary upload is complete.
    # Render UI for secondary upload once !is.null(rv$content_primary$data) && 
    # nrow(rv$content_primary$data) > 0
    
    uiOutput(ns("secondaryUploadUI")),
    
    uiOutput(ns("secondaryJoinUI")),
    
  )
} # end secondary UI

###### SERVER ######
# Takes in id and a list of reactive values which is assumed to contain a tibble of the file contents called 'content' and thus accessed by the function as rv$content.
# Also takes in a list of reactive values (can be empty)
secondaryServer <- function(id, rv = NULL){
  moduleServer(id, 
    function(input, output, session){
      
      #############################
      #### Secondary upload UI ####
      #############################
      
      output$secondaryUploadUI <- renderUI({
        
        ns <- NS(id)
        true_UI <- tagList(
          
          h3("Secondary data"),
          p("Merge datasets based on common columns to combine information."),
          p("Import a secondary .csv/.tsv file to adjoin, then choose a column common to both datasets to join on."),
          
          tags$a(href="https://r4ds.hadley.nz/joins.html#sec-mutating-joins", 
                 "Learn more about joins here"),
          
          
          # Table upload with fileInput
          fileInput(session$ns("csvtsvUpload2"), label = NULL, 
                    multiple = FALSE, accept = c(".csv", ".tsv"), 
                    placeholder = "Select .csv/.tsv file..."),
          hr(class = "hr-blank"),
          
          # Display uploaded files
          DT::dataTableOutput(session$ns("file_tibble_secondary")),
          hr(),
          
        ) # end tagList
        
        false_UI <- tagList(
          em("Note: to adjoin a secondary table, submit your primary data 
             first.", style = "color: grey")
        )
        
        # if there is a primary upload present, render secondary UI
        # otherwise UI is rendered to let user know to complete primary upload
        if(!is.null(rv$content_primary$data) && nrow(rv$content_primary$data) > 0){
          return(true_UI)
        }
        return(false_UI)
        
      }) # end render secondary UI ##
      
      
      ###########################
      #### Secondary join UI ####
      ###########################
      # This UI goes under the secondary upload section and is rendered only
      # when secondary content is uploaded.
      output$secondaryJoinUI <- renderUI({
        req(rv$content_primary$data)
        req(rv$content_secondary)
        
        ns <- NS(id)

        # saving desired UI output in ui_ouput
        tagList(
          h3("Join datasets"),
          p("Select a column from each dataset to join on:"),
    
          column(width = 6,
                 # Input for which column to join on in primary
                 selectInput(session$ns("col_primary"),
                             label = "Primary data",
                             choices = colnames(rv$content_primary$data)),
          ),
          column(width = 6,
                 # Input for which column to join on in secondary
                 selectInput(session$ns("col_secondary"),
                             label = "Secondary data",
                             choices = colnames(rv$content_secondary)),
                ), # end column
    
                # Join type drop down menu
                fluidRow(
                  column(width = 12,
                         selectInput(session$ns("join_type"),
                                     label = "Choose a join type:",
                                     choices =
                                       list("Inner (natural)" = "inner",
                                            "Outer (full)" = "full",
                                            "Left" = "left",
                                            "Right" = "right"))
                  ),
                ), # end fluid row
    
                # Join and undo buttons
                fluidRow(
                  column(width = 6,
                         actionButton(session$ns("join_datasets"),
                                      label = "Join",
                                      class ="btn-primary")),
                  column(width = 6,
                         actionButton(session$ns("undo_join"),
                                      label = "Undo",
                                      class ="btn-danger")),
                ),
                hr(),
    
                # Using row & column layout for consistency in padding aesthetics
                fluidRow(
                  column(12,
                         em(textOutput(session$ns("join_results"))),
                         tags$br(),
                         actionButton(session$ns("confirm_join"),
                                      label = "Confirm",
                                      class = "btn-success")
              ) # end column
            ) # end fluid row
          ) # end tagList
        
      }) # end render secondary join UI
      
      #####################################
      #### Secondary upload processing ####
      #####################################
      
      observe({
        req(input$csvtsvUpload2)
        rv$csvtsvUpload2 <- input$csvtsvUpload2
      })

      # Cleaning & mutating dataframe object created by fileInput
      file_tibble_secondary <- reactive({
        req(rv$csvtsvUpload2)
        rv$csvtsvUpload2 %>%
          rename('File' = name,
                 "Size (KB)" = size,
                 "Datapath" = datapath)
      })
      
      # Creating DT to display file
      output$file_tibble_secondary <- DT::renderDataTable(
        file_tibble_secondary(),
        options = list(
          paging = FALSE, scrollX = TRUE, scrollY = TRUE,
          dom = 'rti' # only want processing, table and info around the DT.
        ),
        escape = FALSE, selection = 'none', rownames = FALSE
      )
        
      ####################################
      #### Reading in secondary file  ####
      ####################################
      # Reading in contents of secondary file
      # Using if else to apply correct read function depending on format
      # Saving secondary content as reactive value
      observe({
        req(rv$csvtsvUpload2$datapath)
        
        if(tools::file_ext(rv$csvtsvUpload2$datapath) == 'csv'){
          rv$content_secondary <- read_csv(rv$csvtsvUpload2$datapath)
        } else { # else the file is tsv
          rv$content_secondary <- read_tsv(rv$csvtsvUpload2$datapath)
        }
        
        # To generate the list of columns in each data set
        req(rv$content_primary$data)
        req(rv$content_secondary)
        updateSelectInput(session, session$ns("col_primary"),
                          choices = colnames(rv$content_primary$data), 
                          selected= colnames(rv$content_primary$data)[1])
        updateSelectInput(session, session$ns("col_secondary"),
                          choices = colnames(rv$content_secondary), 
                          selected= colnames(rv$content_secondary)[1])
      })
      
      ######################
      ### Join datasets ####
      ######################
      # When the Join button clicked, first text mining secondary file, 
      # then joining based on selected columns
      observeEvent(input$join_datasets, {
        
        req(rv$content_primary$data)
        req(rv$content_secondary) # require secondary content first
        rv$pre_joined_content <- rv$content_primary$data # saving pre-joined content
        
        # Attempt to join datasets with inputs given
        # Performing join using join_secondary function outlined in 
        # utils.R - joins tibbles based on col names and join type.
        join_attempt <- try(join_secondary(rv$content_primary$data, 
                                              rv$content_secondary,
                                              input$col_primary, input$col_secondary,
                                              input$join_type)
        )
        
        # Alert if join failed
        if("try-error" %in% class(join_attempt)){
          shinyalert(
            title = "Join failed",
            text = "Datasets could not be joined given the specified inputs. \n \n Ensure the chosen secondary file is suitable for joining with the primary uploads. \n \n Alternatively, try the .csv/.tsv primary upload option to upload a table containing text data.",
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
        
        rv$content_joined <- join_attempt
          
        output$join_results <- renderText({
          req(rv$content_joined)
          
          paste(c("Resulting dataset will contain ", 
                  ncol(rv$content_joined), " column(s) and ", 
                  nrow(rv$content_joined), " row(s)."), sep = ",")
        })
          
      }) # end observe event join datasets
      
      
      ######################
      #### Confirm join ####
      ######################
      # On submitting joined dataset, assign content as content_joined
      observeEvent(input$confirm_join, {
        req(rv$content_joined)
        
        # Creating list for dataset created and its characteristics
        content_primary <- shiny::reactiveValues(data = rv$content_joined,
                                                 is_stop_rm = FALSE, 
                                                 is_tokenised = FALSE, 
                                                 is_filtered = FALSE,
                                                 is_mutated = FALSE, 
                                                 content_prepared = rv$content_joined, 
                                                 content_edited = rv$content_joined, 
                                                 content_tf_idf = NULL)

        # Saving content_primary list containing data & characteristics in main rv list
        rv$content_primary <- content_primary
        rv$content_to_visualise <- NULL
        rv$content_stats <- NULL
        
      }) # end submit join observe event
      
      ###################
      #### Undo join ####
      ###################
      # When revert join button clicked, reset rv$content_primary$data to 
      # rv$join_content_primary if rv$content_joined exists (as a proxy for
      # joining has occurred)
      observeEvent(input$undo_join, {
        req(rv$content_joined)
        rv$content_primary$data <- rv$pre_joined_content
        rv$content_primary$content_prepared <- rv$pre_joined_content
        rv$content_primary$content_edited <- rv$pre_joined_content
        rv$content_primary$is_stop_rm <- FALSE
        rv$content_primary$is_tokenised <- FALSE
        rv$content_primary$is_filtered <- FALSE
        rv$content_primary$is_mutated <- FALSE
        shinyjs::reset("join_results") # used to reset any input object
      }) # end undo join
      
      
    }
  )
} # end secondary server logic
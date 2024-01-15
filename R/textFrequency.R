
# Text frequency reporting tab

#### TEXT FREQUENCY UI #####
textFreqUI <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidPage(

      wellPanel(
        h1("03 | Visualisation"),
        em("Generate custom visualisations by interacting with settings."),
      ),
      hr(class = "hr-blank"),
      
      fluidRow(
      
        box(title = NULL, 
            status = "primary", 
            solidHeader = T, collapsible = T,
            width = 3, 
            
            # Data selection
            h3("Dataset selector"),
            selectInput(ns("data_to_visualise"), 
                        label = "Select a tokenised dataset to visualise:", 
                        choices = list("N/A" = "na")),
            
            selectInput(ns("col_name_to_visualise"), 
                        label = "Select variable to visualise:", 
                        choices = list("N/A" = "na")),
            
            actionButton(ns("confirm_data_to_visualise"),
                         label = "Confirm",
                         class = "btn-success"),
            ),
        
         box(title = NULL, 
             status = "primary", 
             solidHeader = T,
             collapsible = T,
             width = 9, # width is relative to to column
             
             # Display tokenised text data
             h2("Prepared Text Data"),
             p("Selected text data with prepartion customisations applied."),
             hr(),
             DT::dataTableOutput(ns("content_to_visualise_DT")),
             hr(class = "hr-blank"),
       ), # end box
      ), # end fluid row
      
      hr(),
      
      fluidRow(
          reportingUI(ns("reporting"))
      ) # end fluid row 
    ) # end fluid page
  )
}


#### TEXT FREQ SERVER ####
textFreqServer <- function(id, rv = rv){
  moduleServer(id, function(input, output, session){
      
      #######################
      ### Dataset chooser ###
      #######################
      observe({
        # Create list of possible datasets to select from
        req(rv$content_primary)
        potential_sets_visualise <- list("Primary data")
        if(!is.null(rv$subset_one)){
          potential_sets_visualise[length(potential_sets_visualise) + 1] <- "Subset one"
        }
        if(!is.null(rv$subset_two)){
          potential_sets_visualise[length(potential_sets_visualise) + 1] <- "Subset two"
        }
        updateSelectInput(session, "data_to_visualise",
                          choices = potential_sets_visualise,
                          selected= potential_sets_visualise[1])
      })
      
      # Always update possible dataset list with current available subsets
      observe({
        
        req(rv$content_primary$content_prepared)
        req(input$data_to_visualise)
        
        # Rendering whatever dataset selected in datatable display. Needs to be
        # rendered as diff sets selected before confirm button clicked
        rv$content_to_visualise_DT <- 
          switch(input$data_to_visualise, 
                 "Primary data" =  rv$content_primary$content_prepared, 
                 "Subset one" = rv$subset_one$content_prepared, 
                 "Subset two" = rv$subset_two$content_prepared)
        
        ###################
        ## Column chooser #
        ###################
        # When a dataset is chosen to visualise on, need to load which 
        # columns available in that dataset
        # (subsets' content_prepared is initialized on save of subsets)
        rv$content_to_visualise_colnames <- switch(input$data_to_visualise, 
                               "Primary data" = 
                                     colnames(rv$content_primary$content_prepared), 
                               "Subset one" = colnames(rv$subset_one$content_prepared), 
                               "Subset two" = colnames(rv$subset_two$content_prepared)
        )
        updateSelectInput(session, "col_name_to_visualise",
                          choices = rv$content_to_visualise_colnames[-1],
                          selected= rv$content_to_visualise_colnames[2])
      })
      

      ########################################
      ## Validate content data to visualise ##
      ########################################
      observeEvent(input$confirm_data_to_visualise, {
        
        # Require something to be upload (content_primary) and inputs selected
        req(rv$content_primary)
        req(input$data_to_visualise)
        req(input$col_name_to_visualise)
        
        # Double check the selected column exists in the dataset chosen
        req(input$col_name_to_visualise %in% rv$content_to_visualise_colnames)
        
        # Verifying the dataset is convertible to chars and has 1-3 words/tokens per row
        # Have rv$content_to_visualise_DT from switch before, checking this
        req(rv$content_to_visualise_DT)
        
        # Attempt to convert the selected column to characters
        col_to_visualise_char_attempt <- try(
            as.character(rv$content_to_visualise_DT[[input$col_name_to_visualise]])
        )
        
        # If conversion to characters failed, produce alert and return
        # else assign the converted column to data to visualise
        if("try-error" %in% class(col_to_visualise_char_attempt)){
          shinyalert(
            title = "Selection invalid: data invalid",
            text = "The contents of the selected column could not be converted to strings in order to visualise. \n \n Ensure your selected column contains strings of text/words.",
            size = "xs", 
            closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning",
            showConfirmButton = TRUE, showCancelButton = FALSE,
            confirmButtonText = "Dismiss",
            confirmButtonCol = "#4169E1",
            timer = 0, imageUrl = "", animation = TRUE
          )
          return()
        }
        rv$content_to_visualise_DT[[input$col_name_to_visualise]] <- 
          col_to_visualise_char_attempt
        
        # Validating the supplied data to visualise is tokenised enough, see utils.R
        is_tokenised <- tokenised_enough(rv$content_to_visualise_DT, 
                                         input$col_name_to_visualise)
        
        if(!is_tokenised){
          shinyalert(
            title = "Too many tokens per row",
            text = "Data should contain no more than 3 words per row to produce visualisations. \n \n Use the Tokenise tool in the Text Preparation tab to split up your text into 1-3 words per row.",
            size = "s", 
            closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning",
            showConfirmButton = TRUE, showCancelButton = FALSE,
            confirmButtonText = "Dismiss",
            confirmButtonCol = "#4169E1",
            timer = 0, imageUrl = "", animation = TRUE
          )
          return()
        }
        
        # Creating list for dataset created and its characteristics
        content_to_visualise <- shiny::reactiveValues(
                                        data = rv$content_to_visualise_DT, 
                                        col_to_visualise = input$col_name_to_visualise
                                        )
        rv$content_to_visualise <- content_to_visualise
        
        # At this point we have dataset's column to visualise which is tokenised enough
        # Extracting from data to visualise just the ID and column of interest
        rv$content_to_visualise$plotting_data <- rv$content_to_visualise_DT %>%
          select("ID", {input$col_name_to_visualise}) %>%
          rename(Token = {input$col_name_to_visualise})
        
        print("content_to_visualise$plotting_data initialised in textFreq.")
      }) # end observe event input$confirm_data_to_visualise
      
      
      # Creating datatable of content parameterised
      output$content_to_visualise_DT <- DT::renderDataTable({
        
        validate(need(rv$content_to_visualise_DT, "Submit text data in the 
                      Text Selector Tab to unlock visualisation features."))
        
        DT::datatable(rv$content_to_visualise_DT,
        options = list(
          paging = TRUE, pageLength = 6,
          scrollX = TRUE, scrollY = TRUE,
          dom = 'frtip',
          columnDefs = list(
            list(targets = '_all', className = 'dt-left'),
            list(targets = 1,
                 render = JS(
                   "function(data, type, row, meta) {",
                   "return type === 'display' && data.length > 100 ?",
                   "'<span title=\"' + data + '\">' + 
                      data.substr(0, 100) + '...</span>' : data;","}")))
        ),
        selection = 'single',
        rownames = FALSE
        )
      }) # end render datatable
      
      
      ######################
      ## Reporting stuff ###
      ######################
      # Inner modules for file uploads, csv uploads and secondary uploads
      reportingServerAttempt <- try(reportingServer("reporting", rv = rv))
      if("try-error" %in% class(reportingServerAttempt)){
        shinyalert(
          title = "Reporting tab error",
          text = "There was a fatal error in the server function of the reporting module. Refresh your app and try again.",
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
      
    }
  )
}

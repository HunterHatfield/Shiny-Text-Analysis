
# Text frequency reporting tab

#### TEXT FREQUENCY UI #####
textFreqUI <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(

      wellPanel(
        h1("03 | Visualisation"),
        em("Generate custom visualisations by interacting with settings."),
      ),
      hr(),
      
      fluidRow(
      
        box(title = " ", 
            status = "primary", 
            solidHeader = T, collapsible = T,
            width = 3, 
            
            # Data selection
            h3("Dataset selector"),
            selectInput(ns("data_to_visualise"), 
                        label = "Select a tokenised dataset to visualise:", 
                        choices = list("N/A" = "na")),
            
            actionButton(ns("confirm_data_to_visualise"),
                         label = "Confirm",
                         class = "btn-success"),
            ),
        
         box(title = " ", 
             status = "primary", 
             solidHeader = T, 
             collapsible = T,
             width = 9, # width is relative to to column
             
             # Display tokenised text data
             h2("Prepared Text Data"),
             p("Selected text data with prepartion customisations applied."),
             hr(),
              
             h4(textOutput(ns("parameterised_subtitle1")) %>%
                   tagAppendAttributes(style = "background-color: crimson;
                                              color: white;")),
             h4(textOutput(ns("parameterised_subtitle2")) %>%
                   tagAppendAttributes(style = "background-color: crimson;
                                              color: white;")),
              
             DT::dataTableOutput(ns("content_prepared")),
       ), # end box
      ), # end fluid row
    
      hr(),
      
      fluidRow(
        wellPanel(
          reportingUI("reporting"),
        )
      ) # end fluid row 
    ) # end fluid page
  )
}


#### TEXT FREQ SERVER ####
textFreqServer <- function(id, rv = rv){
  moduleServer(id, function(input, output, session){
      
      reportingServer("reporting", rv = rv)
      
      # if no files submitted, print none found. Else print nothing.
      output$parameterised_subtitle1 <- renderText({
        validate(
          need(rv$content, "Submit files in the Text Selector Tab to continue")
        )
        return(NULL)
      })
      
      output$parameterised_subtitle2 <- renderText({
        validate(
          need(rv$content_prepared,
               "Select stop-words and/or tokenise your data.")
        )
        return(NULL)
      })
      
      #######################
      ### Dataset chooser ###
      #######################
      # Always update possible dataset list with current available subsets
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
        
        # Updating select input list to contain datasets available
        updateSelectInput(session, "data_to_visualise",
                          choices = potential_sets_visualise,
                          selected= potential_sets_visualise[1])
      })
      
      observeEvent(input$confirm_data_to_visualise, {
        
        print("clicked confirm")
        # Save selected dataset in rv$content_prepared, or content_to_visualise
        req(rv$content_primary)
        print(input$data_to_visualise)
        
        if(input$data_to_visualise == "Primary data"){
          print("setting rv$content_prepared:")
          req(rv$content_primary$is_tokenised)
          rv$content_prepared <- rv$content_primary$content_prepared
          rv$is_tokenised <- TRUE
          colnames(rv$content_prepared)[2] <- "Token"
          print(rv$content_prepared)
        } else if(input$data_to_visualise == "Subset one") {
          req(rv$subset_one$is_tokenised)
          rv$content_prepared <- rv$subset_one$content_prepared
          rv$is_tokenised <- TRUE
        } else if(input$data_to_visualise == "Subset two") {
          req(rv$subset_two$is_tokenised)
          rv$content_prepared <- rv$subset_two$content_prepared
          rv$is_tokenised <- TRUE
        }
        
        
      }) # end observe event input$confirm_data_to_visualise
      
      
      # Creating datatable of content parameterised
      output$content_prepared <- DT::renderDataTable(
        rv$content_prepared,
        options = list(
          paging = TRUE,
          pageLength = 10,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = 'frtip',
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
      

      # # Rendering a download button with downloadHandler()
      # output$download_parameterised_csv <- downloadHandler(
      #   filename = function() {
      #     paste("Parameterised_Content.csv")
      #   }, 
      #   content = function(file){
      #     write_delim(as.data.frame(rv$content_parameterised), file, 
      #                        delim = ",")
      #   }
      # )
      # 
      # output$download_parameterised_tsv <- downloadHandler(
      #   filename = function() {
      #     paste("Parameterised_Content.tsv")
      #   }, 
      #   content = function(file){
      #     write_tsv(as.data.frame(rv$content_parameterised), file)
      #   }
      # )
      
        
      onStop(function() {
        cat("This will run on session stop")
        outputDir <- "R"
        
        print(rv)
        print(report_rv)
      })
    }
  )
}

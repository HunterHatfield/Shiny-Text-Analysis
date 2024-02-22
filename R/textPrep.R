# Text preparation tab

#### TEXT PREP UI #####
textPrepUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidPage(
      wellPanel(
        h1("02 | Text Preparation"),
        p("Prepare your text data effortlessly. Remove stop words, tokenize, and stem your data. Optionally, mutate and filter your dataset to customize your analysis further. Streamline your text preprocessing workflow for smoother data exploration and analysis."),
        em("Note: to access visualisations, remember to tokenise your dataset using the Tokenise tool below.")
      ),
      
      fluidRow(
        # Box for data editing
        box(title = "Mutate & filter (optional)",
          status = "primary",
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidRow(
            column(4,
              h3("Mutate"),
              p("Edit your text data by double-clicking on the table. For advanced editing, use the mutate tools to add or edit columns."),

              # Radio buttons to render which type of mutation controls
              radioButtons(ns("mutate_option"),
                label = "Choose a mutate method:",
                choices = list(
                  "Add new column" = "mutate_new",
                  "Update existing column" = "mutate_update"
                )
              ),
              wellPanel(

                # Conditional panel renders either new col name box or dropdown box of
                # existing columns depending on the mutate method option selected
                conditionalPanel(
                  paste0(
                    "input['", ns("mutate_option"),
                    "'] == 'mutate_new' "
                  ),
                  uiOutput(ns("mutate_new_UI"))
                ),
                conditionalPanel(
                  paste0(
                    "input['", ns("mutate_option"),
                    "'] == 'mutate_update' "
                  ),
                  uiOutput(ns("mutate_update_UI"))
                ),

                # Conditional panel for simple or advanced mutate options
                conditionalPanel(
                  paste0(
                    "input['", ns("is_advanced_mutate"),
                    "'] == true "
                  ),
                  uiOutput(ns("advanced_mutate_UI"))
                ),
                conditionalPanel(
                  paste0(
                    "input['", ns("is_advanced_mutate"),
                    "'] == false "
                  ),
                  textInput(ns("mutate_to_insert_simple"),
                    label = "Value to insert:",
                    placeholder = "e.g. 1, apple, TRUE"
                  ),
                ),
                checkboxInput(ns("is_advanced_mutate"), "Advanced mutate"),
                fluidRow(
                  column(width = 6,
                    actionButton(ns("submit_mutate"),
                      label = "Mutate",
                      class = "btn-primary"
                    )
                  ),
                  column(width = 6,
                    actionButton(ns("undo_mutate"),
                      label = "Undo",
                      class = "btn-danger"
                    )
                  )
                ),
              ), # end mutating options well panel

              hr(class = "hr-blank"),
              
              # Filtering options 
              h3("Filter"),
              p("Use the interactive datatable to filter attributes of your text data. Save your filtered data to use in further analyses."),
              em("Note: filtering data will remove any rows that do not pass the filter. If you want to keep all rows, save the filtered set as a subset.", style = "color: gray; margin-bottom: 0px 0px 50px;"),
              
                wellPanel(
                  fluidRow(
                  column(width = 6,
                    actionButton(ns("submit_filters"),
                                 label = "Filter",
                                 class = "btn-primary"
                    )
                  ),
                  column(width = 6,
                    actionButton(ns("undo_filters"),
                                 label = "Undo filters",
                                 class = "btn-danger"
                    )
                  ),
                ) # end fluidRow
              ),# end well panel
            ), # end col 1

            column(8,
              # Display DT to filter data
              wellPanel(
                # DT::dataTableOutput(ns("content_prepared_DT")),
                DT::DTOutput(ns("content_prepared_DT")) %>%
                  withSpinner(),
                hr(class = "hr-blank"),
              ),
              fluidRow(
                column(width = 3,  hr(class = "hr-blank")),
                column(width = 3,
                  actionButton(ns("add_content_row"),
                    label = "Add row",
                    class = "btn-warning"
                  )
                ),
                column(width = 3,
                  actionButton(ns("delete_content_row"),
                    label = "Delete row",
                    class = "btn-danger"
                  )
                ),
                column(width = 3,
                  actionButton(ns("subset_trigger"),
                               label = "Save as subset",
                               class = "btn-success"
                  )
                )
              ), # end fluid row
            ), # end col 2
          ), # end fluid row
        ), # end box
      ), # end outer fluid row

      fluidRow(
        column(5,
          tabBox(
            side = "left",
            selected = "Tokenisation",
            width = 12,
            tabPanel(title = "Stop-words",
                     stopWordsUI(ns("stopwords"))
            ), # end tab panel

            tabPanel(title = "Tokenisation",
              tokeniseUI(ns("tokenise"))
            ), # end tab panel

            tabPanel(title = "Stemming",
              h3("Stemming"),
              p("Stemming is a linguistic technique that trims words down to their base or root form, removing prefixes and suffixes. For example, the words 'runner', 'running' and 'ran' have the same root: 'run'. "),
              # p("Stemming is typically performed to enhance the efficiency of text analysis and improve the accuracy of searches."),
              p("Your data must be tokenised into words to perform stemming."),
              tags$a(href="https://en.wikipedia.org/wiki/Stemming", 
                     "Learn more about stemming here"),
              
              selectInput(ns("data_to_stem"), 
                          label = "Select a dataset:", 
                          choices = list("N/A" = "na")),
              
              selectInput(ns("col_name_to_stem"), 
                          label = "Select a column to perform stemming on:", 
                          choices = list("N/A" = "na")),
              
              fluidRow(
                column(6,
                  actionButton(ns("submit_stemming"),
                    label = "Stem",
                    class = "btn-success"
                  ),
                ),
                column(6,
                  actionButton(ns("undo_stemming"),
                    label = "Undo",
                    class = "btn-danger"
                  ),
                ),
              ),
            ), # end stemming tabpanel
          ), # end tab box

        ), # end col 1

        column(7,
          box(title = NULL, status = "primary",
              solidHeader = T,
              collapsible = T,
              width = 12,

              h2("Prepared data"),
              hr(class = "hr-blank"),
              DT::dataTableOutput(ns("content_prepared_DT_2")) %>%
                withSpinner(),
              
              hr(class = "hr-blank"),
              fluidRow(
                column(4,
                       downloadButton(ns("download_parameterised_csv"),
                                      label = "Save (.csv)"
                       )
                ),
                column(4, 
                       downloadButton(ns("download_parameterised_tsv"),
                                      label = "Save (.tsv)"
                       )
                ),
                # column(2,
                #        hr(class = "hr-blank")),
                column(4,
                       actionButton(ns("revert_all"),
                                    label = "Revert all edits",
                                    class = "btn-danger"
                       ),
                ),
              ),
              
          ) # end box
        ) # end col 2
      ), # end outer fluid row
    ) # end fluid page
  )
}


#### TEXT PREP SERVER ####
textPrepServer <- function(id, rv = NULL) {
  moduleServer(id,function(input, output, session) {
    
      # Inner modules for file uploads, csv uploads and secondary uploads
      stopWordsServerAttempt <- try(stopWordsServer("stopwords", rv = rv))
      if("try-error" %in% class(stopWordsServerAttempt)){
        shinyalert(
          title = "Stop-words server error",
          text = "There was a fatal error in the server function of the stop-words module. Refresh your app and try again.",
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
      tokeniseServerAttempt <- try(tokeniseServer("tokenise", rv = rv))
      if("try-error" %in% class(tokeniseServerAttempt)){
        shinyalert(
          title = "Tokenise server error",
          text = "There was a fatal error in the server function of the tokeniser module. Refresh your app and try again.",
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
      
      
      # Initialising datatable data to be initial upload
      observe({
        rv$content_prepared_display_2 <- rv$content_primary$content_prepared
      })

      # Creating datatable of content parameterised at top of page
      output$content_prepared_DT <- DT::renderDataTable({
        
        validate(need(rv$content_primary$content_prepared, 
                 "Submit files in the Text Selector tab to continue."))

        DT::datatable(rv$content_primary$content_prepared,
        filter = "top", # server = TRUE,
        options = list(
          paging = TRUE, pageLength = 7,
          scrollX = TRUE, scrollY = TRUE,
          dom = "frtip",
          columnDefs =
            list(
              list(
                targets = '_all',
                render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 200 ?",
                  "'<span title=\"' + data + '\">' + data
                        .substr(0, 200) + '...</span>' : data;", "}"
                )
              )
            )
        ),
        selection = "single",
        rownames = FALSE,
        editable = TRUE
      )
      
      }) # end render datatable


      ###########################
      #### Editing table ########
      ###########################

      # Observe event for each time a cell is edited, so it is saved
      # When edit occurs, subset content_prepared with user-entered value
      # in the datatable display. Result is that the data in server gets
      # updated whenever user makes edits on front end
      # R col indexes start at 1 whilst DT at 0, so col + 1 is used
      observeEvent(input$content_prepared_DT_cell_edit, {

        rv$content_primary$content_prepared[input$content_prepared_DT_cell_edit$row, 
                                            input$content_prepared_DT_cell_edit$col + 1] <<- 
          input$content_prepared_DT_cell_edit$value

      })

      #############
      ## Add row ##
      #############
      # When add row clicked, insert blank row in first index
      # Shiny options used to render NA values in datatable html widgets, see app.R
      observeEvent(input$add_content_row, {
        req(rv$content_primary$content_prepared)

        rv$content_primary$content_prepared <- 
          berryFunctions::insertRows(rv$content_primary$content_prepared, 1, 
                                     new = list(NA))
      })
      
      ################
      ## Delete row ##
      ################
      # When add row clicked, insert blank row in first index
      # Shiny options used to render NA values in datatable html widgets, see app.R
      observeEvent(input$delete_content_row, {
        req(rv$content_primary$content_prepared)
        req(input$content_prepared_DT_rows_selected)
        
        rv$content_primary$content_prepared <- 
          rv$content_primary$content_prepared[-(input$content_prepared_DT_rows_selected),]
          
      })
      
      ###########################
      ###### Mutating UI ########
      ###########################
      # Creating ui for new column mutation 
      # UI for simple add new column
      output$mutate_new_UI <- renderUI({
        ns <- NS(id)
        tagList(
          textInput(ns("mutate_new_col_name"),
                    label = "Choose a name for your new column:",
                    value = "new_column"
          ),
        ) # end tagList of renderUI
      }) # end renderUI
      
      # UI for simple update column
      output$mutate_update_UI <- renderUI({
        ns <- NS(id)
        
        req(rv$content_primary$content_prepared$ID)
        var_select <- rv$content_primary$content_prepared %>%
          select(-ID)
        
        tagList(
          varSelectInput(ns("mutate_col_to_update"),
                         label = "Choose a column to update:",
                         var_select,
                         selected = 2,
                         multiple = FALSE)
        ) # end tagList of renderUI
        
      }) # end renderUI
      
      # UI for advanced mutate
      output$advanced_mutate_UI <- renderUI({
        ns <- NS(id)
        tagList(
          p("Specify condition and values to insert"),
          # if <mutate_advanced_condition_col> <mutate_advanced condition>
          # <mutate_advanced_condition_input> then fill with
          varSelectInput(ns("mutate_advanced_condition_col"),
                         label = "If:",
                         rv$content_primary$content_prepared,
                         selected = 2,
                         multiple = FALSE
          ),
          fluidRow(
            column(
              width = 6,
              selectInput(ns("mutate_advanced_condition"),
                          label = NULL,
                          choices = list(
                            "is equal to (numeric)" = "is equal to (numeric)",
                            "is less than" = "is less than",
                            "is greater than" = "is greater than",
                            "matches string" = "matches string",
                            "contains" = "contains"
                          )
              )
            ),
            column(
              width = 6,
              # Render text input if 'contains' condition selected
              conditionalPanel(
                
                # if condition is contains OR is matches
                condition = paste0("input['", ns("mutate_advanced_condition"),
                                   "'] == 'contains' || 
                                   input['", ns("mutate_advanced_condition"), 
                                   "'] == 'matches string'"),
                textInput(ns("mutate_advanced_condition_text_input"),
                          label = NULL,
                          value = NULL,
                          placeholder = "e.g. shiny"
                ),
              ),
              # Render numeric input if equals/less than/greater than condition
              # is selected
              conditionalPanel(
                condition = paste0("input['", ns("mutate_advanced_condition"),
                                   "'] == 'is equal to (numeric)' || 
                                   input['", ns("mutate_advanced_condition"), 
                                   "'] == 'is less than' || input['", 
                                   ns("mutate_advanced_condition"), "'] ==
                                   'is greater than'"),
                numericInput(ns("mutate_advanced_condition_numeric_input"),
                             label = NULL,
                             value = 1
                ),
              ),
            )
          ),
          fluidRow(
            column(
              width = 6,
              textInput(ns("mutate_advanced_equals_true"),
                        label = "Then fill with:",
                        value = "TRUE"
              ),
            ),
            column(
              width = 6,
              textInput(ns("mutate_advanced_equals_false"),
                        label = "Else fill with:",
                        value = NULL,
                        placeholder = "(optional)"
              ),
            )
          ),
        ) # end tagList of renderUI
      }) # end renderUI

      ###########################
      ###### Mutating data ######
      ###########################

      observeEvent(input$submit_mutate, {
        req(rv$content_primary$content_edited) # is content if no editing yet, or mutating result
        req(rv$content_primary$content_prepared) # is either content/result of filtering/mutating
        rv$content_primary$pre_mutated_temp <- rv$content_primary$content_prepared # saving temp for undo

        # Saving what is in data table, so user can mutate and add column values
        # to just filtered data rows if they wish as opposed to every row
        in_datatable <- 
          rv$content_primary$content_prepared[input[["content_prepared_DT_rows_all"]], ]

        # Add new column selected
        if (input$mutate_option == "mutate_new") {
          
          # First cleaning new column name to ensure no whitespace & 
          # ensuring column name isn't replicated
          rv$mutate_new_col_name <- str_trim(input$mutate_new_col_name, side = "both")
          
          if(rv$mutate_new_col_name %in% colnames(rv$content_primary$content_prepared)){
            shinyalert(
              title = "Mutate failed: column name error",
              text = "Column names must be unique. \n \n Ensure your new column name is unique to all other column names. \n \n To mutate an existing column, select the 'Update existing column' button.",
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
          
          # Basic add column selected
          if (input$is_advanced_mutate == FALSE) {
            # If the content_prepared ID column is in the
            # data table selected, add the value to insert, otherwise NA
            rv$content_primary$content_mutated <- rv$content_primary$content_prepared %>%
              mutate(
                "{rv$mutate_new_col_name}" :=
                  if_else(.$ID %in% in_datatable$ID,
                          input$mutate_to_insert_simple, NA
                  )
              )
            
            rv$content_primary$is_mutated <- TRUE
            
          } else { # Where advanced mutate new is selected:
            
            # Running advanced_mutate from utils.R, parsing in user inputs
            res <- try(advanced_mutate(
              content_prepared_in = rv$content_primary$content_prepared,
              in_datatable = in_datatable,
              mutate_option = input$mutate_option,
              mutate_new_col_name = rv$mutate_new_col_name,
              mutate_col_to_update = NULL,
              mutate_advanced_condition_col = input$mutate_advanced_condition_col,
              mutate_advanced_condition = input$mutate_advanced_condition,
              mutate_advanced_condition_numeric_input = 
                input$mutate_advanced_condition_numeric_input,
              mutate_advanced_condition_text_input = 
                input$mutate_advanced_condition_text_input,
              mutate_advanced_equals_true = input$mutate_advanced_equals_true,
              mutate_advanced_equals_false = input$mutate_advanced_equals_false
            ))
            
            # if res has class try-error, generate failed alert
            if("try-error" %in% class(res)){
              shinyalert(
                title = "Mutate failed",
                text = "This action failed to complete. \n \n Ensure the values submitted are valid numeric or character inputs. \n \n To mutate an existing column, select the 'Update existing column' button.",
                size = "xs", 
                closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                html = FALSE, type = "warning",
                showConfirmButton = TRUE, showCancelButton = FALSE,
                confirmButtonText = "Dismiss",
                confirmButtonCol = "#4169E1",
                timer = 0, imageUrl = "", animation = TRUE
              )
              
              return()
            } else {
              rv$content_primary$content_mutated <- res
              rv$content_primary$is_mutated <- TRUE
            }
          }
          
        } # END mutate new logic

        # START update mutate logic
        # Where basic update column is selected:
        else if (input$mutate_option == "mutate_update") {

          if (input$is_advanced_mutate == FALSE) {
            
            # If row is in datatable, insert the value specified,
            # else input what is originally content_prep but not in datatable 
            # If datatable has been filtered but filters not submitted, update 
            # is performed on the displayed rows and other rows are left alone. 
            # If filters have been selected and submitted, update is performed 
            # on all rows
            for (i in 1:nrow(rv$content_primary$content_prepared[input$mutate_col_to_update])) {
              if (rv$content_primary$content_prepared$ID[i] %in% in_datatable$ID[i]) {
                rv$content_primary$content_prepared[[input$mutate_col_to_update]][[i]] <-
                  input$mutate_to_insert_simple
              }
            }
            rv$content_primary$content_mutated <- rv$content_primary$content_prepared

            rv$content_primary$is_mutated <- TRUE
            
            # Where advanced update column is selected:
          } else {

            res <- try(advanced_mutate(
              content_prepared_in = rv$content_primary$content_prepared,
              in_datatable = in_datatable,
              mutate_option = input$mutate_option,
              mutate_new_col_name = NULL,
              mutate_col_to_update = input$mutate_col_to_update,
              mutate_advanced_condition_col = input$mutate_advanced_condition_col,
              mutate_advanced_condition = input$mutate_advanced_condition,
              mutate_advanced_condition_numeric_input = 
                input$mutate_advanced_condition_numeric_input,
              mutate_advanced_condition_text_input = 
                input$mutate_advanced_condition_text_input,
              mutate_advanced_equals_true = input$mutate_advanced_equals_true,
              mutate_advanced_equals_false = input$mutate_advanced_equals_false
              ))
            
            # if res has class try-error, generate failed alert
            if("try-error" %in% class(res)){
              shinyalert(
                title = "Mutate failed",
                text = "This action failed to complete. \n \n Ensure the values submitted are valid numeric or character inputs. \n \n To mutate an existing column, select the 'Update existing column' button.",
                size = "xs", 
                closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                html = FALSE, type = "warning",
                showConfirmButton = TRUE, showCancelButton = FALSE,
                confirmButtonText = "Dismiss",
                confirmButtonCol = "#4169E1",
                timer = 0, imageUrl = "", animation = TRUE
              )
              
              return()
            } else {
              rv$content_primary$content_mutated <- res
              rv$content_primary$is_mutated <- TRUE
            }

          }
        }

        # Setting content_edited and content_prepared to the
        # new mutated content
        req(rv$content_primary$content_mutated)
        rv$content_primary$content_edited <- rv$content_primary$content_mutated
        rv$content_primary$content_prepared <- rv$content_primary$content_mutated
      
        })


      ## Undo mutations:
      ## If content has been mutated, revert to pre_mutated_temp
      observeEvent(input$undo_mutate, {
        req(rv$content_primary$is_mutated)
        req(rv$content_primary$pre_mutated_temp)

        rv$content_primary$content_prepared <- rv$content_primary$pre_mutated_temp
        rv$content_primary$content_edited <- rv$content_primary$pre_mutated_temp
        rv$content_primary$is_mutated <- FALSE
      })


      ###########################
      ###### Filtering data #####
      ###########################

      # When submit filters clicked, subset content_stats by filtered rows
      observeEvent(input$submit_filters, {
        req(rv$content_primary$content_prepared)

        # Creating temporary storage hold
        rv$content_primary$pre_filtered_temp <- rv$content_primary$content_prepared

        # Subsetting content_stats by filters
        rv$content_primary$content_prepared <-
          rv$content_primary$pre_filtered_temp[input[["content_prepared_DT_rows_all"]], ]

        rv$content_primary$is_filtered <- TRUE
      })

      # When undo filters button clicked, revert content_stats to original
      observeEvent(input$undo_filters, {
        req(rv$content_primary$is_filtered)
        req(rv$content_primary$pre_filtered_temp) 

        rv$content_primary$content_parameterised <- rv$content_primary$pre_filtered_temp # revert to original
        rv$content_primary$content_prepared <- rv$content_primary$content_parameterised

        rv$content_primary$is_filtered <- FALSE
      })

      ###########################
      #### Subsetting data ######
      ###########################
      
      # Using modals within modules means namespace for modal needs to be set again, 
      # which can't be run within the showModal() function. Thus need function to 
      # set the session namespace and return modelDilog() content 
      show_subset_modal <- function(){
        
        # validate(need(rv$content_primary$content_prepared),
        #          "Upload a dataset before subsetting.")
        
        ns <- session$ns
        modalDialog(
          h3("Subset data"),
          p("Filtered data, which is a subset of the originally uploaded data, can be saved in either slot as a subset. Subsets can be prepared, explored, and reported on just like the originally uploaded dataset."),
          fluidRow(
            column(6, 
                   actionButton(ns("save_subset_one"), label = "Save in slot I",
                                class = "btn-success"),
            ),
            column(6, 
                   actionButton(ns("save_subset_two"), label = "Save in slot II",
                                class = "btn-success")
            ),
          ),
        )
      } # end show_subset_modal() function
      
      observeEvent(input$subset_trigger, {
        req(rv$content_primary$content_prepared)
        
        # Run modal function
        showModal(show_subset_modal())
      })
      
      observeEvent(input$save_subset_one, {
        # require content_prepared to be present (i.e. required submitted data)
        req(rv$content_primary$content_prepared)
        
        # Saving whatever is currently in the mutated/filtered/edited datatable
        in_datatable <- rv$content_primary$content_prepared[input[["content_prepared_DT_rows_all"]], ]
        
        # initializing reactive value list of data and characteristics
        subset_one <- shiny::reactiveValues(data = in_datatable, 
                                           is_stop_rm = F, 
                                           is_tokenised = F, 
                                           content_prepared = in_datatable)
        
        # saving data and characteristics list to main rv list
        rv$subset_one <- subset_one
        
        showNotification(paste("Subset I created successfully."), 
                         duration = 5, 
                         type = "message")
      })
      
      observeEvent(input$save_subset_two, {
        # require content_prepared to be present (i.e. required submitted data)
        req(rv$content_primary$content_prepared)
        
        # Saving whatever is currently in the mutated/filtered/edited datatable
        in_datatable <- rv$content_primary$content_prepared[input[["content_prepared_DT_rows_all"]], ]
        
        # initializing reactive value list of data and characteristics
        subset_two <- shiny::reactiveValues(data = in_datatable, 
                                            is_stop_rm = F, 
                                            is_tokenised = F, 
                                            content_prepared = in_datatable)
        
        # saving data and characteristics list to main rv list
        rv$subset_two <- subset_two
        
        showNotification(paste("Subset II created successfully.."), 
                         duration = 5, 
                         type = "message")
      })
      

      # Creating datatable 2 of content parameterised after stop word removal
      output$content_prepared_DT_2 <- DT::renderDataTable(
        # server = F, 
        {
        
        validate(need(rv$content_prepared_display_2, 
                      "Submit files in the Text Selector tab to continue."))
        
        # rv$content_primary$content_prepared,
        DT::datatable(
          rv$content_prepared_display_2,
          # extensions = "Buttons",
          options = list(
            paging = TRUE,
            pageLength = 7,
            scrollX = TRUE,
            scrollY = TRUE,
            dom = "rtip", # B
            columnDefs =
              list(
                list(
                  targets = '_all',
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data.length > 200 ?",
                    "'<span title=\"' + data + '\">' + data
                          .substr(0, 200) + '...</span>' : data;", "}"
                  )
                )
              ) # ,
            # buttons = list(list(extend = 'csv',
            #                     text = "Download data (.csv)")
            #                )
          ),
        selection = "none",
        rownames = FALSE
        )
        
      })


      ###########################
      ####### Stemming ##########
      ###########################
      # Again need to select dataset, choose column to stem
      # Need to enforce that column selected is tokenised before stemming
      
      ## Dataset chooser - update possible dataset list with current available subsets
      observe({
        # Create list of possible datasets to select from
        req(rv$content_primary)
        potential_sets_tokenise <- list("Primary data")
        
        if(!is.null(rv$subset_one)){
          potential_sets_tokenise[length(potential_sets_tokenise) + 1] <- "Subset one"
        }
        if(!is.null(rv$subset_two)){
          potential_sets_tokenise[length(potential_sets_tokenise) + 1] <- "Subset two"
        }
        
        # updating select input list to contain datasets available
        updateSelectInput(session, "data_to_stem",
                          choices = potential_sets_tokenise,
                          selected= potential_sets_tokenise[1])
      })
      
      ## Column chooser 
      # When a dataset is chosen to perform stemming on, need to load which 
      # columns available in that dataset
      observe({
        req(rv$content_primary$content_prepared)
        
        # creating list of available columns
        # subsets' content_prepared is initialized on save of subsets
        colnames <- switch(input$data_to_stem, 
                           "Primary data" = 
                             colnames(rv$content_primary$content_prepared), 
                           "Subset one" = colnames(rv$subset_one$content_prepared), 
                           "Subset two" = colnames(rv$subset_two$content_prepared)
        )
        updateSelectInput(session, "col_name_to_stem",
                          choices = colnames[-1],
                          selected= colnames[2])
      })
      
      # Rendering whatever dataset selected in datatable display
      observeEvent(input$data_to_stem, {
        req(rv$content_primary$content_prepared)
        rv$content_prepared_display_2 <- 
          switch(input$data_to_stem, 
                 "Primary data" =  rv$content_primary$content_prepared, 
                 "Subset one" = rv$subset_one$content_prepared, 
                 "Subset two" = rv$subset_two$content_prepared
          )
      })

      ## Actually stemming data ##
      stemming_alert <- function(){
        shinyalert(
          title = "Stemming failed: data error",
          text = "Stemming requires text data which is tokenised into words. \n \n Tokenise the selected column into words using the Tokenisation panel to ensure text data contains one word per row.",
          size = "s", 
          closeOnEsc = TRUE, closeOnClickOutside = TRUE,
          html = FALSE, type = "info",
          showConfirmButton = TRUE, showCancelButton = FALSE,
          confirmButtonText = "Dismiss",
          confirmButtonCol = "#4169E1",
          timer = 0, imageUrl = "", animation = TRUE
        )
      }
      
      observeEvent(input$submit_stemming, {
        
        # First check if selected column is tokenised
        # Using rv$content_prepared_display_2, which is just
        # the currently selected dataset previous assigned w switch above
        req(rv$content_primary) # always require something submitted
        req(rv$content_prepared_display_2)
        
        # Checking if selected data and column is tokenised into words
        is_word_tokenised <- try(tokenised_enough(rv$content_prepared_display_2, 
                                                input$col_name_to_stem, 
                                                check_if_words = T))
        
        # if not word tokenised return alert
        if("try-error" %in% class(is_word_tokenised)){
          stemming_alert()
          return()
        } else if(!is_word_tokenised){
          stemming_alert()
          return()
        }
        
        req(is_word_tokenised)
        
        # For selected dataset, create pre-stemmed dataset in case of undo, 
        # and then stem with stem_data() function in utils.R
        if(input$data_to_stem == "Primary data"){
          
          # Save copy of content_prepared in case of undo
          rv$content_primary$content_pre_stemmed <- 
            rv$content_primary$content_prepared
          
          # Use stem_data function to stem
          content_stemmed <- try(stem_data(rv$content_primary$content_prepared, 
                                             col_name = input$col_name_to_stem))
          
          # if not word tokenised return alert
          if("try-error" %in% class(is_word_tokenised)){
            stemming_alert()
            return()
          } else if(!is_word_tokenised){
            stemming_alert()
            return()
          }
          
          # Set stemmed data to rv$content_primary$content_prepared and 
          # set is_stemmed = TRUE
          rv$content_primary$content_prepared <- content_stemmed
          rv$content_primary$is_stemmed <- TRUE
          rv$content_prepared_display_2 <- content_stemmed
          
        } else if(input$data_to_stem == "Subset one"){
          
          req(rv$subset_one$content_prepared)
          
          # Save copy of content_prepared in case of undo
          rv$subset_one$content_pre_stemmed <- rv$subset_one$content_prepared
          
          # Stemming
          content_stemmed <- try(stem_data(rv$subset_one$content_prepared, 
                                             col_name = input$col_name_to_stem))
          
          # if not word tokenised return alert
          if("try-error" %in% class(is_word_tokenised)){
            stemming_alert()
            return()
          } else if(!is_word_tokenised){
            stemming_alert()
            return()
          }
          
          rv$subset_one$content_prepared <- content_stemmed
          rv$subset_one$is_stemmed <- TRUE
          rv$content_prepared_display_2 <- content_stemmed
          
        } else if(input$data_to_stem == "Subset two"){
          
          req(rv$subset_two$content_prepared)
          
          # Save copy of content_prepared in case of undo
          rv$subset_two$content_pre_stemmed <- rv$subset_one$content_prepared
          
          # Stemming
          content_stemmed <- try(stem_data(rv$subset_two$content_prepared, 
                                           col_name = input$col_name_to_stem))
          
          if("try-error" %in% class(content_stemmed)){
            stemming_alert()
            return()
          }
          
          rv$subset_two$content_prepared <- content_stemmed
          rv$subset_two$is_stemmed <- TRUE
          rv$content_prepared_display_2 <- content_stemmed
        }

      }) # end observe event submit stemming

      # Undo stemming - set chosen dataset back to pre-stemmed save
      observeEvent(input$undo_stemming, {
        
        if(input$data_to_stem == "Primary data"){
          
          req(rv$content_primary$content_pre_stemmed)
          rv$content_primary$content_prepared <- rv$content_primary$content_pre_stemmed
          rv$content_primary$is_stemmed <- FALSE
          rv$content_prepared_display_2 <- rv$content_primary$content_prepared
          
        } else if(input$data_to_stem == "Subset one"){
          
          req(rv$subset_one$content_pre_stemmed)
          rv$subset_one$content_prepared <- rv$subset_one$content_pre_stemmed
          rv$subset_one$is_stemmed <- FALSE
          rv$content_prepared_display_2 <- rv$subset_one$content_prepared
          
        } else if(input$data_to_stem == "Subset two"){
          
          req(rv$subset_two$content_pre_stemmed)
          rv$subset_two$content_prepared <- rv$subset_two$content_pre_stemmed
          rv$subset_two$is_stemmed <- FALSE
          rv$content_prepared_display_2 <- rv$subset_two$content_prepared
        }
      }) # end observe event undo stemming
      
      
      ###########################
      #### Downloading data #####
      ###########################
      # Rendering a download button with downloadHandler()
      output$download_parameterised_csv <- downloadHandler(
        filename = function() {
          paste("Prepared_text_data.csv")
        },
        content = function(file) {
          write_delim(as.data.frame(rv$content_prepared_display_2), file,
                      delim = ","
          )
        }
      )
      
      output$download_parameterised_tsv <- downloadHandler(
        filename = function() {
          paste("Prepared_text_data.tsv")
        },
        content = function(file) {
          write_tsv(as.data.frame(rv$content_prepared_display_2), file)
        }
      )
      
      
      ######################
      ## Revert all edits ##
      ######################
      observeEvent(input$revert_all, {
        req(rv$content_primary$data)
        content_primary <- shiny::reactiveValues(data = 
                                                   rv$content_primary$data,
                                                 is_stop_rm = FALSE, 
                                                 is_tokenised = FALSE, 
                                                 is_filtered = FALSE,
                                                 is_mutated = FALSE,
                                                 content_prepared = 
                                                   rv$content_primary$data,
                                                 content_edited = 
                                                   rv$content_primary$data,
                                                 content_primary_tf_idf = NULL)
        rv$content_primary <- content_primary
        
        req(rv$subset_one$data)
        # initializing reactive value list of data and characteristics
        subset_one <- shiny::reactiveValues(data = 
                                              rv$subset_one$data, 
                                            is_stop_rm = F, 
                                            is_tokenised = F, 
                                            content_prepared = 
                                              rv$subset_one$data)
        rv$subset_one <- subset_one
        
        req(rv$subset_two$data)
        # initializing reactive value list of data and characteristics
        subset_two <- shiny::reactiveValues(data = 
                                              rv$subset_two$data, 
                                            is_stop_rm = F, 
                                            is_tokenised = F, 
                                            content_prepared = 
                                              rv$subset_two$data)
        rv$subset_two <- subset_two
        
      }) # end observe revert all
      
    }
  )
} # end text preparation server logic

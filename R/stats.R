
######## Statistical Analysis Tab Module ############

#### Stats Tab Module UI ####
statsUI <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      wellPanel(
        h1("0X | Statistical Analysis"),
        em("Perform exploratory data analyses, regression analyses and model checking through a customisable workflow."),
      ),
      
      sidebarLayout(
        sidebarPanel(width = 3, 
                     
                     h3("Data selection"),
                     p("Select a dataset to perform statistical analyses on:"),
                     selectInput(ns("content_stats_choose"), 
                                 label = NULL, 
                                 choices = list(
                                   "Raw submitted data" = "submitted", 
                                   "Parameterised data" = "parameterised", 
                                   "Tf-idf data" = "tfidf"
                                 )),
                     hr(),
                     checkboxInput(ns("show_eda_summary_stats"), 
                                   label = "Show summary statistics")
          
        ),
        
        mainPanel(width = 9,
          
          wellPanel(
            h3("Selected Data"),
            
            uiOutput(ns("content_stats_display"))
          ),

          
        ),
      ),
      
      wellPanel(
        h2("Exploratory Data Analysis"),
        hr(),
        
        fluidRow(
          column(width = 8,
                 
                 wellPanel(
                   plotOutput(ns("eda_hist_plot"))
                 ),
                 
                 
          ), # end column
          column(width = 4, 
                 
                 h3("Histogram"),
                 
                 # Widgets to plot hist and customize
                 selectInput(ns("eda_hist_var"), 
                             label = "Select a variable:", 
                             choices = 
                               list("N/A" = 
                                      "N/A")
                 ),
                 
                 # Widgets to customise breaks in histogram
                 selectInput(ns("eda_hist_breaks"), 
                             label = "Choose a method to calculate the number of breaks:", 
                             choices = list(
                               "Sturges (default)" = "Sturges", 
                               "Scott" = "Scott", 
                               "Freedman-Diaconis" = "Freedman-Diaconis", 
                               "Custom" = "Custom"
                             )),
                 conditionalPanel(paste0("input['", ns("eda_hist_breaks"), 
                                         "'] == 'Custom' "), 
                                  numericInput(ns("eda_hist_breaks_custom"), 
                                               label = "Enter a custom number of breaks:", 
                                               value = 30)
                 ),
                 
                 checkboxInput(ns("include_density_curve"),
                               label = "Overlay fitted density curve", 
                               value = FALSE),
                 
                 hr(),
                 h4("Transformations"),
                 p("Apply a mathematical transformation to the selected variable. Saving a transformation will append a column to your data, not overwrite it."),
                 
                 selectInput(ns("eda_transformation"), 
                             label = NULL, 
                             choices = list(
                               "None" = "none", 
                               "Natural Logarithm (ln)" = "log", 
                               "Square Root" = "sqrt", 
                               "Arc-sine" = "arcsine",
                               "Reciprocal" = "recip"
                             ), 
                             selected = "none"),
                 
                 actionButton(ns("eda_save_transformed_var"),
                              label = "Save transformation",
                              class = "btn-success"),
                 
                 
                
            ), # end column
 
        ), # end fluid row
        
        hr(),
        
        fluidRow(
          column(width = 3, 
                 
                 h3("Normality tests"), 
                 p("Explore whether a selected variable approximates a theoretical normal distribution."),
                 hr(),
                 p("Select a variable:"),
                 
                 # Widgets to plot hist and customize
                 selectInput(ns("eda_normality_var"), 
                             label = NULL, 
                             choices = 
                               list("N/A" = 
                                      "N/A")
                 ),
                 
                 radioButtons(ns("normality_test_type"), 
                              label = "Type of statistical test:", 
                              choices = list("Shapiro-Wilk (default)" = "shapiro", 
                                             "Anderson-Darling (n > 500)" = "anderson"))
                 
                 ), # end normality testing column
          column(width = 5, 
                 plotOutput(ns("eda_qqplot"))
                 ),
          column(width = 4, 
                 h3("Statistical Test for Normality"),
                 p("Used to test whether the selected variable approximates a normal distribution. A significantly low p-value indicates the null hypothesis of normality can be rejected."),
                 hr(),
                 
                 # Render the different statistical tests for normality depending on user
                 # selection
                 conditionalPanel(
                   condition = paste0("input['", ns("normality_test_type"), 
                                      "'] == 'shapiro' "),
                   verbatimTextOutput(ns("eda_shapiro_wilk")),
                 ),
                 conditionalPanel(
                   condition = paste0("input['", ns("normality_test_type"), 
                                      "'] == 'anderson' "),
                   verbatimTextOutput(ns("eda_anderson_darling"))
                 ),
                 
                 ),
        ) # end fluid row

      ), # end EDA well panel
      
      fluidRow(
        
        
          box(title = NULL, 
              status = "success", 
              solidHeader = T, 
              collapsible = T,
              width = 12,
              
              plotlyOutput(ns("corr_plot")),
              
          ), # end box
          
          
      ), # end corr plot fluid row
      
      wellPanel(
        h2("Regression Analysis"), 
        p("Perform regression analysis to gain a deeper understanding of the relationships between your variables and to uncover patterns and insights in your data."),
        em("Disclaimer: Not all regression models make sense, and the validity of results from a regression model depends on several factors. Please consider carefully whether the models you fit are appropriate for your data. "),
        hr(), 
        
        fluidRow(
            column(
              width = 4,
              
              h3("Regression Type"),
              p("Select a type of regression to show a brief summary of the method and perform an analysis"),
              
              hr(),
              radioButtons(ns("regression_type"), 
                           label = NULL,
                           choices = list("Linear" = "linear", 
                                          "Logistic" = "logistic", 
                                          "Poisson" = "poisson", 
                                          "Mixed effects" = "mixed"
                           )
              ),
              
            ), # end column
            
            column(
              width = 8,
              box(
                status = "primary",
                width = 12,
                
                # Here render UI based on radio button selection
                conditionalPanel(
                  condition = paste0("input['", ns("regression_type"), 
                                     "'] == 'linear' "),
                  uiOutput(ns("info_linear"))
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("regression_type"), 
                                     "'] == 'logistic' "),
                  uiOutput(ns("info_logistic"))
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("regression_type"), 
                                     "'] == 'poisson' "),
                  uiOutput(ns("info_poisson"))
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("regression_type"), 
                                     "'] == 'mixed' "),
                  uiOutput(ns("info_mixed"))
                ),
                # Add other regression types
                
              ), # end information box
            ), # end column
          
        ), # end regression type & info fluid row
        
        hr(), 
        
        fluidRow(
          column(width = 4, 
                 # Here render UI based on radio button selection
                 # Rendering same ui to choose vars for liner, log 
                 # and poisson, however different ui for mixed 
                 # regression to choose random & fixed effects
                 uiOutput(ns("perform_regression"))
                 ),
          
          column(width = 8, 
                   wellPanel(
                     tabsetPanel(
                       tabPanel("Tabularised summary", uiOutput(ns("lin_log_pois_res"))), 
                       tabPanel("Raw model summary", 
                                verbatimTextOutput(ns("reg_result_raw")))
                     ),
                   )
                 )
          
        ) # end fluid row
      ), # end regression analysis well panel 
      
      verbatimTextOutput(ns("test_stats")),
      
      wellPanel(
        
        h2("Model Checking"),
        p("Check your model meets required assumptions regarding residuals and variances."),
        
        wellPanel(
          plotOutput(ns("residual_plots"))
        ),
        
      ), # end model checking well panel
      
    
      wellPanel(
        fluidRow(
        h2("Analysis of Variances (ANOVA)"),
        p("Perform simple and factorial ANOVA based on previously specified formulas."),
        
        column(width = 12, 
               wellPanel(
                 tabsetPanel(
                   tabPanel("Tabularised summary", 
                            verbatimTextOutput(ns("anova_res_raw"))
                   ), 
                   tabPanel("Raw model summary", 
                            # verbatimTextOutput(ns("anova_res_raw"))
                   )
                 ),
               )
          ),
          
        ), # end anova well panel
      )
      
      
    ) # end fluidPage
  ) # end tagList
} # end module UI

  
### Stats Tab Module Server-side logic ####
statsServer <- function(id, rv = rv){
  moduleServer(
    id, 
    function(input, output, session){
      
      #### Information on regression types ####
      output$info_linear <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Linear Regression"),
          em("Information regarding linear regression...")
        )
      })

      output$info_logistic <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Logistic Regression"),
          em("Information regarding logistic regression...")
        )
      })
      
      output$info_poisson <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Poisson Regression"),
          em("Information regarding poisson regression...")
        )
      })
      
      output$info_mixed <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Mixed Effects Regression"),
          em("Information regarding mixed effects regression..."), 
          tags$a(href = "https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html")
        )
      })

      #### Creating contents_stats to be used  ####
      # Content stats is rendered to be whatever is selected in drop-down
      content_stats <- reactive({
        req(rv$content)
        
        if(input$content_stats_choose == "submitted"){
          rv$content %>%
            clean_names()
        } else if(input$content_stats_choose == "parameterised"){
          req(rv$content_parameterised)
          rv$content_parameterised %>%
            clean_names()
        } else if(input$content_stats_choose == "tfidf"){
          req(rv$content_parameterised)
          req(rv$content_tf_idf)
          req(rv$is_tokenised)
          rv$content_parameterised %>%
            full_join(rv$content_tf_idf, by = c('ID', 'Token')) %>%
            dplyr::select(-`Term freq.`, -`Mean token count`, -`Corpus token count`) %>%
            clean_names()
        }
      })
      
      # Ensuring rv$content_stats updated to content_stats() created above
      observe({
        req(content_stats())
        rv$content_stats <- content_stats()
      })
      
      # Table output for raw content stats
      output$content_stats_raw <- DT::renderDataTable(
        rv$content_stats,
        options = list(
          paging = TRUE,
          pageLength = 5,
          scrollX = TRUE,
          scrollY = TRUE,
          dom = 'frtip',
          columnDefs =
            list(
              list(targets = 1,
                   render = JS("function(data, type, row, meta) {",
                               "return type === 'display' && data.length > 100 ?",
                               "'<span title=\"' + data + '\">' + data
                        .substr(0, 100) + '...</span>' : data;","}"
                   )
              )
            )
        ),
        selection = 'none',
        rownames = FALSE
      )
      
      # Glimpse from finalfit package used to show summary stats
      eda_summary_stats <- reactive({
        req(rv$content_stats)
        finalfit::ff_glimpse(rv$content_stats)
      })
      
      # Saving only the summary stats for continuous variables
      observe({
        req(eda_summary_stats())
        rv$content_stats_summary <- eda_summary_stats()$Continuous
      })
      
      # Table output for summary stats for content stats selected
      output$content_stats_summary <- renderDataTable({
        DT::datatable(rv$content_stats_summary,
                      rownames=FALSE, 
                      colnames = c("", "N", "Missing N", "Missing %", "Mean", "SD", "Min", "25% quartile", "Median", "75% quartile", "Max"),
                      options = list(dom = 't', 
                                     scrollX = TRUE, 
                                     paging=FALSE,
                                     fixedColumns = list(leftColumns = 1, 
                                                         rightColumns = 0),
                                     searching = FALSE
                      )
        )
      })
      
      # Generating ui for content stats display. If user selects checkbox to 
      # show summary stats, this will be rendered instead of raw data
      output$content_stats_display <- renderUI({
        validate(
          need(
            rv$content_stats,
            "Select and submit text data to continue"
          ),
          
          errorClass = "validation-red")
        
        ns <- NS(id)
        raw <- tagList(
          DT::dataTableOutput(ns("content_stats_raw"))
        )
        
        summary_stats <- tagList(
          
          DT::dataTableOutput(ns("content_stats_summary"))
        )
        
        # Rendering either raw or summary of content stats depending on input
        if(input$show_eda_summary_stats){
          summary_stats
        } else {
          raw
        }
        
      })
      #### Exploratory data analysis ####
      
      # Updates dropdown for histogram to column names as soon 
      # as content stats rendered
      # Rendering last variable in content_stats since this is most
      # likely what user wants to see, not ID (not numeric)
      # Also means that when transformation saved it is rendered immediately
      observeEvent(!is.null(rv$content_stats), {
        req(rv$content_stats)
        updateSelectInput(session, "eda_hist_var",
                          choices = colnames(rv$content_stats), 
                          selected = 
                            colnames(rv$content_stats)[ncol(rv$content_stats)])
        
        updateSelectInput(session, "eda_normality_var",
                          choices = colnames(rv$content_stats), 
                          selected = 
                            colnames(rv$content_stats)[ncol(rv$content_stats)])
      })

      # Plotting a histogram of selected variable

      # Token frequency plot for entire corpus
      # Creating frequency plot, where users can customise min
      # frequency of a token to be displayed on the plot
      eda_hist_plot_breaks <- reactive({
        switch(input$eda_hist_breaks,
               "Sturges" = "Sturges",
               "Scott" = "Scott",
               "Freedman-Diaconis" = "Freedman-Diaconis",
               "Custom" = input$eda_hist_breaks_custom)
      })
      
      # Reactive value for the variable to plot, with or without transformation
      # Passing to apply_transformation function defined in utils.R which 
      # applies the given transformation based on inputted variable and returns
      # transformed vector
      eda_hist_plot_var <- reactive({
        selected_var <- input$eda_hist_var
        transformation <- input$eda_transformation
        
        apply_transformation(var = rv$content_stats[[selected_var]],
                             transformation = transformation)
        
      })

      # Validate statements to ensure numeric value selected for histogram
      # Not neccessary to check if tokenised anymore
      eda_hist_plot <- reactive({
        req(rv$content_stats)

        validate(
          need(
            is_numeric(rv$content_stats[[input$eda_hist_var]]),
            "Select a numeric variable."
          ),
          errorClass = "validation-red")
        
        validate(
          need(
            eda_hist_plot_var(),
            "Transformation invalid."
          ),
          errorClass = "validation-red")

        # Creating histogram. If include density curve is selected, 
        # the probability / proportion density is 
        # plotted instead of frequency.
        hist(eda_hist_plot_var(),
             breaks = eda_hist_plot_breaks(),
             main = paste0("Histogram of ", input$eda_hist_var),
             xlab = input$eda_hist_var,
             col = "white",
             prob = input$include_density_curve)

      }) # end reactive val eda hist plot

      # If checkbox to include density curve is checked, the 
      # lines function is used to render on top of histogram
      output$eda_hist_plot <- renderPlot({
        req(eda_hist_plot())

        eda_hist_plot()
        
        if(input$include_density_curve == TRUE){
          
          validate(
            need(
              try(density(eda_hist_plot_var())),
              "Density curve cannot be plotted due to missing values."
            ),
            errorClass = "validation-red")
          
          lines(density(eda_hist_plot_var()),
                col = "royalblue")
        }
      })
      
      # When button clicked to save transformation...
      observeEvent(input$eda_save_transformed_var, {
        req(rv$content_stats)
        req(eda_hist_plot_var())
        
        # Create copy of content_stats to avoid rv$content_stats calling itself
        temp <- cbind(rv$content_stats, 
                      "Transformed var" = eda_hist_plot_var())
        
        rv$content_stats <- temp
        
        # Renaming column name to transformation and variable used 
        colnames(rv$content_stats)[which(names(rv$content_stats) == "Transformed var")] <- paste0(input$eda_transformation, "_", input$eda_hist_var)
        
      }) # end observe event save transformed var
      
      eda_normality_var <- reactive({
        req(rv$content_stats)
        var <- rv$content_stats[[input$eda_normality_var]]
        
        validate(
          need(
            is_numeric(var),
            "Select a numeric variable."
          ),
          errorClass = "validation-red")
        
        var
        
      })
      
      # Rendering qqplot
      output$eda_qqplot <- renderPlot({
        
        req(eda_normality_var())
        qqnorm(eda_normality_var())
        qqline(eda_normality_var())
        
      })
      
      #### Normality statistical tests ####
      # Saving shapiro wilk test result
      shapiro_wilk_res <- reactive({
        req(eda_normality_var())
        shapiro.test(eda_normality_var())
      })
      
      # Saving Anderson-darling test result
      anderson_darling_res <- reactive({
        req(eda_normality_var())
        nortest::ad.test(eda_normality_var())
      })
      
      # saving shapiro result in reactive value list
      observe({
        rv$shapiro_wilk_res <- shapiro_wilk_res()
        rv$anderson_darling_res <- anderson_darling_res()
      })
      
      # Rendering table with shapiro-wilk result
      output$eda_shapiro_wilk <- renderPrint({
        rv$shapiro_wilk_res
      })
      
      # Rendering table with Anderson-darling result
      output$eda_anderson_darling <- renderPrint({
        rv$anderson_darling_res
      })
      
      
      #### Perform regression ####
      # RenderUI to only render when linear, log or poisson regression option chosen
      output$perform_regression <- renderUI({
        req(rv$content_stats)
        
        ns <- NS(id)
        tagList(
          
          # Select input for dependent (response) variable
          varSelectInput(ns("dependent"), 
                         label = "Dependent (response) variable:", 
                         rv$content_stats, 
                         selected = NULL, 
                         multiple = FALSE
          ),
          
          # Select input for independent predictor variables
          varSelectInput(ns("independents"), 
                         label = "Independent (predictor) variable(s):", 
                         rv$content_stats, 
                         selected = NULL, 
                         multiple = TRUE
                         ),
          
          checkboxInput(ns("include_interaction_lin_log_pois"), 
                        label = "Include interaction (optional)"
                        ),
          
          # If include interaction box ticked, 
          conditionalPanel(
            condition = paste0("input['", ns("include_interaction_lin_log_pois"), 
                               "'] == true "),
            varSelectInput(ns("interactions"), 
                           label = "Interacting variables:", 
                           rv$content_stats, 
                           selected = NULL, 
                           multiple = TRUE
            ),
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("regression_type"), 
                               "'] == 'mixed' "),
            uiOutput(ns("perform_mixed"))
          ),

          
          p("Selected regression formula:"),
          verbatimTextOutput(ns("formula_reg")),
          
          # Action button to actually perform regression once clicked
          actionButton(ns("submit_regression"), 
                       label = "Submit", 
                       class = "btn-success")
  
        )
      }) # end renderUI 
      
      
      # Creating formulas for regression using formula() function defined in R/utils.R
      # returns formula to pass into perform_regression() function also in utils.R
      formula_reg <- reactive({
        req(rv$content_stats)
        req(input$dependent)
        req(input$independents)
        
        # Remove interactions if not ticked to include
        if(input$include_interaction_lin_log_pois == FALSE){
          interactions <- NULL
        } else {
          interactions <- input$interactions
        }
        
        # If checkbox to include interactions not checked, don't include
        if(input$regression_type == "mixed"){
          formula(input$regression_type, input$dependent, input$independents, 
                  interactions, input$mixed_random_slopes_1, 
                  input$mixed_random_intercept_1, input$mixed_random_slopes_2, 
                  input$mixed_random_intercept_2)
        } else {
          formula(input$regression_type, input$dependent, input$independents, 
                  interactions)
        }
      })
      
      observe({
        req(formula_reg())
        rv$formula_reg <- formula_reg()
      })
      
      # Render text to display selected formula
      output$formula_reg <- renderPrint({
        formula_reg()
      })
      
      # When submit regression clicked, validate that regression is 
      # valid with try in need statement.
      # If error produced, indicate invalid with rv$is_valid_regression
      # If no error, save result of perform_regression() in rv
      observeEvent(input$submit_regression, {
        req(rv$content_stats)
        req(rv$formula_reg)
        
        # First checking that regression can happen w try statement, 
        reg_res <- try(perform_regression(rv$formula_reg, 
                                     input$regression_type,
                                     rv$content_stats))
        # If the try-catch produced error (class try-error), 
        # indicate with rv$is_valid_regression <- FALSE for global use 
        # If valid then indicate opposite 
        if("try-error" %in% class(reg_res)){
          rv$is_valid_regression <- FALSE
          rv$reg_error <- reg_res
        } else { # regression is valid so save summary too
          rv$is_valid_regression <- TRUE
          rv$reg_result <- reg_res
          rv$reg_summary_raw <- 
            summary(perform_regression(rv$formula_reg, 
                                       input$regression_type,
                                       rv$content_stats))
        }

      }) # end observe event 
      
      output$lin_log_pois_res <- renderUI({
        
        req(rv$content_stats)
        # req(!is.null(rv$reg_result_res))
        
        ns <- NS(id)
        
        # UI to be displayed if valid regression submitted
        validUI <- tagList(
          # Rendering results table as html output since using kable
          htmlOutput(ns("reg_result"))
        )
        
        # UI to be displayed if regression produces error (invalid)
        invalidUI <- tagList(
          p("Submitted regression resulted in the following error:"),
          verbatimTextOutput(ns("reg_error"))
        )
        
        validate(
          need(!is.null(rv$is_valid_regression), 
               "Submit a valid formula to begin.")
        )
        
        if(rv$is_valid_regression){
          return(validUI)
        } else if(!rv$is_valid_regression){
          return(invalidUI)
        } 
        
      })
      
      # Render text with regression error if present
      output$reg_error <- renderPrint({
        rv$reg_error
      })
      
      # Render datatable of regression results
      # Using tab_model to generate, then returning HTML
      output$reg_result <- renderText({
        
        table <- sjPlot::tab_model(rv$reg_result, 
                                   show.fstat = TRUE, 
                                   show.aic = TRUE, 
                                   show.se = TRUE, 
                                   # show.std = TRUE, 
                                   show.stat = TRUE,
                                   CSS = list(
                                     css.thead = 'font-size: 14px;', 
                                     css.summary = 'font-weight: bold;'
                                   ))
        HTML(table$knitr)
      })
      
      # Rendering raw model output
      output$reg_result_raw <- renderPrint({
        
        validate(
          need(!is.null(rv$is_valid_regression), 
               "Submit a valid formula to begin.")
          )
        
        if(rv$is_valid_regression){
          rv$reg_summary_raw
        } else {
          rv$reg_error
        }
        
      })

      
      #### Perform mixed regression ####
      # RenderUI to only render when linear, log or poisson regression option chosen
      output$perform_mixed <- renderUI({
        req(rv$content_stats)
        
        ns <- NS(id)
        tagList(
          
          fluidRow(
            column(width = 6, 
                   # Var selector for random effects
                   varSelectInput(ns("mixed_random_slopes_1"), 
                                  label = "Random slope(s)", 
                                  rv$content_stats, 
                                  selected = NULL, 
                                  multiple = TRUE
                   ),
                  ), 
            column(width = 6, 
                   # Var selector for random effects
                   varSelectInput(ns("mixed_random_slopes_2"), 
                                  label = "Secondary random slope(s) (optional)", 
                                  rv$content_stats, 
                                  selected = NULL, 
                                  multiple = TRUE)
                   ),
          ),
            
          fluidRow(
            column(width = 6, 
                   
                   # Var selector for grouping var
                   varSelectInput(ns("mixed_random_intercept_1"), 
                                  label = "Random intercept", 
                                  rv$content_stats, 
                                  selected = NULL, 
                                  multiple = FALSE)
                   ),
            
            column(width = 6, 
                   # varSelectize used to enable nothing to be selected
                   varSelectizeInput(ns("mixed_random_intercept_2"),
                                     label = 
                                       "Secondary random intercept 
                                     (optional)",
                                     rv$content_stats,
                                     selected = NULL,
                                     options = list(create = TRUE, 
                                                    maxItems = 1))
             )
             
            )
          )
      }) # end renderUI 
      
      
      
      # Creating correlation matrix for heatmap correlation plot
      output$corr_plot <- renderPlotly({
        
        validate(
          need(!is.null(rv$content_stats), 
               "Submit text files to continue.")
        )
        
        # Filtering to only include numeric data
        content_stats_numeric <- rv$content_stats %>%
          dplyr::select(where(is.numeric))
        
        # Creating correlation matrix
        corr <- round(cor(content_stats_numeric), 3)

        # Reshaping data to has three cols (pair of vars & cor coef)
        cor_melted <- melt(corr)
        
        # Generating heat map with ggplot
        ggheatmap <- ggplot(cor_melted, aes(Var2, Var1, fill = value))+
          geom_tile()+
          geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
          scale_fill_gradient2(low = "royalblue", high = "darkred", mid = "white", 
                               midpoint = 0, limit = c(-1,1), space = "Lab", 
                               name="Pearson\nCorrelation") +
          theme_minimal() + # minimal theme
          theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                           size = 12, hjust = 1)
                ) +
          ggtitle("Correlation matrix heatmap")

        # Converting ggplot object into plotly object to be interactive
        ggplotly(ggheatmap)
      })
      
      
      # Generating residual plots for fitted model
      output$residual_plots <- renderPlot({
        
        validate(
          need(!is.null(rv$is_valid_regression),
               "Submit a valid formula to begin."),
          need(rv$is_valid_regression, 
               "Submit regression to begin.")
        )
        
        if(input$regression_type == "mixed"){
          plot(fitted(rv$reg_result), residuals(rv$reg_result), 
               xlab = "Fitted", ylab = "Residuals")
          abline(h = 0, lty = 2)
          lines(smooth.spline(fitted(rv$reg_result), residuals(rv$reg_result)))
        } else {
          autoplot(rv$reg_result) + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())
        }

      })
      
      output$anova_res_raw <- renderPrint({
        req(rv$formula_reg)
        req(rv$is_valid_regression)
        
        aov_res <- aov(rv$formula_reg, data = rv$content_stats)
        
        print(aov_res)
      })
      
      
      
      
    }) # end module server inner function
} # end module server
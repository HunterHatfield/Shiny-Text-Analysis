
######## Statistical Analysis Tab Module ############


waiting_screen_stats <- tagList(
  spin_flower(),
  h4("Performing regression analysis..."),
  em("For datasets with over 10,000 observations, regression analysis may take a couple of minutes.")
) 

#### Stats Tab Module UI ####
statsUI <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      useWaiter(),
      
      wellPanel(
        h1("04 | Statistical Analysis"),
        em("<<Perform exploratory data analyses, regression analyses and model checking through a customisable workflow.>>"),
      ),
      
      sidebarLayout(
        sidebarPanel(width = 3, 
                     
                     h3("Select data"),
                     p("Select a dataset for analyses:"),
                     selectInput(ns("content_stats_choose"), 
                                 label = NULL, 
                                 choices = list(
                                   "Primary data (raw)" = "submitted", 
                                   "Primary data (prepared)" = "primary_prepared"
                                 )),
                     actionButton(ns("content_stats_choose_button"), 
                                  label = "Select", 
                                  class = "btn-primary"),
                     hr(),
                     
                     h4("Filter"),
                     p("Use the interactive table to filter text data.  Save/undo filters data using the buttons below."),
                     
                     fluidRow(
                       column(width = 6, 
                              actionButton(ns("submit_filters"), 
                                           label = "Submit", 
                                           class = "btn-primary")
                       ),
                       column(width = 6, 
                              actionButton(ns("undo_filters"), 
                                           label = "Undo", 
                                           class = "btn-danger")
                       )
                     ),
                     
                     hr(),
                     
                     h4("Summary statistics"),
                     p("Render summary statistics for numeric data below."),
                     checkboxInput(ns("show_eda_summary_stats"), 
                                   label = "Show summary statistics")
        ),
        
        mainPanel(width = 9,
                  fluidRow(
                    box(title = NULL, 
                        status = "primary", 
                        solidHeader = T,
                        collapsible = T,
                        width = 12,
                        
                        uiOutput(ns("content_stats_display")),
                        hr(class = "hr-blank")
                    ),
                  ),
        ),
      ),
      
      wellPanel(
        h2("Exploratory Data Analysis"),
        hr(),
        
        fluidRow(
          column(width = 8,
                 
                 wellPanel(
                   plotOutput(ns("eda_hist_plot_display")) %>%
                     withSpinner()
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
                 plotOutput(ns("eda_qqplot")) %>%
                   withSpinner()
          ),
          column(width = 4, 
                 h3("Statistical Test for Normality"),
                 p("<<Test for approximation of a variable to the theoretical normal distribution. Results containing a significantly low p-value may indicate the null hypothesis of normality could be rejected.>>"),
                 hr(),
                 
                 # Render the different statistical tests for normality depending on user
                 # selection
                 conditionalPanel(
                   condition = paste0("input['", ns("normality_test_type"), 
                                      "'] == 'shapiro' "),
                   verbatimTextOutput(ns("eda_shapiro_wilk")) %>%
                     withSpinner(),
                 ),
                 conditionalPanel(
                   condition = paste0("input['", ns("normality_test_type"), 
                                      "'] == 'anderson' "),
                   verbatimTextOutput(ns("eda_anderson_darling")) %>%
                     withSpinner()
                 ),
                 
          ),
        ) # end fluid row
        
      ), # end EDA well panel
      
      fluidRow(
        box(title = NULL, 
            status = "primary", 
            solidHeader = T, 
            collapsible = T,
            width = 12,
            
            plotlyOutput(ns("corr_plot")) %>%
              withSpinner(),
            
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
            
            h3("Select regression type"),
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
              collapsible = T,
              solidHeader = T,
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
                 h3("Specify formula"),
                 uiOutput(ns("perform_regression"))
          ),
          
          column(width = 8, 
                 wellPanel(
                   tabsetPanel(
                     tabPanel("Tabularised summary", 
                              uiOutput(ns("lin_log_pois_res")) %>%
                                withSpinner()
                      ),
                     tabPanel("Raw model summary", 
                              verbatimTextOutput(ns("reg_result_raw")) %>%
                                withSpinner()
                     )
                   ),
                 )
          )
          
        ) # end fluid row
      ), # end regression analysis well panel 
      
      wellPanel(
        
        h2("Model Checking"),
        p("Check your model meets required assumptions regarding residuals and variances."),
        
        wellPanel(
          plotOutput(ns("residual_plots")) %>%
            withSpinner()
        ),
        
      ), # end model checking well panel
      
      
      wellPanel(
        
        h2("Further Statistical Testing"),
        p("<<Perform analyses of variances, t-tests, and/or Chi-squared tests based on previously specified formulas.>>"),
        hr(),
        
        navlistPanel(
          tabPanel(
            
            "Analysis of Variances (ANOVA)", 
            h3("Analysis of Variances (ANOVA)"),
            p("The ANOVA test is used to determine if there is a statistically significant difference between the means of three or more independent groups."),
            hr(),
            
            tabsetPanel(
              tabPanel("Tabularised summary", 
                       tableOutput(ns("anova_table")) %>%
                         withSpinner()
              ), 
              tabPanel("Raw model summary", 
                       verbatimTextOutput(ns("anova_res_raw")) %>%
                         withSpinner()
              )
            ),
          ), 
          tabPanel("T-test", 
                   h3("T-test"), 
                   p("A t-test is a statistical test used to determine if their is a significant difference in the means of two independent groups being compared."),
                   hr(),
                   tabsetPanel(
                     tabPanel("Tabularised summary", 
                              htmlOutput(ns("t_test_res_table")) %>%
                                withSpinner()
                     ), 
                     tabPanel("Raw model summary", 
                              verbatimTextOutput(ns("t_test_res_raw")) %>%
                                withSpinner()
                     )
                   ),
          ), 
          tabPanel("Chi-square test", 
                   h3("Chi-square test"), 
                   p("The Chi-square test is used to determine whether there is a significant association between two categorical variables."),
                   hr(),
                   uiOutput(ns("perform_chisq"))
                   
          ), 
          tabPanel("Non-parametric tests", 
                   
                   h3("Non-parametric Tests"),
                   p("Non-parametric tests do not require any assumptions to be made about data being analyzed, so can be used to analyze data that is not normally distributed or does not meet the assumptions of parametric tests."),
                   hr(),
                   
                   tabsetPanel(
                     tabPanel("Kruskal-Wallis", 
                              
                              h3("Kruskal-Wallis"), 
                              p("The Kruskal-Wallis test is used to test whether there are statistically significant differences between the medians of two or more independent groups. As this test is non-parametric, it does not assume normality of residuals.
The parametric equivalent of the Kruskal-Wallis is the one-way ANOVA."),

verbatimTextOutput(ns("kw_res")) %>%
  withSpinner()

                     ), 
tabPanel("Welch one-way test", 
         
         h3("Welch one-way test"),
         p("The Welch one-way test is used to test if there is a statistically significant difference in the means of two or more groups. This test is useful when the assumption of equal variances between groups is violated."),
         p("The parametric equivalent of the Welch one-way test is the independent samples t-test."),
         
         verbatimTextOutput(ns("welch_res")) %>%
           withSpinner(),
         
),

tabPanel("Mann-Whitney U Test", 
         
         h3("Mann-Whitney U Test"),
         p("The Mann-Whitney U Test (also know as the Wilcoxon rank-sum test) is used to compare two independent samples which are not normally distributed and of small size (n < 30)"),
         p("The parametric equivalent of this test is the two-sample independent t-test."),
         
         verbatimTextOutput(ns("mann_wil_res")) %>%
           withSpinner(),
         
),


                   ), # end tabset panel of non-para tests

          ), 
        ),
      )


    ) # end fluidPage
  ) # end tagList
} # end module UI


### Stats Tab Module Server-side logic ####
statsServer <- function(id, rv = NULL){
  moduleServer(
    id, 
    function(input, output, session){
      
      #######################
      ### Dataset chooser ###
      #######################
      observe({
        # Create list of possible datasets to select from
        req(rv$content_primary)
        potential_sets_stats <- list("Primary data (raw)", "Primary data (prepared)", 
                                     "Visualised")
        if(!is.null(rv$subset_one)){
          potential_sets_stats[length(potential_sets_stats) + 1] <- "Subset one (original)"
          potential_sets_stats[length(potential_sets_stats) + 1] <- "Subset one (prepared)"
        }
        if(!is.null(rv$subset_two)){
          potential_sets_stats[length(potential_sets_stats) + 1] <- "Subset two (original)"
          potential_sets_stats[length(potential_sets_stats) + 1] <- "Subset two (prepared)"
        }
        
        updateSelectInput(session, "content_stats_choose",
                          choices = potential_sets_stats,
                          selected= potential_sets_stats[1])
      })
      
      # Content stats is rendered to be whatever is selected in drop-down
      # Rendering whatever dataset selected in datatable display. Needs to be
      # rendered as diff sets selected before select button clicked
      
      observeEvent(input$content_stats_choose_button, {
        req(rv$content_primary$data)
        
        if(input$content_stats_choose == "Primary data (raw)"){
          req(rv$content_primary$data)
          rv$content_stats <- try(rv$content_primary$data %>%
                                    clean_names())
        } else if(input$content_stats_choose == "Primary data (prepared)"){
          req(rv$content_primary$content_prepared)
          rv$content_stats <- try(rv$content_primary$content_prepared %>%
                                    clean_names())
        } else if(input$content_stats_choose == "Subset one (original)"){
          req(rv$subset_one$data)
          rv$content_stats <- try(rv$subset_one$data %>%
                                    clean_names())
        } else if(input$content_stats_choose == "Subset two (original)"){
          req(rv$subset_two$data)
          rv$content_stats <- try(rv$subset_two$data %>%
                                    clean_names())
        } else if(input$content_stats_choose == "Subset one (prepared)"){
          req(rv$subset_one$content_prepared)
          rv$content_stats <- try(rv$subset_one$content_prepared %>%
                                    clean_names())
        } else if(input$content_stats_choose == "Subset two (prepared)"){
          req(rv$subset_two$content_prepared)
          rv$content_stats <- try(rv$subset_two$content_prepared %>%
                                    clean_names())
        } else if(input$content_stats_choose == "Visualised"){
          req(rv$content_to_visualise_DT)
          req(rv$content_to_visualise$content_tf_idf)
          rv$content_stats <- try(rv$content_to_visualise$content_tf_idf %>%
                                    #full_join(rv$content_to_visualise$plotting_data, by = c('ID', 'Token')) %>%
                                    dplyr::select(-`Term freq.`, -`Mean token count`, -`Corpus token count`) %>%
                                    clean_names())
        }
        
        # Saving content_stats to display in DT
        req(rv$content_stats)
        rv$content_stats_DT <- rv$content_stats
      })
      
      ###################
      ## Summary stats ##
      ###################
      eda_summary_stats <- reactive({
        req(rv$content_stats)
        try(finalfit::ff_glimpse(rv$content_stats))
      })
      observe({
        req(eda_summary_stats())
        rv$content_stats_summary <- try(eda_summary_stats()$Continuous)
      })
      
      # Removed this - user must select dataset from dropdown and confirm to enforce
      # confirmation and avoid confusion surrounding users assuming the shown data has
      # been selected
      # observe({
      #   req(rv$content_primary$data)
      #   
      #   rv$content_stats_DT <- 
      #     switch(input$content_stats_choose, 
      #            "Primary data (raw)" =  try(rv$content_primary$data), 
      #            "Primary data (prepared)" = try(rv$content_primary$content_prepared),
      #            "Subset one (original)" = try(rv$subset_one$data),
      #            "Subset two (original)" = try(rv$subset_one$data),
      #            "Subset one (prepared)" = try(rv$subset_one$content_prepared),
      #            "Subset two (prepared)" = try(rv$subset_one$content_prepared),
      #            "Visualised" = try(rv$content_to_visualise$content_tf_idf)
      #     )
      # })
      
      # Table output for raw content stats
      output$content_stats_raw <- DT::renderDataTable({
        validate(need(rv$content_stats_DT, "Selected data not available."))
        
        DT::datatable(
          rv$content_stats_DT,
          filter = 'top',
          options = list(
            paging = TRUE,
            pageLength = 6,
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
      })
      
      # Table output for summary stats for content stats selected
      output$content_stats_summary <- 
        DT::renderDataTable({
          
          validate(need(rv$content_stats_summary,
                        "Summary statistics only available for 
                        submitted numeric variables."))
          
          DT::datatable(rv$content_stats_summary,
                        rownames=FALSE, 
                        colnames = c("", "N", "Missing N", "Missing %", "Mean", "SD", 
                                     "Min", "25% quartile", "Median", "75% quartile", "Max"),
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
        validate(need(rv$content_stats,
                      "Select and submit text data to continue"))
        
        ns <- NS(id)
        raw <- tagList(
          DT::dataTableOutput(ns("content_stats_raw")) %>%
            withSpinner()
        )
        
        summary_stats <- tagList(
          DT::dataTableOutput(ns("content_stats_summary")) %>%
            withSpinner(),
          em("Summary statistics only available for submitted numeric variables.")
        )
        
        # Rendering either raw or summary of content stats depending on input
        if(input$show_eda_summary_stats){
          summary_stats
        } else {
          raw
        }
      })
      
      #### Filtering data #####
      # When submit filters clicked, subset content_stats by filtered rows
      observeEvent(input$submit_filters, {
        req(rv$content_stats)
        
        # Saving. pre-filtered then subsetting content_stats by filters
        rv$pre_filtered_content_stats <- rv$content_stats
        rv$content_stats <- 
          rv$pre_filtered_content_stats[input[["content_stats_raw_rows_all"]], ]
        rv$content_stats_DT <- rv$content_stats
      })
      
      # When undo filters button clicked, revert content_stats to pre-filtered
      observeEvent(input$undo_filters, {
        req(rv$pre_filtered_content_stats) # require pre-filtered content 
        rv$content_stats <- rv$pre_filtered_content_stats # revert to original 
        rv$content_stats_DT <- rv$pre_filtered_content_stats
        rv$pre_filtered_content_stats <- NULL
      })
      
      
      #### Exploratory data analysis ####
      
      # Updates dropdown for histogram to column names as soon 
      # as content stats rendered
      # Rendering last variable in content_stats since this is most
      # likely what user wants to see, not ID (not numeric)
      # Also means that when transformation saved it is rendered immediately
      observe({
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
        req(rv$content_stats)
        selected_var <- input$eda_hist_var
        transformation <- input$eda_transformation
        
        apply_transformation(var = rv$content_stats[[selected_var]],
                             transformation = transformation)
      })
      
      # Validate statements to ensure numeric value selected for histogram
      # Not necessary to check if tokenised anymore
      eda_hist_plot <- reactive({
        req(rv$content_stats)
        req(eda_hist_plot_var())
        req(is.numeric(rv$content_stats[[input$eda_hist_var]]))
        req(eda_hist_plot_breaks())
        
        
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
      output$eda_hist_plot_display <- renderPlot({
        
        validate(need(rv$content_stats, "Upload and submit numeric data.",
        ))
        validate(need(is.numeric(rv$content_stats[[input$eda_hist_var]]),
                      "Select a numeric variable."
        ))
        validate(need(eda_hist_plot_var(),"Transformation invalid."))
        validate(need(eda_hist_plot(),"Histogram could not be rendered. Ensure you have uploaded and selected valid numeric data."))
        
        eda_hist_plot()
        
        if(input$include_density_curve == TRUE){
          
          validate(
            need(
              try(density(eda_hist_plot_var())),
              "Density curve cannot be plotted due to missing values."
            ))
          
          lines(density(eda_hist_plot_var()),
                col = "royalblue")
        }
      })
      
      ##### When button clicked to save transformation... ####
      observeEvent(input$eda_save_transformed_var, {
        req(rv$content_stats)
        req(eda_hist_plot_var())
        
        if(input$eda_transformation != "none"){
          # Create copy of content_stats to avoid rv$content_stats calling itself
          temp <- cbind(rv$content_stats, 
                        "Transformed var" = eda_hist_plot_var())
          rv$content_stats <- temp
          
          # Renaming column name to transformation and variable used 
          colnames(rv$content_stats)[which(names(rv$content_stats) == "Transformed var")] <- paste0(input$eda_transformation, "_", input$eda_hist_var)
        }
        
      }) # end observe event save transformed var
      
      # Isolating and saving the selected variable to use in normality testing
      eda_normality_var <- reactive({
        req(rv$content_stats)
        var <- rv$content_stats[[input$eda_normality_var]]
        
        # ensuring variable is numeric
        validate(need(is.numeric(var),
                      "Select a numeric variable."))
        
        var
      })
      
      #### Rendering qqplot ####
      eda_qqplot_res <- reactive({
        req(eda_normality_var())
        qqnorm(eda_normality_var())
        qqline(eda_normality_var())
      })
      
      output$eda_qqplot <- renderPlot({
        validate(need(rv$content_stats, "Upload and submit numeric data.",
        ))
        validate(need(is.numeric(eda_normality_var()),
                      "Select a numeric variable."))
        eda_qqplot_res()
      })
      
      #### Normality statistical tests ####
      # Saving shapiro wilk test result
      shapiro_wilk_res <- reactive({
        req(eda_normality_var())
        req(length(eda_normality_var()) < 5000)
        
        try(shapiro.test(eda_normality_var()))
      })
      
      # Saving Anderson-darling test result
      anderson_darling_res <- reactive({
        
        req(eda_normality_var())
        req(length(eda_normality_var()) > 7)
        
        try(nortest::ad.test(eda_normality_var()))
      })
      
      
      # Rendering table with shapiro-wilk result
      output$eda_shapiro_wilk <- renderPrint({
        validate(need(shapiro_wilk_res(), 
                      "Invalid input for normality test selected."))
        shapiro_wilk_res()
      })
      
      # Rendering table with Anderson-darling result
      output$eda_anderson_darling <- renderPrint({
        validate(need(anderson_darling_res(), 
                      "Invalid input for normality test selected."))
        anderson_darling_res()
      })
      
      # Creating non-plotly heatmap for pdf/word reports
      corr_plot_non_plotly <- reactive({
        req(rv$content_stats)
        
        # Filtering to only include numeric data
        content_stats_numeric <- rv$content_stats %>%
          dplyr::select(where(is.numeric)) %>%
          dplyr::select(-std_dev_token_count)
        
        # Creating correlation matrix
        # & reshaping data to has three cols (pair of vars & cor coef)
        cor_melted <- melt(round(cor(content_stats_numeric), 3))
        
        # Generating heat map with ggplot
        ggplot(cor_melted, aes(Var2, Var1, fill = value))+
          geom_tile()+
          geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
          labs(x = NULL, y = NULL) +  # no axis titles
          scale_fill_gradient2(low = "royalblue", high = "darkred", mid = "white",
                               midpoint = 0, limit = c(-1,1), space = "Lab",
                               name="Pearson\nCorrelation") +
          theme_minimal() + # minimal theme
          theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                           size = 12, hjust = 1)
          ) +
          ggtitle("Correlation matrix heatmap")
      })
      
      # Creating correlation heatmap
      corr_plot <- reactive({
        validate(
          need(!is.null(rv$content_stats),
               "Submit text files to continue."),
          need(length(rv$content_stats) > 2,
               "Not enough finite observations to produce heatmap."),
        )
        
        # Filtering to only include numeric data
        content_stats_numeric <- rv$content_stats %>%
          dplyr::select(where(is.numeric)) %>%
          dplyr::select(-std_dev_token_count)
        
        # Creating correlation matrix
        corr <- round(cor(content_stats_numeric), 3)
        
        # Reshaping data to has three cols (pair of vars & cor coef)
        cor_melted <- melt(corr)
        
        # Generating heat map with ggplot
        ggheatmap <- ggplot(cor_melted, aes(Var2, Var1, fill = value))+
          geom_tile()+
          geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
          labs(x = NULL, y = NULL) +  # no axis titles
          scale_fill_gradient2(low = "royalblue", high = "darkred", mid = "white",
                               midpoint = 0, limit = c(-1,1), space = "Lab",
                               name="Pearson\nCorrelation") +
          theme_minimal() + # minimal theme
          theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                           size = 12, hjust = 1)
          ) +
          ggtitle("Correlation matrix heatmap")
        
        req(ggheatmap)
        
        # Converting ggplot object into plotly object to be interactive
        try(ggplotly(ggheatmap))
      })
      
      # Rendering corr_plot object with plotly
      output$corr_plot <- renderPlotly({
        req(corr_plot())
        corr_plot()
      })
      
      # Saving corr_plot objects to rv list 
      observe({
        req(corr_plot())
        rv$corr_plot <- corr_plot()
        req(corr_plot_non_plotly())
        rv$corr_plot_non_plotly <- corr_plot_non_plotly()
      })
      
      
      #### Information on regression types ####
      output$info_linear <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Linear Regression"),
          p("<<Linear regression is a statistical method used to predict a continuous outcome variable based on one or more predictor variables.>>")
        )
      })
      
      output$info_logistic <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Logistic Regression"),
          em("<<Information regarding logistic regression...>>")
        )
      })
      
      output$info_poisson <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Poisson Regression"),
          em("<<Information regarding poisson regression...>>")
        )
      })
      
      output$info_mixed <- renderUI({
        ns <- NS(id)
        tagList(
          h3("Mixed Effects Regression"),
          em("<<Information regarding mixed effects regression...>>"), 
          # tags$a(href = "https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html")
        )
      })
      
      
      #### Perform regression ####
      # RenderUI to only render when linear, log or poisson regression option chosen
      output$perform_regression <- renderUI({
        validate(need(rv$content_stats, 
                      "Submit and select a valid dataset to perform regression analyses."))
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
      
      
      # Render text to display selected formula
      output$formula_reg <- renderPrint({
        validate(need(formula_reg(), "Invalid formula."))
        formula_reg()
      })
      
      # When submit regression clicked, validate that regression is 
      # valid with try in need statement.
      # If error produced, indicate invalid with rv$is_valid_regression
      # If no error, save result of perform_regression() in rv
      observeEvent(input$submit_regression, {
        req(rv$content_stats)
        req(formula_reg()) # req(rv$formula_reg) 
        
        # waiter_show(html = waiting_screen_stats, color = "royalblue")
        
        rv$formula_reg <- formula_reg()
        
        # First checking that regression can happen w try statement, 
        reg_model <- try(perform_regression(rv$formula_reg, 
                                            input$regression_type,
                                            rv$content_stats))
        # If the try-catch produced error (class try-error), 
        # indicate with rv$is_valid_regression <- FALSE for global use 
        # If valid then indicate opposite 
        if("try-error" %in% class(reg_model)){
          rv$is_valid_regression <- FALSE
          rv$reg_error <- reg_model
        } else { # regression is valid so save summary too
          rv$is_valid_regression <- TRUE
          rv$reg_model <- reg_model
          # rv$reg_summary_raw <- 
          #   summary(perform_regression(rv$formula_reg, 
          #                              input$regression_type,
          #                              rv$content_stats))
          rv$reg_summary_raw <- summary(reg_model)
        }
        
        # waiter_hide()
        
      }) # end observe event 
      
      output$lin_log_pois_res <- renderUI({
        
        validate(need(rv$content_stats,  "Select a valid dataset to perform regression analyses."))
        
        ns <- NS(id)
        
        # UI to be displayed if valid regression submitted
        validUI <- tagList(
          # Rendering results table as html output since using kable
          htmlOutput(ns("reg_model"))
        )
        
        # UI to be displayed if regression produces error (invalid)
        invalidUI <- tagList(
          em("Submitted regression model is invalid and could not be run. Check the raw model summary for more information."),
          verbatimTextOutput(ns("reg_error")) %>%
            withSpinner()
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
        req(rv$reg_error)
        return(rv$reg_error)
      })
      
      # Render datatable of regression results
      # Using tab_model to generate, then returning HTML
      output$reg_model <- renderText({
        validate(need(rv$content_stats, "Select a valid dataset."))
        req(rv$reg_model)
        table <- sjPlot::tab_model(rv$reg_model,
                                   show.fstat = TRUE,
                                   show.aic = TRUE,
                                   show.se = TRUE,
                                   # show.std = TRUE,
                                   show.stat = TRUE,
                                   CSS = list(
                                     css.thead = 
                                       'font-size: 14px;',
                                     css.summary = 
                                       'font-weight: bold;'
                                   ))
        
        rv$reg_result_table <- table # saving table in rv list
        
        return(HTML(table$knitr))
      })
      
      
      # Rendering raw model output
      output$reg_result_raw <- renderPrint({
        validate(need(rv$content_stats, "Select a valid dataset."),
                 need(rv$reg_summary_raw, "Invalid regression model."))
        validate(need(!is.null(rv$is_valid_regression), 
                      "Invalid formula submitted."))
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
      
      # Generating residual plots for fitted model
      residual_plots <- reactive({
        validate(
          need(rv$is_valid_regression,
               "Submit a valid regression model to render model diagnostic plots.")
        )
        req(rv$reg_model)
        
        if(input$regression_type == "mixed"){
          plot(fitted(rv$reg_model), residuals(rv$reg_model),
               xlab = "Fitted", ylab = "Residuals")
          abline(h = 0, lty = 2)
          lines(smooth.spline(fitted(rv$reg_model), residuals(rv$reg_model)))
        } else {
          autoplot(rv$reg_model) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
        }
      })
      
      # Displaying residual plots
      output$residual_plots <- renderPlot({
        validate(need(residual_plots(), "Submit a valid regression model to render residual plots."))
        
        req(residual_plots())
        residual_plots()
      })
      
      observe({
        req(residual_plots())
        rv$residual_plots <- residual_plots()
      })
      
      # Performing anova
      anova_res <- reactive({
        req(rv$formula_reg)
        req(rv$is_valid_regression)
        try(rstatix::anova_test(rv$reg_model))
      })
      observe({
        req(anova_res())
        rv$anova_res <- anova_res()
      })
      # Rendering raw anova output
      output$anova_res_raw <- renderPrint({
        validate(
          need(rv$anova_res, paste0("ANOVA could not be performed given the current (or lack of) model formula.", 
                                    rv$anova_res))
        )
        rv$anova_res
      })
      
      # Render datatable of ANOVA results
      # Using tab_model to generate, then returning HTML
      anova_table <- reactive({
        req(rv$anova_res)
        try(get_anova_table(rv$anova_res))
      })
      observe({
        rv$anova_table <- anova_table()
      })
      output$anova_table <- renderTable({
        validate(
          need(rv$anova_table, paste0("ANOVA could not be performed given the current (or lack of) model formula.", 
                                      rv$anova_table))
        )
        rv$anova_table
      })
      
      # Performing t-test
      ttest_res <- reactive({
        req(rv$content_stats)
        req(rv$formula_reg)
        tt_formula <- try(as.formula(rv$formula_reg))
        
        try(rstatix::t_test(data = rv$content_stats, 
                            formula = tt_formula))
      })
      
      # Saving t-test result
      observe({
        req(ttest_res())
        rv$ttest_res <- ttest_res()
      })
      
      # Rendering raw t-test result
      output$t_test_res_raw <- renderPrint({
        # Check that the t-test result is not an error
        validate(need(nrow(rv$content_stats) < 5000, "T-test cannot be rendered for models with over 5,000 observations."))
        validate(
          need(rv$ttest_res, paste0("T-test could not be performed given the current (or lack of) model formula.", 
                                    rv$ttest_res))
        )
        
        
        rv$ttest_res
      })
      
      # Rendering formatted t-test result, rendering HTML with kable
      output$t_test_res_table <- renderText({
        # Check that the t-test result is not an error
        validate(need(nrow(rv$content_stats) < 5000, "T-test cannot be rendered for models with over 5,000 observations."))
        validate(
          need(rv$ttest_res, paste0("T-test could not be performed given the current (or lack of) model formula.", 
                                    rv$ttest_res))
        )
        req(rv$ttest_res)
        table <- kable(rv$ttest_res) %>% 
          kable_styling(latex_options = 'striped')
        
        rv$ttest_table <- table
        
        table
      })
      
      #### Chi-square testing ####
      output$perform_chisq <- renderUI({
        ns <- NS(id)
        tagList(
          
          fluidRow(
            column(width = 4, 
                   varSelectInput(ns("chisq_var1"), 
                                  label = "Select variables for testing:", 
                                  rv$content_stats, 
                                  selected = NULL),
                   varSelectInput(ns("chisq_var2"), 
                                  label = NULL, 
                                  rv$content_stats, 
                                  selected = NULL),
                   actionButton(ns("submit_chisq"),
                                label = "Perform test",
                                class = "btn-success"),
            ),
            column(width = 8, 
                   verbatimTextOutput(ns("chisq_res")) %>%
                     withSpinner()
            ),
          ),
          
        )
      })
      
      # When perform chisq button clicked, perform with selected variables
      observeEvent(input$submit_chisq, {
        req(rv$content_stats[[input$chisq_var1]])
        req(rv$content_stats[[input$chisq_var2]])
        # Try handler to ensure conversion to factors does not produce a fatal error
        var1 <- try(as.factor(rv$content_stats[[input$chisq_var1]]))
        var2 <- try(as.factor(rv$content_stats[[input$chisq_var2]]))
        
        # Ensure that var1 & var2 are both categorical/can be converted to categorical
        
        req(var1)
        req(var2)
        
        # Creating table of selected vars
        chisq_table <- try(table(var1, var2))
        
        # Saving reactive variable
        chisq_res <- reactive({
          req(chisq_table)
          try(stats::chisq.test(chisq_table))
        })
        
        # Saving in reactive list to eventually export to report generation
        rv$chisq_res <- chisq_res()
        
      })
      
      output$chisq_res <- renderPrint({
        validate(need(rv$chisq_res, 
                      "Submit categorical variables to begin."))
        rv$chisq_res
      })
      
      #### Kruskal-Wallis test ####
      kw_res <- reactive({
        req(rv$content_stats)
        req(rv$formula_reg)
        formula <- try(as.formula(rv$formula_reg))
        try(kruskal.test(formula, data = rv$content_stats))
      })
      
      ##################
      observe({
        req(kw_res())
        rv$kw_res <- kw_res()
      })
      
      output$kw_res <- renderPrint({
        validate(need(rv$kw_res, 
                      "Submit valid data and formula to perform Kruskal-Wallis test."))
        rv$kw_res
      })
      
      #### Welch one-way test ####
      welch_res <- reactive({
        req(rv$content_stats)
        req(rv$formula_reg)
        formula <- try(as.formula(rv$formula_reg))
        res <- try(oneway.test(formula, data = rv$content_stats, var.equal = FALSE))
        
        if("try-error" %in% class(res)){
          paste0("Invalid variables selected. Error returned: ", res)
        } else {
          res
        }
      })
      
      observe({
        req(welch_res())
        rv$welch_res <- welch_res()
      })
      
      output$welch_res <- renderPrint({
        validate(need(rv$welch_res, "Submit valid data and formula to perform Welch one-way test."))
        
        rv$welch_res
      })
      
      
      #### Mann-whitney/Wilcoxon ####
      mann_wil_res <- reactive({
        req(rv$content_stats)
        req(rv$formula_reg)
        formula <- try(as.formula(rv$formula_reg))
        res <- try(wilcox.test(formula, data = rv$content_stats))
        
        if("try-error" %in% class(res)){
          paste0("Invalid variables selected. Error returned: ", res)
        } else {
          res
        }
        
      })
      
      observe({
        req(mann_wil_res())
        rv$mann_wil_res <- mann_wil_res()
      })
      
      output$mann_wil_res <- renderPrint({
        validate(need(rv$mann_wil_res, "Submit valid data and formula to perform Mann-Whitney test."))
        
        rv$mann_wil_res
      })
      
      
    }) # end module server inner function
} # end module server



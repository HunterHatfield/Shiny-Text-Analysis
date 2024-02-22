######## Home page ###########

###############################

waiting_screen_home <- tagList(
  spin_flower(),
  h4("Initialising Text Analysis App...")
) 

homeUI <- function(id){
  
  ns <- NS(id)
  
  tagList(

    fluidPage( 
      useWaiter(),
      waiterPreloader(html = waiting_screen_home, color = "royalblue"),

      h1("Kia ora.", style = "font-size: 10vw;"),
      p("This interactive app is designed with researchers in mind and is tailored for mining, exploring, and visualizing text data. Dive in with guided steps or explore freely using the sidebar to your left."),
      
      em("Developed at the Department of Linguistics, University of Otago by Hunter Hatfield & Emelia Hogg. Built with ShinyR (v1.7.0)."),
      
      fluidRow(style = "margin: 1vw;",
        
        fluidRow(
          column(12, {
            h1("1| Import", style = "color: royalblue;")
          }),
        ),
        fluidRow(
          column(12,{
            p("Import text data using various methods available for seamless integration into the analysis process.")
          }),
        ),
        fluidRow(
          column(12, {
            h1("2| Analyse", style = "color: royalblue;")
          }),
        ),
        fluidRow(
          column(12,{
            p("Understand your text data and generate insights with interactive visualisations and statistical tests.")
          }),
        ),
        
        fluidRow(
          column(12, {
            h1("3| Report", style = "color: royalblue;")
          }),
        ),
        fluidRow(
          column(12,{
            p("Generate a report of your findings in a format of your choice. Currently supported formats include PDF (.pdf), Microsoft Word (.docx), and HTML (.html).")
          }),
        ),
      
      ),
      
      hr(class = "hr-blank"),
      
      fluidRow(
    
        column(6, {
          actionButton(ns("start"), 
                       label = "Start", 
                       class ="btn-success")
        }),
        
      )
    )
  )
}

###### SERVER ######
homeServer <- function(id, parent){
  moduleServer(
    id, 
    function(input, output, session = parent){
      
      # When start is clicked, take user to text selector tab
      # In order to do this, the session passed to the update function
      # is the parent session of the app.R server. 
      # In app.R see that when homeServer() is called, the parent
      # session is passed in. Then in the function above session = 
      # parent. 
      observeEvent(input$start, {
        updateTabItems(parent, "sidebar", "textSelectorTab")
      }) # end observe submit
      
    }
  )
} # end upload server

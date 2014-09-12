shinyUI(fluidPage(
  titlePanel("Some things I do - tracked with TagTime"), 
  
  sidebarLayout( 
    sidebarPanel(
      p("Display time use patterns from my ", 
        a("TagTime", href = "http://messymatters.com/tagtime/"), 
        " log file."),
    
      selectInput("tag", 
        label = "Pick a category to look at:",
        choices, 
        selected = choices[1]), 
      
      helpText(strong("Description: "), textOutput("description")), 
      
      br(), 
      
      uiOutput("daterangeUI"),  
      
      selectInput(inputId = "n.bins",
                  label = "Number of bins in the histogram:",
                  choices = c(10, 15, 20, 30, 50),
                  selected = 15),
      
      sliderInput("bandwidth", 
                  label = "Density estimate smoothness:",
                  min = 0.3, 
                  max = 1, 
                  value = 0.85)
    ),
  
    mainPanel(plotOutput("hist"))
  )
))

shinyUI(fluidPage(
  titlePanel("Some things I do - tracked with TagTime"), 
  
  sidebarLayout( 
    sidebarPanel(
      p("Display time use patterns from my ", 
        a("TagTime", href = "http://messymatters.com/tagtime/"), 
        " log file."),
    
      selectInput("cat", 
        label = "Pick a category to look at:",
        choices = catnames, 
        selected = catnames[1]), 
      
      helpText(strong("Description: "), textOutput("description")), 
      
      uiOutput("daterangeUI"), 
      
      br(), 
      strong("Graph options:"), 
      
      conditionalPanel(
        condition = "input.tabID == 1", 
        selectInput(inputId = "n.bins",
                    label = "Number of bins in the histogram",
                    choices = c(10, 15, 20, 30, 50),
                    selected = 15),
        
        sliderInput("bandwidth", 
                    label = "Density estimate smoothness",
                    min = 0.3, 
                    max = 1, 
                    value = 0.85)
      ), 
      
      conditionalPanel(
        condition = "input.tabID == 2", 
        selectInput("midnight", 
                    label = "Custom midnight", 
                    choices = 0:6, 
                    selected = 0)
      ), 
      
      conditionalPanel(
        condition = "input.tabID == 3", 
        selectInput("xcat", 
                    label = "Independent variable",
                    choices = catnames, 
                    selected = catnames[2]), 
        checkboxInput("jitter", 
                      label = "Add jittering", 
                      value = TRUE)
      )
    ),
  
    # mainPanel(plotOutput("hist"))
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Trend", plotOutput("hist"), value = 1), 
        tabPanel("Matrix", plotOutput("matrix"), value = 2), 
        tabPanel("Scatterplot", plotOutput("xy"), value = 3), 
        id = "tabID")
    )
  )
))

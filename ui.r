shinyUI(fluidPage(
  titlePanel("Some things I do"), 
  
  sidebarLayout(
    sidebarPanel(
      p("Display time use patterns from my ", 
        a("TagTime", href = "http://messymatters.com/tagtime/"), 
        " log file. (See my ", a("GitHub repo", 
          href = "http://github.com/alexschell/tagtime-trends"), 
        " for the code.)"), 
      
      selectInput("cat", 
        label = "Pick a category to look at:", 
        choices = catnames, 
        selected = catnames[1]), 
      
      helpText(textOutput("description")), 
      
      uiOutput("daterangeUI"), 
      
      br(), 
      strong("Graphing options:"), 
      br(), 
      
      conditionalPanel(
        condition = "input.tabID == 1", 
        selectInput("n.bins.hist", 
                    label = "Number of bins in the histogram", 
                    choices = c(10, 15, 20, 30, 50), 
                    selected = 15), 
        sliderInput("bandwidth.hist", 
                    label = "Density estimate smoothness", 
                    min = 0.3, 
                    max = 1, 
                    value = 0.85), 
        radioButtons("units.hist", 
                     label = "Units to display:", 
                     choices = c("Hours per week", "Hours per day"), 
                     selected = "Hours per week")
      ), 
      
      conditionalPanel(
        condition = "input.tabID == 3", 
        selectInput("xcat", 
                    label = "Independent variable", 
                    choices = catnames, 
                    selected = catnames[15]), 
        checkboxInput("jitter", 
                      label = "Add jittering", 
                      value = TRUE), 
        selectInput("ccat", 
                    label = "Conditioning variable for trellis plot (optional)", 
                    choices = c("(None)", catnames), 
                    selected = "(None)")
      ), 
      
      conditionalPanel(
        condition = "input.tabID == 4", 
        radioButtons("weekend", 
                     label = "", 
                     choices = c("Weekdays only", 
                                 "Weekends only", 
                                 "All days"), 
                     selected = "All days"), 
        sliderInput("bandwidth.tod", 
                    label = "Density estimate smoothness",
                    min = 0.3, 
                    max = 0.6, 
                    value = 0.45)
      ), 
      
      conditionalPanel(
        condition = "input.tabID == 5", 
        checkboxInput("ordered.week", 
                     label = "Order chronologically within bins", 
                     value = FALSE)
      ), 
      
      conditionalPanel(
        condition = "input.tabID == 2 | input.tabID == 4 | input.tabID == 5", 
        selectInput("midnight", 
                    label = "Custom midnight", 
                    choices = 0:6, 
                    selected = 0)
      )
    ), 
  
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Trend", plotOutput("hist"), value = 1), 
        tabPanel("Matrix", plotOutput("matrix"), value = 2), 
        tabPanel("Scatterplot", plotOutput("scatter"), value = 3), 
        tabPanel("Time of day", plotOutput("timeofday"), value = 4), 
        tabPanel("Weekdays", plotOutput("week"), value = 5), 
        id = "tabID")
    )
  )
))

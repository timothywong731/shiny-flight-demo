library(shiny)
library(lubridate)
library(nycflights13)
library(dplyr)
library(ggplot2)
library(nycflights13)
library(rpart)
library(plotly)

myUniqueOrigin <- flights$origin %>% unique() %>% sort()
myUniqueDest <- flights$dest %>% unique() %>% sort()
myUniqueCarrier <- flights$carrier %>% unique() %>% sort()

myFlightModel <- rpart(dep_delay ~
                         months(date) + weekdays(date) + 
                         hour + origin + dest + carrier,
                       data = flights %>% mutate(date = make_date(year, month, day)),
                       na.action = na.omit,
                       control = rpart.control(cp = 0.0005))

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Flight Delay Calculator"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      dateInput("myDate",
                "Departure Date",
                value = make_date(2019, 1, 1)),
      
      radioButtons("myOrigin",
                   "Depart from",
                   choices = myUniqueOrigin, 
                   selected = myUniqueOrigin[1]),
      
      selectInput("myDest", 
                  "Arrive at",
                  choices = myUniqueDest, 
                  selected = myUniqueDest[1]),
      
      selectInput("myCarrier",
                  "Carrier",
                  choices = myUniqueCarrier, 
                  selected = myUniqueCarrier[1])
    ),
    
    # Show a plot
    mainPanel(
      plotlyOutput("myPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  myNewFlightReactive <- reactive({
    tibble(date = input$myDate,
           hour= 0:23,
           origin=input$myOrigin,
           dest = input$myDest, 
           carrier= input$myCarrier)})
  
  output$myPlot <- renderPlotly({
    
    myNewFlightDelay <- predict(myFlightModel, myNewFlightReactive())
    
    tibble(hour=0:23, dep_delay = myNewFlightDelay) %>%
      ggplot(aes(x=hour, y=dep_delay)) +
      geom_col() +
      scale_y_continuous(limits = c(-10,60)) + 
      labs(x="Planned Departure Hour",
           y="Expected Departure Delay (Minutes)",
           title=sprintf("%s flight from %s to %s in %s (%s)",
                         input$myCarrier,
                         input$myOrigin,
                         input$myDest,
                         input$myDate %>% months(TRUE),
                         input$myDate %>% weekdays(TRUE)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

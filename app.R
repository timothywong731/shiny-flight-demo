#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(nycflights13)
library(dplyr)
library(ggplot2)
library(forcats)
library(nycflights13)
library(rpart)

myUniqueOrigin <- flights$origin %>% unique() %>% sort()
myUniqueDest <- flights$dest %>% unique() %>% sort()
myUniqueCarrier <- flights$carrier %>% unique() %>% sort()

myFlightModel <- rpart(dep_delay ~
                         months(date) + weekdays(date) + 
                         hour + origin + dest + carrier,
                       data = flights %>% mutate(date = make_date(year, month, day)),
                       na.action = na.omit,
                       control = rpart.control(cp = 0.0005))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Flight Delay Calculator"),
   
   # Sidebar with a slider input for number of bins 
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
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  myNewFlighReactive <- reactive({
    tibble(date = input$myDate,
           hour= 0:23,
           origin=input$myOrigin,
           dest = input$myDest, 
           carrier= input$myCarrier)})
  
   output$distPlot <- renderPlot({
     
     myNewFlightDelay <- predict(myFlightModel, myNewFlighReactive())
     
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


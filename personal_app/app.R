library(shiny)
library(tidyverse)
ges <- read.csv("GraduateEmploymentSurvey.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("The change in employment rate and starting salary of Singaporean university graduates changed over time for different majors from 2013 to 2021?"),
  sidebarLayout(
    sidebarPanel(sliderInput("samplesize","Sample Size:",min = 100,max = 10000,value = 1000)),
    mainPanel(plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    if (input$data_type == "Employment Rate") {
      ggplot(ges, aes(x = year, y = employment_rate_overall)) +
        geom_line() +
        labs(x = "Year", y = "Employment Rate", title = "Employment Rate Over Time")
    } else if (input$data_type == "Starting Salary") {
      ggplot(ges, aes(x = year, y = starting_salary)) +
        geom_line() +
        labs(x = "Year", y = "Starting Salary", title = "Starting Salary Over Time")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

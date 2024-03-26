#library(shiny)
library(tidyverse)
library(ggplot2)
ges <- read.csv("GraduateEmploymentSurvey.csv")
ges_clean <- na.omit(ges)
ges_clean2 <- ges_clean$employment_rate_overall <- as.numeric(ges_clean$employment_rate_overall)

ggplot(ges_clean) +
  aes(x=year,y=employment_rate_overall, group=university) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(60, 100, by = 10), limits = c(60, 100)) +
  scale_x_continuous(breaks= seq(2013, 2021, by=1))

ui <- fluidPage(
  titlePanel("Graduate Employment Survey"),
  sidebarLayout(
    position="left",
    sidebarPanel(
      inputId = "university", 
      label = "Choose a University", 
      choices = unique(ges_clean2$university)), 
    mainPanel(plotOutput("line")
    )
  )
)

server <- function(input, output) {
  output$line <- renderPlot({
    ggplot(ges_clean2 %>% filter(University == input$university), 
           aes(year, `Graduates' Overall Employment Rate `, 
           )) + geom_line()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
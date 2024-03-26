library(shiny)
library(tidyverse)
library(ggplot2)
ges <- read.csv("GraduateEmploymentSurvey.csv")
ges_clean <- na.omit(ges)
ges_clean$employment_rate_overall <- as.numeric(ges_clean$employment_rate_overall)

head(ges_clean)

ui <- fluidPage(
  titlePanel("Graduate Employment Survey"),
  sidebarLayout(
    position="left",
    headerPanel(h3("Choose your interested University, Major")),
    sidebarPanel(
      selectInput("university", "Choose a University",
                  choices = c("NUS","NTU","SMU","SIT", "SUSS","SUTD")),
        selectInput("degree", "Choose a Degree",
                  choices = c("Accountancy and Business","Accountancy (3-yr direct Honours Programme)
","Business (3-yr direct Honours Programme)","Business and Computing","Aerospace Engineering
","Bioengineering","More")),
      selectInput("dataobserved", "Choose a Measurement",
                  choices = c("Overall Employment Rate","Gross Monthly Income")),
      mainPanel(plotOutput("line")
      )
    )
  )
)

server <- function(input, output) {
  eventReactive(input$university, {
    switch(input$university,
           "NUS" = "National University of Singapore",
           "NTU" = "Nanyang Technological University",
           "SMU" = "Singapore Management University",
           "SIT" = "Singapore Institute of Technology",
           "SUSS" = "Singapore University of Social Sciences",
           "SUTD" = "Singapore University of Technology and Design")
  }, ignoreNULL = FALSE)
  output$line <- renderPlot({
    ggplot(ges_clean %>% filter(University == input$university), 
           aes(year, "Graduates' Overall Employment Rate" 
           )) + geom_line()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
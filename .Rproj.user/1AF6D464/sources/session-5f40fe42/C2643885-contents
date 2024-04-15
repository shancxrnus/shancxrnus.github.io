library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(plotly)
ges <- read.csv("GraduateEmploymentSurvey.csv")
ges_clean <- na.omit(ges)
ges_clean$employment_rate_overall <- as.numeric(ges_clean$employment_rate_overall)
ges_clean$gross_monthly_median <- as.numeric(ges_clean$gross_monthly_median)
head(ges_clean)

average_ero <- ges_clean %>%
  group_by(year, university) %>%
  summarise(average_employment_rate_overall = mean(employment_rate_overall, na.rm = TRUE))

average_gmm <- ges_clean %>%
  group_by(year, university) %>%
  summarise(average_gross_monthly_median = mean(gross_monthly_median, na.rm = TRUE))

combined_data <- list()

university_ero <- list(ges_clean$year,ges_clean$university, ges_clean$degree)

nus_years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
ntu_years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
smu_years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
sit_years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
suss_years <- c(2018, 2019, 2020, 2021)
sutd_years <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021)

head(ges_clean)

ui <- fluidPage(
  titlePanel("Graduate Employment Survey"),
  sidebarLayout(
    sidebarPanel(
      h4("Choose your University and Major"),
      selectInput("university", "Choose a University",
                  choices = c("NUS","NTU","SMU","SIT", "SUSS","SUTD")),
      selectInput("dataobserved", "Choose a Measurement",
                  choices = c("Average Overall Employment Rate","Average Gross Monthly Income"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Line Graph", plotlyOutput("line")),
        tabPanel("Bar Chart Comparing Overall Employment Rate (OER)", plotlyOutput("bar_ero")),
        tabPanel("Bar Chart Comparing Average Gross Monthly Income (GMI)", plotlyOutput("bar_gmm")),
    )
  )
  )
)

server <- function(input, output) {
  university_data_ero <- reactive({
    switch(input$university,
           "NUS" = average_ero %>% filter(university == "National University of Singapore"),
           "NTU" = average_ero %>% filter(university == "Nanyang Technological University"),
           "SMU" = average_ero %>% filter(university == "Singapore Management University"),
           "SIT" = average_ero %>% filter(university == "Singapore Institute of Technology"),
           "SUSS" = average_ero %>% filter(university == "Singapore University of Social Sciences"),
           "SUTD" = average_ero %>% filter(university == "Singapore University of Technology and Design")
    )
  })
  
  university_data_gmm <- reactive({
    switch(input$university,
           "NUS" = average_gmm %>% filter(university == "National University of Singapore"),
           "NTU" = average_gmm %>% filter(university == "Nanyang Technological University"),
           "SMU" = average_gmm %>% filter(university == "Singapore Management University"),
           "SIT" = average_gmm %>% filter(university == "Singapore Institute of Technology"),
           "SUSS" = average_gmm %>% filter(university == "Singapore University of Social Sciences"),
           "SUTD" = average_gmm %>% filter(university == "Singapore University of Technology and Design")
    )
  })
  
  output$line <- renderPlotly({
    req(input$university)  # Ensure university is selected
    
    # What happens if option for Average Overall Employment Rate is selected
    if (input$dataobserved == "Average Overall Employment Rate") {
      # Plot average overall employment rate over time
      ggplot(university_data_ero(), aes(x = year, y = average_employment_rate_overall)) +
        geom_line() +
        labs(x = "Year", y = "Average Overall Employment Rate (%)", title = "Overall Employment Rate Over Time") +
        scale_x_continuous(breaks = unique(university_data_ero()$year))
    } else if (input$dataobserved == "Average Gross Monthly Income") { #What happens if other option is selected
      # Plot average gross monthly income over time
      ggplot(university_data_gmm(), aes(x = year, y = average_gross_monthly_median)) +
        geom_line() +
        labs(x = "Year", y = "Gross Monthly Income Median (S$)", title = "Gross Monthly Income Median Over Time") +
        scale_x_continuous(breaks = unique(university_data_gmm()$year))
    }
  })
  # Define the output for the combined data table
  output$bar_ero <- renderPlotly({
    ggplot(average_ero, aes(x = year, fill=university)) +
      geom_bar(stat = "identity", aes(y = average_employment_rate_overall), position = "dodge") +
      labs(title = "Bar Graph showing Universities' Average OER Over Time", 
           x = "Year", y = "Average Employment Rate overall (%)") +
    scale_x_continuous(breaks = unique(university_data_ero()$year))
  })
  output$bar_gmm <- renderPlotly({
    ggplot(average_gmm, aes(x = year, fill=university)) +
      geom_bar(stat = "identity", aes(y = average_gross_monthly_median), position = "dodge") +
      labs(title = "Bar Graph showing Universities' Average GMI Over Time", 
           x = "Year", y = "Average Average Gross Monthly Income (S$)") +
      scale_x_continuous(breaks = unique(university_data_gmm()$year))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)




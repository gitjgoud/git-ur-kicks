ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  textInput('name',
                            label="Project Name",
                            value='name your project!'),
                  textInput('blurb',
                            label="Project Blurb",
                            value="what's it about?"),
                  selectizeInput("category",
                                 label='Project Category',
                                 choices=unique(ks_app[,"sub_category"])
                                 ),
                  sliderInput('duration',
                              label='Campaign Duration',
                              min=0,
                              max=90,
                              value=30)
                  #add BS factor
                  ),
                box(
                  numericInput("goal_usd",
                               label = "Funding Goal [USD]",
                               value = 1000),
                  numericInput("backers_count",
                               label = "Estimated Backers",
                               value = 500),
                  dateInput('launch_date',
                            label = 'Launch Date',
                            value='2022-04-01'),
                  sliderInput('launch_hour',
                              label = 'Launch Hour [UTC]',
                              min=0,
                              max=23,
                              value=12)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  library(shiny)
  library(shinydashboard)
  library(ggplot2)
  library(tidyverse)
  library(dplyr)
  library(shinyTime)
  
  ks_app <- read.csv(file='./clean_kickstarter_data.csv')
  
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)

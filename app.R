library(shinydashboard)

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
                box(plotOutput("plot1", height = 250)),

                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              ),
              # add title bar eventually
              # fluidRow(
              #   box(title='Plan your project!',solidHeader = T,status = 'primary')
              # ),
              fluidRow(
                box(
                  textInput('project name',
                            label="Project Name",
                            value='Enter text...'),
                  selectizeInput("category",
                                 label='Project Category',
                                 choices=unique(ks_app[,"sub_category"])
                                 ),
                  sliderInput('duration',
                              label='Campaign Duration',
                              min=0,
                              max=90,
                              value=30)
                  ),
                box(
                  numericInput("goal_usd",
                               label = "Funding Goal [USD]",
                               value = 1000),
                  numericInput("backers_count",
                               label = "Estimated Backers",
                               value = 500),
                  
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
  
  ks_app <- read.csv(file='./clean_kickstarter_data.csv')
  
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)

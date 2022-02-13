library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)

ks_app <- read.csv(file='./clean_kickstarter_data.csv')

logit.overall.app = glm(status_binary ~ avg_plg.bkr + sub_category + goal_usd + duration + launch_hour + name_len,
                        family = 'binomial',
                        data = ks_app)

ui <- dashboardPage(
  dashboardHeader(title = "Can You Kickstart?"),
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
                box(plotOutput("plot1",
                               height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider",
                              "Number of observations:",
                              1, 100, 50)
                ),
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
              ),
              fluidRow(
                box(
                  actionButton('calculate',
                               label='GO')
                ),
                textOutput('percent_success')
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

  # user_proj <- data.frame(
  #   agv_plg.bkr = input$goal_usd/input$backers_count,
  #   sub_category = input$category,
  #   goal_usd = input$goal_usd,
  #   duration = input$duration,
  #   launch_hour = input$launch_hour,
  #   name_len = length(input$name)
  # )
  
  output$percent_success <- renderText({
    newdata = with(ks_app, data.frame(avg_plg.bkr=mean(avg_plg.bkr),
                                      sub_category='technology',
                                      goal_usd=input$goal_usd,
                                      duration=mean(duration),
                                      launch_hour=median(launch_hour),
                                      name_len=mean(name_len)))
    
    
    paste(round(predict(logit.overall.app, newdata,type='response'),3)*100,'%')
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)

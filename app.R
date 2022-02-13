library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DT)

ks_app <- read.csv(file='./clean_kickstarter_data.csv')


logit.overall.app = glm(status_binary ~ avg_plg.bkr + sub_category + goal_usd + duration + launch_hour + name_len,
                        family = 'binomial',
                        data = ks_app)

#load with initial values
user_proj = with(ks_app, data.frame(avg_plg.bkr=mean(avg_plg.bkr),
                                  sub_category='technology',
                                  goal_usd=3000,
                                  duration=mean(duration),
                                  launch_hour=median(launch_hour),
                                  name_len=mean(name_len)))

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
                  numericInput("est_pledge",
                               label = "Estimated Pledge",
                               value = 20),
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
                               label='Predict')
                ),
                textOutput('percent_success')
              ),
              fluidRow(
                box(width=12,
                  #this may not need a box
                  DT::dataTableOutput('project_params')
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

server <- function(input, output,session) {

  
  observeEvent(input$calculate, {
    user_proj <- data.frame(
      avg_plg.bkr = input$est_pledge,
      sub_category = input$category,
      goal_usd = input$goal_usd,
      duration = input$duration,
      launch_hour = input$launch_hour,
      name_len = length(input$name)
    )
    
    output$percent_success <- renderText({
      paste(round(predict(logit.overall.app, user_proj,type='response'),3)*100,'%')
    })
    
    output$project_params <- DT::renderDataTable(
      user_proj,
      options=list(dom='t')
    )
    
  })
  
  output$percent_success <- renderText({
    '<-- Click Predict to see chance of success!'
  })
  
  output$project_params <- DT::renderDataTable(
    user_proj,
    options=list(dom='t')
  )
  
  
  
}


shinyApp(ui, server)

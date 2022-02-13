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
user_proj = with(ks_app, data.frame(avg_plg.bkr=round(mean(avg_plg.bkr)),
                                  sub_category='technology',
                                  goal_usd=3000,
                                  duration=round(mean(duration)),
                                  launch_hour=median(launch_hour),
                                  name_len=round(mean(name_len))))

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
              box(title = "Project Parameters",
                  width = 12,
                  solidHeader = T,
                  status = 'primary',
                  column(width=6,
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
                                min=1,
                                max=90,
                                value=30)
                    #add more BS factors if time allows
                    ),
                  column(width=6,
                  numericInput("goal_usd",
                               label = "Funding Goal [USD]",
                               value = 1000),
                  numericInput("est_pledge",
                               label = "Estimated Average Pledge",
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
                )
              ),
              fluidRow(
                box(title= "Predict Chances of Success",
                    width = 12,
                    solidHeader = T,
                    status = 'success',
                    column(width=4, offset=2,
                           actionButton('calculate',
                                        label='Predict')
                    ),
                    column(width=6,
                           textOutput('percent_success')
                    )
                )
              ),
              fluidRow(
                box(width=12,
                    collapsible = T,
                    solidHeader = T,
                    title = "Prediction History",
                    status="warning",
                    collapsed = T,
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
      name_len = str_length(input$name)
    )
    
    output$percent_success <- renderText({
      paste(round(predict(logit.overall.app, user_proj,type='response'),3)*100,'%')
    })
    
    output$project_params <- DT::renderDataTable(
      user_proj,
      rownames=FALSE,
      options=list(dom='t')
    )
    
  })
  
  output$percent_success <- renderText({
    '<-- Click Predict to see chance of success!'
  })
  
  output$project_params <- DT::renderDataTable(
    user_proj,
    rownames=FALSE,
    options=list(dom='t')
  )
  
  
  
}


shinyApp(ui, server)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DT)

ks_app <- read.csv(file='./data/clean_kickstarter_data.csv')
data.txt <- read_lines('./data/data_sources.txt')
summary.txt <- read_lines('./data/summary.txt')
objective.txt <- read_lines('./data/objective.txt')
model.txt <- read_lines('./data/model_summary.txt')

logit.overall.app = glm(status_binary ~ avg_plg.bkr + sub_category + goal_usd + duration + launch_hour + name_len,
                        family = 'binomial',
                        data = ks_app)

#load with initial values
user_proj = with(ks_app, data.frame(avg_plg.bkr=round(mean(avg_plg.bkr)),
                                  sub_category='food',
                                  goal_usd=3000,
                                  duration=round(mean(duration)),
                                  launch_hour=median(launch_hour),
                                  name_len=round(mean(name_len))))

ui <- dashboardPage(skin='black',
  dashboardHeader(title = "Can You Kickstart?"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Success Predictor", tabName = "predictor", icon = icon("hat-wizard")),
      menuItem("Project Overview", tabName = "overview", icon = icon("splotch"),
               menuItem('Objective', tabName='objective'),
               menuItem('Modeling', tabName='model')
               ),
      menuItem("EDA",tabName = 'eda', icon= icon("ruler-combined"),
               menuItem('Funding Goal', tabName = 'eda_funding'),
               menuItem('Backer Trends', tabName = 'eda_backers'),
               menuItem('Name & Blurb', tabName = 'eda_name'),
               menuItem('Location', tabName = 'eda_location'),
               menuItem('Category', tabName = 'eda_category'),
               menuItem('Launch Timing', tabName = 'eda_timing'),
               menuItem('Duration', tabName= 'eda_duration'),
               menuItem('Experience', tabName = 'eda_experience')
               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "predictor",
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
                               value = 3000),
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
      tabItem(tabName = "objective",
              h2("Project Overview"),
              fluidRow(
                tabBox(id='tabset1',
                       width = 12,
                       tabPanel('Summary',
                                htmlOutput('summary_text')),
                       tabPanel('Objective',
                                htmlOutput('objective_text')),
                       tabPanel('Data Sources',
                                htmlOutput('data_text'))
                    )
                  )
              ),
      tabItem(tabName="model",
              h2("Modeling Summary"),
                 fluidRow(
                   img(src='residuals_plot.JPG', height=490, width=700)
                         ),
                 fluidRow(
                   tabBox(id='tabset2',
                          width = 12,
                          tabPanel('Model Summary',
                                    htmlOutput('model_text'))
                         )
                        )
            ),
      tabItem(tabName='eda_funding',
              h2("Set A Reasonable Funding Goal"),
              fluidRow(tabBox(width = 12,
                              tabPanel('Inspecting Funding Goals',
                                       img(src='fund1.JPG', height=490, width=700)),
                              tabPanel('Mean and Median',
                                       img(src='fund2.JPG', height=490, width=700)),
                              tabPanel("Don't Plan To Exceed...",
                                       img(src='fund3.JPG', height=490, width=700))
                              ))),
      tabItem(tabName='eda_backers',
              h2("Backer Engagement Is Critical"),
              fluidRow(tabBox(width = 12,
                              tabPanel('Backer Stats',
                                       img(src='bkr1.JPG', height=490, width=700)),
                              tabPanel('Maximize Reward Tier Value',
                                       img(src='bkr3.JPG', height=490, width=700)),
                              tabPanel('Backer Behavior Near Funding Goal',
                                       img(src='bkr2.JPG', height=490, width=700))
              )))
    )
  )
)

server <- function(input, output,session) {
  
  output$data_text <- renderUI({HTML(data.txt)})
  output$summary_text <- renderUI({HTML(summary.txt)})
  output$objective_text <- renderUI({HTML(objective.txt)})
  output$model_text <- renderUI({HTML(model.txt)})
  
    
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

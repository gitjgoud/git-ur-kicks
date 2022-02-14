library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DT)

#read data files
ks_app <- read.csv(file='./data/clean_kickstarter_data.csv')
data.txt <- read_lines('./data/data_sources.txt')
summary.txt <- read_lines('./data/summary.txt')
objective.txt <- read_lines('./data/objective.txt')
model.txt <- read_lines('./data/model_summary.txt')

#create algorithm
#(dear grader - how do we export this so we don't need to create in-app?)
logit.overall.app = glm(status_binary ~ avg_plg.bkr + sub_category + goal_usd + duration + launch_hour + name_len,
                        family = 'binomial',
                        data = ks_app)

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
               menuItem('Creator Experience', tabName = 'eda_creator'),
               menuItem('Name & Blurb', tabName = 'eda_name'),
               menuItem('Location', tabName = 'eda_location'),
               menuItem('Category', tabName = 'eda_category'),
               menuItem('Launch Timing', tabName = 'eda_timing'),
               menuItem('Duration', tabName= 'eda_duration')
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
              h2("Set A Reasonable Goal"),
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
              ))),
      tabItem(tabName='eda_creator',
              h2("The Value Of Experience"),
              fluidRow(tabBox(width = 12,
                              tabPanel('Learn from failure',
                                       img(src='ctr1.JPG', height=490, width=700))
              ))),
      tabItem(tabName='eda_name',
              h2("Describe the dream"),
              fluidRow(tabBox(width = 12,
                              tabPanel('Lenght is length',
                                       img(src='name2.JPG', height=490, width=700)),
                              tabPanel('Be (relatively) vebose',
                                       img(src='name1.JPG', height=490, width=700))
              ))),
      tabItem(tabName='eda_location',
              h2("It doesn't matter where"),
              fluidRow(tabBox(width = 12,
                              tabPanel('It seems important',
                                       img(src='con1.JPG', height=490, width=700)),
                              tabPanel('...but probably isnt',
                                       img(src='con2.JPG', height=490, width=700))
              ))),
      tabItem(tabName='eda_category',
              h2("What will you make?"),
              fluidRow(tabBox(width = 12,
                              tabPanel("It's all relative",
                                       img(src='cat1.JPG', height=490, width=700)),
                              tabPanel('Focus on fundamentals',
                                       img(src='cat2.JPG', height=490, width=700))
              ))),
      tabItem(tabName='eda_timing',
              h2("Be new when it matters"),
              fluidRow(tabBox(width = 12,
                              tabPanel("Kickstarter is getting more popular",
                                       img(src='lch1.JPG', height=490, width=700)),
                              tabPanel('Avoid the holidays',
                                       img(src='lch2.JPG', height=490, width=700)),
                              tabPanel("Kick off in a new month",
                                       img(src='lch3.JPG', height=490, width=700)),
                              tabPanel("Tuesdays are for slackers",
                                       img(src='lch4.JPG', height=490, width=700)),
                              tabPanel('Aim for the front page',
                                       img(src='lch5.JPG', height=490, width=700))
              ))),
      tabItem(tabName='eda_duration',
              h2("It doesn't take long"),
              fluidRow(tabBox(width = 12,
                              tabPanel('More time != reaching goal',
                                       img(src='dur1.JPG', height=490, width=700))
              )))
    )
  )
)

server <- function(input, output,session) {
  
  #load external text data
  output$data_text <- renderUI({HTML(data.txt)})
  output$summary_text <- renderUI({HTML(summary.txt)})
  output$objective_text <- renderUI({HTML(objective.txt)})
  output$model_text <- renderUI({HTML(model.txt)})
  
  #calculate probability of success on button push
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
  
  #initialize output box
  output$percent_success <- renderText({
    '<-- Click Predict to see chance of success!'
  })
}


shinyApp(ui, server)

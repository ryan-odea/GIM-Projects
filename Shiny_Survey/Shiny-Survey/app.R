library(shiny)
library(shinydashboard)
library(magrittr)
library(tidyverse)
library(lubridate)
library(DT)
library(rsconnect)
library(shinythemes)
library(lubridate)
library(RColorBrewer)
library(ggthemes)
#-------------------------
likert_lables <- c("Very Disatisfied",
                   "Disatisfied",
                   "Neutral",
                   "Satisfied",
                   "Very Satisfied")

ui <- 
  fluidPage(
    titlePanel("GIM Clinic Survey"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File to Upload",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        tags$hr(),
        selectInput("grouping", "How would you like to compare the Data?",
                    choices = c("Clinic",
                                "Pronouns",
                                "Dependents",
                                "URG")),
        selectInput("year", "Which year would you like to observe?",
                    choices = c(2021:2050))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Question 1", plotOutput("ques1")),
          tabPanel("Question 2", plotOutput("ques2")),
          tabPanel("Question 3", plotOutput("ques3")),
          tabPanel("Question 4", plotOutput("ques4")),
          tabPanel("Question 5", plotOutput("ques5"))
        )
      )
    )
  )





server <-   function(input, output) {
  user_data <- reactive({
    inFile <- input$file1
    if(is.null(inFile)){
      return(NULL)
    } else {
      dataframe <- read_csv(inFile$datapath) %>%
        rename(
          Pronouns = pronouns,
          Dependents = depends,
          URG = urg
        ) %>% 
        mutate(
          across(c(Clinic, Pronouns, Dependents, URG), as.factor),
          Timestamp = mdy_hms(Timestamp),
          year  = year(Timestamp),
          month = factor(months(as.Date(Timestamp)), levels = month.name)
        ) %>% 
        filter(year == input$year)
      
      dataframe
    }
  })

  output$ques1 <- renderPlot({
    req(user_data())
    user_data() %>%
      filter(year == input$year) %>%
      ggplot(aes(y = calls_msgs, x = month)) + 
      geom_violin(aes_string(fill  = input$grouping)) + 
      scale_y_discrete(limits = factor(c(1:5)), labels = likert_lables) +
      theme_fivethirtyeight() + 
      labs(title = "How satisfied are you with:\nHow the nursing staff handles calls and messages?",
           fill = paste(input$grouping)) + 
      scale_fill_brewer(palette = "Set2") + 
      theme(plot.title = element_text(size = 14, hjust = .5)) 
  })
  
  output$ques2 <- renderPlot({
    req(user_data())
    user_data() %>%
      filter(year == input$year) %>%
      ggplot(aes(y = remote_forms, x = month)) + 
      geom_violin(aes_string(fill  = input$grouping)) + 
      scale_y_discrete(limits = factor(c(1:5)), labels = likert_lables) +
      theme_fivethirtyeight() + 
      labs(title = "How satisfied are you with:\nThe process for completing forms when working remotely?",
           fill = paste(input$grouping)) + 
      scale_fill_brewer(palette = "Set2") + 
      theme(plot.title = element_text(size = 14, hjust = .5))
  })
  
  output$ques3 <- renderPlot({
    req(user_data())
    user_data() %>%
      filter(year == input$year) %>%
      ggplot(aes(y = real_time_support, x = month)) + 
      geom_violin(aes_string(fill  = input$grouping)) + 
      scale_y_discrete(limits = factor(c(1:5)), labels = likert_lables) +
      theme_fivethirtyeight() + 
      labs(title = "How satisfied are you with:\nYour ability to reach team members for real time support when working remotely?",
           fill = paste(input$grouping)) + 
      scale_fill_brewer(palette = "Set2") + 
      theme(plot.title = element_text(size = 14, hjust = .5))
  })
  
  output$ques4 <- renderPlot({
    req(user_data())
    user_data() %>%
      filter(year == input$year) %>%
      ggplot(aes(y = flexibility, x = month)) + 
      geom_violin(aes_string(fill  = input$grouping)) + 
      scale_y_discrete(limits = factor(c(1:5)), labels = likert_lables) +
      theme_fivethirtyeight() + 
      labs(title = "How satisfied are you with:\nyour options for flexibility?",
           fill = paste(input$grouping)) + 
      scale_fill_brewer(palette = "Set2") + 
      theme(plot.title = element_text(size = 14, hjust = .5))

  })
  
  output$ques5 <- renderPlot({
    req(user_data())
    user_data() %>%
      filter(year == input$year) %>%
      ggplot(aes(y = work_life, x = month)) + 
      geom_violin(aes_string(fill  = input$grouping)) +  
      scale_y_discrete(limits = factor(c(1:5)), labels = likert_lables) +
      theme_fivethirtyeight() + 
      labs(title = "How satisfied are you with:\nyour overall work-life balance?",
           fill = paste(input$grouping)) + 
      scale_fill_brewer(palette = "Set2") + 
      theme(plot.title = element_text(size = 14, hjust = .5))
    
  })
  
  
}


shinyApp(ui, server)

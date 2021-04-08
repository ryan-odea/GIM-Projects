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
      geom_violin(aes_string(fill  = input$grouping), position = "dodge") + 
      geom_jitter(alpha = .5) + 
      scale_y_discrete(limits = c(1:5), labels = likert_lables) +
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
      geom_violin(aes_string(fill  = input$grouping), position = "dodge") + 
      geom_jitter(alpha = .5) + 
      scale_y_discrete(limits = c(1:5), labels = likert_lables) +
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
      geom_violin(aes_string(fill  = input$grouping), position = "dodge") + 
      geom_jitter(alpha = .5) + 
      scale_y_discrete(limits = c(1:5), labels = likert_lables) +
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
      geom_violin(aes_string(fill  = input$grouping), position = "dodge") + 
      geom_jitter(alpha = .5) + 
      scale_y_discrete(limits = c(1:5), labels = likert_lables) +
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
      geom_violin(aes_string(fill  = input$grouping), position = "dodge") + 
      geom_jitter(alpha = .5) + 
      scale_y_discrete(limits = c(1:5), labels = likert_lables) +
      theme_fivethirtyeight() + 
      labs(title = "How satisfied are you with:\nyour overall work-life balance?",
           fill = paste(input$grouping)) + 
      scale_fill_brewer(palette = "Set2") + 
      theme(plot.title = element_text(size = 14, hjust = .5))
  })
  
}


shinyApp(ui, server)





















#-G-L-O-B-A-L------------------------------------------------------------------
# responsesDir <- file.path("responses")
# fieldsMandatory <- c("clinic",
#                      "pronouns",
#                      "dependents",
#                      "urg",
#                      "ques1",
#                      "ques2",
#                      "ques3",
#                      "ques4",
#                      "ques4",
#                      "ques5")
# 
# 
# labelMandatory <- function(label) {
#   tagList(
#     label,
#     span("*", class = "mandatory_star")
#   )
# }
# 
# humanTime <- function() {
#   format(Sys.time(), "%Y%m%d-%H%M%OS")
# }
# 
# # save the results to a file
# saveData <- function(df) {
#   fileName <- sprintf("%s_%s.csv",
#                       humanTime(),
#                       digest::digest(df))
#   
#   write.csv(x = df, file = file.path(responsesDir, fileName),
#             row.names = FALSE, quote = TRUE)
# }
# 
# # load all responses into a data.frame
# loadData <- function() {
#   files <- list.files(file.path(responsesDir), full.names = TRUE)
#   df <- lapply(files, read.csv, stringsAsFactors = FALSE)
#   df <- do.call(rbind, df)
#   df
# }
# 
# # directory where responses get stored
# responsesDir <- file.path("responses")
# 
# # CSS to use in the app
# appCSS <-
#   ".mandatory_star { color: red; }
#    .shiny-input-container { margin-top: 25px; }
#    #submit_msg { margin-left: 15px; }
#    #error { color: red; }
#    body { background: #fcfcfc; }
#    #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
#   "
# 
# #-U-S-E-R---I-N-T-E-R-F-A-C-E--------------------------------------------------
# ui <- navbarPage(
#   shinyjs::useShinyjs(),
#   shinyjs::inlineCSS(appCSS),
#   title = "GIM Clinic Survey",
#   theme = shinytheme("journal"),
#   tabPanel("Survey",
#     fluidRow(
#       column(10,
#         div(
#           id = "form",
#           dateInput("date", "Today's Date", value = Sys.Date()),
#             selectInput("clinic", labelMandatory("Suite"),
#               choices = c("Suite 5A",
#                           "Suite 5B",
#                           "Suite 5C",
#                           "Suite 6A",
#                           "Suite 6B",
#                           "Suite 6C")),
#             selectInput("pronouns", labelMandatory("Which pronouns do you identify with?"),
#               choices = c("He/Him/His",
#                           "She/Her/Hers",
#                           "They/Them",
#                           "Other")),
#           selectInput("dependents", labelMandatory("Do you have dependents at home?"),
#               choices = c("Yes","No")),
#           selectInput("urg", labelMandatory("Do you identify within an underrepresented group?"),
#               choices = c("Yes", "No")),
#           sliderInput("ques1", labelMandatory("How satisfied are you with how the nursing staff handles calls and messages?"),
#               min = 1, max = 5, value = 1),
#           sliderInput("ques2", labelMandatory("How satisfied are you with the process for completing forms when working remotely?"),
#               min = 1, max = 5, value = 1),
#           sliderInput("ques3", labelMandatory("How satisfied are you with your ability to reach team members for real time support when working remotely?"),
#               min = 1, max = 5, value = 1),
#           sliderInput("ques4", labelMandatory("How satisfied are you with your options for flexibility (e.g. shifting clinic days/times, choosing more/less telemedicine, reduction of cFTE)?"),
#               min = 1, max = 5, value = 1),
#           sliderInput("ques5", labelMandatory("How satisfied are you with your overall work-life balance?"),
#               min = 1, max = 5, value = 1),
#           actionButton("submit", "Submit", class = "btn-primary"),
#           
#           shinyjs::hidden(
#             span(id = "submit_msg", "Submitting...."),
#             div(id = "error",
#                 div(br(), tags$b("Error: "), span(id = "error_msg")))
#           ),
#           shinyjs::hidden(
#             div(id = "thankyou_msg",
#                 h3("Thanks! Your response was submitted successfully."),
#                 actionLink("submit_another", "Submit Another Response"))
#           )
#         )
#       )
#     )
#   ),
#   mainPanel(tableOutput("table1")),
#   tabPanel("Data Visualization",
#            selectInput("grouping", "How would you like to compare the data?",
#                        choices = c("By Clinic",
#                                    "By Pronouns",
#                                    "By Dependents Y/N",
#                                    "By Underrepresented Group Y/N")),
#            uiOutput("month_select"),
#            uiOutput("year_select")
#            )
# )
# 
# server <- function(input, output){
#   responses <- reactiveValues()
#   responses$df <- data.frame(month = numeric(0),
#                              year = numeric(0),
#                              clinic = numeric(0),
#                              pronouns = numeric(0),
#                              dependents = numeric(0),
#                              urg = numeric(0),
#                              ques1 = numeric(0),
#                              ques2 = numeric(0),
#                              ques3 = numeric(0),
#                              ques4 = numeric(0),
#                              ques5 = numeric(0))
#   
#   observeEvent(input$submit,{
#     shinyjs::disable("submit")
#     shinyjs::show("submit_msg")
#     shinyjs::hide("error")
#     
#     tryCatch({
#       shinyjs::reset("form")
#       shinyjs::hide("form")
#       shinyjs::show("thankyou_msg")
#     },
#     error = function(err) {
#       shinyjs::html("error_msg", err$message)
#       shinyjs::show(id = "error", anim = TRUE, animType = "fade")
#     },
#     finally = {
#       shinyjs::enable("submit")
#       shinyjs::hide("submit_msg")
#     })
#   
#   # submit another response
#     observeEvent(input$submit_another, {
#     shinyjs::show("form")
#     shinyjs::hide("thankyou_msg")
#   })
# })
#     observeEvent(input$submit, {
#       likert_levels <- c(1, 2, 3, 4, 5)
#       likert_lables <- c("Very Disatisfied",
#                          "Disatisfied",
#                          "Neutral",
#                          "Satisfied",
#                          "Very Disatisfied")
#       
#       newLine <- isolate(c(input$month,
#                            input$year,
#                            input$clinic,
#                            input$pronouns,
#                            input$dependents,
#                            input$urg,
#                            input$ques1,
#                            input$ques2,
#                            input$ques3,
#                            input$ques4,
#                            input$ques5))
#       isolate(responses$df[nrow(responses$df) + 1, ] <- c(as.factor(month.name[month(input$date)]),
#                                                           as.factor(year(input$date)),
#                                                           as.factor(input$clinic),
#                                                           as.factor(input$pronouns),
#                                                           as.factor(input$dependents),
#                                                           as.factor(input$urg),
#                                                           factor(input$ques1, levels = likert_levels, labels = likert_lables),
#                                                           factor(input$ques1, levels = likert_levels, labels = likert_lables),
#                                                           factor(input$ques2, levels = likert_levels, labels = likert_lables) ,   
#                                                           factor(input$ques3, levels = likert_levels, labels = likert_lables),
#                                                           factor(input$ques4, levels = likert_levels, labels = likert_lables),
#                                                           factor(input$ques5, levels = likert_levels, labels = likert_lables)))
#       
#     })
# 
#     output$month_select <- renderUI({
#     mydata = df()
#     selectInput("month_choice", "Choose the month to observe (Clinic Only)",
#                 choices = c(month.name[unique(responses$df$month)]))
#   })
#     
#   output$year_select <- renderUI({
#     mydata = df()
#     selectInput("year_choice", "Choose the year to observe",
#                 choices = c(unique(responses$df$year)))
#   })
#   output$table1 <- renderTable({responses$df})
# }
# 
# 
# shinyApp(ui = ui, server = server)


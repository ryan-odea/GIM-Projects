library(shiny)
library(shinydashboard)
library(magrittr)
library(tidyverse)
library(lubridate)
library(DT)
library(rsconnect)
library(googlesheets4)

rsconnect::setAccountInfo(name='ryan-odea', 
                          token='F4A4130C19C8BD8D03B49B6714DF82E7', 
                          secret='AgzEZitVyUeXMTgi0CQvfeCs4vU/cK9TDDfQNvtq')

shiny_token <- gs4_auth()

data <- gs4_create("GIM-Survey", sheets = "data")
ui <- fluidPage(
    table <- "responses",
    #App title ---------
    titlePanel("GIM Clinic Survey"),
    #Sidebar layout with input/output definitions
    sidebarLayout(
        #Sidebar to demonstrate slider options
        sidebarPanel(
            dateInput("date", "Today's Date"),
            selectInput("clinic", "Suite", 
                        choices = c("Suite 5A",
                                    "Suite 5B",
                                    "Suite 5C",
                                    "Suite 6A",
                                    "Suite 6B",
                                    "Suite 6C")),
            selectInput("pronouns", "Which pronouns do you identify with?",
                        choices = c("He/Him/His",
                                    "She/Her/Hers",
                                    "They/Them",
                                    "Other")),
            selectInput("dependents", "Do you have dependents at home?",
                        choices = c("Yes","No")),
            selectInput("urg", "Do you identify within an underrepresented group?",
                        choices = c("Yes", "No")),
            sliderInput("ques_1", "How satisfied are you with how the nursing staff handles calls and messages?",
                        min = 1, max = 5, value = 1),
            sliderInput("ques_2", "How satisfied are you with the process for completing forms when working remotely?",
                        min = 1, max = 5, value = 1),
            sliderInput("ques_3", "How satisfied are you with your ability to reach team members for real time support when working remotely?",
                        min = 1, max = 5, value = 1),
            sliderInput("ques_4", "How satisfied are you with your options for flexibility (e.g. shifting clinic days/times, choosing more/less telemedicine, reduction of cFTE)?",
                        min = 1, max = 5, value = 1),
            sliderInput("ques_5", "How satisfied are you with your overall work-life balance?", 
                        min = 1, max = 5, value = 1),
            actionButton("submit", "Submit")
        )
    )
)

server <- function(input, output, session){
    responses <- reactive({
        data.frame(
            Name = c("Date",
                     "Clinic",
                     "Pronouns",
                     "Dependents",
                     "URG",
                     "calls/messages",
                     "completing_forms",
                     "real_time_support",
                     "flexibility",
                     "wl_balance"),
            Value = as.character(c(input$date,
                                   input$clinic,
                                   input$pronouns,
                                   input$dependents,
                                   input$urg,
                                   input$ques_1,
                                   input$ques_2,
                                   input$ques_3,
                                   input$ques_4,
                                   input$ques_5)),
            stringsAsFactors = TRUE
        )
    })
}
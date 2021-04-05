library(shiny)
library(shinydashboard)
library(magrittr)
library(tidyverse)
library(lubridate)
library(DT)
library(rsconnect)
library(shinythemes)
#-------------------------
responsesDir <- file.path("responses")


fieldsMandatory <- c("clinic", 
                     "pronouns", 
                     "dependents", 
                     "urg", 
                     "ques1", 
                     "ques2", 
                     "ques3", 
                     "ques4", 
                     "ques5")

labelMandatory <- function(label){
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star {color:red; }"


epochTime <- function(){
  as.integer(Sys.time())
}
humanTime <- function(){
  format(Sys.time(), "%Y%m%d")
}




shinyApp(
  ui <- navbarPage("GIM Clinic Survey",
                   theme = shinytheme("journal"),
                   tabPanel("Likert Scale Intake",
                            div(
                              id = "form",
                                  selectInput("clinic", labelMandatory("Suite"),
                                              choices = c("Suite 5A",
                                                          "Suite 5B",
                                                          "Suite 5C",
                                                          "Suite 6A",
                                                          "Suite 6B",
                                                          "Suite 6C")),
                                  selectInput("pronouns", labelMandatory("Which pronouns do you identify with?"),
                                              choices = c("He/Him/His",
                                                          "She/Her/Hers",
                                                          "They/Them",
                                                          "Other")),
                                  selectInput("dependents", labelMandatory("Do you have dependents at home?"),
                                              choices = c("Yes","No")),
                                  selectInput("urg", labelMandatory("Do you identify within an underrepresented group?"),
                                              choices = c("Yes", "No")),
                                  sliderInput("ques1", labelMandatory("How satisfied are you with how the nursing staff handles calls and messages?"),
                                              min = 1, max = 5, value = 1),
                                  sliderInput("ques2", labelMandatory("How satisfied are you with the process for completing forms when working remotely?"),
                                              min = 1, max = 5, value = 1),
                                  sliderInput("ques3", labelMandatory("How satisfied are you with your ability to reach team members for real time support when working remotely?"),
                                              min = 1, max = 5, value = 1),
                                  sliderInput("ques4", labelMandatory("How satisfied are you with your options for flexibility (e.g. shifting clinic days/times, choosing more/less telemedicine, reduction of cFTE)?"),
                                              min = 1, max = 5, value = 1),
                                  sliderInput("ques5", labelMandatory("How satisfied are you with your overall work-life balance?"),
                                              min = 1, max = 5, value = 1),
                                  actionButton("submit", "Submit", class = "btn-primary")
                            )),
                   tabPanel("Visualization",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("grouping", "Select a Grouped Comparison to Display",
                                            choices = c("By Clinic",
                                                        "By Identified Pronouns",
                                                        "By Dependents Y/N",
                                                        "By Underrepresented Group Y/N")),
                                
                                selectInput("year", "Select A Year to Display",
                                            choices = c(unique(year(formData$humanTime)))),
                                
                                selectInput("month", "Select a Month to Display",
                                            choices = c(unique(month.name[month(formData$humanTime)])))),
                              mainPanel()
                            ))),

server <- function(input, output, session) {
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  formData <- reactive({
    data <- sapply(fieldsMandatory, function(x) input[[x]])
    data <- c(data, timestamp = humanTime)
    data <- t(data)
    data
  })
  saveData <- function(data){
    fileName <- sprintf("%s_%s.csv",
                        humanTime,
                        digest::digest(data))
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = FALSE)
  }
  observeEvent(input$submit, {
    saveData(formData())
  })
}
)


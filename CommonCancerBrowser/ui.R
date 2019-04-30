
library(shiny)
library(shinydashboard)

#Header

header <- dashboardHeader(title="Common Cancer Sites",
                          dropdownMenu(
                            type = "messages",
                            messageItem(
                              from = "Data Source",
                              message = "Centers for Disease Control and Prevention",
                              href = "https://wonder.cdc.gov/cancer.html"
                            )
                          )
)
#Side Bar
gender = c("All","Female"="Female", "Male" = "Male")

age = c("All", "20-24" = "20-24", "25-29" = "25-29")
race = c("All","American Indian or Alaska Native"= "AI-AN","Asian or Pacific Islander" = "A-PI",
         "Black or African American" = "B-AA","Other Races and Unknown combined" = "ORU","White" = "White")
sidebar <- dashboardSidebar(
  sidebarMenu(  
    menuItem("Get Start",tabName="welcome",icon=icon("arrow-alt-circle-down")),
    menuItem("Choose Conditions",tabName="filter",icon=icon("filter"),
             selectInput("gender", "Gender", choices = gender),
             br(),
             selectInput("age","Age", choices = age),
             br(),
             selectInput("race", "Race", choices = race)
             )
  )
)

#Body

body <- dashboardBody(
  fluidPage(
    tabItems(
      tabItem(tabName = "welcome",
              fluidRow (column(12,h4("The common cancer site(s) for young adults are shown below:"))),
              fluidRow( column(12,imageOutput("image1"),
                               br(),
                               br(),
                               br(),
                               br(),
                               column(12,h6("Note: The numbers represent the Morbidity ranking and the data is in 2015")),
                               column(12,h4("Detail:")),
                               column(12,plotOutput("plot1"))
              )
              )
      )
    )
  )
)

dashboardPage(skin = "green", header, sidebar, body)
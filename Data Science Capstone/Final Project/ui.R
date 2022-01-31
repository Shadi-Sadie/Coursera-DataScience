
#  load library

library(shiny)
library(markdown)
library(shinyBS)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Coursera Data Science Capstone: Course Project", theme = shinytheme("united"),
                   tabPanel("Word Prediction",
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("The user should enter a sequence of words"),
                                    textInput("inputString", "Write here",value = "")
                                ),
                                mainPanel(
                                    h2("Prediction"),
                                    verbatimTextOutput("prediction"),
                                    strong("User's sequence"),
                                    tags$style(type='text/css', '#answer {background-color:color: white;}'), 
                                    textOutput('answer'),
                                    br(),
                                    strong("Which dataset has been used?"),
                                    tags$style(type='text/css', '#method {background-color:color: white;}'),
                                    textOutput('method')
                                )
                            )
                   ),
                   
                   tabPanel(
                       
                       "Help",
                       
                       p("A Shiny app has been developped to take as input a phrase (multiple words) in a text box input and to output a prediction of the next word."),
                       
                       tags$div("Please find attached the method to predict the next word:",
                                tags$ul(
                                    tags$li(strong("First,"), "build bigram.RData, trigram.RData and quadgram.RData are obtained with the milestone report"),
                                    tags$li(strong("Then,"), "quadgram.RData is used if the last used 3 words are in the dataset"),
                                    tags$li(strong("Else,"), "trigram.RData is used if the last used 2 words are in the dataset"),
                                    tags$li(strong("Else,"), "bigram.RData is used if the last used word is in the dataset"),
                                    tags$li(strong("Else,"), "the word with the highest frequency is returned"),
                                    tags$li(strong("Finally,", "here is your prediction"))
                                ),
                                
                                tableOutput("dataStructure")
                                
                       )
                       
                   ),
                   
                  
                   
                   tabPanel(
                       
                       "About",
                       
                       h3("Data Science Capstone: Final project"),
                       
                       h3("Author: Shadi  - 5th of July, 2020"),
                       
                       br(),
                       
                       p("This application is a web application using R and ",
                         a(href = "https://shiny.rstudio.com/", "Shiny library"),
                         "corresponding to the last assignment from,",
                         a(href = "https://www.coursera.org/learn/data-science-project", "Data Science Capstone from Coursera")
                       ),
                       
                       p("Source code is available at",
                         a(href = "https://github.com/Shadi-Sedi/Capstone-Project",
                           "https://github.com/Shadi-Sedi/Capstone-Project")
                       )
                       
                   )
                   
                   
)
)
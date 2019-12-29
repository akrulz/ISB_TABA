#TABA Assignment
#Group members: Abhishek Kumar, Deepak Dwivedi, Haribrahadeesh ND, Papiya Batabyal, Naveen Ganesan
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 
if (!require(udpipe)) {install.packages('udpipe')}
if (!require(dplyr)) {install.packages('dplyr')}
if (!require(topicmodels)){install.packages("topicmodels")}  
if (!require(gutenbergr)){install.packages("gutenbergr")}  
if (!require(tidyverse)){install.packages("tidyverse")}  
if (!require(tidytext)) {install.packages("tidytext")}

library(topicmodels)
library(gutenbergr)  
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(udpipe)
library(dplyr)
library(ggplot2)
library(wordcloud)

library(shiny)
require(tibble)
library(tm)



# Define UI for application that answers the assignment
ui <- fluidPage(
  
  # Application title
  titlePanel("3 Car Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        fileInput("file", 
                  "Input CSV file")),
      
      #radioButtons("radio", h3("Select your client"),
      #choices = list("Kia Seltos" = 1, "MG Hector" = 2,
      # "Jeep Compass" = 3),selected = 1)),
      fluidRow(
        textInput("text", h3("Enter Keywords"), 
                  value = "engine,mileage,comfort,performance,interior,maintenance,price")
      ),
      fluidRow(
        
        ## actionButton("action", "Action"),
        ## br(),
        ## br(), 
        submitButton("Process Keywords",icon("refresh"))
      )
      
    ),#end of sidebarPanel
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Overview",
                           h4(p("App created by")),
                           p("Abhishek Kumar, Deepak Dwivedi, Haribrahadeesh ND, Papiya Batabyal, Naveen Ganesan"),
                           h4(p("Data input")),
                           p("Review file can be donwloaded from the following link"),
                           a(href="https://github.com/akrulz/ISB_TABA/blob/a32495343ab26cc696a24c3bcd4eb710bf640d36/reviews.csv"
                             ,"Sample data input file"), 
                           
                           br(),
                           
                           p("Please select keywords you want to analyse. Keywords must be separated by comma"),
                           p("Here is an example:"),
                           p("engine,mileage,comfort,performance,interior,maintenance,price"),
                           br(),
                           h4('How to use this App'),
                           p("1. Select input review file. CSV only. download from link above"),
                           p("2. Write in keywords separated by comma and press Process Keywords"),
                           p("3. Output can be referred in adjacent tabs as:"),
                           p( "a. Tab Wordcloud: wordcloud for the given keywords"),
                           p("b. Tab Barchart: Bar-chart analysis for the given keywords")),
                  #p("c. Tab Emotional: For the selected client, what product features evoke emotional response"),
                  #p("d. Tab Insights: For the selected client, how should one promote the product")),
                  
                  #tabPanel("Comparison", 
                  # plotOutput('plot1')),
                  
                  tabPanel("Wordcloud",
                           plotOutput('plot')),
                  tabPanel("BarChart",
                           plotOutput('bar'))
                  
                  #tabPanel("Emotional",
                  # dataTableOutput('clust_data')),
                  #tabPanel("Insights",
                  #tableOutput('clust_summary2'))
                  
      ) # end of tabsetPanel
    )
  )
)
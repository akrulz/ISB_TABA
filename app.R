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

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    Dataset <- reactive({
        
        if (is.null(input$file)) { return(NULL) } else
        {
            
            df1<-read.csv(input$file$datapath, header = TRUE)
        }   
            #cleaning up data
            df1[[2]]  =  gsub("<.*?>", " ", df1[[2]])
            df1[[2]]  =  gsub("[^\x01-\x7f]", " ", df1[[2]])
            df1[[2]]=gsub('\n',"",df1[[2]])
            df1[[2]]=gsub("^[\x80-\xff]"," ",df1[[2]])
            
            data<-df1%>%rename(ID=X) %>% select(reviewtext,brand,ID)
            #seltos <- filter(data, brand=="Kia")
            #hector<-filter(data, brand=="MG Hector")
            #jeep<-filter(data, brand=="Jeep Compass")
            
            return(data)
         # else stmt ends
        
    })  # reactive stmt ends
    
    keyword_list<-reactive({
      
       kw =input$text
       kw=tolower(kw)
       kw=strsplit(kw,",")
       ll=data.frame(kw,stringsAsFactors = TRUE)
       colnames(ll)<-c("word")
       #browser()
       
        return(ll)
    })
    set.seed(1234)
    wc_rep<-repeatable(wordcloud)
    
   output$plot<-renderPlot({
       if (is.null(input$file)) {return(NULL)}
       data=Dataset()
       
       kw=keyword_list()
       #browser()
       data<-data[[1]]
       data=data_frame(text=data)
       sent_tokenized = data %>% unnest_tokens(sentence, text, token = "sentences", to_lower = TRUE)
       #browser()
       keyword_filtered = sent_tokenized %>%
  mutate(linenumber = row_number()) %>%
  ungroup()%>% unnest_tokens(word, sentence)%>%
inner_join(kw)
       #,by=c("word"="text"
      # data_frame(text=tolower(kw)
       fr=count(keyword_filtered,word)
     
       #browser()
       
       
       
       
       wc_rep(words = fr$word, freq = fr$n, min.freq = 1,
                     max.words=100, random.order=FALSE, rot.per=0.35, 
                     colors=brewer.pal(8, "Dark2"))   
       
       
       
       
   })
   
   output$bar<-renderPlot({   
     
     data=Dataset()
     
     kw=keyword_list()
     
     data<-data[[1]]
     data=data_frame(text=data)
     sent_tokenized = data %>% unnest_tokens(sentence, text, token = "sentences", to_lower = TRUE)
     
     keyword_filtered = sent_tokenized %>%
       mutate(linenumber = row_number()) %>%
       ungroup()%>% unnest_tokens(word, sentence)%>%
       inner_join(kw)
     
     fr=count(keyword_filtered,word)
     
     color <- c("blue", "red")
     #browser()
     
     barplot(height = as.vector(fr[[2]]),  # A vector of heights
             data=as.vector(fr[[1]]),       
             names.arg = as.vector(fr[[1]]), # A vector of names
                    main = "Words Barchart", 
                    xlab = "Word", 
                    ylab = "Count")
     #browser()
   
   })
}
# Run the application 
shinyApp(ui = ui, server = server)

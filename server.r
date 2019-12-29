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
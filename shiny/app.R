library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(plotly)
library(stringr)
library(DT)

ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),  
  titlePanel("Facebook ads analysis"),
  tags$div(class="header", checked=NA,
           tags$p("")),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(class="header", checked=NA,
               tags$p("This Shiny app uses Facebook ads collected by ProPublica"),
               tags$a(href="https://propublica.org/datastore/dataset/political-advertisements-from-facebook", "Download the data.")),
      hr(),
      textAreaInput("keyword", "Search for specific politician or keyword"),
      tags$div(class="header", checked=NA,
               tags$p("Try Beto, Cruz, immigration, Russia...")),
    #  textInput("removeoutlier", "", ""),
      hr(),
      textInput("pos", "Change color #1", "dodgerblue"),
      textInput("neg", "Change color #2", "firebrick"),
      textInput("neutral", "Change color #3", "gray48"),
      hr(),
      tags$div(class="header", checked=NA,
               tags$p("Have a TXT file for textual and sentiment analysis?"),
               tags$a(href="https://storybench.shinyapps.io/textanalysis/", "Try out our TXT Shiny app.")),
      hr(),
      tags$div(class="header", checked=NA,
               tags$p("Have a CSV file for textual and sentiment analysis?"),
               tags$a(href="https://storybench.shinyapps.io/csvanalysis/", "Try out our CSV Shiny app.")),
      hr(),
      tags$div(class="header", checked=NA,
               tags$p("This analysis uses the R packages 'tidytext' and 'plotly' and the 'labMT' sentiment dictionary from Andy Reagan. Created by Aleszu Bajak."))
    ),
    
    mainPanel(
      h4("Political ads over time and who ran them", align = "center"),
      plotlyOutput("p_waves", height=700),
      h4("Histogram of Facebook ads containing your keyword", align = "center"),
      plotOutput("p2"),
      h4("Top 20 advertisers whose Facebook ads mention your keyword", align = "center"),
      plotOutput("p1", height=500),
      h4("Sentiment of Facebook ads mentioning your keyword", align = "center"),
      plotlyOutput("p3", height=500),
      h4("Facebook posts with your keyword", align = "center"),
      DTOutput("dt1")
    )
  )
)

server <- function(input, output, session) {
  
  df <- read.csv("fbads_bcir.csv", header=TRUE, stringsAsFactors = FALSE) 
  
  # interactive plot
  
  output$p_waves <- renderPlotly({
    if (is.null(input$keyword)){
      return(NULL)      
    }
    
    selectcomments <- df %>% filter(str_detect(message, input$keyword))
    
    betowaves <- ggplot(selectcomments, aes(date, advertiser, text = message, impressions = impressions)) + 
      geom_point(aes(size=impressions)) 
    
    ggplotly(betowaves, tooltip=c("text","impressions"))
    
  })
  
  # Plot 1
  
  output$p1 <- renderPlot({
    
    if (is.null(input$keyword)){
      return(NULL)      
    }
    
    selectcomments <- df %>% filter(str_detect(message, input$keyword))
    
    top_advs <- selectcomments %>%
      count(advertiser) %>% 
      arrange(desc(n)) %>%
      top_n(20)
  
    p1 <-  ggplot(top_advs, aes(reorder(advertiser, n), n)) + 
      geom_bar(stat="identity", fill=input$neutral) + 
      coord_flip() +
      theme(legend.position = 'none') +
      xlab("advertiser") 
    p1 
    
  })
  
  # DT1 of title, message, date, advertiser
  
  output$dt1 <- DT::renderDataTable({
    
    if (is.null(input$keyword)){
      return(NULL)      
    }
    
    selectcomments <- df %>% filter(str_detect(message, input$keyword))
    
    dt1 <- selectcomments %>%
      select(date, title, message, advertiser, impressions, page)
    
    DT::datatable(dt1)
    
  })
  
  # Plot 2
  
  output$p2 <- renderPlot({
    
    if (is.null(input$keyword)){
      return(NULL)      
    }
    
    selectcomments <- df %>% filter(str_detect(message, input$keyword))
    
    p2 <- ggplot(selectcomments, aes(date)) + 
      geom_histogram(bins = 300, stat="count", fill=input$neutral) +
      xlab("")
    p2
    
  })
  
  # plot 3 sentiment over time 
  
  output$p3 <- renderPlotly({
    
    if (is.null(input$keyword)){
      return(NULL)
    }
    
    selectcomments <- df %>% filter(str_detect(message, input$keyword))
    
    selectcomments$post <- selectcomments$message
    
    tokenized_comments <- selectcomments %>%
      select(paid_for_by, advertiser, post, date, impressions, title, message) %>%
      unnest_tokens(word, message) %>%
      anti_join(stop_words) %>%
      group_by(word, paid_for_by, advertiser, date, post, impressions, title) %>%
      tally() %>%
      arrange(desc(n))
    
    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
      select(word, happs)
    
    all_sentiment <- tokenized_comments %>%  
      inner_join(labMT, by = "word") %>%
      group_by(post, paid_for_by, advertiser, date, impressions, title) %>% 
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("score" = sentiment-5.372) 
    
    allsentplot <- ggplot(all_sentiment, aes(date, score, color=score, text = post, score = score)) + 
      geom_point() +
      scale_color_gradient(low=input$neg, high=input$pos)
    
    ggplotly(allsentplot, tooltip=c("text","word"))
    
  })
  
}


shinyApp(ui = ui, server = server)
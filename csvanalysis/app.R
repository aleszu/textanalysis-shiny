
library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(DT)

ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),  
  titlePanel("Drag-and-drop CSV textual analysis"),
  tags$div(class="header", checked=NA,
           tags$p("Upload a CSV file with 'date' and 'headline' columns to run an exploratory sentiment analysis. 
                  Your 'date' column should have a MM/DD/YYYY or MM-DD-YYYY format.")),
  hr(),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    
    sidebarPanel(
      fileInput("file", "Upload your CSV file"),
      selectInput("smooth", "Choose number of words to score", choices = c("25","50","100", "150")),
      tags$div(class="header", checked=NA,
               tags$p("By choosing 50, for example, you'll only be visualizing the 50 most negative and 50 most positive words across time.")),
      hr(),
      textAreaInput("keyword", "Optional search for specific names"),
      tags$div(class="header", checked=NA,
               tags$p("Try looking for specific keywords to see their use over time.")),
      hr(),
      #     actionButton("update", "Update"),
      #     hr()
      tags$div(class="header", checked=NA,
               tags$p("Some datasets you might be curious to explore."),
               tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/r-politics-three-months.csv", "Three months of r/politics headlines"),
               tags$br(),
               tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/trumptweets-nov16.csv", "@realDonaldTrump tweets through Nov. 2016"),
               tags$br(),
               tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/murkowski.csv", "Six months of Sen. Lisa Murkowski articles")),
      tags$br(),
      tags$div(class="header", checked=NA,
               tags$p("Have a TXT file for sentiment analysis?"),
               tags$a(href="https://storybench.shinyapps.io/textanalysis/", "Try out my other drag-and-drop app.")),
      hr(),
      tags$div(class="header", checked=NA,
               tags$p("This analysis uses the R package 'tidytext' and the 'labMT' sentiment dictionary from Andy Reagan. Created by Aleszu Bajak."))
      ),
    
    mainPanel(
    #  hr(),
    #  h4("Sentiment of top 25 most negative and positive words across time", align = "center"),
    #  plotOutput("p_sentC"),
    #  hr(),
      h4("Sentiment analysis of headlines with smoothing", align = "center"),
      plotOutput("p_smooth"),
      hr(),
      h4("Headlines - or keyword - over time", align = "center"),
      plotOutput("keywordplot"),
      h4("Scores and context of the most positive words", align = "center"),
      DTOutput("tbtop50"),
      hr(),
      h4("Scores and context of the most negative words", align = "center"),
      DTOutput("tbbot50"),
      hr(),
      h4("Full dataset", align = "center"),
      DTOutput('tb')
    )
  )
)


server <- function(input, output, session) {
  
  filedata <- reactive({
    
    infile <- input$file
    if (is.null(infile)){
      return(NULL)   
    }
    read.csv(infile$datapath, header=TRUE, stringsAsFactors = FALSE)
    
  })
  
  # output$column <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   nums <- sapply(df, is.numeric) # keep only number columns
  #   items=names(nums[nums]) # keep only number columns
  #   names(items)=items
  #   selectInput("date", "Choose column with date either in MM/DD/YYYY or MM-DD-YYYY format", items)
  # })

  output$p_sentC <- renderPlot({
    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)
    
    df <- filedata()
    df$date <- as.Date(df$date, format = "%m/%d/%Y")
    
    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
      select(word, happs)
    
    tokenizedC <- df %>%
      select(date, headline) %>% #input date column 
      unnest_tokens(word, headline) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word, date) %>%
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("score" = sentiment-5.372 ) 
    tokenizedC
    
    topsentC <- head(arrange(tokenizedC,desc(score)), n = input$smooth) 
    topsentC
    bottomsentC <- head(arrange(tokenizedC,score), n = input$smooth) 
    bottomsentC
    
    sentimentC <- rbind(topsentC, bottomsentC)
    sentimentC
    
    # Sentiment over time of top and bottom words
    
    p_sentC <- ggplot(sentimentC, aes(x= date, 
                                      y = score, 
                                      fill = score > 0)) + #this is midpoint of labMT sentiment dictionary
      geom_col(show.legend = FALSE) +
      ylab("sentiment") +
      xlab("time") + 
      scale_y_continuous(limits=c(-4, 4)) +
      scale_x_date(date_breaks = "1 month") +
      scale_fill_manual(values=c("red", "blue"))
    
    p_sentC
    
  })
  
  output$p_smooth <- renderPlot({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)
    library(ggplot2)
    
    df <- filedata()
    df$date <- as.Date(df$date, format = "%m/%d/%Y")
    df$headlines <- df$headline
    
    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
      select(word, happs)
    
    tokenizedC <- df %>%
      select(date, headline, headlines) %>% #input date column 
      unnest_tokens(word, headline) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word, date, headlines) %>%
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("score" = sentiment-5.372 ) 

    topsentC <- head(arrange(tokenizedC,desc(score)), n=input$smooth) 

    bottomsentC <- head(arrange(tokenizedC,score), n=input$smooth) 

    sentimentC <- rbind(topsentC, bottomsentC)

    p_smooth <- ggplot(sentimentC, aes(x = date, y = score)) +
      geom_smooth() +
      ylab("sentiment") +
      xlab("time") +
      scale_x_date(date_breaks = "1 month")
    p_smooth
    
    
  })
  
    # Searching keyword
  
    output$keywordplot <- renderPlot({
    if (is.null(input$file)){
      return(NULL)      
    }
    
      library(dplyr)
      library(tidyverse)
      library(tidytext)
      library(ggplot2)
      
      df <- filedata()
      df$date <- as.Date(df$date, format = "%m/%d/%Y")
      df$headlines <- df$headline
      
      sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
      labMT <- sentiments %>%
        select(word, happs)
      
      headlines_w_keyword <- df %>%
        select(date, headline, headlines) %>%
        unnest_tokens(word, headline) %>%
        anti_join(stop_words) %>%
        group_by(word, date, headlines) %>%
        filter(str_detect(headlines, input$keyword))
      
      p <- ggplot(headlines_w_keyword, aes(date)) + geom_line(stat = "count") 
      p
      
    })
    
    
  
  
    # Table 1
    
    output$tb <- DT::renderDataTable({
      df <- filedata()
      DT::datatable(df)
    })
    
    # Table 2
    
    output$tbtop50 <- DT::renderDataTable({
      
      if (is.null(input$file)){
        return(NULL)      
      }
      
      library(dplyr)
      library(tidyverse)
      library(tidytext)
      
      df <- filedata()
      df$date <- as.Date(df$date, format = "%m/%d/%Y")
      df$headlines <- df$headline
      
      sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
      labMT <- sentiments %>%
        select(word, happs)
      
      tokenizedC <- df %>%
        select(date, headline, headlines) %>% #input date column 
        unnest_tokens(word, headline) %>%
        anti_join(stop_words) %>%
        inner_join(labMT, by = "word") %>%
        group_by(word, date, headlines) %>%
        summarize(sentiment = mean(happs)) %>%
        arrange(desc(sentiment)) %>%
        mutate("score" = sentiment-5.372 ) 
      
      topsentC <- head(arrange(tokenizedC,desc(score)), n = input$smooth)
      
      DT::datatable(topsentC)

    })
    
    # Table 3
    
    output$tbbot50 <- DT::renderDataTable({
      
      if (is.null(input$file)){
        return(NULL)      
      }
      
      library(dplyr)
      library(tidyverse)
      library(tidytext)
      
      df <- filedata()
      df$date <- as.Date(df$date, format = "%m/%d/%Y")
      df$headlines <- df$headline
      
      sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
      labMT <- sentiments %>%
        select(word, happs)
      
      tokenizedC <- df %>%
        select(date, headline, headlines) %>% #input date column 
        unnest_tokens(word, headline) %>%
        anti_join(stop_words) %>%
        inner_join(labMT, by = "word") %>%
        group_by(word, date, headlines) %>%
        summarize(sentiment = mean(happs)) %>%
        arrange(desc(sentiment)) %>%
        mutate("score" = sentiment-5.372 ) 
      
      bottomsentC <- head(arrange(tokenizedC,score), n = input$smooth)
      
      DT::datatable(bottomsentC)
      
    })
  
}



shinyApp(ui = ui, server = server)


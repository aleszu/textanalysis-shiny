
library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(DT)
library(plotly)
library(udpipe)

ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),  
  titlePanel("Drag-and-drop CSV textual analysis"),
  tags$div(class="header", checked=NA,
           tags$p("Upload a CSV file with 'date' and 'headline' columns to run an exploratory sentiment analysis. 
                  Your 'date' column should have a MM/DD/YYYY or MM-DD-YYYY format. Files should not exceed 5MB.")),
  hr(),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    
    sidebarPanel(
      fileInput("file", "Upload your CSV file"),
      selectInput("smooth", "Choose number of words to score", choices = c("50","100", "150")),
      tags$div(class="header", checked=NA,
               tags$p("By choosing 50, for example, you'll only be visualizing the 50 most negative and 50 most positive words across time.")),
      hr(),
      textAreaInput("keyword", "Search for specific words over time"),
      tags$div(class="header", checked=NA,
               tags$i("Optional")),
      hr(),
      textInput("neg", "Change negative color", "red"),
      textInput("pos", "Change positive color", "blue"),
      hr(),
      textInput("filter1", "Remove word from sentiment analysis", ""),
      tags$div(class="header", checked=NA,
               tags$i("Optional")),
      hr(),
      tags$div(class="header", checked=NA,
               tags$p("Some datasets you might be curious to explore."),
               tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/r-politics-three-months.csv", "Three months of r/politics headlines"),
               tags$br(),
               tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/wapo-articles.csv", "One year of Washington Post Politics headlines"),
               tags$br(),
               tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/nyt-articles.csv", "One year of New York Times Politics headlines"),
               tags$br(),
               tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/Heitkamp-articles-3-months.csv", "Three months of Sen. Heidi Heitkamp articles"),
               tags$br(),         
               tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/trumptweets-nov16.csv", "@realDonaldTrump tweets through Nov. 2016")),
      tags$br(),
      hr(),
      tags$div(class="header", checked=NA,
               tags$p("Have a TXT file for sentiment analysis?"),
               tags$a(href="https://storybench.shinyapps.io/textanalysis/", "Try out my other drag-and-drop app.")),
      hr(),
      tags$div(class="header", checked=NA,
               tags$p("This analysis uses the R package 'tidytext' and the 'labMT' sentiment dictionary from Andy Reagan. Created by Aleszu Bajak."))
      ),
    
    mainPanel(
      # tabsetPanel(type = "tabs",
      #             tabPanel("Sentiment", plotOutput("p_smooth")),
      #             tabPanel("Time series", plotOutput("keywordplot")),
      #             tabPanel("Top terms", plotOutput("topterms")),
      #             tabPanel("Positive terms", DTOutput("tbtop50")),
      #             tabPanel("Negative terms", DTOutput("tbbot50")),
      #             tabPanel("Full dataset", DTOutput("tb"))
      # )
    
      
          h4("Sentiment analysis of headlines with smoothing", align = "center"),
          plotOutput("p_smooth"),
          h4("Headlines - or inputted keyword - over time", align = "center"),
          plotOutput("keywordplot"),
          h4("Top words in headlines", align = "center"),
          plotOutput("textsmooth"),
          h4("Most positive and negative words in headlines", align = "center"),
          plotOutput("p_sentC"),
          h4("Top terms in headlines", align = "center"),
          plotOutput("topterms"),
          h2("Tables", align = "center"),
          h4("Scores and context of the most positive words", align = "center"),
          DTOutput("tbtop50"),
          h4("Scores and context of the most negative words", align = "center"),
          DTOutput("tbbot50"),
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
      filter(word != input$filter1) %>%
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
      geom_hline(yintercept=0, linetype="dashed") +
      scale_x_date(date_labels = "%b %d")
    p_smooth
    
    
  })

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
      filter(word != input$filter1) %>%
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("score" = sentiment-5.372 ) 
    tokenizedC
    
    topsentC <- head(arrange(tokenizedC,desc(score)), n = 25) 
    topsentC
    bottomsentC <- head(arrange(tokenizedC,score), n = 25) 
    bottomsentC
    
    sentimentC <- rbind(topsentC, bottomsentC)
    sentimentC
    
    # Sentiment over time of top and bottom words
    
    p_sentC <- ggplot(sentimentC, aes(x= reorder(word, -score), 
                                      y = score, 
                                      fill = score > 0)) + #this is midpoint of labMT sentiment dictionary
      geom_col(show.legend = FALSE) +
      coord_flip() +
      ylab("sentiment") +
      xlab("word") + 
      scale_y_continuous(limits=c(-4, 4)) +
      scale_fill_manual(values=c(input$neg,input$pos))
    
    p_sentC
    
  })
  
  output$textsmooth <- renderPlot({
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
      filter(word != input$filter1) %>%
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("score" = sentiment-5.372 ) 
    
    # topsentC <- head(arrange(tokenizedC,desc(score)), n = 25) 
    # topsentC
    # bottomsentC <- head(arrange(tokenizedC,score), n = 25) 
    # bottomsentC
    # 
    # sentimentC <- rbind(topsentC, bottomsentC)
    # sentimentC
    
    # Sentiment over time of top and bottom words
    
    p_textsmooth <- ggplot(tokenizedC, aes(x=date, y=score)) +
      geom_text(aes(label = word, color=score >0), check_overlap = TRUE, vjust = 2.5) +
      geom_hline(yintercept = 0, lty = 2) +
      scale_color_manual(values=c(input$neg,input$pos)) +
      xlab("date") + 
      ylab("score") +
      theme(legend.position = 'none') +
      geom_smooth() 
    
    p_textsmooth
    
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
      
      headlines_w_keyword <- df %>%
        select(date, headline, headlines) %>%
        unnest_tokens(word, headline) %>%
        anti_join(stop_words) %>%
        group_by(word, date, headlines) %>%
        filter(str_detect(headlines, input$keyword))
      
      p <- ggplot(headlines_w_keyword, aes(date)) + geom_line(stat = "count") 
      p
      
    })
    
    
    
    
   # top terms 
    
    output$topterms <- renderPlot({ 
      
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
      
      tokenizedC <- df %>%
        select(date, headline, headlines) %>% 
        unnest_tokens(word, headline) %>%
        anti_join(stop_words) %>%
        group_by(word) %>%
        tally()
      
      topterms <- tokenizedC %>%
        top_n(20) %>%
        arrange(desc(n)) %>%
        ggplot(aes(x = reorder(word, -n), y = n)) +
        geom_col() +
        labs(x = NULL, y = "frequency") +
        coord_flip() +
        theme(legend.position = "none")
      topterms
      
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
        filter(word != input$filter1) %>%
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


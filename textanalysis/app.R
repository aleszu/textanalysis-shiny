
library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(DT)

ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),  
  titlePanel("Drag-and-drop textual analysis"),
  tags$div(class="header", checked=NA,
           tags$p("Upload a text file and choose a keyword below to run an exploratory textual and sentiment analysis")),
  hr(),
  
  sidebarLayout(
     # Sidebar with a slider and selection inputs
 
      sidebarPanel(
        fileInput("file", "Upload your txt file"),
        hr(),
        textInput("keyword", "Search for a keyword", ""),
        hr(),
        tags$div(class="header", checked=NA,
                 tags$p("Once you've uploaded a document, scroll down to see contextual sentences, sentiment analysis and top bi- and trigrams.")),
        hr(),
        textInput("neg", "Change negative color", "red"),
        hr(),
        textInput("pos", "Change positive color", "blue"),
        hr(),
        tags$div(class="header", checked=NA,
            tags$p("A dataset you might be curious to explore."),
            tags$a(href="https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/trumpspeeches.txt", "Trump's campaign speeches.")),
        tags$br(),
         tags$div(class="header", checked=NA,
                  tags$p("Have a CSV file for sentiment analysis?"),
                  tags$a(href="https://storybench.shinyapps.io/csvanalysis/", "Try out my other drag-and-drop app.")),
         hr(),
         tags$div(class="header", checked=NA,
                  tags$p("This analysis uses the R package 'tidytext' and the 'labMT' sentiment dictionary from Andy Reagan. Created by Aleszu Bajak."))
            
    ),

    mainPanel(
      
      h4("Sentences", align = "center"),
      DTOutput("tb"),
      h4("Most negative and most positive words", align = "center"),
      plotOutput("p_sentT"),
      h4("Top 50 positive words", align = "center"),
      DTOutput("tbpos"),
      h4("Top 50 negative words", align = "center"),
      DTOutput("tbneg"),
      h4("Top bigrams", align = "center"),
      DTOutput("bigramsT"),
      h4("Top bigrams", align = "center"),
      plotOutput("biplot"),
      h4("Top trigrams", align = "center"),
      DTOutput("trigramsT"),
      h4("Top trigrams", align = "center"),
      plotOutput("triplot")
    )
  )
)


server <- function(input, output, session) {
  
  filedata <- reactive({
    infile <- input$file
    if (is.null(infile)){
      return(NULL)   
    }
    scan(infile$datapath, character(0), sep=".",quote=NULL)
    
  })

  output$tb <- DT::renderDataTable({
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata() 
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    DT::datatable(df2)
    
  })

  output$tbpos <- DT::renderDataTable({

    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)

    LookForKeyword <- c(input$keyword)

    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    # tokenizedT <- df2 %>%
    #   select(value) %>%
    #   unnest_tokens(word, value) %>%
    #   count(word, sort = TRUE) %>%
    #   ungroup()
    # tokenizedT
    # 
    # tokenized_rem_stopwordsT <- tokenizedT %>%
    #   anti_join(stop_words)
    
    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
      select(word, happs)
    
    ### Quick sentiment analysis
    
    allsentimentT <- df2 %>%  
      select(value) %>%
      unnest_tokens(word, value) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word) %>%
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("sentiment2" = sentiment-5.372 )
    
    # Bind 10 most positive terms and 10 most negative terms
    
    topsent <- allsentimentT %>%
      top_n(50) 

    DT::datatable(topsent)
    
    # wcT <- wordcloud(words = tokenized_rem_stopwordsT$word, freq = tokenized_rem_stopwordsT$n, min.freq = 1,
    #                  max.words=100, random.order=FALSE, rot.per=0.15,
    #                  colors=brewer.pal(8, "RdGy"))
    # wcT

  })
  
  
  output$tbneg <- DT::renderDataTable({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    # tokenizedT <- df2 %>%
    #   select(value) %>%
    #   unnest_tokens(word, value) %>%
    #   count(word, sort = TRUE) %>%
    #   ungroup()
    # tokenizedT
    # 
    # tokenized_rem_stopwordsT <- tokenizedT %>%
    #   anti_join(stop_words)
    
    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
      select(word, happs)
    
    ### Quick sentiment analysis
    
    allsentimentT <- df2 %>%  
      select(value) %>%
      unnest_tokens(word, value) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word) %>%
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("sentiment2" = sentiment-5.372 )
    
    bottomsent <- head(arrange(allsentimentT,sentiment2), n = 50) 
    
    DT::datatable(bottomsent)
    
    
  })
  
  
  
  output$p_sentT <- renderPlot({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
      select(word, happs)
    
    ### Quick sentiment analysis
    
    allsentimentT <- df2 %>%  
      select(value) %>%
      unnest_tokens(word, value) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word) %>%
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("sentiment2" = sentiment-5.372 )
    
    # Bind 10 most positive terms and 10 most negative terms
    
    bottomsentT <- allsentimentT %>%
      top_n(-10) 
    topsentT <- allsentimentT %>%
      top_n(10) 
    sentimentT <- bind_rows(bottomsentT,topsentT) %>%
      arrange(desc(sentiment2)) %>%
      distinct() # remove duplicates
    sentimentT
    
    p_sentT <- ggplot(sentimentT, aes(x= reorder(word, -sentiment2), 
                                      y = sentiment2, 
                                      fill = sentiment2 > 0)) + #this is midpoint of labMT sentiment dictionary
      geom_col(show.legend = FALSE) +
      coord_flip() +
      ylab("sentiment") +
      xlab("word") + 
      scale_y_continuous(limits=c(-5, 5)) +
      scale_fill_manual(values=c(input$neg,input$pos))
    
    p_sentT
    
  })
  
  output$bigramsT <- DT::renderDataTable({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)
    library(wordcloud)
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    bigrams <- df2 %>%
      select(value) %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 2) %>%
      count(ngram, sort = TRUE) %>%
      ungroup()

    DT::datatable(bigrams)
  
 })
  
  output$biplot <- renderPlot({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)
    library(wordcloud)
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    bigrams_15 <- df2 %>%
      select(value) %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 2) %>%
      count(ngram, sort = TRUE) %>%
      ungroup() %>%
      top_n(25)
    
    p7 <- ggplot(bigrams_15, aes(x=reorder(ngram, -n), y=n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      ylab("frequency") +
      xlab("ngram")
    
    p7
    
  })
  
  # trigramsT
  
  output$trigramsT <- DT::renderDataTable({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)
    library(wordcloud)
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    trigrams <- df2 %>%
      select(value) %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 3) %>%
      count(ngram, sort = TRUE) %>%
      ungroup()
    
    DT::datatable(trigrams)
    
  })
  
  output$triplot <- renderPlot({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    library(dplyr)
    library(tidyverse)
    library(tidytext)
    library(wordcloud)
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    trigrams_15 <- df2 %>%
      select(value) %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 3) %>%
      count(ngram, sort = TRUE) %>%
      ungroup() %>%
      top_n(25)
    
    p6 <- ggplot(trigrams_15, aes(x=reorder(ngram, -n), y=n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      ylab("frequency") +
      xlab("ngram")
    
    p6
    
  })
  
  
    
}


 
shinyApp(ui = ui, server = server)


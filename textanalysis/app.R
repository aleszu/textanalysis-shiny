
library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(wordcloud)
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
        
    #  textAreaInput("text", "Enter text", rows = 7),
      fileInput("file", "Upload your txt file"),
      hr(),
    #  selectInput("file2", "Or select a corpus", choices = c("New York Times headlines")),
    #  hr(),
      textInput("keyword", "Search for a keyword", "addiction"),
      hr(),
      tags$div(class="header", checked=NA,
               tags$p("Once you've uploaded a document or selected a corpus, scroll down to see contextual sentences, a wordcloud of top terms, and sentiment analysis.")),
      hr(),
 #    selectInput("keyword", "Choose a keyword", choices = c("addiction","drugs","opioid"))
 #     actionButton("update", "Update"),
 #     hr()
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
      h4("Sentences with your keyword", align = "center"),
   #  tableOutput("tb"),
      DTOutput('tb'),
      h4("Sentiment of words appearing with your keyword", align = "center"),
      plotOutput("p_sentT"),
      h4("Top words appearing in the context of your keyword", align = "center"),
      plotOutput("wordcloudT") #,
#      h4("Top bigrams appearing with your keyword", align = "center"),
#      plotOutput("bigramsT"),
#      h4("Top trigrams appearing with your keyword", align = "center"),
#      plotOutput("trigramsT")
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

  output$wordcloudT <- renderPlot({
    
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
    tokenizedT <- df2 %>%
      select(value) %>%
      unnest_tokens(word, value) %>%
      count(word, sort = TRUE) %>%
      ungroup()
    tokenizedT
    
    tokenized_rem_stopwordsT <- tokenizedT %>%
      anti_join(stop_words)
    tokenized_rem_stopwordsT
    
    wcT <- wordcloud(words = tokenized_rem_stopwordsT$word, freq = tokenized_rem_stopwordsT$n, min.freq = 1,
                     max.words=100, random.order=FALSE, rot.per=0.15, 
                     colors=brewer.pal(8, "RdGy"))
    wcT
    
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
    allsentimentT
    
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
      scale_y_continuous(limits=c(-4, 4)) +
      scale_fill_manual(values=c("red", "blue"))
    
    p_sentT
    
  })
  
  
  
}


 
shinyApp(ui = ui, server = server)


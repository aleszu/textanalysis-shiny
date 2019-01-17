library(shiny)

ui <- fluidPage(
   
   titlePanel("Drag-and-drop mapmaker"),
   
   sidebarLayout(
    
     sidebarPanel(

       fileInput("file1", "Choose your CSV File",
                 accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
       ),
       h6("*Make sure your CSV contains columns named 'lat' and 'lon' and a numerical column to size the dots", align = "left"),
       tags$hr(),
      # uiOutput("color"),
      textAreaInput("title", "Map title"), 
      textAreaInput("color", "Choose dot color using hex code", "#1e90ff"),
      radioButtons("radio", label = h5("Choose dot shape"), 
                  choices = list("Circle" = 1, "Square" = 0, "Triangle" = 2, "Filled circle" = 16, "Filled square" = 15,"Filled triangle" = 17),
                  selected = 16),
      # selectInput("color", "Choose dot color using hex code", #choices = c("black","gray30","wheat","firebrick","darkgreen","dodgerblue","lightpink")),
     #  tags$hr(),
       uiOutput("column"),
       numericInput("low", "A minimum dot size:", value = 1, min = 0, max = 5),  
       numericInput("high", "A maximum dot size:", value = 15, min = 5, max = 30),  
     textAreaInput("legend", "Legend title"),
       #tags$hr(),
       #uiOutput("lat"),
       #uiOutput("lon"),
       tags$hr() #,
   #    downloadButton("downloadPlot", "Save PNG")
     ),
     mainPanel(
      # h4("My map", align = "left"),
       plotOutput("stateMap"),
       
       h4("My data", align = "left"),
       tableOutput("contents")
     )
     
     
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath)
  })
  
  
  
  library(dplyr)
  library(tidyverse)
  library(tidytext)
  library(ggplot2)
  
  # shapeinput <- reactive({
  #   switch(input$shape,
  #          "Circle" = 1,
  #          "Square" = 0,
  #          "Triangle" = 2,
  #          "Filled circle" = 16,
  #          "Filled square" = 15,
  #          "Filled triangle" = 17
  #   )
  # })
  
  # listVal <- reactive({
  #   if (identical(input$column, "null"))
  #     NULL
  #   else 
  #     input$column
  # })
  
  output$column <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #  items=names(df) # add back in if you want all columns
    nums <- sapply(df, is.numeric) # keep only number columns
    items=names(nums[nums]) # keep only number columns
    names(items)=items
    selectInput("bubble", "Choose column to map to dot size", items)
  })
  
  output$color <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #  items=names(df) # add back in if you want all columns
    nums <- sapply(df, is.numeric) # keep only number columns
    items=names(nums[nums]) # keep only number columns
    names(items)=items
    selectInput("color", "Choose column for color",items)
  })
  
 #  output$lat <- renderUI({
 #    df <- filedata()
 #    if (is.null(df)) return(NULL)
 #    items=names(df) # add back in if you want all columns
 # #   nums <- sapply(df, is.numeric) # keep only number columns
 # #   items=names(nums[nums]) # keep only number columns
 #    names(items)=items
 #    selectInput("latitude", "Choose column for latitude",items, selected=df$lat)
 #  })
 #  
 #  output$lon <- renderUI({
 #    df <- filedata()
 #    if (is.null(df)) return(NULL)
 #    items=names(df) # add back in if you want all columns
 #  #  nums <- sapply(df, is.numeric) # keep only number columns
 #  #  items=names(nums[nums]) # keep only number columns
 #    names(items)=items
 #    selectInput("longitude", "Choose column for longitude",items, selected=df$lon)
 #  })
  
  output$contents = renderTable({
    df <- filedata()
    return(df)
  })
  
  # construct a plot to show the data

  
  # output$stateMap <- renderPlot({
  #   df <- filedata()
  #   
  #   # p1 <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill= "grey92") +
  #   #   geom_point(data = df, aes_string(x=input$longitude, y=input$latitude, size=input$bubble), inherit.aes = FALSE) +
  #   #   scale_size(name="", range = c(2, 15)) +
  #   #   guides(size=guide_legend(input$legend)) +
  #   #   theme_void() +
  #   #   theme(aspect.ratio=1.77/3)
  #   # p1
  #   
  #   p2 <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill= "grey92") +
  #     geom_point(data = df, aes_string(x=df$lon, y=df$lat, size=input$bubble, color=input$color), inherit.aes = FALSE) +
  #     scale_size(name="", range = c(2, 15)) +
  #     guides(size=guide_legend(input$legend)) +
  #     theme_void() +
  #     theme(aspect.ratio=1.62/3)
  #   p2
  
  
  
  output$stateMap <- renderPlot({
  
  df <- filedata()
  #df_subset <- subset(df, input$bubble > 0) # subsetting not working

  p3 <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill= "grey92") +
    geom_point(data = df, aes_string(x=df$lon, y=df$lat, size=input$bubble), color = input$color, shape = as.numeric(input$radio)) +
    scale_size(name="", range = c(input$low, input$high)) +
    ggtitle(input$title) +
    guides(size=guide_legend(input$legend)) +
    theme_void() +
    theme(aspect.ratio=1.62/3)
  p3

  })
  
  output$downloadPlot <- downloadHandler(
   filename = function() {paste('map-', Sys.Date(), '.png', sep='')},
    content = function(file) { 
      png(file, type='cairo')
      ggsave("storybench-mapmaker.png", width=14.4,height=7.43,units="in")
    },
    contentType = 'image/png'
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)


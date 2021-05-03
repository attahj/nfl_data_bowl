# Gridiron Gang Shiny App for Google NFL Project

library(shiny)
library(bigrquery)
library(DBI)
library(dplyr)
library(dbplyr)
library(plotly)
source('Functions.R')


################################################################################
# Define UI for application

ui <- fluidPage(
  fluidRow(
    
    # NFL Logo
    div(img(height = 100, width = 100, src = 'nfl-league-logo.png', 
                      style = "float:left; margin-left: 10px; margin-right: 
                      10px; margin-top: 10px")),
    
    
    # Title
    h1('Google NFL Project'),
    
    # Authors/Owner's
    p('By The Gridiron Gang'),
    
    # Google Cloud Platform Logo
    div(img(height = 100, width = 150, src = 'google-cloud-platform-logo.png', 
                      style = "float:right; margin-left: 10px; margin-right: 
                      50px; margin-top: -85px"))
    
    
  ),
  
  tabsetPanel(
    
  
      
################################################################################
# QB Rank List:
    
    tabPanel("Quarterback Ranks", 
             fluidRow(
               
               # Ranks section
               column(6, wellPanel(h1(style = "font-size: 25px", 
                                     'Quarterback Rank for Finding Open Receiver'), 
                                  style = "background-color: #fff; 
                                  border-color: #2c3e50; height: 720px;",
                                  DT::dataTableOutput('OpenRecRank'))),
                                  
               column(6, wellPanel(h1(style = "font-size: 25px", 
                                      'Quarterback Rank for Difficult Throws'),
                                  style = "background-color: #fff; 
                                  border-color: #2c3e50; height: 720px;",
                                  DT::dataTableOutput('DiffThrowRank')))
            
             )
             
    ),          
    
    
    
################################################################################
# Open Receiver Probability:
    
    tabPanel("Open Receiver",
             fluidRow(
               
               # Graphs section
               column(8, wellPanel(style = "background-color: #fff; 
                                  border-color: #2c3e50; height: 720px;",
                                  plotlyOutput("outOpenRecGraph", width = "100%", height = "675px"))),
                                  
               
               column(4, 
                      
                      # Rank and selection 
                      fluidRow(column(12, wellPanel(style = "background-color: #fff; 
                                      border-color: #2c3e50; height: 500px;",
                                      selectInput('inOpenRecTab', 'Identifying Open Receiver Rank', 
                                                  openRecRank.df$Name, multiple = FALSE, 
                                                  selectize = FALSE, size = 25)))),
                      
                      # Stats and info for selection
                      fluidRow(column(12, wellPanel(h1(style = "font-size: 15px", 
                      'Identifying Open Receiver Percentage'), style = "background-color: #fff; 
                                      border-color: #2c3e50; height: 200px; font-size: 75px",
                                      textOutput('outOpenRecTab'))))
                      
               )
               
             )
             
             
    ),
    
    
    
################################################################################
# Difficult Throw Probability:
    
    tabPanel("Difficult Throw", 
             fluidRow(
               
               # Graphs section
               column(8, wellPanel(style = "background-color: #fff; 
                                  border-color: #2c3e50; height: 720px;", 
                                   imageOutput('outGifImage'))),
                                  
               
               column(4, 
                      
                      # Rank and selection
                      fluidRow(column(12, wellPanel(style = "background-color: #fff; 
                                      border-color: #2c3e50; height: 500px;",
                                      selectInput('inDiffThrowTab', 'Top 20 Difficult Throws', 
                                                  gifComp.df$displayName, multiple = FALSE, 
                                                  selectize = FALSE, size = 25)))),
                      
                      # Stats and info for selection
                      fluidRow(column(12, wellPanel(h1(style = "font-size: 15px", 
                                        'Average Completion Rate'), 
                                        style = "background-color: #fff; 
                                       border-color: #2c3e50; height: 200px; font-size: 75px",
                                      textOutput('outDiffThrowTab'))))
                      
               )
               
             )
             
    ),
    
    
    
################################################################################
# Completion Probability:
    
    tabPanel("Completion Probability",
             fluidRow(
               
               # Completion animation
               column(8, wellPanel(style = "background-color: #fff; 
                                  border-color: #2c3e50; height: 720px;",
                                   imageOutput('outGifImageComp'))),
               
               
               column(4, 
                      
                      # Game and play selection
                      fluidRow(column(12, wellPanel(style = "background-color: #fff; 
                                  border-color: #2c3e50; height: 200px;",
                                                    selectInput('inDiffThrowComp', 'Top 20 Difficult Throws', 
                                                                gifComp.df$displayName, multiple = FALSE, 
                                                                selectize = FALSE, size = 7)))),
                      
                      # Qb List
                      fluidRow(column(12, wellPanel(style = "background-color: #fff; 
                                  border-color: #2c3e50; height: 280px;",
                                                    selectInput('inQbList', 'Quarterback List', 
                                                                qbList.df$displayName, multiple = FALSE, 
                                                                selectize = FALSE, size = 12)))),
                      # Completion Percentage
                      fluidRow(column(12, wellPanel(h1(style = "font-size: 15px", 
                                                       'Selected QB Completion Rate'), 
                                                    style = "background-color: #fff; 
                                       border-color: #2c3e50; height: 200px; font-size: 75px",
                                                    textOutput('outDiffThrowComp')))),
                      
                
              )
    
    
            )
    
        )

    )
  
)



################################################################################
# Define server logic for application

server <- function(input, output) {
  
  output$OpenRecRank = DT::renderDataTable({select(openRecRank.df, 2:3)})
  
  output$DiffThrowRank = DT::renderDataTable({select(difThrow.df, displayName, NumOfDT)})
  
  output$outOpenRecTab = renderText(openRecRank.df
                           [openRecRank.df$Name==input$inOpenRecTab,3])
  
  output$outDiffThrowTab = renderText(gifComp.df
                                      [gifComp.df$displayName==input$inDiffThrowTab, 2])
  
  output$outOpenRecGraph = renderPlotly({eval(parse(text=paste(
    "OpenRec", openRecRank.df[openRecRank.df$Name==input$inOpenRecTab,1],sep="")))})
  
  
  output$outGifImage = renderImage({list(src= paste("www/",gifComp.df
                                  [gifComp.df$displayName==input$inDiffThrowTab,3],
                                  gifComp.df[gifComp.df$displayName==input$inDiffThrowTab,4], 
                                  '.gif',sep=""), height =675,width=775)})
  
  output$outGifImageComp = renderImage({list(src= paste("www/",gifComp.df
                                          [gifComp.df$displayName==input$inDiffThrowComp,3],
                                          gifComp.df[gifComp.df$displayName==input$inDiffThrowComp,4], 
                                          '.gif',sep=""), height =675,width=775)})
  
  output$outDiffThrowComp = renderText({100 * compModel(gifComp.df
                                                 [gifComp.df$displayName==input$inDiffThrowComp, 3],
                                                 gifComp.df
                                                 [gifComp.df$displayName==input$inDiffThrowComp, 4],
                                                 qbList.df[qbList.df$displayName == input$inQbList, 1])})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

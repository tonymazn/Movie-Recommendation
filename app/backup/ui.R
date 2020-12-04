## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')


genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")


ui <- fluidPage(
  titlePanel("Project 4: Movie Recommendation by ZM11"),
  tabsetPanel(
    tabPanel(title = "System 1",
             wellPanel(
               dashboardPage(
                 skin = "blue",
                 dashboardHeader(title = "System 1"),
                 
                 dashboardSidebar(disable = TRUE),
                 
                 dashboardBody(includeCSS("css/books.css"),
                               fluidRow(
                                 box(width = 12, title = "Step 1: Rate as many books as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                     div(class = "rateitems",
                                         selectInput("input_genre", "Genre #1",genre_list),
                                         selectInput("input_genre2", "Genre #2",genre_list),
                                         selectInput("input_genre3", "Genre #3",genre_list)
                                     )
                                 )
                               ),
                               fluidRow(
                                 useShinyjs(),
                                 box(
                                   width = 12, status = "info", solidHeader = TRUE,
                                   title = "Step 2: Discover books you might like",
                                   br(),
                                   withBusyIndicatorUI(
                                     actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                   ),
                                   br(),
                                   tableOutput("results")
                                 )
                               )
                 )
               )
               
               
               
               
             )     
    ),
    tabPanel(title = "System 2",
             wellPanel(
               
               
               dashboardPage(
                 skin = "blue",
                 dashboardHeader(title = "System 2"),
                 
                 dashboardSidebar(disable = TRUE),
                 
                 dashboardBody(includeCSS("css/books.css"),
                               fluidRow(
                                 box(width = 12, title = "Step 1: Rate as many books as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                     div(class = "rateitems",
                                         uiOutput('ratings')
                                     )
                                 )
                               ),
                               fluidRow(
                                 useShinyjs(),
                                 box(
                                   width = 12, status = "info", solidHeader = TRUE,
                                   title = "Step 2: Discover books you might like",
                                   br(),
                                   withBusyIndicatorUI(
                                     actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                   ),
                                   br(),
                                   tableOutput("results")
                                 )
                               )
                 )
               )
               
               
               
               
               
             )
             
    )
  )
)
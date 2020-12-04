library(shiny)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")


ui <- fluidPage(
  titlePanel("Project 4: Movie Recommendation by ZM11"),
  tabsetPanel(
    tabPanel(title = "System 1",
             row(4, h3("Select Movie Genres You Prefer (order matters):"),
                 wellPanel(
                   selectInput("input_genre", "Genre #1",genre_list),
                   selectInput("input_genre2", "Genre #2",genre_list),
                   selectInput("input_genre3", "Genre #3",genre_list)
                 )     
             ),
             row(4, h3("Select Movie Genres You Prefer (order matters):"),
                 wellPanel(
                   selectInput("input_genre", "Genre #1",genre_list),
                   selectInput("input_genre2", "Genre #2",genre_list),
                   selectInput("input_genre3", "Genre #3",genre_list)
                 )     
             )
    ),
    tabPanel(title = "System 2",
             wellPanel(
               selectInput("input_genre", "Genre #1",genre_list),
               selectInput("input_genre2", "Genre #2",genre_list),
               selectInput("input_genre3", "Genre #3",genre_list)
             )
             
    )
  )
)

server <- function(input, output){
  
}


shinyApp(ui = ui, server = server)
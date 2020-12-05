## ui.R
# 
# Reference: https://github.com/pspachtholz/BookRecommender
# 
# =============================================
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)



#modelpath = "model/latent_factor_cofi_rec_SVD_model.rds"
#modelpath = "model/UBCF_N_C_model.rds"
modelpath = "model/svd_model.rds"
databasepath ="data/"
moviesListFileName = "aggr.dat"
numberofmovierecommend = 24
num_rows <- 4
num_movies <- 6

set.seed(4486)
source('functions/helpers.R')
moviesList <- read(paste0(databasepath,moviesListFileName), "::")
movies = read(paste0(databasepath,"movies.dat"), "::")
ratingsdata = read(paste0(databasepath,"ratings.dat"), "::")
users = read(paste0(databasepath, "users.dat"), "::")
colnames(moviesList) = c( 'MovieID', 'AveRating', 'title', 'genres')
colnames(movies) = c('MovieID', 'title', 'genres')
colnames(ratingsdata) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

moviesList$MovieID <- as.numeric(moviesList$MovieID)




#ratings <- as(ratingsdata, 'realRatingMatrix')  
genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")


#model = Recommender(
#            data=ratings,
#            method='SVD',            # Item-Based Collaborative Filtering
#            parameter=list(
#                  categories=30,         # number of latent factors
#                  normalize='center',    # normalizing by subtracting average rating per user;
#                                         # note that we don't scale by standard deviations here;
#                                         # we are assuming people rate on the same scale but have
#                                         # different biases
#                  method='Pearson'       # use Pearson correlation
#            ))
 
#Recommender(ratings, "UBCF", param=list(normalize = NULL, method="Cosine"))

model = readRDS(modelpath)
#readRDS(modelpath)

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}



ui <- dashboardPage(

  dashboardHeader(title="Movie Recommend"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/movies.css")
    ),
    
    sidebarMenu(
      menuItem("System I (Genres)", tabName="first", icon=icon("calendar")),
      menuItem("System II (CF)", tabName = "second", icon=icon("th"))
    )
  ),
  
  dashboardBody(
        useShinyjs(),
        div(
        id = "loading_page",
           h1("Loading...")
        ),
        hidden(
           div(id = "main_content")
        ),

                tabItems(
                       tabItem(tabName = "first",
                               fluidRow(
                                 box(width = 12,title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                     div(class = "genres",
                                         selectInput("input_genre1", "Genre #1",genre_list),
                                         selectInput("input_genre2", "Genre #2",genre_list),
                                         selectInput("input_genre3", "Genre #3",genre_list)
                                     )
                                 )
                               ),
                               fluidRow(
                                 useShinyjs(),
                                 box(
                                   width = 12, status = "info", solidHeader = TRUE,
                                   title = "Step 2: Discover movies you might like",
                                   br(),
                                   withBusyIndicatorUI(
                                     actionButton("btn_genre", "Click here to get your recommendations", class = "btn-warning")
                                   ),
                                   br(),
                                   tableOutput("results_genre")
                                 )
                               )
                       ),
                       tabItem(tabName = "second",
                              fluidRow(
                                box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                   div(class = "rateitems",
                                       uiOutput('ratings')
                                   )
                                )
                              ),
                              fluidRow(
                                useShinyjs(),
                                box(
                                  width = 12, status = "info", solidHeader = TRUE,
                                  title = "Step 2: Discover movies you might like",
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






server <- function(input, output){
    load_data()
  
    # show the movies to be rated
    output$ratings <- renderUI({
      # Randamly picked movie to display
      moviesDisplay <- moviesList[sample(nrow(moviesList), 200),]

      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          list(box(width = 2,
                   div(style = "text-align:center", img(src = paste0( "movieImages/", moviesDisplay$MovieID[(i - 1) * num_movies + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
                   div(style = "text-align:center", paste0( moviesDisplay$title[(i - 1) * num_movies + j]) ),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", moviesDisplay$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5))))
        })))
      })
    })
    
    # Calculate recommendations for System I when the submit button is clicked 
    df_system1 <- eventReactive(input$btn_genre, {
      withBusyIndicatorServer("btn_genre", { # showing the busy indicator
        # hide the rating container
        
        #useShinyjs()
        #jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        #runjs(jsCode)
        
        #systemresult = subset(moviesList,AveRating>=4 & (input$input_genre1 %in% genres | input$input_genre2 %in% genres | input$input_genre3 %in% genres) )
        systemresult = subset(moviesList,AveRating>=4 & (grepl(input$input_genre1, genres, fixed = TRUE) | grepl(input$input_genre2, genres, fixed = TRUE) | grepl(input$input_genre3, genres, fixed = TRUE)) )
        systemresult = systemresult[sample(nrow(systemresult), 60),]

        write.table(systemresult, file=  paste0("log/app1", toString(as.integer(Sys.time()))  ,".log"),col.names=FALSE,row.names=FALSE,sep=",",quote=FALSE)
        
        systemresult
        
       }) # still busy
      
    }) # clicked on button
    
    # display the recommendations
    output$results_genre <- renderUI({

      recom_result1 <- df_system1()
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
              div(style = "text-align:center", img(src = paste0( "movieImages/", recom_result1$MovieID[(i - 1) * num_movies + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
              div(style = "text-align:center; color: #999999; font-size: 80%", 
                  paste0( recom_result1$title[(i - 1) * num_movies + j])
              ),
              div(style="text-align:center; font-size: 100%", 
                  strong(paste0( recom_result1$title[(i - 1) * num_movies + j]))
              )
              
          )        
        }))) # columns
      }) # rows
      
    }) # renderUI function
    
    
    
    
    # Calculate recommendations II when the submit button is clicked
    df_system2 <- eventReactive(input$btn, {
      withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list, ratingsdata) 
        pred <- predict(model, newdata = user_ratings, n = numberofmovierecommend)

        recom_resultID = as(pred, 'list')[[1]]
        recom_results <- subset(movies, movies$MovieID %in% recom_resultID)
        
        write.table(recom_resultID, file=  paste0("log/recom_resultID", toString(as.integer(Sys.time()))  ,".log"),col.names=FALSE,row.names=FALSE,sep=",",quote=FALSE)
        write.table(recom_results, file=  paste0("log/app", toString(as.integer(Sys.time()))  ,".log"),col.names=FALSE,row.names=FALSE,sep=",",quote=FALSE)
        
        recom_results

      }) # still busy
      
    }) # clicked on button
    
    
    # display the recommendations
    output$results <- renderUI({
      recom_result <- df_system1()
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
              div(style = "text-align:center", img(src = paste0( "movieImages/", recom_result$MovieID[(i - 1) * num_movies + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
              div(style = "text-align:center; color: #999999; font-size: 80%", 
                  paste0( recom_result$title[(i - 1) * num_movies + j])
              ),
              div(style="text-align:center; font-size: 100%", 
                  strong(paste0( recom_result$title[(i - 1) * num_movies + j]))
              )
              
          )        
        }))) # columns
      }) # rows
      
    }) # renderUI function

}


shinyApp(ui = ui, server = server)
## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

#modelpath = "model/UBCF_N_C_model.rds"
modelpath = "model/latent_factor_cofi_rec_SVD_model.rds"
databasepath ="data/"
moviesListFileName = "aggr.dat"
numberofmovierecommend = 24

source('functions/helpers.R')

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings <- function(value_list) {
  dat <- data.table(UserID=99999,
                    MovieID = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)),
                    Timestamp = as.numeric(Sys.time())
                    )
  dat <- dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat <- dat[Rating > 0]
  numberofnewratings = nrow(dat)
  #saveRDS(dat, "app.log")
  
  ratings2 = rbind(dat, ratingsdata)
  newratingsdata <- as(ratings2, 'realRatingMatrix')
  
  newratingsdata[1:numberofnewratings,]
}

# read in data


moviesList <- read(paste0(databasepath,moviesListFileName), "::")
moviesList <- moviesList[sample(nrow(moviesList), 200),]


movies = read(paste0(databasepath,"movies.dat"), "::")
ratingsdata = read(paste0(databasepath,"ratings.dat"), "::")
users = read(paste0(databasepath, "users.dat"), "::")

colnames(moviesList) = c( 'MovieID', 'AveRating', 'title', 'genres')

colnames(movies) = c('MovieID', 'title', 'genres')
colnames(ratingsdata) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

moviesList$MovieID <- as.numeric(moviesList$MovieID)

ratings <- as(ratingsdata, 'realRatingMatrix')  


genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")


model = Recommender(ratings, "UBCF", param=list(normalize = NULL, method="Cosine"))
  #readRDS(modelpath)



ui <- dashboardPage(
  

  dashboardHeader(title="Project 4: Movie Recommendation by ZM11"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/movies.css")
    ),
    
    sidebarMenu(
      menuItem("First", tabName="first", icon=icon("calendar")),
      menuItem("Second", tabName = "second", icon=icon("th"))
    )
  ),
  
  dashboardBody(
                tabItems(
                       tabItem(tabName = "first",
                               fluidRow(
                                 box(width = 12,title = "Step 1: Rate as many books as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                     div(class = "genres",
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






server <- function(input, output){

#    observeEvent(input$ratings, {
      
#    })

    # show the books to be rated
    output$ratings <- renderUI({
      
      num_rows <- 20
      num_movies <- 6 # books per row
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          list(box(width = 2,
                   div(style = "text-align:center", img(src = paste0( "movieImages/", moviesList$MovieID[(i - 1) * num_movies + j], ".jpg"), height="60%", width="60%")),
                   div(style = "text-align:center", paste0( moviesList$title[(i - 1) * num_movies + j]) ),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", moviesList$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5))))
        })))
      })
    })
    
    # Calculate recommendations when the sbumbutton is clicked
    df <- eventReactive(input$btn, {
      withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list) 
        pred <- predict(model, newdata = user_ratings, n = numberofmovierecommend)
        recom_resultID = as(pred, 'list')[[1]]
        recom_results <- subset(movies, movies$MovieID %in% recom_resultID)
        
        write.table(recom_results,file="app.log",col.names=FALSE,row.names=FALSE,sep=",",quote=FALSE)
        recom_results

      }) # still busy
      
    }) # clicked on button
    
    
    # display the recommendations
    output$results <- renderUI({
      num_rows <- 4
      num_movies <- 6
      recom_result <- df()
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
              div(style = "text-align:center", img(src = paste0( "movieImages/", recom_result$MovieID[(i - 1) * num_movies + j], ".jpg"), height="60%", width="60%")),
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
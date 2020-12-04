## ui.R
source('functions/helpers.R')

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings <- function(value_list) {
  dat <- data.table(book_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(book_id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (book_id = as.numeric(book_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]
  
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$book_id, 
                               j = rep(1,nrow(dat)), 
                               x = dat$rating, 
                               dims = c(nrow(ratingmat), 1))
}

# read in data
#books <- fread('data/books.csv')
#ratings <- fread('data/ratings_cleaned.csv')

basedir ="data/"
movies <- read(paste0(basedir,"aggr200.dat"), "::")
colnames(movies) = c( 'MovieID', 'AveRating', 'title', 'genres')
movies$MovieID <- as.numeric(movies$MovieID)




genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")


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
                                     actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                                   ),
                                   br(),
                                   tableOutput("results1")
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
                   div(style = "text-align:center", img(src = paste0( "movieImages/", movies$MovieID[(i - 1) * num_movies + j], ".jpg"), height="60%", width="60%")),
                   div(style = "text-align:center", paste0( movies$title[(i - 1) * num_movies + j]) ),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5))))
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
        
        # add user's ratings as first column to rating matrix
        rmat <- cbind(user_ratings, ratingmat)
        
        # get the indices of which cells in the matrix should be predicted
        # predict all books the current user has not yet rated
        items_to_predict <- which(rmat[, 1] == 0)
        prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
        
        # run the ubcf-alogrithm
        res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
        
        # sort, organize, and return the results
        user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
        user_predicted_ids <- as.numeric(names(user_results))
        recom_results <- data.table(Rank = 1:20, 
                                    Book_id = user_predicted_ids, 
                                    Author = books$authors[user_predicted_ids], 
                                    Title = books$title[user_predicted_ids], 
                                    Predicted_rating =  user_results)
        
      }) # still busy
      
    }) # clicked on button
    
    
    # display the recommendations
    output$results <- renderUI({
      num_rows <- 4
      num_movies <- 5
      recom_result <- df()
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
              div(style = "text-align:center", 
                  a(href = paste0('https://www.goodreads.com/book/show/', books$best_book_id[recom_result$Book_id[(i - 1) * num_movies + j]]), 
                    target='blank', 
                    img(src = books$image_url[recom_result$Book_id[(i - 1) * num_movies + j]], height = 150))
              ),
              div(style = "text-align:center; color: #999999; font-size: 80%", 
                  books$authors[recom_result$Book_id[(i - 1) * num_movies + j]]
              ),
              div(style="text-align:center; font-size: 100%", 
                  strong(books$title[recom_result$Book_id[(i - 1) * num_movies + j]])
              )
              
          )        
        }))) # columns
      }) # rows
      
    }) # renderUI function

}


shinyApp(ui = ui, server = server)
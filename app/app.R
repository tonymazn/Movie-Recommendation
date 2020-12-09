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

set.seed(4486)
source('functions/helpers.R')
source('functions/setting.R')


isdebug = FALSE

version = "1.0.0.0"
settingFile = "setting.txt"
setting = readSetting(settingFile)
systemI_AlgorithmKey = "SystemI_Algorithm"
systemII_AlgorithmKey = "SystemII_Algorithm"
modelpath = paste0("model/", getSetting(setting, systemII_AlgorithmKey)  ,"_model.rds")
defaultmodelpath = "model/No_Para_SVD_model.rds"
databasepath ="data/"
moviesListFileName = "aggr.dat"
numberofmovierecommend = 4 * 6
trendyYear = 2000




moviesList <- read(paste0(databasepath,moviesListFileName), "::")
movies = read(paste0(databasepath,"movies.dat"), "::")
ratingsdata = read(paste0(databasepath,"ratings.dat"), "::")
users = read(paste0(databasepath,"users.dat"), "::")
#movies_clean <- read.csv(paste0(databasepath,"movies_clean.dat"), stringsAsFactors=FALSE,fileEncoding = "UTF-8")
movies_clean <- read(paste0(databasepath,"movies_clean.dat"), "::")
  #read.csv(paste0(databasepath,"movies_clean.dat"), stringsAsFactors=FALSE)
colnames(moviesList) = c( 'MovieID', 'AveRating', 'title', 'genres')
colnames(movies) = c('MovieID', 'title', 'genres')
colnames(ratingsdata) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')
colnames(movies_clean) = c("MovieID", "title", "year", 'genres')

moviesList$MovieID <- as.numeric(moviesList$MovieID)

genre_list = c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

systemI_algorithm_list = c("Method1","Method2") 
                          
systemII_algorithm_list = c("UBCF_N_C","UBCF_C_C","UBCF_Z_C", "UBCF_N_E", 
                            "UBCF_C_E", "UBCF_Z_E", "UBCF_N_P", "UBCF_C_P", 
                            "UBCF_Z_P", "IBCF_N_C", "IBCF_C_C", "IBCF_Z_C",
                            "IBCF_N_E", "IBCF_C_E", "IBCF_Z_E", "IBCF_N_P",
                            "IBCF_C_P", "IBCF_Z_P", "latent_factor_cofi_rec_SVD",
                            "No_Para_SVD")


systemI_algorithm_Description_list = c("Method1: randomly pick from rating >= 4(optional) and  genre1 or genre2 or genre3",
                                       "Method2: randomly pick most trendy movies in that genres genre1 or genre2 or genre3")

systemII_algorithm_Description_list = c("UBCF_N_C: User-Based CF, normalize = NULL, method='Cosine'",
                                        "UBCF_C_C: User-Based CF, normalize = 'center',method='Cosine'",
                                        "UBCF_Z_C: User-Based CF, normalize = Z-score',method='Cosine'",
                                        "UBCF_N_E: User-Based CF, normalize = NULL, method=Euclidean",
                                        "UBCF_C_E: User-Based CF, normalize = 'center',method='Euclidean'",
                                        "UBCF_Z_E: User-Based CF, normalize = 'Z-score',method='Euclidean'",
                                        "UBCF_N_P: User-Based CF, normalize = NULL, method='pearson'",
                                        "UBCF_C_P: User-Based CF, normalize = 'center',method='pearson'",
                                        "UBCF_Z_P: User-Based CF, normalize = 'Z-score',method='pearson'",
                                        "IBCF_N_C: Item-Based CF, normalize = NULL, method='Cosine'",
                                        "IBCF_C_C: Item-Based CF, normalize = 'center',method='Cosine'",
                                        "IBCF_Z_C: Item-Based CF, normalize = Z-score',method='Cosine'",
                                        "IBCF_N_E: Item-Based CF, normalize = NULL, method=Euclidean",
                                        "IBCF_C_E: Item-Based CF, normalize = 'center',method='Euclidean'",
                                        "IBCF_Z_E: Item-Based CF, normalize = 'Z-score',method='Euclidean'",
                                        "IBCF_N_P: Item-Based CF, normalize = NULL, method='pearson'",
                                        "IBCF_C_P: Item-Based CF, normalize = 'center',method='pearson'",
                                        "IBCF_Z_P: Item-Based CF, normalize = 'Z-score',method='pearson'",
                                        "latent_factor_cofi_rec_SVD: SVD, normalize='center', method='Pearson'",
                                        "No_Para_SVD: SVD, normalize=NULL, normalize= default, method=default")



loadModel = function(){
   model =  readModel(modelpath)
   if (is.na(model)){
      model = readModel(defaultmodelpath)
   }
   return(model)
}
model = loadModel()


getSystemAlgorithm = function(description){
  result = strsplit(description, ":")[[1]]
  return(result[1])
}

getSystemAlgorithmDesc = function(code){
  index = which(grepl(code, systemII_algorithm_Description_list, fixed = TRUE))[1]
  if(!is.na(index)){
      return(systemII_algorithm_Description_list[index])
  }
  return(systemI_algorithm_Description_list[which(grepl(code, systemI_algorithm_Description_list, fixed = TRUE))])
}



ui <- dashboardPage(

  dashboardHeader(title="Top Movies"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/movies.css")
    ),
    uiOutput("userpanel"),
    sidebarMenu(id = "tabs",
      menuItem("System I (Genres)", tabName="first", icon=icon("calendar")),
      menuItem("System II (CF)", tabName = "second", icon=icon("th")),
      menuItem("Setting", tabName = "third", icon=icon("cog", lib = "glyphicon"))
    )
  ),
  
  dashboardBody(
                tabItems(
                       tabItem(tabName = "first",
                               fluidRow(
                                 box(width = 12,title = "Step 1: Select movie genres", status = "info", solidHeader = TRUE, collapsible = TRUE,
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
                      ),
                      tabItem(tabName = "third",
                              fluidRow(
                                box(width = 12,title = "Setting", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                    div(class = "systemsetting",
                                        verbatimTextOutput("message"),
                                        selectInput("input_SystemI_Algorithm", "System I Algorithm",systemI_algorithm_Description_list, selected = getSystemAlgorithmDesc(getSetting(setting, systemI_AlgorithmKey))),
                                        selectInput("input_systemII_Algorithm", "System II Algorithm",systemII_algorithm_Description_list, selected = getSystemAlgorithmDesc(getSetting(setting, systemII_AlgorithmKey)))
                                    ),
                                    br(),
                                    withBusyIndicatorUI(
                                      actionButton("btn_setting", "Click here to Save", class = "btn-warning")
                                    ),
                                    br()
                                    
                                )
                              ),
                      )
                )
   )
)






server <- function(input, output){
 #   load_data()
  
    output$userpanel <- renderUI({
        # session$user is non-NULL only in authenticated sessions
        sidebarUserPanel(
           span("NetID: ZM11"),
           subtitle = a(icon("user", lib = "glyphicon"), paste0("github  v", version), href="https://github.com/tonymazn/stat542", target = "_blank"))
    })
  
    # show the movies to be rated
    output$ratings <- renderUI({
      num_movies_disaply = 6
      num_rows_display = 10
      
      logfile = paste0("app", toString(as.integer(Sys.time()))  ,".log")

      # Randamly picked movie to display
      moviesDisplay <- moviesList[sample(nrow(moviesList), 200),]

      lapply(1:num_rows_display, function(i) {
        list(fluidRow(lapply(1:num_movies_disaply, function(j) {
          list(box(width = 2,
                   div(style = "text-align:center", img(src = paste0( "movieImages/", moviesDisplay$MovieID[(i - 1) * num_movies_disaply + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
                   div(style = "text-align:center", paste0( moviesDisplay$title[(i - 1) * num_movies_disaply + j]) ),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", moviesDisplay$MovieID[(i - 1) * num_movies_disaply + j]), label = "", dataStop = 5))))
        })))
      })
    })
    
    # Calculate recommendations for System I when the submit button is clicked 
    df_system1 <- eventReactive(input$btn_genre, {
      withBusyIndicatorServer("btn_genre", { # showing the busy indicator
        num_rows = 4
        num_movies = 6

        systemI_Algorithm = getSystemAlgorithm(input$input_SystemI_Algorithm)

        if (systemI_Algorithm == systemI_algorithm_list[1]){
          # Method 1:
          systemresult = subset(moviesList,AveRating>=4 & (grepl(input$input_genre1, genres, fixed = TRUE) | grepl(input$input_genre2, genres, fixed = TRUE) | grepl(input$input_genre3, genres, fixed = TRUE)) )
          if (nrow(systemresult) < num_movies * num_rows){
             systemresult = subset(moviesList, grepl(input$input_genre1, genres, fixed = TRUE) | grepl(input$input_genre2, genres, fixed = TRUE) | grepl(input$input_genre3, genres, fixed = TRUE))
          }

        } else {
          # Method 2:
          systemresult = subset(movies_clean, year >= trendyYear & (grepl(input$input_genre1, genres, fixed = TRUE) | grepl(input$input_genre2, genres, fixed = TRUE) | grepl(input$input_genre3, genres, fixed = TRUE)) )
          if (nrow(systemresult) < numberofmovierecommend){
            systemresult = subset(movies_clean, year >= trendyYear - 1 & (grepl(input$input_genre1, genres, fixed = TRUE) | grepl(input$input_genre2, genres, fixed = TRUE) | grepl(input$input_genre3, genres, fixed = TRUE)) )
          }
        }
        systemresult = systemresult[sample(nrow(systemresult), ifelse(nrow(systemresult)>=numberofmovierecommend,numberofmovierecommend,nrow(systemresult))),]
        outputToFile(systemresult, paste0("app1", toString(as.integer(Sys.time()))  ,".log"), isdebug)
        systemresult
       })
    })
    
    
    
    # display the recommendations System I
    output$results_genre <- renderUI({
      showNotification(paste0("System Message: System I Algorithm - ", getSystemAlgorithmDesc(getSetting(setting, systemI_AlgorithmKey))), duration = 5, type = "message" )
      
      recom_result1 <- df_system1()
      if (nrow(recom_result1) > 0){
          systemI_num_movies = 6
          systemI_num_movies = ifelse(nrow(recom_result1) >= systemI_num_movies, systemI_num_movies, nrow(recom_result1)) 
          systemI_num_rows = nrow(recom_result1) %/% systemI_num_movies

          lapply(1:systemI_num_rows, function(i) {
            list(fluidRow(lapply(1:systemI_num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * systemI_num_movies + j),
                div(style = "text-align:center", img(src = paste0( "movieImages/", recom_result1$MovieID[(i - 1) * systemI_num_movies + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
                div(style = "text-align:center; color: #999999; font-size: 80%", 
                    paste0(recom_result1$title[(i - 1) * systemI_num_movies + j])
                ),
                div(style="text-align:center; font-size: 100%", 
                    strong(paste0(recom_result1$title[(i - 1) * systemI_num_movies + j]))
                )
            )        
          }))) # columns
         }) # rows
      }else{
        showNotification("System Message: there is no movie found", duration = 4, type = "error")
      }

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
        
        outputToFile(recom_resultID, paste0("recom_resultID", toString(as.integer(Sys.time()))  ,".log"), isdebug)
        outputToFile(recom_results, paste0("app", toString(as.integer(Sys.time()))  ,".log"), isdebug)

        recom_results

      })
      
    }) # clicked on button
    
    
    # display the recommendations System II
    output$results <- renderUI({
      showNotification(paste0("System Message: System II Algorithm - ",  getSystemAlgorithmDesc(getSetting(setting, systemII_AlgorithmKey)) ), duration = 5, type = "message" )
      
      num_rows = 4
      num_movies = 6

      recom_result <- df_system2()
      if (nrow(recom_result) > 0){
         systemII_num_movies = ifelse(nrow(recom_result) >= num_movies, num_movies, nrow(recom_result)) 
         systemII_num_rows = nrow(recom_result) %/% systemII_num_movies
         
         lapply(1:systemII_num_rows, function(i) {
            list(fluidRow(lapply(1:systemII_num_movies, function(j) {
             box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
                div(style = "text-align:center", img(src = paste0( "movieImages/", recom_result$MovieID[(i - 1) * num_movies + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
                div(style = "text-align:center; color: #999999; font-size: 80%", 
                    paste0( recom_result$title[(i - 1) * num_movies + j])
                ),
                div(style="text-align:center; font-size: 100%", strong(paste0( recom_result$title[(i - 1) * num_movies + j])))
              
          )        
          })))
        })
      }else{
        showNotification("System Message: there is no movie found", duration = 4, type = "error")
      }
      
    })


    observeEvent(input$btn_setting, {
      # Save the ID for removal later

      systemI_Algorithm = getSystemAlgorithm(input$input_SystemI_Algorithm)
      systemII_Algorithm = getSystemAlgorithm(input$input_systemII_Algorithm)
      setting <<- setSetting(setting, systemI_AlgorithmKey, systemI_Algorithm)
      setting <<- setSetting(setting, systemII_AlgorithmKey, systemII_Algorithm)
      write(setting, settingFile, ":")
      modelpath <<- paste0("model/", getSetting(setting, systemII_AlgorithmKey)  ,"_model.rds")
      model <<- loadModel()
      
      showNotification("System Message: Setting Saved", duration = 5, type = "message" )
      #showNotification(paste0("System Message: Algorithm - ",  getSetting(setting, systemII_AlgorithmKey) )  , duration = 5, type = "message" )
    })
    


}


shinyApp(ui = ui, server = server)
#runApp('app', host = "0.0.0.0", port = 80)
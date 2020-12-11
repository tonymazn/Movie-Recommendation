STAT542 Project 4 - Movie Recommendation
-
In this project, we develop content based recommendation system(System I) and collaborative recommendation system(System II) (1).

* System I: recommendation based on genres. We know user’s favorite genre, we provide two recommendation schemes:
  - Method I: highly-rate movies - select the 4-point or more review
  - Method II: most trendy movies - the newest movie(here is year 2000)

* System II: collaborative recommendation system. we provided three collaborative recommendation algorithms:
  - User-based (UBCF): we assume that similar users will have similar taste. UBCF uses the logic and recommends items by finding similar users to the user;
  - Item-based (IBCF): The similarities between different items in the dataset are calculated by using one of a number of similarity measures, and then these similarity values are used to predict;
Singular value decomposition (SVD): It uses a matrix structure where each row represents a user, and each column represents an item. The elements of this matrix are the ratings that are given to items by users.


The DEMO sites
-

[Primary site](http://project4.westus.cloudapp.azure.com)


    or


[backup site](http://cs598.eastus.cloudapp.azure.com)
    
    
    
System Structure
- 


    .
    ├─ app                                          # A Movie Recommendation App
    │   ├── data                                    # data folder
    │   │     ├── README                            # Movielens data readme
    │   │     ├── aggr.dat                          # each movie with its rates meaning
    │   │     ├── aggr200.dat                       # randamly pick 200 movies from aggr.dat,not in use
    │   │     ├── movies.dat                        # Movielens 1M dataset movies
    │   │     ├── movies_clean.dat                  # remove the movies without rating record
    │   │     ├── ratings.dat                       # Movielens 1M dataset ratings
    │   │     └── users.dat                         # Movielens 1M dataset users.dat
    │   │
    │   ├── functions                               # utility folder
    │   │     ├── helpers.R                         # UI helpers
    │   │     └── setting.R                         # setting untility
    │   │
    │   ├── model                                   # pretrain models
    │   │     ├── IBCF_C_C_model.rds                # pretrain model IBCF normalize = "center",method="Cosine"
    │   │     ├── IBCF_C_E_model.rds                # pretrain model IBCF normalize = "center",method="Euclidean"
    │   │     ├── IBCF_C_P_model.rds                # pretrain model IBCF normalize = "center",method="pearson"
    │   │     ├── IBCF_N_C_model.rds                # pretrain model IBCF normalize = NULL, method="Cosine"
    │   │     ├── IBCF_N_E_model.rds                # pretrain model IBCF normalize = NULL, method="Euclidean"
    │   │     ├── IBCF_N_P_model.rds                # pretrain model IBCF normalize = NULL, method="pearson"
    │   │     ├── IBCF_Z_C_model.rds                # pretrain model IBCF normalize = "Z-score",method="Cosine"
    │   │     ├── IBCF_Z_E_model.rds                # pretrain model IBCF normalize = "Z-score",method="Euclidean"
    │   │     ├── IBCF_Z_P_model.rds                # pretrain model IBCF normalize = "Z-score",method="pearson"
    │   │     ├── UBCF_C_C_model.rds                # pretrain model UBCF normalize = "center",method="Cosine"
    │   │     ├── UBCF_C_E_model.rds                # pretrain model UBCF normalize = "center",method="Euclidean"
    │   │     ├── UBCF_C_P_model.rds                # pretrain model UBCF normalize = "center",method="pearson"
    │   │     ├── UBCF_N_C_model.rds                # pretrain model UBCF normalize = NULL, method="Cosine"
    │   │     ├── UBCF_N_E_model.rds                # pretrain model UBCF normalize = NULL, method="Euclidean"
    │   │     ├── UBCF_N_P_model.rds                # pretrain model UBCF normalize = NULL, method="pearson"
    │   │     ├── UBCF_Z_C_model.rds                # pretrain model UBCF normalize = "Z-score",method="Cosine"
    │   │     ├── UBCF_Z_E_model.rds                # pretrain model UBCF normalize = "Z-score",method="Euclidean"
    │   │     ├── UBCF_Z_P_model.rds                # pretrain model UBCF normalize = "Z-score",method="pearson"
    │   │     ├── latent_factor_cofi_rec_SVD...rds  # SVD categories=30,normalize='center',method='Pearson'
    │   │     └── svd_model.rds                     # SVD default parameter
    │   │
    │   ├── www                                     # 
    │   │     ├── css                               # css folder
    │   │     ├── movieImages                       # movie images folder
    │   │     └── ajax-loader-bar.gif               # ajax called to display
    │   │
    │   ├── app.R                                   # Shiny app
    │   ├── run.bat                                 # windows run template
    │   └── setting.txt                             # site setting
    │
    ├── archive                                     # test/temporary/function code   
    │
    ├── data                                      
    │     ├── ml-1m                                 # Movielens 1M data
    │     ├── ml-latest-small                       # Movielens 1M small data
    │     ├── test                                  # test data
    │     ├── train                                 # train data
    │     ├── README                                # Movielens data readme
    │     ├── movies.dat                            # Movielens 1M dataset movies
    │     ├── movies_clean.dat                   
    │     ├── newratings.dat                     
    │     ├── newratings1.dat                    
    │     ├── newratings2.dat
    │     ├── ratings.dat                           # Movielens 1M dataset ratings
    │     └── users.dat                             # Movielens 1M dataset users
    │
    ├── model 
    │     ├── IBCF_C_C_model.rds                    # pretrain model IBCF normalize = "center",method="Cosine"
    │     ├── IBCF_C_E_model.rds                    # pretrain model IBCF normalize = "center",method="Euclidean"
    │     ├── IBCF_C_P_model.rds                    # pretrain model IBCF normalize = "center",method="pearson"
    │     ├── IBCF_N_C_model.rds                    # pretrain model IBCF normalize = NULL, method="Cosine"
    │     ├── IBCF_N_E_model.rds                    # pretrain model IBCF normalize = NULL, method="Euclidean"
    │     ├── IBCF_N_P_model.rds                    # pretrain model IBCF normalize = NULL, method="pearson"
    │     ├── IBCF_Z_C_model.rds                    # pretrain model IBCF normalize = "Z-score",method="Cosine"
    │     ├── IBCF_Z_E_model.rds                    # pretrain model IBCF normalize = "Z-score",method="Euclidean"
    │     ├── IBCF_Z_P_model.rds                    # pretrain model IBCF normalize = "Z-score",method="pearson"
    │     ├── UBCF_C_C_model.rds                    # pretrain model UBCF normalize = "center",method="Cosine"
    │     ├── UBCF_C_E_model.rds                    # pretrain model UBCF normalize = "center",method="Euclidean"
    │     ├── UBCF_C_P_model.rds                    # pretrain model UBCF normalize = "center",method="pearson"
    │     ├── UBCF_N_C_model.rds                    # pretrain model UBCF normalize = NULL, method="Cosine"
    │     ├── UBCF_N_E_model.rds                    # pretrain model UBCF normalize = NULL, method="Euclidean"
    │     ├── UBCF_N_P_model.rds                    # pretrain model UBCF normalize = NULL, method="pearson"
    │     ├── UBCF_Z_C_model.rds                    # pretrain model UBCF normalize = "Z-score",method="Cosine"
    │     ├── UBCF_Z_E_model.rds                    # pretrain model UBCF normalize = "Z-score",method="Euclidean"
    │     ├── UBCF_Z_P_model.rds                    # pretrain model UBCF normalize = "Z-score",method="pearson"
    │     ├── latent_factor_cofi_rec_SVD_model.rds  # SVD categories=30,normalize='center',method='Pearson'
    │     └── svd_model.rds                         # SVD default parameter
    │
    ├─ GenerateData.RMD                             # split the data into train/test dataset 
    ├─ GenerateData.html                            # split the data into train/test dataset run result
    ├─ LICENSE                                      # project license file
    ├─ Project4.Rproj                               # Project file
    ├─ Project_4_4486_zm11_Zhouningma_v..           # Project report markdown file v1
    ├─ Project_4_4486_zm11_Zhouningma_v..           # Project report markdown file v2
    ├─ Project_4_4486_zm11_Zhouningma_v..           # Project report markdown file v3
    ├─ Project_4_4486_zm11_Zhouningma_v..           # Project report hmtl file  
    ├─ README.md                                    # this file
    ├─ System1.RMD                                  # System I test markdown file
    ├─ System1.html                                 # System I test result
    ├─ System2_MutipleCF.RMD                        # System II test markdown file
    ├─ System2_MutipleCF.html                       # System II test result
    ├─ System2_MutipleCF_SVD.RMD                    # System II SVD test markdown file
    ├─ System2_MutipleCF_SVD.html                   # System II SVD test result
    ├─ Test.RMD                                     # other test markdown file
    └─ Test.html                                    # other test result
    
    
Version
-
1.0.0.0

Team Member
-
Zhouning Ma

NetID
-
ZM11

License
-
Apache License

Reference
-
1. Piazza:  https://piazza.com/class/kdf6l5f8bb78j?cid=868
2. Moviewlens: https://grouplens.org/datasets/movielens/
3. Dataset README: https://github.com/tonymazn/stat542/tree/main/data/README
4. Machine Learning Project – Data Science Movie Recommendation System Project in R 
   https://data-flair.training/blogs/data-science-r-movie-recommendation/
5. Movie Recommendation System: https://jeknov.shinyapps.io/movieRec/
6. Item-Based Collaborative Filtering Recommendation: 
   https://www.kaggle.com/hendraherviawan/itembased-collaborative-filter-recommendation-r
7. User-Based and Item-Based Collaborative Filtering https://rpubs.com/jt_rpubs/285729
8. Movie Recommendation System  https://jeknov.shinyapps.io/movieRec/
9. Lecure Nov26 https://www.youtube.com/watch?v=QMWydVg-uLg&feature=youtu.be


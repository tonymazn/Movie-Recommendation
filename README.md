# STAT542 Project 4 - Project 4: Movie Recommendation
-
In this project, we develop content based recommendation system(System I) and collaborative recommendation system(System II) (1).


The DEMO sites
-

    http://cs598.eastus.cloudapp.azure.com/

    or

    http://project4.westus.cloudapp.azure.com/
    
System Structure
- 


    .
    ├─ cs410-master 
    │    └─── web
    │          ├── templates
    │          │     └── index.html
    │          ├── __init__.py
    │          └── webserver.py                  # Flask 1.0.2(2) entrance point
    ├── core
    │     ├── __init__.py
    │     ├── algorithms.py
    │     ├── preprocess.py
    │     ├── summarizer.py                      # Main function to generate summary result by using bs4(3) and SUMY(4)
    │     └── textsummarize.py                   # Pass summary result service for web 
    ├── tests 
    │     ├───── ROUGE_result                    # Create measure scores by pyrouge 0.1.3(5)
    │     │         ├── LexRank.txt              # Lex Rank algorithm test log
    │     │         ├── LAS.txt                  # LAS algorithm test log 
    │     │         ├── Luhn.txt                 # Luhn algorithm test log 
    │     │         ├── TextRank.txt             # Text Rank algorithm test log 
    │     │         └── ROUGE measure scores.pdf # ROUGE measure scores report 
    │     ├── __init__.py
    │     ├── evaluation.py
    │     ├── runsystem.py                       # Generate summary system data
    │     ├── settting.ini 
    │     └── utils.py
    ├─ README.md
	├─ User Manual.pdf                           # User Manual
	├─ requirements.txt
    ├─ setup.cfg
    └─ setup.py
    
    
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


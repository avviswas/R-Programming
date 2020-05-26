
# load libraries
require(plyr)
require(data.table)
require(tidyverse)
require(xgboost)
library(stringi)
library(rvest)
map.func = function(x, y = 2){
  map = x %>% 
    sapply(FUN = function(x){strsplit(x, '[,.:""]')[[1]][y]}) %>% 
    sub("[[:punct:]]", '',.) %>% 
    sub("'",'',.) %>% as.factor() %>% as.numeric()
  return(map)
}

######### data##################

Train = read_csv("train.csv")
test = read_csv("test.csv")

for (i in 1:nrow(Train)) {
  if(Train[i,"budget"] > 1000 & Train[i,"revenue"] < 100){
    Train[i,"revenue"] = Train[i,"revenue"] * 10^6
   }
}

train.id = Train1$id
label = Train1$revenue
test.id = test$id

Train = Train %>% within(rm("revenue"))
test = test %>% within(rm("id"))
Train = rbind(Train1,test)
train_raw = Train
date.format <- as.Date(Train$release_date, format="%m/%d/%Y")

##### MISSINGG VAUES
Train[Train== ""] <- NA
#check for columns with missing values
na.cols <- which(colSums(is.na(Train)) > 0)
na.cols <- sort(colSums(sapply(Train[na.cols], is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'columns with missing values')


####
sel.cols = c("imdb_id","budget","original_language","popularity","spoken_languages",
             "production_countries","genres","runtime","tagline_nword",
             "tag_hasNA","lan_isEN","same_title","crew_len","cast_len",
             "crewlen_nword_div","homepage_hasNA","collection_hasNA",
             "tag_length","keywords_len","year","month","weekday","week",
             "p_comp_len","dayofweek2","cast_nword")


######### Correct Year Data ##################
year.fun = function(x){
  if(is.na(x)){
    return(paste0("2000"))
  }else if(x < 10){
    return(paste0("200",x))
  }else if(x >=10 & x <= 18){
    return(paste0("20", x))
  }else{
    return(paste0("19",x))
  }
}

Train = Train %>% 
  mutate(year = year(date.format),
         year = sapply(year, year.fun) %>% as.numeric(),
         month = month(date.format),
         weekday =as.numeric(as.factor(weekdays(date.format))),
         quarter = year * 4 + (month) %/% 3 - 8051, 
         dayofweek2 = (as.numeric(lubridate::days(date.format)) + 3) %% 7,
         week = week(date.format),
         cast_len = str_length(cast),
         cast_nword = str_count(cast, "\\w+"),
         p_comp_len = str_length(production_companies),
         p_comp_nword = str_count(production_companies, "\\w+"),
         tag_length = str_length(tagline),
         tagline_nword = str_count(tagline, "\\w+"),
         tag_hasNA = ifelse(is.na(tagline),0,1),
         lan_isEN = ifelse(original_language =="en",1,0),
         collection_id = map.func(belongs_to_collection),
         collection_hasNA = ifelse(is.na(belongs_to_collection),0,1),
         homepage_hasNA = ifelse(is.na(homepage),1,0),
         same_title = ifelse(title == original_title,1,0),
         crew_len = str_length(crew),
         crew_nword = str_count(crew, "\\w+"),
         crewlen_nword_div = crew_len / crew_nword,
         keywords_len = str_length(Keywords),
         keywords_nword = str_count(Keywords,"\\w+"),
         production_countries = map.func(production_countries),
         comp1 = map.func(production_companies),
         spoken_languages = map.func(spoken_languages),
         genres = map.func(genres),
         status = as.numeric(as.factor(status)),
         original_title = as.numeric(as.factor(original_title)),
         original_language = as.numeric(as.factor(original_language))) %>% 
  select(sel.cols)
  
###  Borrowed some ideas from this kernel https://www.kaggle.com/wallgren/boxofficeprediction-using-randomforest

train = train_raw %>%
  separate(belongs_to_collection, 'idPart', sep = 'name', remove = TRUE) %>%
  separate(release_date, c('releaseMonth', 'releaseDay', 'releaseYear'), sep = '/', remove = TRUE) %>%
  mutate(collectionID = ifelse(is.na(idPart) == FALSE, gsub("\\D", "", idPart), idPart),
         collectionID = ifelse(is.na(collectionID) == TRUE, 0, collectionID),
         partOfCollection = ifelse(is.na(idPart) == FALSE, 1, 0),
         hasHomePage = ifelse(is.na(homepage) == TRUE, 1, 0),
         genres = ifelse(is.na(genres) == TRUE, 'NoGen', genres),
         genComedy = ifelse(stri_detect_fixed(genres, 'Comedy'),1, 0),
         genDrama = ifelse(stri_detect_fixed(genres, 'Drama'),1, 0),
         genThriller = ifelse(stri_detect_fixed(genres, 'Comedy'),1,0),
         genAction = ifelse(stri_detect_fixed(genres, 'Action'),1,0),
         genAnimation = ifelse(stri_detect_fixed(genres, 'Comedy'),1,0),
         genHorror = ifelse(stri_detect_fixed(genres, 'Horror'),1, 0),
         genDocumentary = ifelse(stri_detect_fixed(genres, 'Documentary'),1,0),
         genAdventure = ifelse(stri_detect_fixed(genres, 'Adventure'),1, 0),
         genCrime = ifelse(stri_detect_fixed(genres, 'Crime'),1, 0),
         genMystery = ifelse(stri_detect_fixed(genres, 'Mystery'),1, 0),
         genFantasy = ifelse(stri_detect_fixed(genres, 'Fantasy'),1, 0),
         genWar = ifelse(stri_detect_fixed(genres, 'War'),1, 0),
         genScienceFiction = ifelse(stri_detect_fixed(genres, 'Science Fiction'),1, 0),
         genRomance = ifelse(stri_detect_fixed(genres, 'Romance'),1, 0),
         genMusic = ifelse(stri_detect_fixed(genres, 'Music'),1, 0),
         genWestern = ifelse(stri_detect_fixed(genres, 'Western'),1, 0),
         genFamily = ifelse(stri_detect_fixed(genres, 'Family'),1, 0),
         genHistory = ifelse(stri_detect_fixed(genres, 'Comedy'),1, 0),
         genForeign = ifelse(stri_detect_fixed(genres, 'Foreign'),1,0),
         genTVMovie = ifelse(stri_detect_fixed(genres, 'TV Movie'),1,0),
         production_companies = ifelse(is.na(production_companies) == TRUE, 'NoProd', production_companies),
         prodUniversal = ifelse(stri_detect_fixed(production_companies, 'Universal Pictures'),1, 0),
         prodParamount = ifelse(stri_detect_fixed(production_companies, 'Paramount Pictures'),1, 0),
         prodTCF = ifelse(stri_detect_fixed(production_companies, 'Twentieth Century Fox Film Corporation'),1, 0),
         prodColumbia = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures'),1, 0),
         prodWarner = ifelse(stri_detect_fixed(production_companies, 'Warner Bros.'),1, 0),
         prodNLC = ifelse(stri_detect_fixed(production_companies, 'New Line Cinema'),1, 0),
         prodDisney = ifelse(stri_detect_fixed(production_companies, 'Walt Disney Pictures'),1,0),
         prodColumbiaPictures = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures Corporation'),1,0),
         prodTriStar = ifelse(stri_detect_fixed(production_companies, 'TriStar Pictures'),1, 0),
         sizeOfCast = str_count(cast, 'cast_id'), # Size of cast
         sizeOfCrew = str_count(crew, 'name'), # Size of crew
         sizeOfCrew = ifelse(is.na(sizeOfCrew), 0, sizeOfCrew),
         numberOfGenres = str_count(genres, 'name'),
         prodFoxSearchlight2 = ifelse(stri_detect_fixed(production_companies, 'Fox Searchlight Pictures'),1, 0),
         collectionID = as.factor(collectionID)) %>%  
  group_by(collectionID) %>%
  mutate(sizeOfCollection = n()) %>%
  ungroup() %>%
  mutate(sizeOfCollection = ifelse(sizeOfCollection > 1000, 0, sizeOfCollection)) %>%
  select(-idPart, -homepage, -poster_path, -original_title, -genres, -overview, 
         -tagline, -production_companies, -spoken_languages, -cast, -crew, -Keywords, 
         -production_countries, -status, -releaseYear, -releaseMonth, -releaseDay,
         -title, -collectionID)

train = train[,9:ncol(train)]
Train = cbind(Train,train)
rm(train)

Train$rating <- c(rep(NA, times = nrow(Train)))
Train$totalVotes <- c(rep(NA, times = nrow(Train))) 
Train$imdb_id <- as.character(Train$imdb_id)

#gathering IMDB votes and rating data with html Wrangling 
Get_IMDB_Data <- function(X){
  for(i in 1:nrow(X)) {
    if(X['imdb_id'][i,]!= '' & !is.na(X['imdb_id'][i,])){
      imdb <- as.character(X['imdb_id'][i,])
      url <- 'https://www.imdb.com/title/'
      UrlOpen <- paste(url, imdb, sep = "")
      webpage <- tryCatch(read_html(UrlOpen), error = function(e){ NULL})
      if(is.null(webpage)){
        X['rating'][i,] <- 0
        X['totalVotes'][i,] <- 0
        print(paste("Error - Webpage: ", print(UrlOpen)))
        next
      }
      else {
        rating_data_html <- html_nodes(webpage,'.imdbRating')
        rating_data <- html_text(rating_data_html)
        rating_data <- gsub(c('\n')," ",rating_data)
        rating_data <- gsub('/'," ",rating_data)
        rating_data <- gsub(',',"",rating_data)
        rating_data1 <- unlist(strsplit(rating_data, "[ ]+"))
        
        MovieRatingAndVotes <- tryCatch(t(rating_data1), error = function(e){ NULL})        
        if(is.null(MovieRatingAndVotes)){
          print(X['imdb_id'][i,])
          X['rating'][i,] <- 0
          X['totalVotes'][i,] <- 0
          next}
        else{
          MovieRatingAndVotes <- as.numeric(MovieRatingAndVotes)
          Rating <- MovieRatingAndVotes[2]
          Votes <-  MovieRatingAndVotes[4]
          
          X['rating'][i,] <- Rating
          X['totalVotes'][i,] <- Votes
          print(i)
        }
      }
    }
    else{
      next
    }
  }
  return(X)
}


Train <- Get_IMDB_Data(Train)



#gathering metascore and correct runtime data
Train$runtime1 <- c(rep(NA, times = nrow(Train)))
Train$metascore <- c(rep(NA, times = nrow(Train))) 
Train$Awards <- c(rep(NA, times = nrow(Train))) 


#gathering metascore, correct runtime and awards 
GetMetaScoreAndRuntime <- function(X){
  title <- ''
  url <- ''
  APIKey1 <- ''
  UrlOpen <- ''
  webpage <- ''
  IMDB_data_html <- ''
  IMDB_data <- ''
  IMDB_data1 <- ''
  
  for(i in 1:nrow(X)){
    if(X['imdb_id'][i,] !="" & !is.na(X['imdb_id'][i,]) ){
      
      imdb <- X['imdb_id'][i,]
      url <- 'http://www.omdbapi.com/?i='
      APIKey1 <- '&apikey=2a6e4fe5'
      UrlOpen <- paste(url, imdb, APIKey1, sep = "")
      webpage <- tryCatch(read_html(UrlOpen), error = function(e){ NULL})
      if(is.null(webpage)){
        print(paste("Error - Webpage: ", print(UrlOpen)))
        next
      }
      else{

        #Awards
        Awards_data_html <- html_nodes(webpage,'body')
        Awards_data <- gsub('Awards":"',"zzzzz",Awards_data_html)
        Awards_data <- gsub('","',"zzzzz",Awards_data)
        Awards_data <- unlist(strsplit(Awards_data, "(z){5}"))
        X['Awards'][i,] <- Awards_data[14]
        
        #Runtime
        Runtime_html <- html_nodes(webpage,'body')
        Runtime_data <- gsub('","',"zzzzz",Runtime_html)
        Runtime_data <- unlist(strsplit(Runtime_data, "(z){5}"))
        Runtime_data <- grep('Runtime":"',Runtime_data, value = TRUE )
        Runtime_data <- gsub('Runtime":"',"",Runtime_data)
        Runtime_data <- gsub(' min',"",Runtime_data)
        X['runtime1'][i,] <- Runtime_data[1]
        
        #metascore
        IMDB_data_html <- html_nodes(webpage,'body')
        IMDB_data <- gsub('"',"",IMDB_data_html)
        IMDB_data <- gsub(':',"",IMDB_data)
        IMDB_data <- gsub("Metascore","zzzzz",IMDB_data)
        IMDB_data1 <- unlist(strsplit(IMDB_data, "(z){5}"))
        IMDB_data2 <- unlist(strsplit(IMDB_data1[2], ","))
        X['metascore'][i,] <- IMDB_data2[1]
        print(i)
      }
      
    }
    else{
      next
    }
  }
  return(X)
}



Train <- GetMetaScoreAndRuntime(Train)


Train$metascore <- as.numeric(Train$metascore)
B<- data.frame(rating = Train[!is.na(Train$metascore), 'rating'],metascore = Train[!is.na(Train$metascore), 'metascore'])
C <- lm(metascore~rating, data = B)


Train[is.na(Train$metascore), 'metascore'] <- round(predict(C, data.frame(rating = Train[is.na(Train$metascore),'rating'])),0)

Train[Train$metascore <0, 'metascore'] <- 0



A <- Train[,c("imdb_id", 'runtime','runtime1')]
A$check <- A$runtime == A$runtime1



A$runtime1 <- as.character(A$runtime1)
A[A$runtime1== "N/A", 'runtime1'] <- A[A$runtime1== "N/A", 'runtime']
View(A)
A[A$runtime1== "N/A", 'runtime']

A[A$imdb_id== "tt0116485", "runtime1"] <- 90



Train <- merge(Train, A[,c(1,3)], by.x = "imdb_id", by.y = "imdb_id", all.x = TRUE)

Train$runtime <- as.numeric(Train$runtime)

Train <- merge(Train, Train1[,c(1,6)], by.x = "imdb_id", by.y = "imdb_id")

Train <- Train[order(Train$id),]
Train <- Train[,-69]
Train <- Train[,-1]
Train <- Train[,-1]


Train$Awards <- as.character(Train$Awards)
Train[grep('[Ww]in|[Ww]on', Train$Awards), 'Awards'] <- 2
Train[grep('[Nn]omination', Train$Awards), 'Awards'] <- 1
Train[grep('N/A', Train$Awards), 'Awards'] <- 0
Train$Awards <- as.numeric(Train$Awards)


## few interactions
Train$fe2 = Train$budget / Train$popularity
Train$fe3 = Train$budget / (Train$year* Train$year)
Train$fe4 = Train$year / Train$popularity
Train$fe5 = Train$popularity * Train$runtime


# Split the data set
Train_train <- Train[1:length(train.id),]
Train_test <- Train[(length(train.id)+1):nrow(Train),]

label2 = log1p(label)
#####

dtrain <- xgb.DMatrix(as.matrix(Train_train), label = label2)
dtest <- xgb.DMatrix(as.matrix(Train_test))

# Fit Model 
param <- list(booster="gbtree",
              eta=0.01,
              colsample_bytree = 0.3,
              max_depth = 6,
              min_child_weight = 2,
              base_score = mean(label2),
              subsample = 0.9)


set.seed(1234)
mod.xgb <- xgb.train(data=dtrain,params = param, nrounds= 1900,print_every_n = 50)

XGBCrossV <- xgb.cv(data=dtrain, params = param, nrounds= 2000, nfold = 10, nthread = 8, early_stopping_rounds = 50)



# Predict on test set
pred = predict(mod.xgb, newdata = dtest)
pred = exp(pred)-1
sub <- data.frame(test.id, pred)
colnames(sub) <- c("id", "revenue")
write.csv(sub, file =paste0(Sys.Date(),"_xgb.csv"), row.names = FALSE)





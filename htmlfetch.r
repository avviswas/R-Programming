HTML Fetch R code

ScrapValues <- function(X){
  for(i in 1:nrow(X)){
    if(X['imdb_id'][i,]!= ''){
      imdb <- as.character(X['imdb_id'][i,])
      url <- 'https://www.imdb.com/title/'
      UrlOpen <- paste(url, imdb, sep = "")
      webpage <- read_html(UrlOpen)
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
        print(X['ï..id'][i,])
      }
      
    }
    else{
      next
    }
      
      }
  return(X)
  
}




ScrubRunVals <- function(X){
  for(i in 1:nrow(X)){
    if(X['imdb_id'][i,]!= ''){
      if(!is.na(X['runtime'][i,])& X['runtime'][i,] ==0 | is.na(X['runtime'][i,] )){
        imdb <- as.character(X['imdb_id'][i,])
        url <- 'https://www.imdb.com/title/'
        UrlOpen <- paste(url, imdb, sep = "")
        webpage <- read_html(UrlOpen)
        Runtime_data_html <- html_nodes(webpage,'time')
        Runtime_data <- html_text(Runtime_data_html)
        Runtime_data <- gsub(c('\n')," ",Runtime_data)
        Runtime_data <- gsub('/'," ",Runtime_data)
        Runtime_data <- gsub(',',"",Runtime_data)
        Runtime_data1 <- unlist(strsplit(Runtime_data, "[ ]+"))
        Runtime <- tryCatch(t(Runtime_data1), error = function(e){ NULL})
        print(X['ï..id'][i,])
        if(is.null(Runtime)){
          print(X['imdb_id'][i,])
          X['runtime'][i,] <- 0
          next 
          } else {
          Runtime <- as.integer(Runtime)
          Runtime <- Runtime[4]
          Runtime
          X['runtime'][i,] <- Runtime
          
          print(X['ï..id'][i,])
          }
        } 
      else{
        next
      }
      }
    else{
      next
      }
    }
  return(X)
  
}



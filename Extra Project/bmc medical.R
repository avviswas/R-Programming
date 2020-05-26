#install.packages("dplyr")
library("rvest")
library("dplyr")
mainpage <- read_html("https://bmcmedgenet.biomedcentral.com/articles")

mainpage%>%  html_node("title")%>%  html_text()
print(mainpage)
a<-mainpage%>%
  html_nodes("a")%>%
  html_attr("href")


page<-mainpage%>%
  html_node(".u-reset-margin")%>%
  html_text()

r <- regexpr("[0-9]{1,5}[0-9]$", page)
pages <- regmatches(page, r)
noOfPages<-as.numeric(pages)

typeof(noOfPages)


listOfURLs<-c(c())
insertionPointer=1
first=1

for(i in 1:noOfPages){
  
  HTMLpage <- paste("https://bmcmedgenet.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=",i,sep="")
  mainpage <- read_html(HTMLpage)
  a<-mainpage%>%
    html_nodes("a")%>%
    html_attr("href")
  
  for(j in 1:length(a)){
    
    if(grepl("^/articles/",a[j])  & grepl("[0-9]$",a[j]) ){
      
      if(first==1){
        
        listOfURLs<-c(listOfURLs,a[j])
        first=first+1
        
      }else if(first > 1 ){
        
        if(listOfURLs[insertionPointer]!=a[j]){
          
          listOfURLs<-c(listOfURLs,a[j])
          insertionPointer=insertionPointer+1
          
        }
      }
    }
  }
}

length(listOfURLs)

filePath<-file("C:/Users/amanj/OneDrive/Documents/Data analytics with R/extra Proj/output.txt")

records<-c()

for(i in 1:length(listOfURLs)){
  
  pageAddress<-paste("https://bmcmedgenet.biomedcentral.com",listOfURLs[i],sep="")
  pageLink<- read_html(pageAddress)
  
  pageTitle<-pageLink%>%
    html_node(".ArticleTitle")%>%
    html_text()
  
  pageAbs<-pageLink%>%
    html_node("#Abs1")%>%
    html_text()
  
  keywords<-pageLink%>%
    html_node(".c-keywords")%>%
    html_text()
  
  pubDate<-pageLink%>%
    html_node(".HistoryOnlineDate")%>%
    html_text()
  
  authors<-pageLink%>%
    html_node(".AuthorNames")%>%
    html_text()
  
  emailAddr<-pageLink%>%
    html_nodes(".EmailAuthor")%>%
    html_attr("href")
  
  doi<-pageLink%>%
    html_nodes(".u-text-inherit")%>%
    html_attr("href")
  
  fullText<-pageAddress
  
  records[[i]]<-c(DOI<-doi,Title<-pageTitle,Author<-authors[1],Email<-emailAddr,Publication_Date<-pubDate,Abstract<-pageAbs,Keywords<-keywords,FullText<-fullText)
  
}

for(i in 1:length(records)){
  write.table(records[[i]],file = "C:/Users/amanj/OneDrive/Documents/Data analytics with R/extra Proj/data.txt",append=TRUE)
}




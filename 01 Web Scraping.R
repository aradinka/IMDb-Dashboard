# eas
library(xml2)
library(rvest)
library(tidyverse)
library(stringr)
library(rebus)
library(writexl)

url_cluster <- c('https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&ref_=adv_prv',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=51&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=101&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=151&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=201&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=251&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=301&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=351&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=401&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=451&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=501&ref_=adv_nxt',
                 'https://www.imdb.com/search/title/?groups=top_1000&sort=boxoffice_gross_us,desc&start=551&ref_=adv_nxt'
)

Title <- c()
Metascore <- c()
Rating <- c()
Synopsis <- c()
Votes <- c()
Gross <- c()
Runtime <- c()
Genre <- c()
Director <- c()
Director_and_stars <- c()
Image <- c()
Age_Rate <- c()



for(i in url_cluster){
  html <- read_html(i)
  title <- html %>% html_nodes('.lister-item-header') %>% html_text()
  metascore <- html %>% html_nodes('.metascore') %>% html_text()
  rating <- html %>% html_nodes('.ratings-imdb-rating strong') %>% html_text()
  synopsis <- html %>% html_nodes('.ratings-bar+ .text-muted') %>% html_text()
  votes <- html %>% html_nodes('.sort-num_votes-visible span:nth-child(2)') %>% html_text()
  gross <- html %>% html_nodes('.ghost~ .text-muted+ span') %>% html_text()
  runtime <- html %>% html_nodes('.runtime') %>% html_text()
  genre <- html %>% html_nodes('.genre') %>% html_text()
  director <- html %>% html_nodes('.text-muted+ p a:nth-child(1)') %>% html_text()
  director_and_stars <- html %>% html_nodes('.text-muted+ p') %>% html_text()
  image <- html %>% html_nodes('#main .loadlate') %>% as.character() %>% str_extract('https:.*jpg')
  
  Title <- append(Title, title)
  Metascore <- append(Metascore, metascore)
  Rating <- append(Rating, rating)
  Synopsis <- append(Synopsis, synopsis)
  Votes <- append(Votes, votes)
  Gross <- append(Gross, gross)
  Runtime <- append(Runtime, runtime)
  Genre <- append(Genre, genre)
  Director <- append(Director, director)
  Director_and_stars <- append(Director_and_stars, director_and_stars)
  Image <- append(Image, image)
}

df <- tibble(Title, Rating, Synopsis, Votes, Gross, 
             Runtime, Genre, Director, Director_and_stars)
glimpse(df)


### Cleaning Data
# Rating
df$Rating=as.numeric(df$Rating)
# Votes
df$Votes=as.numeric(gsub(",","",df$Votes))
# Gross
df$Gross=gsub("M","",df$Gross)
df$Gross=as.numeric(substring(df$Gross,2,6))
# Runtime
df$Runtime=as.numeric(gsub(" min","",df$Runtime))
# Year
df$Year=str_extract(df$Title, DGT%R%DGT%R%DGT%R%DGT)
# Title
df$Title=str_sub(df$Title,start=22, end = -13)
df$Title=str_trim(df$Title, side = "both")
# Genre
mgenre = str_split(df$Genre, pattern = ", ", simplify = T)
mgenre=gsub("\\n","",mgenre)
mgenre=gsub(" ","",mgenre)
df$Genre_1=mgenre[,1]
df$Genre_2=mgenre[,2]
df$Genre_3=mgenre[,3]
df=df[, -7]
# Stars
stars=gsub(".*Stars:","",df$Director_and_stars)
stars=gsub("\n","",stars)
stars=str_trim(stars,side = "both")
stars=str_split(stars, pattern = ", ", simplify = T)
df=df[,-8]
df$Stars_1=stars[,1]
df$Stars_2=stars[,2]
df$Stars_3=stars[,3]
df$Stars_4=stars[,4]
# Missing value
df[df==""]=NA

drop=c('The Ten Commandments',
          "The Lion in Winter",
          "Baahubali 2: The Conclusion",
          "Dangal",
          "Zelig",
          "The Red Shoes",
          "PK",
          "The Quiet Man",
          "The Lost Weekend",
          "Bajrangi Bhaijaan",
          "Witness for the Prosecution",
          "Bãhubali: The Beginning",
          "The Big Sleep",
          "Batman: Mask of the Phantasm",
          "The Gold Rush",
          "The Kid",
          "Once Upon a Time in America",
          "Hamlet",
          "Laura")

for (i in drop){
  df=df[!df$Title==i,]
}

df['Metascore'] = Metascore
df$Metascore=as.numeric(df$Metascore)
df <- df[-c(501:581),]

glimpse(df)
write_xlsx(df, 'data.xlsx')

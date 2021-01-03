library(httr)
library(tidyverse)
library(lubridate)
library(tm)
library(stringr)

setwd("~/LaunchCode/LC Final Project Updated")

#Turn this off for APIs
options(stringsAsFactors = FALSE)


##### Get the Data using the Nobel Prize API #####
# I am using the Nobel Prize Laureate APT to find data about each of the Nobel Prize recipients.
# It seems that the /laureate url returns more details on the prize winners.  The /prize API only returns name and motivation
# For this reason, I am commenting out the prizeURL below.
# I could uncomment this later to do an analysis of the prizes (what years were which categories of prizes awarded)

# prizeUrl  <- "http://api.nobelprize.org/v1/prize.json"
# nobelPrize <- fromJSON(prizeUrl)

# names(prizes)
# The prize data frame contains year, category, laureates, and overallMotivation
# Year, Category, and overallMotivation variables are of class character, laureates is a list of dataframes

laureateURL <- "http://api.nobelprize.org/v1/laureate.csv"
laureates <- read.csv(laureateURL)

names(laureates)

factor(laureates$category)

awards_by_category <- laureates %>%
#  filter(category == input$category) %>%
  group_by(year, gender) %>%
  count()
  

# %>%
#   

######  Clean data and add additional calculated fields ######
## Add age variable
## All nobel prizes are awarded on December 10
awardDate <- as.Date(with(laureates, paste(year, 12, 10,sep="-")), "%Y-%m-%d")
birthDate <- as.Date(laureates$born)

age_period <- as.period(interval(birthDate, awardDate), unit = 'year')
laureates$age <- age_period$year


## Calculate decade variable.  This should round down to previous decade  i.e, 1987 would be 1980
laureates$decadeFloor   <- laureates$year - laureates$year %% 10
# This number represents the end of the decade that the prize
laureates$decadeCeiling <- laureates$decadeFloor + 9



##### Export csv file to work with in Tableau #####
write.csv(laureates,'nobelLaureates.csv')




# ######  Clean data and add additional calculated fields ######
# ## Add age variable to show age on December 10 (when prize was received)
# ## All nobel prizes are awarded on December 10
# awardDate <- as.Date(with(laureates, paste(year, 12, 10,sep="-")), "%Y-%m-%d")
# 
# ## Some birth dates in data show month and date as '00'.  (Most of these are organizations,
# ## but some are people.  Reassign these birthdays to first day of the year.
# ## Add adjusted birth date variable
# 
# # List birthdates that are invalid format
# hits <- grep(pattern = "-00", x = laureates$birthdate)
# laureates$id[hits]
# laureates$name[hits]
# laureates$birthdate[hits]
# laureates$adjustedBirthdate <- as.Date(gsub(pattern = "-00", replacement = "-01", x = laureates$birthdate), "%Y-%m-%d")
# 
# class(adjustedBirthdate)
# 
# age_period <- as.period(interval(birthdate, awardDate), unit = 'year')
# laureates$age <- age_period$year
# 
# awardDate <- as.Date(with(laureates, paste(year, 12, 10,sep="-")), "%Y-%m-%d")
# birthDate <- as.Date(laureates$born)
# 
# age_period <- as.period(interval(birthDate, awardDate), unit = 'year')
# laureates$age <- age_period$year
# 
# 
# 
# ## Calculate decade variable.  This should round down to previous decade  i.e, 1987 would be 1980
# laureates$decadeFloor   <- laureates$year - laureates$year %% 10
# # This number represents the end of the decade that the prize
# laureates$decadeCeiling <- laureates$decadeFloor + 9
# 
 
# 
# ##### Export csv file to work with in Tableau #####
# write.csv(laureates,'nobelLaureates.csv')

###### Text Mining

## Create a subset of the laureates data for females
femaleLaureates <- laureates %>%
  filter(gender == 'female') %>%
  select(gender, motivation, category)
 

## Create a subset of the laureates data for males
maleLaureates <- laureates %>%
  filter(gender == 'male') %>%
  select(gender, motivation, category)


###### Use text mining for Motivation Word Cloud for Females ######
Corpus <- Corpus(VectorSource(femaleLaureates$motivation))# join all the motivation text in a corpus
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))
Corpus <- tm_map(Corpus, stripWhitespace)
Corpus <- tm_map(Corpus, content_transformer(tolower))
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, removePunctuation)

## The code below lists count of each word in matrix for each female laureate
#femaleLaureateMotivation <- as.data.frame(as.matrix(TermDocumentMatrix(Corpus)))


termMatrix <- as.matrix(TermDocumentMatrix(Corpus))
#matrix <- as.matrix(termMatrix)
v <- sort(rowSums(termMatrix),decreasing=TRUE)
laureateMotivation <- data.frame(motivation = names(v),freq=v, gender = 'female')# Generates a data frame with word frequencies


###### Use text mining for Motivation Word Cloud for Males ######
Corpus <- Corpus(VectorSource(maleLaureates$motivation))# join all the motivation text in a corpus
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))
Corpus <- tm_map(Corpus, stripWhitespace)
Corpus <- tm_map(Corpus, content_transformer(tolower))
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, removePunctuation)



termMatrix <- as.matrix(TermDocumentMatrix(Corpus))
v <- sort(rowSums(termMatrix),decreasing=TRUE)
# Add male motivation to dataframe
laureateMotivation <- rbind(laureateMotivation, data.frame(motivation = names(v),freq=v, gender = 'male'))# Generates a data frame with word frequencies

## Remove Unicode for apostrophe
#str_remove_all(laureateMotivation$motivation, "<e2><U+0092>")

str_remove_all(laureateMotivation$motivation, "'")
 

write.csv(laureateMotivation, 'laureateMotivation.csv')






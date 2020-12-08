library(rtweet)
library(tidyverse)
library(tidytext)
library(lubridate)
library(igraph)
library("stringr")
library("stringi")
library("tm")

key= "XXX"
secret= "XXX"
appname <- "XXX"

access_token = "XXX"
access_secret= "XXX"
# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

taba <- search_tweets(q = "Tabaré", n=18000, retryonratelimit = TRUE, type="recent")
taba <- taba[order(taba$retweet_count),]

taba$count <- 1

retuiteados <- taba %>% filter(retweet_count>0)

saveRDS(taba, "base tuits tabita.rds")

textos <- retuiteados %>% select(text)

quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))

preprocesamiento <- function(x) removeWords(removePunctuation(removeNumbers(tolower(x))),stopwords("spanish"))

textos$text <- preprocesamiento(quitarparentesis(textos$text))

textos$text <- str_remove_all(textos$text, "¡")
textos$text <- str_remove_all(textos$text, "!")
textos$text <- str_remove_all(textos$text, "¿")

DTM1 <- DocumentTermMatrix(Corpus(VectorSource(t(textos$text))))
DTM1<-as.matrix(DTM1)
DTM1<-as.data.frame(DTM1)
DTM1<-DTM1[,which(apply(DTM1,2,sum) > 10)]

dat <- data.frame(textos,DTM1)
rm("DTM1")
gc()

dat1 <- dat
dat1$text = NULL
dat2 <- reshape2::melt(dat1)

dat2$variable <- as.character(dat2$variable)


dat3 <- aggregate(value ~ variable, dat2, FUN=sum)

dat3 <- dat3[order(-dat3$value),]

write.csv2(dat3, "palabras tabaré.csv", row.names=F)



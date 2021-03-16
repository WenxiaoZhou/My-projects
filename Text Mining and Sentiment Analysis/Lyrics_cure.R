#install.package(qdap)
#install.package(tidyverse)
library(qdap)
library(tidyverse)
setwd("/Users/zhouwenxiao/Desktop/STAT5099-Investigation of Special Topics/Presentation")
cure<-read.table("The_cure_lyrics.txt",sep="\n")
#take a look of the cure data frame first, 28 rows

cur1<-paste(unlist(cure),collapse=" ")
polarity(cur1)
#Check key.pol for any words containing "stress". Use grep() to index the data frame by searching in the x column.
# Check the subjectivity lexicon
key.pol[grep("take care", x)]

#Define a new lexicon
custom_pol<- sentiment_frame(c(positive.words,"take care", "fix"), negative.words)
# Compare new score
polarity(cur1, polarity.frame=custom_pol)

#create word clouds
#install.package()
library(tm)
library(wordcloud)
library(RColorBrewer)
#create a corpus
curcorpus<-VCorpus(VectorSource(cur1))
#clean the corpus
cur2<- curcorpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
cur2<- tm_map(cur2, content_transformer(tolower))
cur2<- tm_map(cur2, removeWords, stopwords("english"))

#create a document-term-matrix
dtm <- TermDocumentMatrix(cur2) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

#generate word cloud
set.seed(20210313)  #for reproducibility
wordcloud(word=df$word,freq=df$freq,min.freq=1,
          max.words=100,random.order=FALSE,rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))


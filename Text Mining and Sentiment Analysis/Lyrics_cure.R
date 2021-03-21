#Make sure you install all the packages before loading: in this case, we requires qdap and 
#tidyverse packages, also load our lyric file first, take a look of the cure data frame first, 
#28 rows. Unlist function gives a list structure of text, and we paste all the lines together. 
#Now, apply polarity function to calculate the overall polarity score.


#install.package(qdap)
#install.package(tidyverse)
library(qdap)
library(tidyverse)
setwd("/Users/zhouwenxiao/Desktop/STAT5099-Investigation of Special Topics/Presentation")
cure<-read.table("The_cure_lyrics.txt",sep="\n")
#take a look of the cure data frame first, 28 rows

cur1<-paste(unlist(cure),collapse=" ")
polarity(cur1)
#Now we can see the average score is 1.336, which is positive
#Do you still remember the tips I provided, adjust lexicons to fit the text source under 
#certain environment. For example, if we consider short term "take care" "fix"that influence 
#the whole tone of the song, we need to check if our lexicon contains this term
#first of all, we check key.pol() to see a list of positive and negative words
#And then, we check if this list contain any words contain "care"
#How about short term "take care"? 


#Check key.pol for the subjectivity lexicon
key.pol()
key.pol[grep("care",x)]
key.pol[grep("take care", x)]

#there is no same term in the lexicon, so we define our new lexicon by adding this term 
#into positive.words, and compare lyrics text with lexicon again.  

#Define a new lexicon
custom_pol<- sentiment_frame(c(positive.words,"take care", "fix"), negative.words)
# Compare new score
polarity(cur1, polarity.frame=custom_pol)

#Now you see that we have average score 1.978 which is higher under the default lexicon 1.336, which means 
#this song is actually has stronger positive intent than we first consider. However, this example may not
#show how important to adjust lexicon since we first got a positive score. But if you have a score that is 
#really close to 0, if you adjust the lexicon by adding new words to positive or negative ones, it may 
#totally change the whole tone of the texts.


#What’s more, you may wonder how this song is actually saying? Let’s construct a word cloud. 
#Again, try install packages first before loading.

#create word clouds
#install.package()
library(tm)
library(wordcloud)
library(RColorBrewer)

#First, we create a corpus, corpus, A collection of linguistic data. 
#The main purpose of a corpus is to determine how the usage of a particular sound, word, or 
#syntactic construction varies. 

#create a corpus
curcorpus<-VCorpus(VectorSource(cur1))

#Clean the corpus by using tm_map function, remove numbers, punctuation, space, turn 
#all the characters into lower case, delete all the stop words that supports the English language. 
#clean the corpus
cur2<- curcorpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
cur2<- tm_map(cur2, content_transformer(tolower))
cur2<- tm_map(cur2, removeWords, stopwords("english"))

#Now, you want to have a dataframe containing each word in your first column and their 
#frequency in the second column. This can be done by creating a document term matrix with 
#the TermDocumentMatrix function from the tm package. 

#DTM is document-term matrix, that is used when you want to have each 
#document/review represented as a row. It describes the frequency of terms that occur in a collection 
#of documents. Then data frame is consisted with two columns, one for word collection, one for frequency 
#calculated. Then, we need to reclassify the DTM with as. Matrix() function.
#Sort the data by the count frequencies, now we can create the word cloud, 
#the larger and darker the word is representing, the higher frequency is for this word.
#create a document-term-matrix
dtm <- TermDocumentMatrix(cur2) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

df

#generate word cloud
set.seed(20210313)  #for reproducibility
wordcloud(word=df$word,freq=df$freq,min.freq=1,
          max.words=100,random.order=FALSE,rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))
#Now you see, there are some words are bigger and have deeper colors, which represents
#high frequencies, e.g. ...


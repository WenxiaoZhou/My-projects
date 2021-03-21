#Airbnb Reviews Sentiment Analysis

#loading packages and data
setwd("/Users/zhouwenxiao/Desktop/STAT5099-Investigation of Special Topics/Presentation")
library(qdap)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(tidytext)
library(dplyr)
library(tm)
library(tidyverse)
library(wordcloud)


bos_reviews<-readRDS('bos_reviews.rds')
# Have a look of the structure and dimension of the dataset

#structure
str(bos_reviews)
# Dimensions
dim(bos_reviews)


#remove punctuation and digts
bos_reviews$comments<-lapply(bos_reviews$comments,function(x) iconv(x,"latin1","ASCII",sub=""))
#second and third lines are setting for removing punctuation and digits
bos_reviews$comments<-gsub("[[:punct:]]","",bos_reviews$comments)
bos_reviews$comments<-gsub("\\d+","",bos_reviews$comments)

#now calculate the polarity score for comments, it may take some time to wait 

#polarity score for every comment and make summary
bos_pol<-polarity(bos_reviews$comments)

summary(bos_pol$all$polarity)

#we have a statistical summary on the overall polarity scores, mean score is 0.9133, we consider 
#reviews are positive in average. Now, draw a kernel density plot to see clearly.

#Kernel Density Plot
ggplot(bos_pol$all, aes(x = polarity, y = ..density..)) + 
  geom_histogram(binwidth = 0.25, fill = "lightblue", colour = "grey60") +
  geom_density(size = 0.75) +
  theme_gdocs()+
  labs(title="Kernel Density Plot of Polarity Scores")


bos_reviews$scaled_polarity <- scale(bos_pol$all$polarity)

#Next, organize and clean the text, we use Corpus to make analysis

#Organize & Clean the text
#Define positive terms from the text
pos_terms<-bos_reviews %>%
  # Add new variable polarity column
  mutate(polarity = bos_reviews$scaled_polarity) %>%
  # Filter for positive polarity
  filter(polarity>0) %>%
  
  #use pull() function works like works like double brackets [[ to extract a single variable.

  # Extract comments column
  pull(comments) %>% 
  # Paste and collapse by space
  paste(collapse = " ")

#similar for negative terms

neg_terms<- bos_reviews %>%
  # Add polarity column
  mutate(polarity = bos_reviews$scaled_polarity) %>%
  # Filter for negative polarity
  filter(polarity<0) %>%
  # Extract comments column
  #pull() function works like works like [[ to extract a single variable.
  pull(comments) %>% 
  # Paste and collapse
  paste(collapse = " ")

#Now you have two clusters represents positive and negative terms separately. We create corpus to 
#concatenate all the terms together. And create a term-document matrix from the corpus.

# Concatenate the terms
all_corpus <- c(pos_terms,neg_terms) %>% 
  # Source from a vector
  VectorSource %>% 
  # Create a volatile corpus
  VCorpus

#Create a term-document matrix from all_corpus
all_tdm <- TermDocumentMatrix(
  # Use all_corpus
  all_corpus, 
  control = list(
    # Use TFIDF weighting
    weighting = weightTfIdf, 
    # Remove the punctuation
    removePunctuation = TRUE,
    # Use English stopwords
    stopwords =stopwords(kind="en")
  )
)
#Examine the TDM 
all_tdm
# Matrix
all_tdm_m<-as.matrix(all_tdm)
# Column names
colnames(all_tdm_m) <- c("positive","negative")

#Now let's have a look at the positive words as well as the negative words overall, I select top 10 positive and top 10 negative
#reviews here

# Order the positive reviews
order_by_pos <- order(all_tdm_m[, 1], decreasing =TRUE)
# Review top 10 pos words
all_tdm_m[order_by_pos,] %>% head(n=10)
#  Order the negative reviews
order_by_neg <- order(all_tdm_m[,2], decreasing = TRUE)
# Review top 10 neg words
all_tdm_m[order_by_neg, ] %>% head(n=10)


set.seed(12348)


png("bosreviews_cloud.jpeg", height=1000,width=1000, res=100)
comparison.cloud(
  # Use the term-document matrix
  all_tdm_m,
  # Limit to 100 words
  max.words =100,
  colors = c("plum1","navyblue")
)
dev.off()



# Vector to tibble
tidy_reviews <- bos_reviews %>% 
  unnest_tokens(word, comments)

tidy_reviews
# Group by and mutate
tidy_reviews <- tidy_reviews %>% 
  group_by(id) %>% 
  mutate(original_word_order = seq_along(word))
#Print out the tibble
tidy_reviews

#Now we first clean the texts by removing stop words, first load stopwords,and apply anti_join
# Load stopwords
data("stop_words")
# Perform anti-join
tidy_reviews<- tidy_reviews %>% 
  anti_join(stop_words)


#Then we get access to bing lexicon and apply inner_join
# Get the correct lexicon
bing <- get_sentiments("bing")


# Calculate polarity for each review
pos_neg <- tidy_reviews %>% 
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative)
# Check outcome
summary(pos_neg)


#Examine relationship between effort and sentiment
#use tidy_reviews & pos_neg 
pos_neg_pol <- tidy_reviews %>% 
  # Effort is measured as count by id
  count(id) %>% 
  # Inner join to pos_neg
  inner_join(pos_neg) %>% 
  # Add polarity status
  mutate(pol = ifelse(polarity>=0, "Positive", "Negative"))
  # Examine results
pos_neg_pol

#Visualization
# Plot n vs. polarity, colored by pol
ggplot(pos_neg_pol, aes(polarity,n, color =pol)) + 
  # Add point layer
  geom_point(alpha = 0.25) +
  # Add smooth layer
  geom_smooth(method = "lm", se = FALSE) +
  theme_gdocs() +
  ggtitle("Relationship between word effort & polarity")

#It is shown that our assumption on relationship betweeen word effort and polarity is right.



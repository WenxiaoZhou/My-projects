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

#A small remainder here, to save space and time, I will use pipe to connect functions with variables
#together, if you are not familiar with it, that's ok, I will try my best to explain every function.
#Since I've already called the raw data and stored it in a data format under R, 
#it's easy to call it directly, I will share the link to access datasets at 
#the end of my presentation

bos_reviews<-readRDS('bos_reviews.rds')
# Have a look of the structure and dimension of the dataset

#structure
str(bos_reviews)
# Dimensions
dim(bos_reviews)

# When starting a sentiment project, sometimes a quick polarity() will
# help you set expectations or learn about the problem.
#First of all, we need to clean the reviews by removing punctuation, digits
#first step apply a function to convert a character vector between encoding, if can't 
#convert, use sub index to set as null

#remove punctuation and digts
bos_reviews$comments<-lapply(bos_reviews$comments,function(x) iconv(x,"latin1","ASCII",sub=""))
#second and third lines are setting for removing punctuation and digits
bos_reviews$comments<-gsub("[[:punct:]]","",bos_reviews$comments)
bos_reviews$comments<-gsub("\\d+","",bos_reviews$comments)

#now calculate the polarity score for comments, it may take some time to wait 

#polarity score for everycomment and make summary
bos_pol<-polarity(bos_reviews$comments)
summar(bos_pol$all$polarity)

#we have a statistical summary on the overall polarity scores, mean score is 0.9133, we consider 
#reviews are positive in average. Now, draw a kernel density plot to see clearly.

#Kernel Density Plot
ggplot(bos_pol$all, aes(x = polarity, y = ..density..)) + 
  geom_histogram(binwidth = 0.25, fill = "lightblue", colour = "grey60") +
  geom_density(size = 0.75) +
  theme_gdocs()+
  labs(title="Kernel Density Plot of Polarity Scores")

#Notice that the reviews do not center on 0. Often there are two causes for this sentiment "grade inflation." 
#First, social norms may lead respondents to be pleasant instead of neutral. This may caused by channel 
#specific. Particularly, social media posts may tight negative posts. 
#A second possible reason could be "feature based sentiment". In some reviews an author may write like 
#"the bed was comfortable and nice but the kitchen was dirty and gross." The sentiment of this type of 
#review encompasses multiple features simultaneously and therefore could make an average score skewed.
#Thus, we need to make a scale on the polarity scores first
bos_reviews$scaled_polarity <- scale(bos_pol$all$polarity)

#Next, organize and clean the text, we use Corpus to make analysis

#Organize & Clean the text
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

#Now, we will create a tidy text tibble by using unnest_tokens() function that split a column into 
#tokens, then a table with one-token-per-row will be created. 

# Vector to tibble
tidy_reviews <- bos_reviews %>% 
  unnest_tokens(word, comments)

#It is useful to capture the original word order within each group of a corpus. 
#To do so, use mutate(). In mutate() you will use seq_along() to create a sequence of numbers from 1 
#to the length of the object. This will capture the word order as it was written.

# Group by and mutate
tidy_reviews <- tidy_reviews %>% 
  group_by(id) %>% 
  mutate(original_word_order = seq_along(word))
#Print out the tibble
tidy_reviews

#In the tm package, you would use removeWords() to remove stopwords. 
#In the tidyverse you first need to load the stop words lexicon and then apply an anti_join() 
#between the tidy text data frame and the stopwords. Here, we use the second method.
# Load stopwords
data("stop_words")

# Perform anti-join
tidy_reviews_without_stopwords <- tidy_reviews %>% 
  anti_join(stop_words)

#Now, we almost done the text mining part, all the preparation for sentiment analysis are done.
#We can compare tidy sentiment by using qdap polarity function.

#Compare Tidy Sentiment to qdap polarity
# Get the correct lexicon
bing <- get_sentiments("bing")

#Recall, we already creates tidy_reviews contain each word that is separated, and inner_join with bing
#lexicon to make comparison, if there are same words in two tables, it will be kept. In that way, we
#can calculate the polarity score for each review.

# Calculate polarity for each review
pos_neg <- tidy_reviews %>% 
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative)
# Check outcome
summary(pos_neg)

#From the summary above, we can see that maximum of positive words is 42, it may represent a long review
#with positive comments. Here, we want to have a small proof that often authors will use more words when 
#they are more passionate. Conversely a less impassioned passenger may not feel compelled to spend a lot of 
#time writing a review. 

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
#Now, at last, we construct a word cloud for summarizing positive and negative words frequencies

# Matrix
all_tdm_m<-as.matrix(all_tdm)
# Column names
colnames(all_tdm_m) <- c("positive","negative")
# Top pos words
order_by_pos <- order(all_tdm_m[, 1], decreasing =TRUE)
# Review top 10 pos words
all_tdm_m[order_by_pos,] %>% head(n=10)
# Top neg words
order_by_neg <- order(all_tdm_m[,2], decreasing = TRUE)
# Review top 10 neg words
all_tdm_m[order_by_neg, ] %>% head(n=10)

set.seed(12345)
#Make sure to include all the words in the word_cloud picture, we generate the plot in a png form
png("bosreviews_cloud.jpeg", height=1000,width=1000, res=100)
comparison.cloud(
  # Use the term-document matrix
  all_tdm_m,
  # Limit to 50 words
  max.words =50,
  colors = c("plum1","navyblue")
)
dev.off()

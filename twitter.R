# import twitteR library
library(twitteR)

# Authorisation for using twitter api to download tweets

accessTokenkey <- "****"
accessTokenSecret <- "****"
consumerKey <- "****"
consumerSecret <- "****"

setup_twitter_oauth(consumerKey, consumerSecret, accessTokenkey, accessTokenSecret)

number <- 500
# Downloading tweets regarding swine flu
tweets_swineflu <- searchTwitter('#swineflu', n = number, lang = "en")
length(tweets_swineflu)

# Downloading tweets regarding cacncer
tweets_cancer <- searchTwitter('#cancer', n = number, lang = "en")
length(tweets_cancer)

# Downloading tweets regarding diseases
tweets_disease <- searchTwitter('#disease', n = number, lang = "en")
length(tweets_disease)

#Downloading tweets regarding Cricket World Cup '15
tweets_cwc <- searchTwitter('#cwc15', n = number, lang = "en")
length(tweets_cwc)

# Downloading tweets regarding Europa League
tweets_europa <- searchTwitter('#EuropaLeague', n = number, lang = "en")
length(tweets_europa)

# Downloading tweets regarding sports
tweets_sports <- searchTwitter('#sports', n = number, lang = "en")
length(tweets_sports)

# Downloading tweets regarding aib knockout
tweets_aibko <- searchTwitter('#aibknockout', n = number, lang = "en")
length(tweets_aibko)

# Downloading tweets regarding oscars
tweets_oscars <- searchTwitter('#oscars', n = number, lang = "en")
length(tweets_oscars)

# Downloading tweets regarding entertainment
tweets_entertainment <- searchTwitter('#entertainment', n = number, lang = "en")
length(tweets_entertainment)

# merging all kinds of tweets
tweets <- c(tweets_cancer, tweets_swineflu, tweets_disease, tweets_entertainment, tweets_aibko, tweets_oscars, tweets_cwc, tweets_europa, tweets_sports)

# Converting tweets into data frame
tweets.df <- twListToDF(tweets)
dim(tweets.df)

# Remove garbage symbols like smileys, etc.
tweets.df$text <- sapply(tweets.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

# Text Mining starts here

# Import tm library
library(tm)

#Create a corpus for the tweets
myCorpus <- Corpus(VectorSource(tweets.df$text))

# Create a copy of the Corpus
tweets.corp <- myCorpus

# lowercasing the alphabets
myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy = TRUE)
# End: done lowercasing

# Whitespace tokenisation
for(i in 1:length(myCorpus))
{
  a <- scan_tokenizer(myCorpus[[i]])
  a <- paste(a, collapse = ' ')
  myCorpus[[i]]$content <- a
}
# Whitespace tokenisation ends here

vocab0 <- myCorpus

# Remove terms which have doc freq <= 5
tdm <- TermDocumentMatrix(myCorpus)
tags <- findFreqTerms(tdm, lowfreq = 0, highfreq = 5)

for(j in 1:length(myCorpus))
{
  for (i in 1:length(tags))
  {
    myCorpus[[j]]$content <- gsub(tags[[i]], "", myCorpus[[j]], fixed=TRUE)
  }
  write(myCorpus[[j]]$content, "output.txt", append = TRUE)
}

# Removal of rare terms end here

vocab1 <- myCorpus
copy <- vocab1

dtm <- DocumentTermMatrix(copy)
dtm_tfidf <- weightTfIdf(dtm)
matrix <- as.matrix(dtm_tfidf)

# For heirarchial clustering
dist_matrix <- dist(scale(matrix))
fit <- hclust(dist_matrix, method="ward")
plot(fit)
rect.hclust(fit, k = 3)
# heirarchial clustering ends here

norm_eucl <- function(m) m/apply(m, MARGIN = 1, FUN = function(x) sum(x^2)^0.5)
m_norm <- norm_eucl(matrix)
k <- 3
kcluster <- kmeans(m_norm, k)

calc <- function(list, x)
{
  for(i in 1:length(list))
  {
    num <- 0
    num <- as.numeric(list[[i]])
    #print (num)
    if(num <= 1500)
    {
      #print ("category 1")
      cat_11 <- cat_11 + 1
      if(num <= 500)
      {
        cat1 <- cat1 + 1
      }
      else if(num <= 1000)
      {
        cat2 <- cat2 + 1
      }
      else
      {
        cat3 <- cat3 + 1
      }
    }
    else if(num <= 3000)
    {
      #print ("category 2")
      cat_22 <- cat_22 + 1
      if(num <= 2000)
      {
        cat4 <- cat4 + 1
      }
      else if(num <= 2500)
      {
        cat5 <- cat5 + 1
      }
      else
      {
        cat6 <- cat6 + 1
      }
    }
    else
    {
     # print ("category 3")
      cat_33 <- cat_33 + 1
      if(num <= 3500)
      {
        cat7 <- cat7 + 1
      }
      else if(num <= 4000)
      {
        cat8 <- cat8 + 1
      }
      else
      {
        cat9 <- cat9 + 1
      }
    }
  }
  if (x == "COURSE")
  {
    return (c(cat_11, cat_22, cat_33))
  }
  else if (x == "FINE")
  {
    return (c(cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9))
  }
}

if (k == 9)
{
  # Calculate Purity on Fine level

  docs_in_1 <- names(which(kcluster$cluster == 1))
  docs_in_2 <- names(which(kcluster$cluster == 2))
  docs_in_3 <- names(which(kcluster$cluster == 3))
  docs_in_4 <- names(which(kcluster$cluster == 4))
  docs_in_5 <- names(which(kcluster$cluster == 5))
  docs_in_6 <- names(which(kcluster$cluster == 6))
  docs_in_7 <- names(which(kcluster$cluster == 7))
  docs_in_8 <- names(which(kcluster$cluster == 8))
  docs_in_9 <- names(which(kcluster$cluster == 9))
  
  n <- 0
  r <- calc(docs_in_1, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_2, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_3, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_4, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_5, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_6, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_7, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_8, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_9, "FINE")
  n <- n + max(r)
  
  print (n/4500)
  
# Purity on Fine level calculated
}

if(k == 3)
{
  # Calculate Purity on Course level
  
  docs_in_1 <- names(which(kcluster$cluster == 1))
  docs_in_2 <- names(which(kcluster$cluster == 2))
  docs_in_3 <- names(which(kcluster$cluster == 3))
    
  n <- 0
  r <- calc(docs_in_1, "COURSE")
  n <- n + max(r)
  r <- calc(docs_in_2, "COURSE")
  n <- n + max(r)
  r <- calc(docs_in_3, "COURSE")
  n <- n + max(r)
  
  print (n/4500)

  # Purity on Fine level calculated
}

# remove non alphanumeric characters
removenonalphanum <- function(x) gsub("[^[:alnum:] _@#-]", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removenonalphanum), lazy=TRUE)
# Removal of non alphanumeric characters ends here

vocab2 <- myCorpus

dtm_LDA <- LDA(dtm, k)

if(k == 9)
{
  docs_in_1 <- names(which(topics(dtm_LDA) == 1))
  docs_in_2 <- names(which(topics(dtm_LDA) == 2))
  docs_in_3 <- names(which(topics(dtm_LDA) == 3))
  docs_in_4 <- names(which(topics(dtm_LDA) == 4))
  docs_in_5 <- names(which(topics(dtm_LDA) == 5))
  docs_in_6 <- names(which(topics(dtm_LDA) == 6))
  docs_in_7 <- names(which(topics(dtm_LDA) == 7))
  docs_in_8 <- names(which(topics(dtm_LDA) == 8))
  docs_in_9 <- names(which(topics(dtm_LDA) == 9))
  
  n <- 0
  r <- calc(docs_in_1, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_2, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_3, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_4, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_5, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_6, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_7, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_8, "FINE")
  n <- n + max(r)
  r <- calc(docs_in_9, "FINE")
  n <- n + max(r)
  
  print (n/4500)
}

if(k == 3)
{
  docs_in_1 <- names(which(topics(dtm_LDA) == 1))
  docs_in_2 <- names(which(topics(dtm_LDA) == 2))
  docs_in_3 <- names(which(topics(dtm_LDA) == 3))
  
  n <- 0
  r <- calc(docs_in_1, "COURSE")
  n <- n + max(r)
  r <- calc(docs_in_2, "COURSE")
  n <- n + max(r)
  r <- calc(docs_in_3, "COURSE")
  n <- n + max(r)
  
  print (n/4500)
}

# Copy of the corpus before any transformations
myCorpus_before <- myCorpus

# Remove punctuations, numbers, URLs from the corpus and lowercase the words



myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation), lazy=TRUE)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers), lazy=TRUE)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL), lazy=TRUE)
myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy=TRUE)
myStopWords <- c(stopwords("english"), "available", "via")
myStopWords <- setdiff(myStopWords, c("r", "big"))
myCorpus <- tm_map(myCorpus, removeWords, myStopWords)

myCorpusCopy <- myCorpus

# TODO: stemming not happening
# myCorpus <- tm_map(myCorpusCopy, content_transformer(stemDocument), lazy=TRUE)
# myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary=myCorpusCopy)

# myCorpusCopy ---------> Corpus 
MC_tokenizer(myCorpus)
scan_tokenizer(myCorpus)

# Create a term document matrix
tdm <- TermDocumentMatrix(myCorpus_before, control=list(minDocFreq=6))
tdm

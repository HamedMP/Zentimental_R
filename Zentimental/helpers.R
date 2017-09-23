# Team: Marcos BERNAL & Hamed MOHAMMADPOUR

library(devtools)
library(twitteR)
library(tm)
library(wordcloud)
library(ggplot2) 
library(entropy)
library(quanteda)
library(proxy)
library(reshape)

library(dplyr)
library(tidytext)

# Insert your credentials to use the application
connectTwitter <- function() {
  api_key <- ''
  api_secret <- ""
  access_token <- ""
  access_token_secret <- ""
  
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret) # authentification
}

getCleanTweets <- function(word, nbTweets = 200) {
  tweets <-searchTwitter(word, n=nbTweets)
  tweetTxt <- sapply(tweets, function(x) x$getText())
  tweetTxt <- sapply(tweetTxt,
                     function(row) iconv(
                       row,
                       from = 'latin1',
                       to = 'ASCII',
                       sub = ""
                     ))
  myCorpus <- Corpus(VectorSource(tweetTxt))
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  tdm <- TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE, 
                                                     stopwords = c(stopwords("english"), 
                                                                   "rt", "amp", "via"),
                                                     removeNumbers = TRUE,
                                                     tolower = TRUE
  ))
  
  # inspect(removeSparseTerms(tdm[, 1:10], 0.7))
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  dm <- data.frame(word=names(word_freqs), freq = word_freqs) # , tdf = m, tdm = tdm
  return(dm)
}

get_tweet_sentiment <- function(tweets){
  merged_data = merge(tweets, words)
  merged_data$sentiment = factor(merged_data$sentiment)
  tweets.sentiment = count(merged_data$sentiment)
  
  
  neg_count = tweets.sentiment[1,]
  pos_count = tweets.sentiment[2,]
  sentiment = data.frame("neg"=neg_count, "pos"=pos_count)
  
}

get_positive_percentage <-function(sentiment){
  sentiment$pos.freq / (sentiment$neg.freq + sentiment$pos.freq)
}

words = get_sentiments("bing")
pos = subset(words, words$sentiment == "positive")
neg = subset(words, words$sentiment == "negative")



library(Quandl)

get_finance_data <- function(symbol, 
                             start_date="2016-08-31", 
                             end_date="2017-08-31"){
  
  mydata = Quandl(c(paste("WIKI", symbol, sep = "/")), 
                  start_date=start_date, 
                  end_date=end_date)
}

plot_sentiment <- function(pos){
  
  slices <- c(1-pos, pos)
  lbls <- c("Negetiveness", "Positiveness")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,labels = lbls, col=  c("green", "purple"))
}

plot_financials <- function(mydata){
  plot(mydata$Date, mydata$Open, 
       type="l",
       lwd=2,
       ann=FALSE,
       col="brown")
}

get_timeline <- function(username, n, rt = TRUE){
  tweets = userTimeline(username,n=n, includeRts = rt)
  tweetTxt <- sapply(tweets, function(x) x$getText())
  tweetTxt <- sapply(tweetTxt,
                     function(row) iconv(
                       row,
                       from = 'latin1',
                       to = 'ASCII',
                       sub = ""
                     ))
  myCorpus <- Corpus(VectorSource(tweetTxt))
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  tdm <- TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE, 
                                                     stopwords = c(stopwords("english"), 
                                                                   "rt", "amp", "via"),
                                                     removeNumbers = TRUE,
                                                     tolower = TRUE
  ))
  
  # inspect(removeSparseTerms(tdm[, 1:10], 0.7))
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  dm <- data.frame(word=names(word_freqs), freq = word_freqs) # , tdf = m, tdm = tdm
  return(dm)
}

# Retreive tweets and apply data cleaning on their text
# Returns: A data frame of words and their frequency
processWords <- function(word = "datascience",
                         nbTweets = 200){
  
  tweets <-searchTwitter(word, n=nbTweets)
  tweetTxt <- sapply(tweets, function(x) x$getText())
  tweetTxt <- sapply(tweetTxt,
                     function(row) iconv(
                       row,
                       from = 'latin1',
                       to = 'ASCII',
                       sub = ""
                     ))
  
  myCorpus <- Corpus(VectorSource(tweetTxt))
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  tdm <- TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE, 
                                                     stopwords = c(stopwords("english"), 
                                                                   "rt", "amp", "via"),
                                                     removeNumbers = TRUE,
                                                     tolower = TRUE
                                                     ))
  
  # inspect(removeSparseTerms(tdm[, 1:10], 0.7))
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  dm <- data.frame(word=names(word_freqs), freq = word_freqs) # , tdf = m, tdm = tdm
  return(dm)
}


# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)

# get bigram tokens
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

# Get top n words. Has option to either remove the keyword itself or not
chooseTopN <- function(keyword, dm, n = 10, removeKW = TRUE){
  if(removeKW){
    dm = dm[dm$word != keyword,]
  }
  return(head(dm, n))
}

# Create a histogram of the words and their frequency
word_hist <- function(topNdm, keyword, fill = "turquoise"){
  p <- ggplot(topNdm, aes(word, freq))    
  p <- p + geom_bar(stat="identity", fill = fill)
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
  p <- p + scale_x_discrete(name=keyword)
  return(p)
}

# Create a wordcloud of the words
cloud_twitter <- function(words){
  wordcloud(words$word, freq = words$freq, colors = brewer.pal(8, "Dark2"))
}

# Calculae Kullback-Leiber divergence with Naive method, considering each keyword's histogram as P or Q
calc_dlk_naive <- function(w1, w2){
  dm1 = processWords(w1)
  dm1 = chooseTopN(w1, dm1, n = 30)
  dm2 = processWords(w2)
  dm2 = chooseTopN(w2, dm2, n = 30)
  
  p1 = dm$freq
  q1 = dm2$freq 
  
  # Make p1 and q1 to sum to 1
  p1 = p1/sum(p1)
  q1 = q1/sum(q1)
  kl = KL.plugin(p1, q1) 
  return(kl)
}

calc_dkl_method_4 <- function(word1, word2, 
                              nbTweets = 400, 
                              topn = 30,
                              removeKeyword = TRUE) {
  # Get dataframe for the keywords
  dm.w1 = processWords(word = word1, nbTweets = nbTweets)
  dm.w2 = processWords(word = word2, nbTweets = nbTweets)
  
  # Get topwords for each keyword
  top_n.w1 = chooseTopN(word1, dm.w1, n = topn, removeKW = removeKeyword)
  top_n.w2 = chooseTopN(word2, dm.w2, n = topn, removeKW = removeKeyword)
  
  # Create the global dictionary of top 2*n words
  top_n.merged = merge(top_n.w1, top_n.w2, all = TRUE)
  top_n.merged = aggregate(. ~ word, transform(top_n.merged, word = tolower(word)), sum)
  
  # Define dataframe of each keyword (containing all the words) in 
  # the histogram of new dictionary (containing topwords from both k1 and k2)
  top_n.merged.w1 = merge(top_n.merged, dm.w1, by = "word", all.x = TRUE)
  top_n.merged.w2 = merge(top_n.merged, dm.w2, by = "word", all.x = TRUE)
  
  # Make the sum to be 1
  p = top_n.merged.w1
  p = p$freq.y/sum(p$freq.y, na.rm = TRUE)
  
  q = top_n.merged.w2
  q = q$freq.y/sum(q$freq.y, na.rm = TRUE)
  
  # Calculate the KL divergence for keywords with respects to each other
  KL_W1_W2 = sum(p*log(p/q), na.rm = TRUE)
  KL_W2_W1 = sum(q*log(q/p), na.rm = TRUE)
  
  dm <- list("KL_1_2" = KL_W1_W2, "KL_2_1" = KL_W2_W1, 
                   "dm.w1" = dm.w1, "dm.w2" = dm.w2, 
                   "top_n.w1" = top_n.w1, "top_n.w2" = top_n.w2,
                   "p" = p, "q" = q)
  
  return(dm)
}

# ------ Future Works

# Clustering with cosine dissimilarity and visualize with dendograms
dendogram_clus <- function(tdm){
  dissim = proxy::dist(as.matrix(tdm), method = "cosine")
  h <- hclust(dissim, method = "ward.D")
  return(h)
}


# tf/idf for categorising new tweets to one of the topics
create_tf_idf_dataframe <- function(tdf_tweet){
  # tf
  tf <- tdf_tweet
  
  # idf
  idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) %>% diag()
  
  tf_idf <- crossprod(tf, idf)
  colnames(tf_idf) <- rownames(tf)
  
  # Normalization row-wise
  tf_idf = tf_idf / sqrt( rowSums( tf_idf^2 ) )
  
  return(as.data.frame(word = names(tf_idf), weight = tf_idf))
}
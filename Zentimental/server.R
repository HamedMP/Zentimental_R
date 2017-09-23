# Team: Marcos BERNAL & Hamed MOHAMMADPOUR

library(shiny)
library(colorspace)
library(reshape)
library(entropy)

source("helpers.R")

choice = c("1: Naive", 
           "2: Intersection", 
           "3: Union")

# Define server logic required to draw a histogram
function(input, output) {
  
  connectTwitter()
  
  # Get tweets only when their relative param. is changed (i.e. number of tweets), 
  # in other case use the current downloaded one
  dm1 <- reactive({
    if(input$submit == 0) 
      return()
    dm1 = getCleanTweets(word = input$keyword1, nbTweets = input$nbTweets)
    dm1
  })
  
  dm2 <- reactive({
    if(input$submit == 0) 
      return()
    dm2 = get_timeline(input$keyword2, input$nbTweets)
    dm2
  })
  
  
  output$financials <- renderPlot({
    mydata = get_finance_data(input$symbol)
    plot_financials(mydata = mydata)
  })
  
  output$choice <- renderText(input$which)
  
  # Render the histogram of keywords by getting tweets, computing the topwords with its functions from helper
  output$hist1 <- renderPlot({
    if(input$submit == 0) 
      return()
    tw1 = chooseTopN(input$keyword1, dm = dm1(), n = input$cloudsize, removeKW = input$removeKeyword)
    word_hist(tw1,input$keyword1)
  })
  
  output$hist2 <- renderPlot({
    if(input$submit == 0) 
      return()
    tw2 = chooseTopN(input$keyword2, dm = dm2(), n = input$cloudsize, removeKW = input$removeKeyword)
    word_hist(tw2, input$keyword2, fill = "purple")
  })
  
  
  # Show hisitogram of Intersected words in the top n words
  output$hist_intersection <- renderPlot({
    if(input$submit == 0) 
      return()
    dm.w1 = dm1()
    dm.w2 = dm2()
    top_n.w1 = chooseTopN(input$keyword1, dm.w1, n = input$cloudsize, removeKW = input$removeKeyword)
    top_n.w2 = chooseTopN(input$keyword2, dm.w2, n = input$cloudsize, removeKW = input$removeKeyword)
    
    # Inner join the two top words
    topn.intersection = merge(top_n.w1, top_n.w2, by = "word")
    
    # p1 = top_n.merged$freq1
    # q1 = top_n.merged$freq2
    barplot(t(data.frame(topn.intersection$freq.x, topn.intersection$freq.y)), 
            legend = c(input$keyword1, input$keyword2), 
            names.arg=topn.intersection$word, 
            beside=TRUE, col = c("turquoise", "purple"), las=2)
  })
  
  # Show histogram for method 3.
  output$hist_union <- renderPlot({
    if(input$submit == 0) 
      return()
    dm.w1 = dm1()
    dm.w2 = dm2()
    top_n.w1 = chooseTopN(input$keyword1, dm.w1, n = input$cloudsize, removeKW = input$removeKeyword)
    top_n.w2 = chooseTopN(input$keyword2, dm.w2, n = input$cloudsize, removeKW = input$removeKeyword)
    
    # Create the global dictionary of top 2*n words
    top_n.merged = merge(top_n.w1, top_n.w2, all = TRUE)
    top_n.merged = aggregate(. ~ word, transform(top_n.merged, word = tolower(word)), sum)
    
    # Define dataframe of each keyword (containing all the words) in 
    # the histogram of new dictionary (containing topwords from both k1 and k2)
    top_n.merged.w1 = merge(top_n.merged, dm.w1, by = "word", all.x = TRUE)
    top_n.merged.w2 = merge(top_n.merged, dm.w2, by = "word", all.x = TRUE)
       
    barplot(t(data.frame(top_n.merged.w1$freq.y, top_n.merged.w2$freq.y)),
            legend = c(input$keyword1, input$keyword2),
            names.arg=top_n.merged.w1$word,
            beside=TRUE,
            col = c("turquoise", "purple"),
            las=2)
  })
  
  output$companySentiment <- renderText({
    paste(get_positive_percentage(get_tweet_sentiment(dm1())))
  })
  
  output$companySentimentPlot <- renderPlot({

    plot_sentiment(get_positive_percentage(get_tweet_sentiment(dm1())))
  })
  
  output$employeeSentiment <- renderText({
    paste(get_positive_percentage(get_tweet_sentiment(dm2())))
  })
  
  output$employeeSentimentPlot <- renderPlot({
    plot_sentiment(get_positive_percentage(get_tweet_sentiment(dm2())))
  })
  
  output$dkl1naive <- renderText({
    tw1 = dm1()
    tw1 = chooseTopN(input$keyword1, dm = tw1, n = input$cloudsize, removeKW = input$removeKeyword)
    tw2 = dm2()
    tw2 = chooseTopN(input$keyword2, dm = tw2, n = input$cloudsize, removeKW = input$removeKeyword)
    
    p1 = tw1$freq / input$cloudsize
    q1 = tw2$freq / input$cloudsize
    
    p1 = p1/sum(p1)
    q1 = q1/sum(q1)
    # kl = sum(p1*log(p1/q1), na.rm = TRUE)
    kl = KL.plugin(p1, q1)
    paste(kl)
  })
  
  output$dkl1intersection <- renderText({
    dm.w1 = dm1()
    dm.w2 = dm2()
    top_n.w1 = chooseTopN(input$keyword1, dm.w1, n = input$cloudsize, removeKW = input$removeKeyword)
    top_n.w2 = chooseTopN(input$keyword2, dm.w2, n = input$cloudsize, removeKW = input$removeKeyword)
    
    top_n.merged = merge(top_n.w1, top_n.w2, by = "word")
    
    p = top_n.merged$freq.x
    q = top_n.merged$freq.y
    
    p = p/sum(p, na.rm = TRUE)
    q = q/sum(q, na.rm = TRUE)
    
    KL_W1_W2 = sum(p*log(p/q), na.rm = TRUE)
    KL_W2_W1 = sum(q*log(q/p), na.rm = TRUE)
    paste(KL_W1_W2)
    
  })
  
  output$dkl1union <- renderText({
    dm.w1 = dm1()
    dm.w2 = dm2()
    top_n.w1 = chooseTopN(input$keyword1, dm.w1, n = input$cloudsize, removeKW = input$removeKeyword)
    top_n.w2 = chooseTopN(input$keyword2, dm.w2, n = input$cloudsize, removeKW = input$removeKeyword)
    
    top_n.merged = merge(top_n.w1, top_n.w2, all = TRUE)
    top_n.merged = aggregate(. ~ word, transform(top_n.merged, word = tolower(word)), sum)
    
    top_n.merged.w1 = merge(top_n.merged, dm.w1, by = "word", all.x = TRUE)
    top_n.merged.w2 = merge(top_n.merged, dm.w2, by = "word", all.x = TRUE)
    
    p = top_n.merged.w1
    p = p$freq.y/sum(p$freq.y, na.rm = TRUE)
    
    q = top_n.merged.w2
    q = q$freq.y/sum(q$freq.y, na.rm = TRUE)
    
    KL_W1_W2 = sum(p*log(p/q), na.rm = TRUE)
    KL_W2_W1 = sum(q*log(q/p), na.rm = TRUE)
    paste(KL_W1_W2)
  })
  
  output$dkl2naive <- renderText({
    tw1 = dm1()
    tw1 = chooseTopN(input$keyword1, dm = tw1, n = input$cloudsize, removeKW = input$removeKeyword)
    tw2 = dm2()
    tw2 = chooseTopN(input$keyword2, dm = tw2, n = input$cloudsize, removeKW = input$removeKeyword)
    
    p1 = tw1$freq / input$cloudsize
    q1 = tw2$freq / input$cloudsize
    
    p1 = p1/sum(p1)
    q1 = q1/sum(q1)
    # kl = sum(p1*log(q1/p1), na.rm = TRUE)
    kl = KL.plugin(q1, p1)
    paste(kl)
  })
  
  output$dkl2intersection <- renderText({
    dm.w1 = dm1()
    dm.w2 = dm2()
    top_n.w1 = chooseTopN(input$keyword1, dm.w1, n = input$cloudsize, removeKW = input$removeKeyword)
    top_n.w2 = chooseTopN(input$keyword2, dm.w2, n = input$cloudsize, removeKW = input$removeKeyword)
    
    # full join, a.k.a. intersection
    top_n.merged = merge(top_n.w1, top_n.w2, by = "word")
    
    p = top_n.merged$freq.x
    q = top_n.merged$freq.y
    
    p = p/sum(p, na.rm = TRUE)
    q = q/sum(q, na.rm = TRUE)
    
    # KL_W1_W2 = sum(p*log(p/q), na.rm = TRUE)
    
    KL_W2_W1 = sum(q*log(q/p), na.rm = TRUE)
    paste(KL_W2_W1)
  })
  
  output$dkl2union <- renderText({
    dm.w1 = dm1()
    dm.w2 = dm2()
    top_n.w1 = chooseTopN(input$keyword1, dm.w1, n = input$cloudsize, removeKW = input$removeKeyword)
    top_n.w2 = chooseTopN(input$keyword2, dm.w2, n = input$cloudsize, removeKW = input$removeKeyword)
    
    top_n.merged = merge(top_n.w1, top_n.w2, all = TRUE)
    top_n.merged = aggregate(. ~ word, transform(top_n.merged, word = tolower(word)), sum)
    
    top_n.merged.w1 = merge(top_n.merged, dm.w1, by = "word", all.x = TRUE)
    top_n.merged.w2 = merge(top_n.merged, dm.w2, by = "word", all.x = TRUE)
    
    p = top_n.merged.w1
    p = p$freq.y/sum(p$freq.y, na.rm = TRUE)
    
    q = top_n.merged.w2
    q = q$freq.y/sum(q$freq.y, na.rm = TRUE)
    
    KL_W1_W2 = sum(p*log(p/q), na.rm = TRUE)
    KL_W2_W1 = sum(q*log(q/p), na.rm = TRUE)
    paste(KL_W2_W1) 
  })
  
  # Word cloud representation of the tweets
  output$wordcloud1 <- renderPlot({
    if(input$submit == 0)
     return()
    tw1 = dm1()
    tw1 = chooseTopN(input$keyword1, dm = tw1, n = input$cloudsize, removeKW = input$removeKeyword)
    
    isolate({
     cloud_twitter(tw1)
    })
  })

  output$wordcloud2 <- renderPlot({
    if(input$submit == 0)
     return()
    tw2 = dm2()
    tw2 = chooseTopN(input$keyword2, dm = tw2, n = input$cloudsize, removeKW = input$removeKeyword)
    isolate({
     cloud_twitter(tw2)
    })
  })
  
}

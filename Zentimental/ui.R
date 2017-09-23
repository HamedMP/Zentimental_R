# Team: Marcos BERNAL & Hamed MOHAMMADPOUR

library(shiny)
library(colorspace)
source('helpers.R')

# Options for the selectInput
choice = c("1: Naive",
           "2: Intersection",
           "3: Union")


shinyUI(fluidPage(
  titlePanel("Zentimental"),
  h3("Find your unhappy employees before they leave"),
  hr(),
  sidebarPanel(
    width = 3,
    textInput("keyword1",
              "First keyword (k1):",
              value = "Google"),
    textInput("symbol",
              "Your company stock symbol:",
              value = "GOOGL"),
    textInput("keyword2",
              "Second keyword (k2):",
              value = "mrry"),
    # selectInput("which", "The method to calculate the P and Q",
    #             choices = choice,
    #             selected = choice[1]),
    checkboxInput("removeKeyword", label = "Remove keyword from top-n:",
                  value = TRUE),
    actionButton("submit", "Update"),
    hr(),
    sliderInput(
      "cloudsize",
      "Top-n words:",
      min = 5,
      max = 30,
      value = 10
    ),
    sliderInput(
      "nbTweets",
      "Num of tweets:",
      min = 10,
      max = 400,
      value = 100
    )
  ),
  
  mainPanel(width = 9,
            mainPanel(
              width = 12,
              tabsetPanel(
                type = "tabs",
                tabPanel("Sentiment Analysis",
                         fluidRow(),
                         hr(),
                         fluidRow(column(
                           6,
                           h4("Your company sentiment"),
                           textOutput("companySentiment"),
                           plotOutput("companySentimentPlot")
                         ),
                         column(
                           6,
                           h4("Your employee sentiment"),
                           textOutput("employeeSentiment"),
                           plotOutput("employeeSentimentPlot")
                         )),
                         fluidRow(
                           plotOutput("financials")
                         )
                         ),
                tabPanel("Word Cloud",
                         fluidRow(
                           column(6,
                                  plotOutput("wordcloud1")),
                           column(width = 6,
                                  plotOutput("wordcloud2"))
                         ))
              )
            ))
))

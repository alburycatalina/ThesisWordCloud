# A little script for making a word cloud from my thesis
# Code adapted from: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a


# Load required packages
library(wordcloud) # generates wordclouds
library(dplyr) # data wrangling
library(MetBrewer)# color pallettes
library(tm) # text mining packages that converts text to corpus

# set working directory
setwd("C:/Users/catalina/Desktop/thesis_wordcloud")

# load in thesis data (pasted into notepad)
text <- read.delim("thesis_wordcloud.txt")

# Create a corpus from text with text mining package
docs <- Corpus(VectorSource(text))

# Clean text
docs <- docs %>% # Use tm to remove numbers, punctuation, white spaces
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Create document-term matrix (dataframe with each word and it's frequency)
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) # sort in decreasing frequency
df <- data.frame(word = names(words),freq=words) 


# Change vitamin to vitamin B12 (analysis excludes numbers)
df[df$word== "vitamin",]$word <- "vitamin B12"

# Change Southern to Southern Ocean and cylindrus to F. cylindrus (analysis only reports individual words)
df[df$word== "southern",]$word <- "Southern Ocean"
df[df$word== "cylindrus",]$word <- "F. cylindrus"

# Fix some capitilization errors
df[df$word== "meth",]$word <- "MetH"
df[df$word== "mete",]$word <- "MetE"
df[df$word== "bertrand",]$word <- "Bertrand"
df[df$word== "dmsp",]$word <- "DMSP"
df[df$word== "tmt",]$word <- "TMT"
df[df$word== "dmb",]$word <- "DMB"
df[df$word== "sam",]$word <- "SAM"
df[df$word== "heal",]$word <- "Heal"
df[df$word== "helliwell",]$word <- "Helliwell"

# Remove unwanted words
df <- df[!df$word == "table", ]
df <- df[!df$word == "figure", ]
df <- df[!df$word == "three", ]
df <- df[!df$word == "two", ]
df <- df[!df$word == "bdeplete", ]
df <- df[!df$word == "protein", ]
df <- df[!df$word == "???l", ]
df <- df[!df$word == "???l", ]
df <- df[!df$word == "???l", ]
df <- df[!df$word == "bindependent", ]
df <- df[!df$word == "???c", ]


# Use MetBrewer to generate a color Pallette (10 colors)
pallette <- met.brewer(name="VanGogh1", n=10)

# Set background color
par(bg= "#FEF4E4") 

# Make wordcloud
wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 1, 
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.3,
          colors = pallette , 
          scale = c(4.5, .25))



#' Title: NBA Fan Engagement Case B
#' NAME: Khoa Le
#' MBAN1
#' Date: March 12 2022
#############################################
# Set your working directory
setwd("/Users/khoale/Desktop/NLP/Text-Mining-NLP/Case/Case I/Data")

# To limit errors please run this code
Sys.setlocale('LC_ALL','C')
options(stringsAsFactors = FALSE)

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Load the following libraries ggplot2, ggthemes stringi, and tm
library(ggplot2)
library(ggthemes)
library(ggalt)
library(stringi)
library(stringr)
library(readr)
library(tm)
library(qdap) # Comment out if you have qdap problems
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(plyr)
library(dplyr)
library(tidytext)

# Create custom stop words
stops <- c(stopwords("SMART"), 'nba', 'celtics', 'nets', 'knicks', '76ers', 'raptors', 'bulls', 'cavaliers',
           'pistons', 'pacers', 'bucks', 'hawks', 'hornets', 'heat', 'magic', 'wizards', 'nuggets', 
           'timberwolves', 'thunder', 'blazers', 'jazz', 'warriors', 'clippers', 'lakers', 'suns', 'kings',
           'mavericks', 'rockets', 'grizzlies', 'pelicans', 'spurs', 'nike', 'adidas', 'â')
# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# load data
oct_txt  <- read.csv('M_Oct2020.csv')

# Sampling data 
# October 2020
oct_dx <- 1:nrow(oct_txt)
set.seed(123)
oct_dx <- sample(oct_dx, 7000)
oct_sample = oct_txt[oct_dx,]

### Saving files
write.csv(oct_sample,'oct20_cleanurl.csv', row.names = F)
# Reading new files
oct_twt <- read.csv('oct20_cleanurl.csv', header = TRUE)
# Nike - Lebron James
oct_twt <- oct_twt[grep("@KingJames|lebron|james",ignore.case = FALSE, oct_twt$text),]


# Tidy and unnest token
tidy_lebron <- oct_twt %>% unnest_tokens(word, text) %>% count(word, sort = TRUE)


# UDF to substitute some  
Subs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- gsub('@[a-z,A-Z]*', '', x)
  x <- gsub("[^\x01-\x7F]", "",x)
  x <- tolower(x)
  return(x)
}

# Applying SUBS to text column to a new object called txt
oct_txt <- lapply(oct_twt$text, Subs)

# Clean Corpus function
cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Volatile Corpus
oct_cleanTxt <- VCorpus(VectorSource(oct_txt))

# Clean the Corpus
oct_cleanTxt <- cleanCorpus(oct_cleanTxt, stops)

# Document Term Matrix or Term Document Matrix
oct_txtTdm  <- TermDocumentMatrix(oct_cleanTxt)
oct_txtTdmM <- as.matrix(oct_txtTdm)

# Frequency Data Frame
oct_twt_sums<- sort(rowSums(oct_txtTdmM), decreasing = TRUE)
topterm_oct_freq <- data.frame(word=names(oct_twt_sums),frequency=oct_twt_sums)

# Remove the row attributes meta family
rownames(topterm_oct_freq) <- NULL

# Simple barplot; values greater than 15
topWords_oct  <- subset(topterm_oct_freq, topterm_oct_freq$frequency >= 5) 

# Remove row attributes for simple barplot (topWords)
rownames(topWords_oct) <- NULL

# Chg to factor for ggplot
topWords_oct$word <- factor(topWords_oct$word, 
                            levels=unique(as.character(topWords_oct$word)))


### Bar chart for Top words by each month
# October - Lebron James
ggplot(topWords_oct, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='blue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words Lebron Oct 2020")


# Inspect word associations
associations <- findAssocs(oct_txtTdm, 'lebron', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3) 

# End

#' Title: NBA Fan Engagement Case A
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

# Create a Clean Corpus Function
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
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

### Loading csv files 
# Load the file named K August 2020
aug20_txt <- read.csv('K_Aug2020.csv', header = TRUE)
# Load the file named L September 2020
sep20_txt <- read.csv('L_Sep2020.csv', header = TRUE)
# Load the file named M October 2020
oct20_txt <- read.csv('M_Oct2020.csv', header = TRUE)


### Sampling data 
# August 2020
aug_dx <- 1:nrow(jul20_txt)
set.seed(123)
aug_dx <- sample(aug_dx, 5000)
aug20_sample = aug20_txt[aug_dx,]

# September 2020
sep_dx <- 1:nrow(sep20_txt)
set.seed(123)
sep_dx <- sample(sep_dx, 5000)
sep20_sample = aug20_txt[sep_dx,]

# October 2020
oct_dx <- 1:nrow(oct20_txt)
set.seed(123)
oct_dx <- sample(oct_dx, 5000)
oct20_sample = oct20_txt[oct_dx,]

# Removing non- AcSII in text
gsub("[^\x01-\x7F]", "", aug20_sample$text)
gsub("[^\x01-\x7F]", "", sep20_sample$text)
gsub("[^\x01-\x7F]", "", oct20_sample$text)
gsub('http\\S+\\s*', "", aug20_sample$text)
gsub('http\\S+\\s*', "", sep20_sample$text)
gsub('http\\S+\\s*', "", oct20_sample$text)

### Saving files
write.csv(aug20_sample,'aug20_unclean.csv', row.names = F)
write.csv(sep20_sample,'sep20_unclean.csv', row.names = F)
write.csv(oct20_sample,'oct20_unclean.csv', row.names = F)
# Reading new files
aug20_twt <- read.csv('aug20_unclean.csv', header = TRUE)
sep20_twt <- read.csv('sep20_unclean.csv', header = TRUE)
oct20_twt <- read.csv('oct20_unclean.csv', header = TRUE)

#unique(aug20_txt$team)
#unique(sep20_txt$team)
#unique(oct20_txt$team)

# Apply the VCorpus Function to a VectorSource
aug20_txt <- VCorpus(VectorSource(aug20_twt$text))
sep20_txt <- VCorpus(VectorSource(sep20_twt$text))
oct20_txt <- VCorpus(VectorSource(oct20_twt$text))


# Clean the Corpus with your cleanCorpus function
aug20_txt <- cleanCorpus(aug20_txt, stops)
sep20_txt <- cleanCorpus(sep20_txt, stops)
oct20_txt <- cleanCorpus(oct20_txt, stops)


### Document Term Matrix or Term Document Matrix
#August
aug_TDM  <- TermDocumentMatrix(aug20_txt, 
                               control=list(tokenize=bigramTokens))
aug_TDMm <- as.matrix(aug_TDM)
# September
sep_TDM  <- TermDocumentMatrix(sep20_txt, 
                               control=list(tokenize=bigramTokens))
sep_TDMm <- as.matrix(sep_TDM)
# October
oct_TDM  <- TermDocumentMatrix(oct20_txt, 
                              control=list(tokenize=bigramTokens))
oct_TDMm <- as.matrix(oct_TDM)     

# Frequency Data Frame
aug_twt_sums <- sort(rowSums(aug_TDMm), decreasing = TRUE)
topterm_aug20_freq <- data.frame(word=names(aug_twt_sums),frequency=aug_twt_sums)

sep_tweet_sums <- sort(rowSums(sep_TDMm), decreasing = TRUE)
topterm_sep20_freq <- data.frame(word=names(sep_tweet_sums),frequency=sep_tweet_sums)

oct_tweet_sums <- sort(rowSums(oct_TDMm), decreasing = TRUE)
topterm_oct20_freq <- data.frame(word=names(oct_tweet_sums),frequency=oct_tweet_sums)


# Remove the row attributes meta family
rownames(topterm_aug20_freq) <- NULL
rownames(topterm_sep20_freq) <- NULL
rownames(topterm_oct20_freq) <- NULL

# Review after removing, view top 10
topterm_aug20_freq[01:10,]
topterm_sep20_freq[01:10,]
topterm_oct20_freq[01:10,]

# Simple barplot; values greater than 150
# August
topWords_aug  <- subset(topterm_aug20_freq, topterm_aug20_freq$frequency >= 150) 
topWords_aug  <- topWords_aug[order(topWords_aug$frequency, decreasing=F),]

# September
topWords_sep <- subset(topterm_sep20_freq, topterm_sep20_freq$frequency >= 150) 
topWords_sep <- topWords_sep[order(topWords_sep$frequency, decreasing=F),]

# October
topWords_oct  <- subset(topterm_oct20_freq, topterm_oct20_freq$frequency >= 150) 
topWords_oct  <- topWords_oct[order(topWords_oct$frequency, decreasing=F),]

# Remove row attributes for simple barplot (topWords)
rownames(topWords_aug) <- NULL
rownames(topWords_sep) <- NULL
rownames(topWords_oct) <- NULL

# Chg to factor for ggplot
topWords_aug$word <- factor(topWords_aug$word, 
                            levels=unique(as.character(topWords_aug$word)))
topWords_sep$word <- factor(topWords_sep$word, 
                            levels=unique(as.character(topWords_sep$word)))
topWords_oct$word <- factor(topWords_oct$word, 
                            levels=unique(as.character(topWords_oct$word)))

### Bar chart for Top words by each month
# August
ggplot(topWords_aug, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkgreen') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words on August - 2020")
# September
ggplot(topWords_sep, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkblue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words on September - 2020")
# October
ggplot(topWords_oct, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='lightblue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)+
  ggtitle("Top words on October - 2020")

### Word Clouds ###
# Review all Palettes
display.brewer.all()

## Choose a color & drop light ones
# Set 1
pal1 <- brewer.pal(8, "Set2")
pal1 <- pal[-(1:2)]
# Set 2
pal2 <- brewer.pal(8, "Set3")
pal2 <- pal[-(1:2)]
# Set 3
pal3 <- brewer.pal(8, "Blues")
pal3 <- pal[-(1:2)]


## Create word cloud
# August
set.seed(1234)
wordcloud(topterm_aug20_freq$word,
          topterm_aug20_freq$freq,
          max.words    = 100,
          random.order = FALSE,
          colors       = pal1,
          scale        = c(2,1))
# September
set.seed(1234)
wordcloud(topterm_sep20_freq$word,
          topterm_sep20_freq$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal2,
          scale        = c(2,1))
# October 
set.seed(1234)
wordcloud(topterm_oct20_freq$word,
          topterm_oct20_freq$freq,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal3,
          scale        = c(2,1))


# End
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

oct_nike<- read.csv("/Users/khoale/Desktop/NLP/Text-Mining-NLP/Case/Case I/Data/oct20_unclean.csv")

#Exploring link to Nike 
# Use Gsub subsituting 
oct_Subs <- function(x){
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', 'RT', x)
  x <- gsub('@[a-z,A-Z]', '', x)
  x <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x)
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub("http[[:alnum:]]*", "", x)
  x <- tolower(x)
  return(x)
}

# Apply the function to JUST THE TEXT COLUMN to  a new object txt
oct_subs_txt <- oct_Subs(oct_nike$text)
# Reading pre-prepared file 

library(tidyverse)


oct_nike <- as.data.frame(oct_nike)
oct_nike <- mutate(oct_nike, doc_id = rownames(oct_nike))
oct_nike <- oct_nike[ , c("doc_id", "text")]    

# Creating corpus 
txt_Corpus <- VCorpus(VectorSource(oct_nike$text))

# Removing the text_oct from memory to have more ram space 
rm(oct_nike)

# Creating DTM/TDM of sample
txt_Tdm <- TermDocumentMatrix(txt_Corpus)

# Find associations
associations <- findAssocs(txt_Tdm, 'nike', 0.10)

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))

assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL

# Selecting interesting terms through filter 
assocDF2 <- assocDF %>%
  filter(terms %in% c("nike")) 

# Plotting 
ggplot(assocDF, aes(y=terms)) +
geom_point(aes(x=value), data=assocDF, col='steelblue') +
theme_minimal()+
geom_text(aes(x=value,label=value), colour="steelblue",hjust="inward", vjust ="inward" , size=3)+
ggtitle("Associations to Nike")
  


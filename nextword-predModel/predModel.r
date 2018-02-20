library(stringr); library(data.table)
library(openNLP); library(NLP)
library(tm)

# import n-grams files
unigram.dt <- readRDS("data/unigram.rds")
bigram.dt <- readRDS("data/bigram.rds")
trigram.dt <- readRDS("data/trigram.rds")
badwords <- readRDS("data/badwords.rds")

# speech tagging Annotators
sent.token.annotator <- Maxent_Sent_Token_Annotator()
word.token.annotator <- Maxent_Word_Token_Annotator()
pos.tag.annotator <- Maxent_POS_Tag_Annotator()













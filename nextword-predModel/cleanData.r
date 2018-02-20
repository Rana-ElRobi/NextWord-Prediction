# Load the required data for cleaning data
library(NLP); library(tm);library(dplyr)
library(RColorBrewer);library(wordcloud); library(stringi)
library(ggplot2); library(knitr);library(RWeka) 
library(gridExtra); library(magrittr); library(SnowballC)
library(doParallel); registerDoParallel(makeCluster(4))
library(stringr); library(dplyr); library(caret)
library(tau); library(data.table)

# download & unzip data
fileUrl <-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists("Coursera-SwiftKey.zip")){
  download.file(fileUrl, destfile = "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}


# As we see the data consist of text from 3 different sources: 
# blogs, news, and twitter feeds provided in 4 different languages: 
# German, English (US), Finnish, and Russian. 

# For the remainder of this project, we will use only the the English (US) data sets only.

# load & read all data files


# list of Names/urls for all english files
en.list.files = c("final/en_US/en_US.blogs.txt",
                  "final/en_US/en_US.news.txt",
                  "final/en_US/en_US.twitter.txt")

# list of empty text vars for the 3 data sources
en.text.data <- list(blogs = "", news = "", twitter = "")

# initialize zeros matrix to keep data summary structured
en.data.summary <- matrix(0, nrow = 3, ncol = 3,
                          dimnames = list(c("English Blogs", "English News", "English Twitts"),
                                          c("file-size(MB)", "Number of lines", "Number of words")))

# Get collective summary for the data

# loop on list of data sources list
for (i in 1:length(en.list.files)) {
  # start connection with current file in sources list
  con <- file(en.list.files[i], "rb")
  # fill the coresponding text var with data read
  en.text.data[[i]] <- readLines(con, encoding = "UTF-8",skipNul = TRUE)
  # close the connection
  close(con)
  en.text.data[[i]]
  # Get some basic summary data from current file 
  en.data.summary[i,1] <- round(file.info(en.list.files[i])$size / 1024^2, 2)
  en.data.summary[i,2] <- length(en.text.data[[i]])
  en.data.summary[i,3] <- sum(stri_count_words(en.text.data[[i]]))
}
# View Summary about English data
kable(en.data.summary)

# Combine the english resources
if (!file.exists("EnSources.txt")){
  # Combine 3 Data sources
  en.sources <- c(en.text.data$blogs, en.text.data$news, en.text.data$twitter)
  
  #Save sample in text file, to not need to generate it again & remove the main data from environment to free the RAMs
  writeLines(en.sources,con = file("EnSources.txt"))
  close(con)
  rm( en.text.data) #free memory
  
}else{
  # load generated sample
  en.sources <- readLines(con =file("EnSources.txt"),skipNul =TRUE )
}
# check the num of words now in this sample
en.sources.count <- sum(stri_count_words(en.sources))
en.sources.count #  102402051

# Pre-processing & Exploring data
# Now we need to clean the data, as the raw text file contains some characters, & symbols that may not provide useful information.
# We will remove such things as (numbers, punctuation, white space, etc.)
# But we will clean data one time with keeping the stop words and the without stemming.

# NB : (R codes with pipes (%>%) to save some running time).

# remove emoticons
en.sources <- iconv(en.sources, 'UTF-8', 'ASCII')

# Build corpus
en.corpus <- Corpus(VectorSource(as.data.frame(en.sources, stringsAsFactors = FALSE))) 

clean.en.corpus <- en.corpus %>%
  tm_map(tolower) %>%  # transfer all letters to lowercase
  tm_map(PlainTextDocument) %>%  #transfer to plain text document
  tm_map(removePunctuation) %>% # remove punctuation
  tm_map(removeNumbers) %>% # remove numbers
  tm_map(stripWhitespace) # strip white spaces

# Calculate word frequency of cleaned data 
# set data to matrix format
clean.en.corpus.matrix <- as.matrix(TermDocumentMatrix(clean.en.corpus))

# calculate word frequancy
word.freqs <- sort(rowSums(clean.en.corpus.matrix), decreasing=TRUE) 
# resturcture it in dataframe
clean.en.corpus.dataFram <- data.frame(word=names(word.freqs), freq=word.freqs)

# ================= 
# Generate traning set
set.seed(42)
inFrame <- createDataPartition(y = 1:length(en.sources), p = 0.15, list = F)
train <- en.sources[inFrame]
rm(en.sources, inFrame)


# ======================
# Do some cleaning
clean.train <- train %>% str_replace_all("www [a-z]+ [a-z]+", "")%>%
  str_replace_all( " ([a-z])\\1+ |^([a-z])\\1+ | ([a-z])\\1+$|^([a-z])\\1+$", " ")%>%
  str_replace_all( "([a-z])\\1{2,}", "\\1\\1")%>%
  str_replace_all( "\\'+([a-z]+)\\'+", "\\1")%>%
  str_replace_all( "\\'+ \\'+", " ")%>%
  str_replace_all( "(\\'+ )+|( \\'+)+|^\\'+|\\'+$", " ")%>%
  str_replace_all( "^[a-z]+$", "")%>%
  str_replace_all( "( [^ai])+ |^([^ai] )+|( [^ai])+$", " ")%>%
  str_replace_all( "^ +| +$|", "")%>%
  str_replace_all( " {2,}", " ")%>%
  str_replace_all( " +$|^ +", "")
clean.train <- clean.train[clean.train != ""]

# =====================
# Tokenize 
token.1 <- textcnt(clean.train, method = "string", split = "[[:space:]]", n = 1L, decreasing = T)
token.2 <- textcnt(clean.train, method = "string", split = "[[:space:]]", n = 2L, decreasing = T)
token.3 <- textcnt(clean.train, method = "string", split = "[[:space:]]", n = 3L, decreasing = T)

# =====================
# generate n-grams
# ----------------
## Uni-grams

unigram.dt <- data.table(text = names(token.1), as.matrix(token.1))
setnames(unigram.dt, "V1", "count")
setnames(unigram.dt, "text", "n0")

uni.count <- sum(unigram.dt$count)
unigram.dt <- mutate(unigram.dt, freq = round(count/uni.count, 7))

unigram.dt$count <- NULL
setkeyv(unigram.dt, c("n0", "freq"))

saveRDS(unigram.dt, "n-grams/unigram.rds")
# free memory
rm(uni.count, unigram.dt)

# ----------------
## Bi-grams

bibase.dt <- data.table(text = names(token.2), as.matrix(token.2))
setnames(bibase.dt, "V1", "count")
bigram.dt <- bibase.dt

bigram.dt[, c("n1", "n0")  := do.call(Map, c(f = c, strsplit(text, " ")))]
bigram.dt <- mutate(bigram.dt, freq = round(count/base1[n1][[1]], 7))

bigram.dt$text <- NULL
bigram.dt$count <- NULL

setkey(bigram.dt, n1)
bigram.dt <- bigram.dt[,lapply(.SD, function(x) head(x, 5)), by = key(bigram.dt)]

setkeyv(bigram.dt, c("n1", "freq", "n0"))
saveRDS(bigram.dt, "n-grams/bigram.rds")
# free memory
rm(bibase.dt, token.1, bigram.dt)

# ----------------
## Tri-grams

tribase.dt <- data.table(text = names(base3), as.matrix(base3))
setnames(tribase.dt, "V1", "count")

trigram.dt <- subset(tribase.dt, count > 1)
trigram.dt[, c("n2", "n1", "n0")  := do.call(Map, c(f = c, strsplit(text, " ")))]
trigram.dt <- mutate(trigram.dt, freq = round(count/base2[paste(n2, n1)][[1]], 7))

trigram.dt$text <- NULL
trigram.dt$count <- NULL

setkeyv(trigram.dt, c("n2", "n1"))
trigram.dt <- trigram.dt[,lapply(.SD, function(x) head(x, 5)),by = key(trigram.dt)]

setkeyv(trigram.dt, c("n2", "n1", "freq", "n0"))
saveRDS(trigram.dt, "n-grams/trigram.rds")

# free memory
rm(tribase.dt, token.2, token.3, trigram.dt)

# ---------
## Remove bad Words

badwords <- readLines("http://badwordslist.googlecode.com/files/badwords.txt", warn = F)
badwords <- tolower(badwords)
badwords <- str_replace_all(badwords, "\\(", "\\\\(")
# save as rds
saveRDS(badwords, "n-grams/badwords.rds")
rm(badwords) # free memory












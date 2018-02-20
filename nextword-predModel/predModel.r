library(stringr); library(data.table)
library(openNLP); library(NLP)
library(tm)

# import n-grams files
unigram.dt <- readRDS("n-grams/unigram.rds")
bigram.dt <- readRDS("n-grams/bigram.rds")
trigram.dt <- readRDS("n-grams/trigram.rds")
badwords <- readRDS("n-grams/badwords.rds")

# speech tagging Annotators
sent.token.annotator <- Maxent_Sent_Token_Annotator()
word.token.annotator <- Maxent_Word_Token_Annotator()
pos.tag.annotator <- Maxent_POS_Tag_Annotator()

# Function that clean text entered 
clean_text <- function(text) {
  input_str <- text %>% tolower %>%    
    str_replace_all("([iu]n)-([a-z])", "\\1\\2")%>%
    str_replace_all("([0-9])(st|nd|rd|th)", "\\1")%>%
    str_replace_all("[^a-z.' ]", " ")%>%
    str_replace_all("www\\.[a-z]+\\.[a-z]+", "")%>%
    str_replace_all("\\.", " ")%>%
    str_replace_all(" ([a-z])\\1+ |^([a-z])\\1+ | ([a-z])\\1+$|^([a-z])\\1+$", " ")%>%
    str_replace_all("([a-z])\\1{2,}", "\\1\\1")%>%
    str_replace_all("\\'+([a-z]+)\\'+", "\\1")%>%
    str_replace_all("\\'+ \\'+", " ")%>%
    str_replace_all("(\\'+ )+|( \\'+)+|^\\'+|\\'+$", " ")%>%
    str_replace_all("^[a-z]+$", "")%>%
    str_replace_all("( [^ai])+ |^([^ai] )+|( [^ai])+$", " ")%>%
    str_replace_all("^ +| +$|", "")%>%
    str_replace_all(" {2,}", " ")%>%
    str_replace_all(" +$|^ +", "")
  return(input_str)
}

# Function filter the words from bad words 
filter_text <- function(text) {
  # make copy as temporary var
  curr.txt <- text
  # check entered text lenght
  if (length(curr.txt) > 0) {
    # parse text
    words <- parse_text(curr.txt)
    # get lenght
    num_words <- length(words)
    # search for matched bad word
    if (num_words > 0) {
      for (i in 1:num_words) {
        if (words[i] %in% badwords) 
          # replace with *** 
          {words[i] <- paste(substring(words[i], 1, 1), "***", sep = "")}
      }
      curr.txt_w <- paste(words[1]) 
      if (num_words > 1) {
        for (i in 2:num_words) curr.txt_w <- paste(curr.txt_w, words[i])
      }
      # return filtered text
      return(curr.txt_w)
    }
  }
  return(curr.txt)
  }

# Function that gets annotation
get_default <- function(text) {
  if (length(text) > 0) {
    # Annotate words & sentence
    words.sent.ann <- annotate(as.String(text), list(sent.token.annotator, word.token.annotator))
    
    # Get pos annotation
    pos.ann <- annotate(as.String(text), pos.tag.annotator, words.sent.ann)
    
    # subset words
    words.pos.ann <- subset(pos.ann, type == "word")
    
    # get tagges 
    tags <- sapply(words.pos.ann$features, `[[`, "POS")
    
    # make return conitioning tree
    if (tags %like% "NN") {return("in")} 
    else if (tags %like% "VB") {return("a")} 
    else if (tags %like% "JJ") {return("time")} 
    else if (tags %like% "PRP") {return("first")} 
    else if (tags %like% "CC") {return("i")} 
    else if (text == "the") {return("first")}
    
  }else{return("the")}
  
}

















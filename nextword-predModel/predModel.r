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

# Function that parse the text entered
parse.text <- function(text) {
  # unlist the text
  curr.text <- unlist(str_split(text, " "))
  # remove "" from list
  curr.text <- curr.text[curr.text != ""]
  return(curr.text)
}

# Function that clean text entered 
clean.text <- function(text) {
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
filter.text <- function(text) {
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
get.default <- function(text) {
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

# function get the word uniram frequancy  
get.word <- function(text) {
  if (text != " ") 
  { 
    # call pars to unlist text
    words <- parse_text(tolower(text))
    # get list of words count
    num_words <- length(words)
    # Filter words
    if (num_words > 0) 
    {
      # creat regualar expression start with each words
      filter <- paste("^", words[num_words], sep = "")
      # check unigrame frquancy 
      tmp_dt <- unigram.dt[n0 %like% filter]
      # get uni gram word frequancy
      pred_word <- dim(tmp_dt)[1]
      # if frequacy > 0
      if (pred_word > 0) 
      {
        # sort words list decendingly
        tmp_dt <- tmp_dt[order(rank(-freq))]
        # get n0 of pred words
        pred <- tmp_dt[1]$n0
        if (num_words > 2) 
        { 
          tmp_w <- paste(words[1])
          for (i in 2:(num_words - 1)) tmp_w <- paste(tmp_w, words[i])
          return(paste(tmp_w, filter_text(pred)))
        } 
        else if (num_words > 1) 
        {
          tmp_w <- paste(words[1])
          return(paste(tmp_w, filter_text(pred)))
        }
      }
    }
  }
  return(text)
}

# function predect the next word from entered text
get.pred <- function(text) {
  if (text != " ") 
  { 
    # lets first clean text entered
    clean.input.text <- parse.text(clean.text(text))
    len <- length(clean.input.text)
    
    # get 1st and last words
    if (len > 1) {
      word.1 <- clean.input.text[len]
      word.2 <- clean.input.text[len - 1]
    } else if (len > 0) {
      word.1 <- clean.input.text[len]
      word.2 <- "NA"
    } else return("the")
    
    tri.len <- length(trigram.dt[trigram.dt[n2 == word.2 & n1 == word.1]]$freq)
    bi.len <- length(bigram.dt[bigram.dt[n1 == word.1]]$freq)
    matches <- matrix(nrow = tri.len + bi.len, ncol = 2)
    matches[,1] <- ""
    matches[,2] <- 0
    
    # set some propaplities for each n-grame
    prop.1 <- .95 # for unigram
    prop.2 <- .04 # for bi-grams
    prop.3 <- .01 # for tri grams
    
    if (tri.len > 0) {
      for (i in 1:tri.len) {
        matches[i, 1] <- trigram.dt[trigram.dt[n2 == word.2 & n1 == word.1]]$n0[i]
        cnt2 <- length(bigram.dt[bigram.dt[n1 == word.1 & n0 == matches[i, 1]]]$freq)
        cnt1 <- length(unigram.dt[unigram.dt[n0 == matches[i, 1]]]$freq)
        if (cnt2 > 0) freq2 <- bigram.dt[bigram.dt[n1 == word.1 & n0 == matches[i, 1]]]$freq else freq2 <- 0
        if (cnt1 > 0) freq1 <- unigram.dt[unigram.dt[n0 == matches[i, 1]]]$freq else freq1 <- 0
        matches[i, 2] <- trigram.dt[trigram.dt[n2 == word.2 & n1 == word.1]]$freq[i] * 
          prop.1 + freq2 * prop.2 + freq1 * prop.3     
      }
    }
    if (bi.len > 0) {
      for (i in sum(tri.len, 1):sum(tri.len, bi.len)) {
        matches[i, 1] <- bigram.dt[bigram.dt[n1 == word.1]]$n0[i - tri.len]
        cnt1 <- length(unigram.dt[unigram.dt[n0 == matches[i, 1]]]$freq)
        if (cnt1 > 0) freq1 <- unigram.dt[unigram.dt[n0 == matches[i, 1]]]$freq else freq1 <- 0
        matches[i, 2] <- bigram.dt[bigram.dt[n1 == word.1]]$freq[i - tri.len] * prop.2 + freq1 * prop.3   
      }
    }
    match_len <- length(matches[which.max(matches[,2])])
    if (match_len > 0) return(matches[which.max(matches[,2])])
    return(get.default(word.1))
  }
  return(" ")
}













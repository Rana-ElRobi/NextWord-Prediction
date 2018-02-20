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






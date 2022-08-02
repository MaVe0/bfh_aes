cat("/014")
rm(list=ls(all=TRUE))
options(scipen=999)
library(tidyverse)
library(readxl)
library(tidytext)
library(purrr)
library(stringr)
library(hunspell)
library(GGally)
library(sjmisc)
library(caret)
library(LanguageToolR)
library(data.table)
library(stringi)
library(koRpus)
library(koRpus.lang.en)
library(quanteda)
library(quanteda.textstats)

# repeat POS tagging?
rePOS <- FALSE
# repeat grammar check?
reGrammar <- FALSE
# repear redability score calculation?
reRead <- FALSE

# data <- fread("training_set_rel3.tsv") %>%
#   filter(essay_set == 7) %>%
#   select(essay,
#          idea1 = rater1_trait1, idea2 = rater2_trait1,
#          orga1 = rater1_trait2, orga2 = rater2_trait2,
#          style1 = rater1_trait3, style2 = rater2_trait3,
#          conv1 = rater1_trait4, conv2 = rater2_trait4,
#          tot1 = rater1_domain1, tot2 = rater2_domain1)




# Read and prepare data
data <- read_xlsx("training_set_rel3.xlsx", col_types = c("numeric", "numeric", "text", rep("numeric", 25))) %>%
  filter(essay_set == 7) %>%
  select(essay,
         idea1 = rater1_trait1, idea2 = rater2_trait1,
         orga1 = rater1_trait2, orga2 = rater2_trait2,
         style1 = rater1_trait3, style2 = rater2_trait3,
         conv1 = rater1_trait4, conv2 = rater2_trait4,
         tot1 = rater1_domain1, tot2 = rater2_domain1)

# Replace ’ with '
data <- mutate(data,
               essay = str_replace_all(essay, "’", "'"))

# Interrater reliability
# cor(data$tot1, data$tot2)
# cor(data$idea1, data$idea2)
# cor(data$orga1, data$orga2)
# cor(data$style1, data$style2)
# cor(data$conv1, data$conv2)
#
# kappa2(data[, c("tot1", "tot2")], weight = "unweighted")

# Tidy format for overall frequencies of scores
# data2 <- pivot_longer(
#   data = select(data, -essay),
#   cols = everything()
# ) %>%
#   mutate(
#     rater = str_sub(name, -1, -1),
#     trait = str_sub(name, 1, -2)
#   ) %>%
#   select(trait, score = value, rater)
#
# ggplot(filter(data2, trait != "tot"), aes(x = score)) +
#   geom_histogram(binwidth = 1) +
#   facet_grid(vars(rater), vars(trait))

# => use only rater1 - makes everything less complicated.
data <- select(data, essay, ends_with("1"))

# check score frequencies
frq(data$idea1)
frq(data$orga1)
frq(data$style1)
frq(data$conv1)
# => collapse classes with less than 1% samples (orga, style, conv)
data <- mutate(data,
               idea = idea1,
               orga = ifelse(orga1 == 0, 1, orga1),
               style = ifelse(style1 == 0, 1, style1),
               conv = ifelse(conv1 == 0, 1, conv1)
               ) %>%
  select(-ends_with("1"))


# add ID
data <- mutate(data, id = row_number()) %>%
  select(id, everything())

# save labels/scores separately for criteria
saveRDS(select(data, id, idea) %>% mutate(idea = factor(idea)), "ideaScores.RDS")
saveRDS(select(data, id, orga) %>% mutate(orga = factor(orga)), "orgaScores.RDS")
saveRDS(select(data, id, style) %>% mutate(style = factor(style)), "styleScores.RDS")
saveRDS(select(data, id, conv) %>% mutate(conv = factor(conv)), "convScores.RDS")

# with @Words
dataR <- data

# without @Words
dataN <- mutate(data, essay = gsub(pattern = "\\@\\w*", "", essay)) # remove @words

# tokenization ########################################################################

# tokenize words with @words
wordsR <- unnest_tokens(select(dataR, id, essay),
                        output = "word",
                        input = essay,
                        token = "words")




# tokenize words without @words
wordsN <- unnest_tokens(select(dataN, id, essay),
                        output = "word",
                        input = essay,
                        token = "words")

# number of words (total and unique; with @words)
nWord <- wordsR %>%
  group_by(id) %>%
  mutate(nWord = n()) %>%
  unique() %>%
  mutate(nWordUnq = n()) %>%
  select(id, nWord, nWordUnq) %>%
  mutate(TTR = round(nWordUnq/nWord, 2)) %>%
  unique()

# without @words: number of words, average word length, number of characters, % of short words, % of long words
characters <- wordsN %>%
  mutate(wordLength = nchar(word),
         nChar = nchar(word),
         tile20 = ntile(nChar, 20),
         shortWord = tile20 == 1,
         longWord = tile20 == 20) %>%
  group_by(id) %>%
  summarise(nWordN = n(),
            nCharWord = round(mean(wordLength), 2),
            nCharTotal = sum(nChar),
            pShortWord = round(100*sum(shortWord)/nWordN, 2),
            pLongWord = round(sum(100*longWord)/nWordN, 2)
            )

# number of sentences, number of characters per sentence, & of long and short sentences => JOIN TO FEATURES
sentences <- unnest_tokens(select(dataN, id, essay),
                           output = "sentence",
                           input = essay,
                           token = "sentences") %>%
  mutate(sentence2 = str_remove_all(sentence, " "),
         nCharSent = nchar(sentence2),
         tile10 = ntile(nCharSent, 10),
         shortSent = tile10 == 1,
         longSent = tile10 == 10) %>%
  group_by(id) %>%
  summarise(nSentence = n(),
            mCharSentence = round(mean(nCharSent), 2),
            pShortSentence = round(100*sum(shortSent)/nSentence, 2),
            pLongSentence = round(100*sum(longSent)/nSentence, 2))

# stop words removal ###########################################################################

# remove stop words
# View(stop_words)
# which stop words dictionary to use? all lexica remove 72% of words
stop_words_sb <- stop_words %>%
  filter(lexicon == "snowball")

# based on unique words without stop words: average word length, word count, % of short and long words
nWordNSunq <- wordsN %>%
  filter(!word %in% stop_words_sb$word) %>%
  mutate(wordLength = nchar(word),
         tile20 = ntile(wordLength, 20),
         shortWord = tile20 == 1,
         longWord = tile20 == 20) %>%
  unique() %>%
  group_by(id) %>%
  summarise(nWordUnqNS = n(),
            mCharUnqWord = round(mean(wordLength), 2),
            nCharUnqTotal = sum(wordLength),
            nShortUnqWordNS = sum(shortWord),
            nLongUnqWordNS = sum(longWord))

# without stop words: average word length, word count, % of short and long words
nWordNS <- wordsN %>%
  filter(!word %in% stop_words$word) %>%
  mutate(wordLength = nchar(word),
         tile20 = ntile(wordLength, 20),
         shortWord = tile20 == 1,
         longWord = tile20 == 20) %>%
  group_by(id) %>%
  summarise(nWordNS = n(),
            mCharWordNS = round(mean(wordLength), 2),
            nCharTotalNS = sum(wordLength),
            nShortWordNS = sum(shortWord),
            nLongWordNS = sum(longWord)) %>%
  ungroup() %>%
  # join the same thing based on unique words
  left_join(nWordNSunq, by = "id")

# join to df containing word numbers with stop words and characters df => JOIN TO FEATURES
words <- left_join(nWord, characters, by = "id") %>%
  left_join(nWordNS, by = "id") %>%
  mutate(TTRNS = round(nWordUnqNS/nWordNS, 2))

# spell checking ###########################################################################
spellCheck <- wordsN %>%
  mutate(correct = hunspell_check(word))

# percentage of Inc words => JOIN TO FEATURES
nInc <- spellCheck %>%
  group_by(id) %>%
  summarise(nIncWords = sum(!correct))

# replace Inc words with correct ones for other analyses?
# IncWords <- filter(spellCheck, !correct) %>%
#   mutate(suggestion = "")
# for(i in 1:nrow(IncWords)) {
#   IncWords[i, "suggestion"] <- hunspell_suggest(pull(IncWords[i, "word"]))[[1]][1]
# }
# suggestions <- hunspell_suggest(IncWords$word)
# unnest(suggestions)

# Grammar Check ########################################################################################################################

if(reGrammar) {
  # tokenize raw essays
  test <- unnest_tokens(select(data, id, essay),
                        output = "sentence",
                        input = essay,
                        token = "sentences",
                        to_lower = FALSE) %>%
    mutate(nChar = nchar(sentence),
           nWord = str_count(sentence, '\\w+')) %>%
    arrange(desc(nWord))

  # Function splitting extremely long sentences
  splitInParts <- function(string, size){
    pat <- paste0('(?<=.{',size,'})')
    strsplit(string, pat, perl=TRUE)
  }

  # extract very long sentences and split them up while keeping id
  longSent <- test %>%
    filter(nChar > 250)

  longSentSplit <- data.frame(NULL)
  for (i in 1:nrow(longSent)) {
    text <- pull(longSent[i, "sentence"])
    split <- splitInParts(text, 250)
    df <- data.frame(sentence = split[[1]])
    df$id <- pull(longSent[i, "id"])
    longSentSplit <- rbind(longSentSplit, df)
  }

  # remove long sentences from main df and rbind split sentences
  test2 <- test %>%
    filter(nChar <= 250) %>%
    select(id, sentence) %>%
    rbind(longSentSplit) %>%
    mutate(nChar = nchar(sentence),
           nWord = str_count(sentence, '\\w+')) %>%
    arrange(desc(nChar))

  # Loop checking each sentence; write errors if languagetools doesn't work
  grammar <- data.frame(NULL)
  for(i in 1:nrow(test2)) {
    # get id
    id <- pull(test2[i, "id"])
    # pull sentence
    text <- pull(test2[i, "sentence"])

    # check if languagetool works - if not rbind error message
    check <- tryCatch({
      LanguageToolR::languagetool(text)
    }, warning = function(w) {
      print("warning")
    }, error = function(e) {

      split <- splitInParts(testText, 50)[[1]]
      check <- data.frame(NULL)
      for(ii in length(split)) {
        x <- LanguageToolR::languagetool(split[ii])
        check <- rbind(split, x)
      }
      return(check)

    }, finally = {
    })

    # add id
    check$id <- id
    # rbind to all language tool checks
    grammar <- rbind(grammar, check)
  }

  # Save grammar check
  saveRDS(grammar, "grammar_check_2.RDS")
}

# Create grammar fetaures => JOIN TO FEATURES!
grammar <- readRDS("grammar_check_2.RDS") %>%
  filter(rule_category_id %in% c("CASING", "CONFUSED_WORDS", "GRAMMAR", "PUNCTUATION", "TYPOGRAPHY", "TYPOS")) %>%
  group_by(id, rule_category_id) %>%
  mutate(n = n()) %>%
  select(id, rule_category_id, n) %>%
  unique() %>%
  arrange(id) %>%
  pivot_wider(id_cols = id, names_from = rule_category_id, values_from = n) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
  # relative to nWords
  left_join(nWord, by = "id") %>%
  mutate(
    pIncWord2 = round(TYPOS/nWord, 5),
    pIncCase = round(CASING/nWord, 5),
    pIncConf = round(CONFUSED_WORDS/nWord, 5),
    pIncGrammar = round(CONFUSED_WORDS/nWord, 5),
    pIncPunct = round(PUNCTUATION/nWord, 5),
    pIncTypog = round(TYPOGRAPHY/nWord, 5)
  ) %>%
  select(id, starts_with("pI"))

# POS tagging ##########################################################################################################################
if(rePOS) {
  m_eng <- udpipe::udpipe_download_model(language = "english-ewt")
  m_eng <- udpipe::udpipe_load_model(m_eng)

  text <- pull(data[11,2])
  text_anndf <- udpipe::udpipe_annotate(m_eng, x = text) %>%
    as.data.frame() %>%
    dplyr::select(-sentence)

  pos <- udpipe::udpipe_annotate(m_eng, x = data$essay)
  posDF <- as.data.frame(pos)

  saveRDS(posDF, "posTagging.RDS")
}
posDF <- readRDS("posTagging.RDS")

# number of commas => JOIN TO FEATURES
nComma <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  group_by(id) %>%
  summarise(nComma = sum(token == ","))

# number of unique lemmatized words => JOIN TO FEATURES
nWordUnqLem <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  filter(upos != "PUNCT") %>%
  select(id, lemma) %>%
  filter(!lemma %in% stop_words$word) %>%
  unique() %>%
  group_by(id) %>%
  summarise(nWordUnqLem = n())

# share of tags by essay => JOIN TO FEATURES
posShares <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  select(id, tag = upos) %>%
  ungroup() %>%
  group_by(id, tag) %>%
  summarise(n = n()) %>%
  mutate(sum = sum(n),
         percTag = round(100 * n/sum, 1)) %>%
  ungroup() %>%
  select(id, tag, percTag) %>%
  pivot_wider(id_cols = id, names_from = tag, values_from = percTag) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
  mutate(nPOStags = rowSums(select(., -id) > 0))

# Lexical Densities ##########################################################################################

# Lexical density
lexDens <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  filter(upos %in% c("NOUN", "ADJ", "VERB", "ADV")) %>%
  group_by(id) %>%
  summarise(nLexItems = n()) %>%
  left_join(nWord, by = "id") %>%
  mutate(lexDens = round(nLexItems/nWord, 2)) %>%
  select(id, lexDens)

# get share of non-stop-words as defined by other lexica (SMART, onix)
stop_words_SMART <- stop_words %>%
  filter(lexicon == "SMART")

smartDens <- wordsN %>%
  filter(!word %in% stop_words_SMART$word) %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  left_join(nWord, by = "id") %>%
  mutate(smartDens = round(n/nWord, 2)) %>%
  select(id, smartDens)

stop_words_onix <- stop_words %>%
  filter(lexicon == "onix")

onixDens <- wordsN %>%
  filter(!word %in% stop_words_onix$word) %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  left_join(nWord, by = "id") %>%
  mutate(onixDens = round(n/nWord, 2)) %>%
  select(id, onixDens)

# join densities => JOIN TO FEATURES
densities <- lexDens %>%
  left_join(smartDens, by = "id") %>%
  left_join(onixDens, by = "id")

# sentiments #######################################################################
nrc <- get_sentiments("nrc")

# => JOIN TO FEATURES!
sentiments <- wordsN %>%
  left_join(nrc, by = "word") %>%
  group_by(id, sentiment) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = id, names_from = sentiment, values_from = n) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
  left_join(nWord, by = "id") %>%
  mutate(
    angerScore = round(100*anger/nWord, 2),
    anticScore = round(100*anticipation/nWord, 2),
    disgustScore = round(100*disgust/nWord, 2),
    fearScore = round(100*fear/nWord, 2),
    joyScore = round(100*joy/nWord, 2),
    negativeScore = round(100*negative/nWord, 2),
    positiveScore = round(100*positive/nWord, 2),
    sadScore = round(100*sadness/nWord, 2),
    surpScore = round(100*surprise/nWord, 2),
    trustScore = round(100*trust/nWord, 2)
  ) %>%
  select(id, ends_with("Score")) %>%
  mutate(
    sumEmotions = rowSums(select(., ends_with("Score"))),
    nEmotions = rowSums(select(., ends_with("Score")) > 0)
  )

##########################################################################################
# KoRpus package?
# Unclear how to identify texts after taggin multiple texts => tagging within loops

# koRpus tagging function
tagEssay <- function(x) {
  taggedText <- treetag(
    x,
    format = "obj",
    treetagger="manual",
    lang="en",
    TT.options=list(
      path="C:/TreeTagger",
      preset="en"
    )
  )
}

# Readability ############################################################################

if(reRead) {

  # get empty df with readability index names
  rInd <- readability(tagEssay(pull(data[1:5, "essay"])))
  rInd <- data.frame(
    index = names(rInd[1:31]),
    value = rInd[1:31]
  ) %>%
    mutate(subID = 1) %>%
    pivot_wider(subID, names_from = index, values_from = value) %>%
    filter(subID == 999)

  # loop extracting readability indices
  essays <- data$essay

  for(i in 1:length(essays)){
    taggedEssay <- tagEssay(pull(data[i, "essay"]))
    x <- readability(taggedEssay)
    x <- data.frame(
      index = names(x[1:31]),
      value = x[1:31]
    ) %>%
      mutate(subID = i) %>%
      pivot_wider(subID, names_from = index, values_from = value)
    rInd <- rbind(rInd, x)
  }

  saveRDS(rInd, "readabilityScores.RDS")

}

# JOIN TO FEATURES!
readability <-readRDS("readabilityScores.RDS") %>%
  rename(id = subID)





# Join Features ##############################################################################
features <- left_join(sentences, words, by = "id") %>%
  mutate(nWordPerSentence = round(nWord/nSentence, 0),
         nWordPerSentenceNS = round(nWordNS/nSentence, 0),
         percStopWords = round((1 - nWordNS/nWord) * 100, 1)) %>%
  left_join(nWordUnqLem, by = "id") %>%
  left_join(nInc, by = "id") %>%
  left_join(nComma, by = "id") %>%
  left_join(posShares, by = "id") %>%
  left_join(sentiments, by = "id") %>%
  left_join(densities, by = "id") %>%
  left_join(readability, by = "id") %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))


# save features
saveRDS(features, "features.RDS")








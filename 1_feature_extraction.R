cat("/014")
rm(list=ls(all=TRUE))
options(scipen=999)
library(tidyverse)
library(readxl)
library(tidylog)
library(irr)
library(tidytext)
library(purrr)
library(stringr)
library(quanteda)
library(hunspell)
library(ggplot2)
library(GGally)
library(caret)
library(textplot)
library(sjmisc)
library(skimr)
library(caret)
library(broom)
set.seed(1898)

# repeat POS tagging?
rePOS <- FALSE

# Directory for data frames and results
dfDir <- "C:/BFH/Seminararbeit/Analysen/"

# train/test fraction
ttf <- 0.7

# Read and prepare data
data <- read_xlsx("C:/BFH/NLP/kaggle_data/training_set_rel3.xlsx", col_types = c("numeric", "numeric", "text", rep("numeric", 25))) %>%
  filter(essay_set == 7) %>%
  select(essay, 
         idea1 = rater1_trait1, idea2 = rater2_trait1,
         orga1 = rater1_trait2, orga2 = rater2_trait2,
         style1 = rater1_trait3, style2 = rater2_trait3,
         conv1 = rater1_trait4, conv2 = rater2_trait4,
         tot1 = rater1_domain1, tot2 = rater2_domain1)

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
               conv = ifelse(conv1 == 0, 1, conv1)) %>%
  select(-ends_with("1"))
skim(data)

# add ID
data <- mutate(data, id = row_number()) %>%
  select(id, everything())

dataR <- data
# replace @words with valid word group [NOT SATISFYING ATM]
# dataR <- mutate(data,
#                essay = gsub(pattern = "\\@CAPS1", "Kevin", essay),
#                essay = gsub(pattern = "\\@CAPS2", "Bob", essay),
#                essay = gsub(pattern = "\\@CAPS3", "Cheyenne", essay),
#                essay = gsub(pattern = "\\@CAPS\\w*", "Martin", essay),
#                essay = gsub(pattern = "\\@PERSON1", "Lisa", essay),
#                essay = gsub(pattern = "\\@PERSON2", "Sputim", essay),
#                essay = gsub(pattern = "\\@PERSON3", "Mark", essay),
#                essay = gsub(pattern = "\\@PERSON4", "Anna", essay),
#                essay = gsub(pattern = "\\@ORGANIZATION\\w*", "organization", essay),
#                essay = gsub(pattern = "\\@LOCATION\\w*", "downtown", essay),
#                essay = gsub(pattern = "\\@DATE\\w*", "January 1st", essay),
#                essay = gsub(pattern = "\\@TIME\\w*", "12PM", essay),
#                essay = gsub(pattern = "\\@MONEY\\w*", "5$", essay),
#                essay = gsub(pattern = "\\@PERCENT\\w*", "20%", essay),
#                essay = gsub(pattern = "\\@NUM\\w*", "1898", essay)
#                )

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

# count all words and number of unique words (with @words) 
nWords <- wordsR %>%
  group_by(id) %>%
  mutate(nWords = n()) %>%
  unique() %>%
  mutate(nWordsUnq = n()) %>%
  select(id, nWords, nWordsUnq) %>%
  unique()

# average word length and total number of character (without @words); proportion of long and short words
characters <- wordsN %>%
  mutate(
    wordLength = nchar(word),
    nChar = nchar(word),
    tile20 = ntile(nChar, 20),
    shortWord = tile20 == 1,
    longWord = tile20 == 20) %>%
  group_by(id) %>%
  summarise(avgWL = round(mean(wordLength), 2),
            nWord = n(),
            nCharWord = sum(nChar),
            pShortWord = round(100*sum(shortWord)/nWord, 2),
            pLongWord = round(sum(100*longWord)/nWord, 2)
            )

# number of sentences, number of characters per sentence, proportion of long and short sentences => JOIN TO FEATURES
sentences <- unnest_tokens(select(dataN, id, essay),
                           output = "sentence",
                           input = essay,
                           token = "sentences") %>%
  mutate(
    sentence2 = str_remove_all(sentence, " "),
    nCharSent = nchar(sentence2),
    tile10 = ntile(nCharSent, 10),
    shortSent = tile10 == 1,
    longSent = tile10 == 10,
  ) %>%
  group_by(id) %>%
  summarise(nSentence = n(),
            pShortSentence = round(100*sum(shortSent)/nSentence, 2),
            pLongSentence = round(100*sum(longSent)/nSentence, 2)
  )

# stop words removal ###########################################################################

# remove stop words
# View(stop_words)
# which stop words dictionary to use? all lexica remove 72% of words
stop_words <- stop_words %>%
  filter(lexicon == "snowball")

# filter stop words in SNOWBALL & count words and number of unique words without stop and @words
nWordsNSunq <- wordsN %>%
  filter(!word %in% stop_words$word) %>%
  mutate(wordLength = nchar(word),
         tile20 = ntile(wordLength, 20),
         shortWord = tile20 == 1,
         longWord = tile20 == 20) %>%
  unique() %>%
  group_by(id) %>%
  summarise(
    avgUnqWLNS = round(mean(wordLength), 2),
    nWordsUnqNS = n(),
    nShortUnqWordsNS = sum(shortWord),
    nLongUnqWordsNS = sum(longWord)
    )

nWordsNS <- wordsN %>%
  filter(!word %in% stop_words$word) %>%
  mutate(wordLength = nchar(word),
         tile20 = ntile(wordLength, 20),
         shortWord = tile20 == 1,
         longWord = tile20 == 20) %>%
  group_by(id) %>%
  summarise(
    avgWLNS = round(mean(wordLength), 2),
    nWordsNS = n(),
    nShortWordsNS = sum(shortWord),
    nLongWordsNS = sum(longWord)
    ) %>%
  left_join(nWordsNSunq, by = "id")

# join to df containing word numbers with stop words => JOIN TO FEATURES
words <- left_join(nWords, nWordsNS, by = "id")

# average word length and total number of characters (without @words and stopwords)
charactersNS <- wordsNS %>%
  mutate(wordLength = nchar(word),
         nChar = nchar(word)) %>%
  group_by(id) %>%
  summarise(avgWLNS = round(mean(wordLength), 2),
            nCharNS = sum(nChar))

# join to df containing lengths and character numbers including stopwords => JOIN TO FEATURES
characters <- left_join(characters, charactersNS, by = "id")



# spell checking ###########################################################################
spellCheck <- wordsN %>%
  mutate(correct = hunspell_check(word))

# percentage of incorrect words => JOIN TO FEATURES
nIncorrect <- spellCheck %>% 
  group_by(id) %>%
  summarise(nIncorrectWords = sum(!correct))

# calculate these when joining features
  mutate(percIncWords = round(100*nIncorrectWords/nWordsN, 1)) %>%
 
# replace incorrect words with correct ones for other analyses?
# incorrectWords <- filter(spellCheck, !correct) %>%
#   mutate(suggestion = "")
# for(i in 1:nrow(incorrectWords)) {
#   incorrectWords[i, "suggestion"] <- hunspell_suggest(pull(incorrectWords[i, "word"]))[[1]][1]
# }
# suggestions <- hunspell_suggest(incorrectWords$word)
# unnest(suggestions)

# POS tagging ##########################################################################################################################
if(rePos) {
  m_eng <- udpipe::udpipe_download_model(language = "english-ewt")
  m_eng <- udpipe::udpipe_load_model(m_eng)
  
  text <- pull(data[11,2])
  text_anndf <- udpipe::udpipe_annotate(m_eng, x = text) %>%
    as.data.frame() %>%
    dplyr::select(-sentence)
  
  pos <- udpipe::udpipe_annotate(m_eng, x = data$essay) 
  posDF <- as.data.frame(pos)
  
  saveRDS(posDF, paste0(dfDir, "posTagging.RDS"))
}

posDF <- readRDS(paste0(dfDir, "posTagging.RDS"))

# number of commas => JOIN TO FEATURES
nCommas <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  group_by(id) %>%
  summarise(nCommas = sum(token == ","))

# number of unique lemmatized words
nWordsUnqLem <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  filter(upos != "PUNCT") %>%
  select(id, lemma) %>%
  filter(!lemma %in% stop_words$word) %>%
  unique() %>%
  group_by(id) %>%
  summarise(nWordsUnqLem = n())

# share of tags by essay => JOIN TO FEATURES
posShares <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  select(id, tag = upos) %>%
  ungroup() %>%
  group_by(id, tag) %>%
  summarise(n = n()) %>%
  mutate(sum = sum(n),
         percTag = round(100 * n/sum, 1)) %>%
  select(id, tag, percTag) %>%
  pivot_wider(id_cols = id, names_from = tag, values_from = percTag) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
















# Test-Train-Split ##############################################################################
features <- left_join(sentences, words, by = "id") %>%
  mutate(nWordsSentence = round(nWords/nSentences, 0),
         nWordsSentenceNS = round(nWordsNS/nSentences, 0),
         percStopWords = round((1 - nWordsNS/nWords) * 100, 1)) %>%
  left_join(characters, by = "id") %>%
  left_join(posShares, by = "id")

# sample rows for splitting
trainRows <- sample(1:nrow(features), round(ttf*nrow(features), 0))
train <- features[trainRows,]
test <- setdiff(features, train)

# IDEA
train_idea <- left_join(features, select(data, id, idea), by = "id") %>%
  select(-id)

idea_model <- train(idea ~., data = train_idea, method = "svmLinear3")
idea_test <- test
p <- predict(idea_model, idea_test)


idea_test$p <- p 
idea_test <- left_join(idea_test, select(data, id, idea), by = "id") %>%
  mutate(p = round(p, 0),
         correct = idea == p) 

frq(idea_test$correct
    )









# EDA: Continuous Variables ####
summary(data)
# skewness function
skewness <-  function(x) {
  m3 <- mean((x - mean(x))^3)
  skewness <- m3/(sd(x)^3)
  round(skewness, 2)
}

# histogram function
hg <- function (colName, binwidth = NULL) {
  
  medianData = round(median(pull(data, {{colName}})), 2)
  meanData = round(mean(pull(data, {{colName}})), 2)
  
  ggplot(data = data,
         aes(x = {{colName}})) +
    geom_histogram(binwidth = binwidth) +
    geom_vline(xintercept = medianData, colour = 'blue', size = 1) +
    annotate(geom = "text", x = 0.8 * medianData, y = -5, label = medianData, color = "blue", size = 3) +
    geom_vline(xintercept = meanData, colour = 'red', size = 1) +
    annotate(geom = "text", x = 1.2 * meanData, y = -5, label = meanData, color = "red", size = 3) +
    labs(caption = paste0("Skewness = ", skewness(pull(data, {{colName}}))))
}

hg(nWordsTok)
hg(nSentences)
hg(nWordSentence)
hg(nIncorrect)
hg(pIncorrect)
hg(nWordsNoStop)
hg(pWordsNoStop)
hg(pIncorrectNoStop)

# stemming
library(SnowballC)
wordStem("took")




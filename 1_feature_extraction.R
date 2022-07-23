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

# repeat POS tagging?
rePOS <- FALSE

# Read and prepare data
data <- read_xlsx("training_set_rel3.xlsx", col_types = c("numeric", "numeric", "text", rep("numeric", 25))) %>%
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
stop_words <- stop_words %>%
  filter(lexicon == "snowball")

# based on unique words without stop words: average word length, word count, % of short and long words
nWordNSunq <- wordsN %>%
  filter(!word %in% stop_words$word) %>%
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
  # join the same thing based on unique words
  left_join(nWordNSunq, by = "id")

# join to df containing word numbers with stop words and characters df => JOIN TO FEATURES
words <- left_join(nWord, characters, by = "id") %>%
  left_join(nWordNS, by = "id")

# spell checking ###########################################################################
spellCheck <- wordsN %>%
  mutate(correct = hunspell_check(word))

# percentage of incorrect words => JOIN TO FEATURES
nIncorrect <- spellCheck %>%
  group_by(id) %>%
  summarise(nIncorrectWords = sum(!correct))

# replace incorrect words with correct ones for other analyses?
# incorrectWords <- filter(spellCheck, !correct) %>%
#   mutate(suggestion = "")
# for(i in 1:nrow(incorrectWords)) {
#   incorrectWords[i, "suggestion"] <- hunspell_suggest(pull(incorrectWords[i, "word"]))[[1]][1]
# }
# suggestions <- hunspell_suggest(incorrectWords$word)
# unnest(suggestions)

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
  select(id, tag, percTag) %>%
  pivot_wider(id_cols = id, names_from = tag, values_from = percTag) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

# Join Features ##############################################################################
features <- left_join(sentences, words, by = "id") %>%
  mutate(nWordPerSentence = round(nWord/nSentence, 0),
         nWordPerSentenceNS = round(nWordNS/nSentence, 0),
         percStopWords = round((1 - nWordNS/nWord) * 100, 1)) %>%
  left_join(nWordUnqLem, by = "id") %>%
  left_join(nComma, by = "id") %>%
  left_join(posShares, by = "id")

# save features
saveRDS(features, "features.RDS")








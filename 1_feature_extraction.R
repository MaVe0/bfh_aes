cat("/014")
rm(list=ls(all=TRUE))
options(scipen=999)
library(readxl)
library(tidytext)
library(tidyverse)
library(purrr)
library(stringr)
library(hunspell)
library(sjmisc)
library(LanguageToolR)
library(data.table)
library(koRpus)
library(koRpus.lang.en)
library(SnowballC)
library(text2vec)
library(dplyr)
library(ggthemr)



# repeat POS tagging?
rePOS <- FALSE
# repeat grammar check?
reGrammar <- FALSE
# repear redability score calculation?
reRead <- FALSE
# repeat tree parsing?
reTreeParse <- FALSE

ggthemr("fresh")

################################################################################
################################################################################

# PREPARE RATINGS ##############################################################

# Read data
data <- read_xlsx("training_set_rel3.xlsx",
                  col_types = c("numeric", "numeric", "text",
                                rep("numeric", 25))) %>%
  # filter for relevant essay set
  filter(essay_set == 7) %>%
  select(essay,
         idea1 = rater2_trait1, orga1 = rater2_trait2, style1 = rater2_trait3,
         conv1 = rater2_trait4, tot1 = rater2_domain1) %>%
  # Replace ’ with '
  mutate(essay = str_replace_all(essay, "’", "'")) %>%
  # Collapse low ratings (1&2) to one class in all criteria
  mutate(
         idea = ifelse(idea1 == 0, 1, idea1),
         orga = ifelse(orga1 == 0, 1, orga1),
         style = ifelse(style1 == 0, 1, style1),
         conv = ifelse(conv1 == 0, 1, conv1)
  ) %>%
  # keep newly created classes only
  select(-ends_with("1")) %>%
  # add identificators to first column
  mutate(id = row_number()) %>%
  select(id, everything())

# remove case 1065 (only stop words and incorrect words)
data <- filter(data,
               id != 1065) %>%
  mutate(id = row_number())

# explore rating distributions
plot1 <- pivot_longer(data,
             cols = c(idea, orga, style, conv),
             names_to = "criteria",
             values_to = "score"
) %>%
  mutate(criteria = case_when(
    criteria == "conv" ~ "Sprachrichtigkeit",
    criteria == "style" ~ "Sprachangemessenheit",
    criteria == "orga" ~ "Textaufbau",
    criteria == "idea" ~ "Inhalt",
    TRUE ~ as.character(NA)
  ),
  Punkte = score - 1) %>%
  ggplot(aes(x = Punkte)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~criteria) +
  scale_y_continuous(name = "Anzahl Aufsätze")
saveRDS(plot1, "plot1")



# save labels/scores separately for criteria
ideaScores <- select(data, id, idea) %>% mutate(idea = factor(idea))
saveRDS(ideaScores, "ideaScores.RDS")
orgaScores <- select(data, id, orga) %>% mutate(orga = factor(orga))
saveRDS(orgaScores, "orgaScores.RDS")
styleScores <- select(data, id, style) %>% mutate(style = factor(style))
saveRDS(styleScores, "styleScores.RDS")
convScores <- select(data, id, conv) %>% mutate(conv = factor(conv))
saveRDS(convScores, "convScores.RDS")
totalScores <- mutate(data, across(-c(id, essay), ~ . - 1)) %>%
  mutate(total = rowSums(select(., idea, orga, style, conv))) %>%
  select(id, total)
saveRDS(totalScores, "totalScores.RDS")


# tidy scores df for EDA plots
scores <- data %>%
  select(-essay) %>%
  pivot_longer(cols = c(idea, orga, style, conv), names_to = "criteria", values_to = "score") %>%
  mutate(criteria = case_when(
    criteria == "conv" ~ "Sprachrichtigkeit",
    criteria == "style" ~ "Sprachangemessenheit",
    criteria == "orga" ~ "Textaufbau",
    criteria == "idea" ~ "Inhalt",
    TRUE ~ as.character(NA)
  ),
  Punkte = as.factor(score - 1),
  Punkte2 = ifelse(Punkte == 2, 1, 0),
  Punkte0 = ifelse(Punkte == 0, 1, 0),
  )

# with @Words
dataR <- data

# without @Words
dataN <- mutate(data, essay = gsub(pattern = "\\@\\w*", "", essay)) # remove @words

# eda plot function ####
edaPlot <- function(df, var, yName) {
  s <- scores %>%
    left_join(get(df), by = "id") %>%
    mutate(Punkte = as.numeric(Punkte))

  cors <- c(
    round(cor(pull(s[s$criteria == "Inhalt", "Punkte"]), pull(s[s$criteria == "Inhalt", var])), 2),
    round(cor(pull(s[s$criteria == "Inhalt", "Punkte0"]), pull(s[s$criteria == "Inhalt", var])), 2),
    round(cor(pull(s[s$criteria == "Inhalt", "Punkte2"]), pull(s[s$criteria == "Inhalt", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachangemessenheit", "Punkte"]), pull(s[s$criteria == "Sprachangemessenheit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachangemessenheit", "Punkte0"]), pull(s[s$criteria == "Sprachangemessenheit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachangemessenheit", "Punkte2"]), pull(s[s$criteria == "Sprachangemessenheit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachrichtigkeit", "Punkte"]), pull(s[s$criteria == "Sprachrichtigkeit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachrichtigkeit", "Punkte0"]), pull(s[s$criteria == "Sprachrichtigkeit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachrichtigkeit", "Punkte2"]), pull(s[s$criteria == "Sprachrichtigkeit", var])), 2),
    round(cor(pull(s[s$criteria == "Textaufbau", "Punkte"]), pull(s[s$criteria == "Textaufbau", var])), 2),
    round(cor(pull(s[s$criteria == "Textaufbau", "Punkte0"]), pull(s[s$criteria == "Textaufbau", var])), 2),
    round(cor(pull(s[s$criteria == "Textaufbau", "Punkte2"]), pull(s[s$criteria == "Textaufbau", var])), 2)
  )
  names(cors) <- c("idea", "idea0", "idea2", "style", "style0", "style2", "conv", "conv0", "conv2", "orga", "orga0", "orga2")

  cordf <- data.frame(
    y = rep(1.05*max(s[, var]), 4),
    Punkte = as.factor(rep(1, 4)),
    r = cors[c(1, 4, 7, 10)],
    r0 = cors[c(2, 5, 8, 11)],
    r2 = cors[c(3, 6, 9, 12)],
    criteria = c("Inhalt", "Sprachangemessenheit", "Sprachrichtigkeit", "Textaufbau")
  ) %>%
    rename(!!var := y) %>%
    mutate(lab = paste0("r=",r,"; rp0=",r0,"; rp2=",r2))


  plot <- scores %>%
    left_join(get(df), by = "id") %>%
    ggplot(aes(x = Punkte, y = get(var), fill = Punkte, color = Punkte)) +
    geom_jitter(alpha = 0.2, width = 0.25, size = 1) +
    geom_violin(width = 0.75, fill = NA, color = "black") +
    geom_boxplot(width = 0.1, color = "black", fill = NA) +
    scale_y_continuous(name = yName) +
    theme(
      legend.position = "none",
      plot.caption=element_text(hjust = 0, color = "#545454", size = 8)
    ) +
    facet_wrap(~criteria) +
    geom_text(data = cordf[1,], label = cordf[1, 7], color = "#545454", size = 2.5, parse = FALSE) +
    geom_text(data = cordf[2,], label = cordf[2, 7], color = "#545454", size = 2.5, parse = FALSE) +
    geom_text(data = cordf[3,], label = cordf[3, 7], color = "#545454", size = 2.5, parse = FALSE) +
    geom_text(data = cordf[4,], label = cordf[4, 7], color = "#545454", size = 2.5, parse = FALSE) +
    labs(caption = "Anmerkungen: r: Korrelation zwischen Prädiktor und Punkten.\nrp0 & rp2: Punktbiseriale Korrelation zw. Prädiktor und binärer Variable, die Aufsätze mit 0 bzw. 2 Punkten markiert.")

  return(plot)
}

# eda plot function ####
edaPlot2 <- function(df, var, yName) {
  s <- scores %>%
    left_join(get(df), by = "id") %>%
    mutate(Punkte = as.numeric(Punkte))

  cors <- c(
    round(cor(pull(s[s$criteria == "Inhalt", "Punkte"]), pull(s[s$criteria == "Inhalt", var])), 2),
    round(cor(pull(s[s$criteria == "Inhalt", "Punkte0"]), pull(s[s$criteria == "Inhalt", var])), 2),
    round(cor(pull(s[s$criteria == "Inhalt", "Punkte2"]), pull(s[s$criteria == "Inhalt", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachangemessenheit", "Punkte"]), pull(s[s$criteria == "Sprachangemessenheit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachangemessenheit", "Punkte0"]), pull(s[s$criteria == "Sprachangemessenheit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachangemessenheit", "Punkte2"]), pull(s[s$criteria == "Sprachangemessenheit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachrichtigkeit", "Punkte"]), pull(s[s$criteria == "Sprachrichtigkeit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachrichtigkeit", "Punkte0"]), pull(s[s$criteria == "Sprachrichtigkeit", var])), 2),
    round(cor(pull(s[s$criteria == "Sprachrichtigkeit", "Punkte2"]), pull(s[s$criteria == "Sprachrichtigkeit", var])), 2),
    round(cor(pull(s[s$criteria == "Textaufbau", "Punkte"]), pull(s[s$criteria == "Textaufbau", var])), 2),
    round(cor(pull(s[s$criteria == "Textaufbau", "Punkte0"]), pull(s[s$criteria == "Textaufbau", var])), 2),
    round(cor(pull(s[s$criteria == "Textaufbau", "Punkte2"]), pull(s[s$criteria == "Textaufbau", var])), 2)
  )
  names(cors) <- c("idea", "idea0", "idea2", "style", "style0", "style2", "conv", "conv0", "conv2", "orga", "orga0", "orga2")

  cordf <- data.frame(
    y = rep(1.05*max(s[, var]), 4),
    Punkte = as.factor(rep(1, 4)),
    r = cors[c(1, 4, 7, 10)],
    r0 = cors[c(2, 5, 8, 11)],
    r2 = cors[c(3, 6, 9, 12)],
    criteria = c("Inhalt", "Sprachangemessenheit", "Sprachrichtigkeit", "Textaufbau")
  ) %>%
    rename(!!var := y) %>%
    mutate(lab = paste0("r=",r,"; rp0=",r0,"; rp2=",r2))


  plot <- scores %>%
    left_join(get(df), by = "id") %>%
    ggplot(aes(x = Punkte, y = get(var), fill = Punkte, color = Punkte)) +
    geom_jitter(alpha = 0.2, width = 0.25, size = 1) +
    geom_violin(width = 0.75, fill = NA, color = "black") +
    geom_boxplot(width = 0.1, color = "black", fill = NA) +
    scale_y_continuous(name = yName, trans = 'log2') +
    theme(
      legend.position = "none",
      plot.caption=element_text(hjust = 0, color = "#545454", size = 8)
    ) +
    facet_wrap(~criteria) +
    geom_text(data = cordf[1,], label = cordf[1, 7], color = "#545454", size = 2.5, parse = FALSE) +
    geom_text(data = cordf[2,], label = cordf[2, 7], color = "#545454", size = 2.5, parse = FALSE) +
    geom_text(data = cordf[3,], label = cordf[3, 7], color = "#545454", size = 2.5, parse = FALSE) +
    geom_text(data = cordf[4,], label = cordf[4, 7], color = "#545454", size = 2.5, parse = FALSE) +
    labs(caption = "Anmerkungen: r: Korrelation zwischen Prädiktor und Punkten.\nrp0 & rp2: Punktbiseriale Korrelation zw. Prädiktor und binärer Variable, die Aufsätze mit 0 bzw. 2 Punkten markiert.")

  return(plot)
}

# TOKENIZATION #################################################################

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

# with @words: total and unique number of words and type-token-ration (TTR)
nWord <- wordsR %>%
  group_by(id) %>%
  mutate(nWord = n()) %>%
  unique() %>%
  mutate(nWordUnq = n()) %>%
  select(id, nWord, nWordUnq) %>%
  mutate(
    TTR = round(nWordUnq/nWord, 2)) %>%
  unique()

# without @words: average word length, number of characters per word,
# % of short words, % of long words, number of words
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
            ) %>%
  select(-nWordN)

# number of sentences, characters per sentence, long and short sentences
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
            nShortSentence = sum(shortSent),
            nLongSentence = sum(longSent),
            pShortSentence = round(100*sum(shortSent)/nSentence, 2),
            pLongSentence = round(100*sum(longSent)/nSentence, 2),
            bShortSentence = ifelse(nShortSentence > 0, 1, 0),
            bLongSentence = ifelse(nLongSentence > 0, 1, 0))

# remove stop words
# View(stop_words)
# which stop words dictionary to use? all lexica remove 72% of words
stop_words_sb <- stop_words %>%
  filter(lexicon == "snowball")

# based on unique words without stop words:
# average word length, word count, % of short and long words
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
  ungroup()

# join all features created with tokens and calculate words per sentence and
# percentage of stop words
features <- nWord %>%
  left_join(nWordNS, by = "id") %>%
  left_join(nWordNSunq, by = "id") %>%
  left_join(characters, by = "id") %>%
  left_join(sentences, by = "id") %>%
  mutate(nWordPerSentence = round(nWord/nSentence, 0),
         nWordPerSentenceNS = round(nWordNS/nSentence, 0),
         percStopWords = round((1 - nWordNS/nWord) * 100, 2),
         TTRNS = round(nWordUnqNS/nWordNS, 2)
  )

# PLOTS
# saveRDS(edaPlot("features", "nWord", "Anzahl Wörter"), "nWordPlot")
# edaPlot("features", "nWordUnq", "Anzahl Wörter")
# edaPlot("features", "TTR", "Anzahl Wörter")
# edaPlot("features", "TTRNS", "Anzahl Wörter")
# edaPlot("features", "pLongSentence", "Anteil langer Sätze")
# edaPlot("features", "mCharWordNS", "Mittlere Wortlänge (ohne Stoppwörter")
# edaPlot("features", "percStopWords", "Anteil Stoppwörter")
# edaPlot("features", "nCharTotal", "Anzahl Zeichen")
# edaPlot("features", "nCharTotalNS", "Anzahl Zeichen")
# edaPlot("features", "nLongWordNS", "Anzahl langer Wörter")
# edaPlot("features", "pLongWord", "Anzahl langer Wörter")
# edaPlot("features", "nWordPerSentence", "Anzahl Wörter pro Satz")

# SPELL CHECK ##################################################################
spellCheck <- wordsN %>%
  mutate(correct = hunspell_check(word))

# number and percentage of incorrectly written words
incorrectW <- spellCheck %>%
  group_by(id) %>%
  summarise(
    nWords = n(),
    nIncWords = sum(!correct),
    pIncWords = round(100*nIncWords/nWords, 2)
    ) %>%
  select(-nWords)

# join to other features
features <- left_join(features,
                      incorrectW,
                      by = "id")

edaPlot("features", "nIncWords", "Anzahl Rechtschreibefehler")
edaPlot("features", "pIncWords", "Ateil Rechtschreibefehler")

# GRAMMAR CHECK ################################################################

if(reGrammar) {
  # tokenize raw essays
  sentTok <- unnest_tokens(select(data, id, essay),
                        output = "sentence",
                        input = essay,
                        token = "sentences",
                        to_lower = FALSE) %>%
    mutate(nChar = nchar(sentence),
           nWord = str_count(sentence, '\\w+')) %>%
    # arrange(desc(nWord))
    arrange(id)

  # Function splitting extremely long sentences
  # splitInParts <- function(string, size){
  #   pat <- paste0('(?<=.{',size,'})')
  #   strsplit(string, pat, perl=TRUE)
  # }
  #
  # # extract very long sentences and split them up while keeping id
  # longSent <- test %>%
  #   filter(nChar > 250)
  #
  # longSentSplit <- data.frame(NULL)
  # for (i in 1:nrow(longSent)) {
  #   text <- pull(longSent[i, "sentence"])
  #   split <- splitInParts(text, 250)
  #   df <- data.frame(sentence = split[[1]])
  #   df$id <- pull(longSent[i, "id"])
  #   longSentSplit <- rbind(longSentSplit, df)
  # }
  #
  # # remove long sentences from main df and rbind split sentences
  # test2 <- test %>%
  #   filter(nChar <= 250) %>%
  #   select(id, sentence) %>%
  #   rbind(longSentSplit) %>%
  #   mutate(nChar = nchar(sentence),
  #          nWord = str_count(sentence, '\\w+')) %>%
  #   arrange(desc(nChar))

  # Loop checking each sentence; save in separate df (grammarError) if languagetools doesn't work
  grammar <- data.frame(NULL)
  grammarError <- data.frame(NULL)

  # languagetools output for sentences that do not work
  errorOutput <- LanguageToolR::languagetool("This is a senctence with a fake error.")


  # for(i in 1:nrow(sentTok)) {
    for(i in 11061:nrow(sentTok)) {
    # get id
    id <- pull(sentTok[i, "id"])
    # pull sentence
    text <- pull(sentTok[i, "sentence"])

    # check if languagetool works - if not rbind error message
    check <- tryCatch({
      LanguageToolR::languagetool(text)
    }, warning = function(w) {
      print("warning")
    }, error = function(e) {


      checkError <- sentTok[i,]
      checkOutput <- errorOutput

      check <- list(checkOutput, checkError)


      # split <- splitInParts(text, 200)[[1]]
      # check <- data.frame(NULL)
      # for(ii in 1:length(split)) {
      #   x <- LanguageToolR::languagetool(split[ii])
      #   check <- bind_rows(check, x) %>%
      #     mutate(incomplete = 1)
      # }



    }

    , finally = {
    })

    # if all went well add id and rbind to other checks
    if(class(check)[1] != "list") {
      check$id <- id
      grammar <- bind_rows(grammar, check)
      }

    else {
      # if error add standard language tool output to other checks and save unchecked sentence in grammar Error
      unchecked <- as.data.frame(check[[2]])
      grammarError <- bind_rows(grammarError, unchecked)

      check <- as.data.frame(check[[1]])
      check$id <- id
      grammar <- bind_rows(grammar, check)
    }
  }

  # Save grammar check
  saveRDS(grammar, "grammar_check_4.RDS")
  saveRDS(grammarError, "grammar_error_4.RDS")
}

#### RECHECK grammar_error_4.RDS !!!!!!





# create grammar fetaures
grammar <- readRDS("grammar_check_4.RDS") %>%
  filter(rule_category_id %in% c("CASING", "CONFUSED_WORDS", "GRAMMAR", "PUNCTUATION", "STYLE", "TYPOGRAPHY", "TYPOS")) %>%
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
    nIncWord2 = TYPOS,
    nIncCase = CASING,
    nIncConf = CONFUSED_WORDS,
    nIncGrammar = GRAMMAR,
    nIncPunct = PUNCTUATION,
    nIncTypog = TYPOGRAPHY,
    nIncStyle = STYLE,
    pIncWord2 = round(100*TYPOS/nWord, 2),
    pIncCase = round(100*CASING/nWord, 2),
    pIncConf = round(100*CONFUSED_WORDS/nWord, 2),
    pIncGrammar = round(100*GRAMMAR/nWord, 2),
    pIncPunct = round(100*PUNCTUATION/nWord, 2),
    pIncTypog = round(100*TYPOGRAPHY/nWord, 2),
    pIncStyle = round(100*STYLE/nWord, 2)
  ) %>%
  ungroup() %>%
  mutate(
    nIncTotal = rowSums(select(., starts_with("nI"), -pIncWord2)),
    pIncTotal = round(100*nIncTotal/nWord, 2)
  ) %>%
  select(id, starts_with("nI"), starts_with("pI"))

# join to other features and flag errorless essays
features <- left_join(features,
                      grammar,
                      by = "id") %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
  mutate(errorLess = ifelse(nIncTotal == 0, 1, 0))

# Plots



ggplot(features, aes(x = pIncWords, y = pIncWord2)) +
  geom_point()

ggplot(features %>%
         left_join(scores, by = "id"),
       aes(x = nIncWords, y = nIncWord2)) +
  geom_jitter(alpha = 0.5, aes(color = as.factor(score))) +
  geom_smooth(method='lm') +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

saveRDS(edaPlot2("features", "pIncWord2", "Prozent inkorrekt geschriebener Wörter (log2)"), "pIncWord2Plot")



edaPlot("features", "pIncTotal", "Anteil Grammatikfehler")




edaPlot("features", "pIncTypog", "Anteil Inkorrekte Punktesetzung")


# POS TAGGING ##################################################################
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

# number of commas
nComma <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  group_by(id) %>%
  summarise(nComma = sum(token == ","))

# number of unique lemmatized words
nWordUnqLem <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  filter(upos != "PUNCT") %>%
  select(id, lemma) %>%
  filter(!lemma %in% stop_words$word) %>%
  unique() %>%
  group_by(id) %>%
  summarise(nWordUnqLem = n())

# share of tags by essay, number of different tags
posShares <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  select(id, tag = upos) %>%
  ungroup() %>%
  group_by(id, tag) %>%
  summarise(n = n()) %>%
  mutate(sum = sum(n),
         pTag = round(100 * n/sum, 2)) %>%
  ungroup() %>%
  select(id, tag, nTag = n, pTag) %>%
  pivot_wider(id_cols = id, names_from = tag, values_from = c(nTag, pTag)) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
  mutate(nPOStags = rowSums(select(., starts_with("nTag")) > 0))

# join all POS feature and add relative number of commas
pos <- nWordUnqLem %>%
  left_join(nComma, by = "id") %>%
  left_join(posShares, by = "id")

# join to other features
features <- left_join(features,
                      pos,
                      by = "id") %>%
  mutate(
    TTRLem = round(nWordUnqLem/nWord, 2),
    pPOStags = round(nPOStags/nWord, 2),
    pComma = round(100*nComma/nWord, 2)
    )

# saveRDS(edaPlot("features", "pTag_AUX", "Anteil Hilfswörter (auxiliaries) in Prozenten"), "pTag_AUXPlot") # Hilfswort (auxiliary) 0.2
# edaPlot("features", "pTag_CCONJ", "Anteil Präpositionen")
# edaPlot("features", "pTag_PART", "Anteil Präpositionen")
# edaPlot("features", "pTag_PRON", "Anteil Präpositionen")
# edaPlot2("features", "pPOStags", "Anteil Präpositionen")

# LEXICAL DENSITY ##############################################################

# Lexical density
lexDens <- posDF %>%
  mutate(id = as.numeric(str_replace(doc_id, "doc", ""))) %>%
  filter(upos %in% c("NOUN", "ADJ", "VERB", "ADV")) %>%
  group_by(id) %>%
  summarise(nLexItems = n()) %>%
  left_join(nWord, by = "id") %>%
  mutate(lexDens = round(100*nLexItems/nWord, 2)) %>%
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

# join densities
densities <- lexDens %>%
  left_join(smartDens, by = "id") %>%
  left_join(onixDens, by = "id")

# join to other features
features <- left_join(features,
                      densities,
                      by = "id")

edaPlot("features", "lexDens", "Lexikalische Dichte")
edaPlot("features", "smartDens", "Lexikalische Dichte")
edaPlot("features", "onixDens", "Lexikalische Dichte")

# SENTIMENTS ###################################################################
nrc <- get_sentiments("nrc")

# calculate sentiments scores (incl. total) and number of different sentiments
sentiments <- wordsN %>%
  left_join(nrc, by = "word") %>%
  group_by(id, sentiment) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = id, names_from = sentiment, values_from = n) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>%
  left_join(nWord, by = "id") %>%
  rename(nSentAnger = anger, nSentAnticipation = anticipation, nSentFear = fear, nSentNegative = negative, nSentPositive = positive,
         nSentSadness = sadness, nSentSurprise = surprise, nSentTrust = trust, nSentDisgust = disgust, nSentJoy = joy) %>%
  mutate(
    nSentTotal = rowSums(select(., starts_with("nSent"))),
    pSentTotal = round(100*nSentTotal/nWord, 2),
    pSentAnger = round(100*nSentAnger/nWord, 2),
    pSentAnticipation = round(100*nSentAnticipation/nWord, 2),
    pSentDisgust = round(100*nSentDisgust/nWord, 2),
    pSentFear = round(100*nSentFear/nWord, 2),
    pSentJoy = round(100*nSentJoy/nWord, 2),
    pSentNegative = round(100*nSentNegative/nWord, 2),
    pSentPositive = round(100*nSentPositive/nWord, 2),
    pSentSadness = round(100*nSentSadness/nWord, 2),
    pSentSurprise = round(100*nSentSurprise/nWord, 2),
    pSentTrust = round(100*nSentTrust/nWord, 2)
  ) %>%
  select(id, starts_with(c("nSent", "pSent")))

# join to other features
features <- left_join(features,
                      sentiments,
                      by = "id")

edaPlot("features", "pSentSurprise", "nSentTotal")
saveRDS(edaPlot("features", "pSentAnticipation", "Anteil Wörter aus der Kategorie 'Antizipation'"), "pSentAnticipationPlot")
saveRDS(edaPlot("features", "pSentPositive", "Anteil positiver Wörter"), "pSentPositive")
edaPlot("features", "pSentTrust", "nSentTotal")




edaPlot("features", "pSentTotal", "nSentTotal") # asymptotisch

# READABILITY ##################################################################

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

readability <-readRDS("readabilityScores.RDS") %>%
  rename(id = subID)

# join to other features
features <- left_join(features,
                      readability,
                      by = "id")

edaPlot("features", "LIX", "LIX")
edaPlot("features", "ARI", "LIX")
edaPlot("features", "TRI", "LIX")
edaPlot("features", "Traenkle.Bailer.TB1", "LIX")

# DEPTH OF PARSE TREE ##########################################################

if(reTreeParse) {

  # tokenize raw essays
  sentTok <- unnest_tokens(select(data, id, essay),
                           output = "sentence",
                           input = essay,
                           token = "sentences",
                           to_lower = FALSE) %>%
    mutate(nChar = nchar(sentence),
           nWord = str_count(sentence, '\\w+')) %>%
    # arrange(desc(nWord))
    arrange(id)

  sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  parse_annotator <- openNLP::Parse_Annotator()

  tree_depths <- data.frame(NULL)

  for(i in 1:nrow(sentTok)) {
    id <- pull(sentTok[i, "id"])
    text <- pull(sentTok[i, "sentence"])
    text <- str_remove_all(text, "\\(|\\)")
    text <- as.String(text)
    a2 <- NLP::annotate(text, list(sent_token_annotator, word_token_annotator))
    p <- parse_annotator(text, a2)
    parsedInfo <- sapply(p$features, '[[', "parse")
    treeInfo <- lapply(parsedInfo, Tree_parse)
    depth <- max(map_int(treeInfo, vec_depth))
    x <- data.frame("id" = id, "sentence" = as.character(text), "depth" = depth)
    tree_depths <- bind_rows(tree_depths, x)
  }

  saveRDS(tree_depths, "tree_depths.RDS")

}

tree_depths <- readRDS("tree_depths.RDS") %>%
  group_by(id) %>%
  summarise(
    meanTreeDepth = round(mean(depth), 2),
    sdTreeDepth = round(sd(depth), 2)
  ) %>%
  mutate(sdTreeDepth = ifelse(is.na(sdTreeDepth), 0 , sdTreeDepth))

# join to other features
features <- left_join(features, tree_depths, by = "id")

edaPlot("features", "meanTreeDepth", "meanTreeDepth")
edaPlot("features", "sdTreeDepth", "meanTreeDepth")

# GENERAL TRAIN-TEST SPLIT #####################################################

set.seed(1898)

# train set
trainIds <- sample(1:nrow(data), nrow(data) * 0.7) %>%
  sort()

train <- data %>%
  filter(id %in% trainIds)

# frq(train$idea)
# frq(train$orga)
# frq(train$style)
# frq(train$conv)

# test set
testIds <- setdiff(data$id, trainIds) %>%
  sort()

test <- data %>%
  filter(id %in% testIds)

# frq(test$idea)
# frq(test$orga)
# frq(test$style)
# frq(test$conv)

# TF-IDF AND TEXT SIMILARITIES #################################################

# SHOULD BE DONE AFTER TRAIN TEST SPLIT!

# identify well written essays in train set
superEssays <- filter(train, idea == 3, conv == 3, orga == 3, style == 3) %>%
  pull(id)

# identify well written essays in each criteria
# idea <- filter(data, idea == 3) %>% pull(id)
# conv <- filter(data, conv == 3) %>% pull(id)
# orga <- filter(data, orga == 3) %>% pull(id)
# style <- filter(data, style == 3) %>% pull(id)

# start with word tokens without stopwords
tfidf <- wordsN %>%
  filter(!word %in% stop_words_sb$word) %>%
  mutate(
    word = wordStem(word, language = "en"),
    correct = hunspell_check(word)
    ) %>%
  filter(correct) %>%
  select(-correct) %>%
  count(id, word, sort  = TRUE) %>%
  bind_tf_idf(word, id, n)

sparseMatrix <- tfidf %>%
  cast_sparse(id, word, tf)

# calculate cosine similarities
similarities <- sim2(sparseMatrix, method = "cosine", norm = "l2") %>%
  as.matrix() %>%
  as.data.frame()

# calculate mean similarities with each well written text
simSuperEssay <- similarities %>%
  select(all_of(superEssays)) %>%
  mutate(simGood = round(rowMeans(., na.rm = TRUE), 4)) %>%
  select(simGood) %>%
  mutate(id = as.numeric(row.names(.))) %>%
  arrange(id)

# EDA
scores %>%
  left_join(simSuperEssay, by = "id") %>%
  ggplot(aes(x = score, y = simGood, fill = score, color = score)) +
  geom_jitter(alpha = 0.2, width = 0.25, size = 1) +
  geom_violin(width = 0.75, fill = NA, color = "black") +
  geom_boxplot(width = 0.1, color = "black", fill = NA) +
  theme(
    legend.position = "none"
  )

# join to other features
features <- left_join(features, simSuperEssay, by = "id")

edaPlot("features", "simGood", "simGood")
edaPlot("features", "sdTreeDepth", "meanTreeDepth")

# SAVE FEATURES ################################################################

# convert NA to 0 and save RDS
features <- features %>%
  ungroup() %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
saveRDS(features, "features.RDS")
writexl::write_xlsx(features, "featureOverview.xlsx")
saveRDS(filter(features, id %in% trainIds), "trainFeatures.RDS")
saveRDS(filter(features, id %in% testIds), "testFeatures.RDS")

# get separate feature set with stemmed non-stop-words
binaryWords <- as.matrix(sparseMatrix) %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ifelse(. > 0, 1, 0))) %>%
  mutate(id = as.numeric(row.names(.))) %>%
  select(id, everything()) %>%
  arrange(id)

filter(binaryWords, id %in% trainIds) %>%
  saveRDS("trainBinaryWords.RDS")
testbinaryWords <- filter(binaryWords, id %in% testIds) %>%
  saveRDS("testBinaryWords.RDS")



saveRDS(binaryWords, "binaryWords.RDS")

tfIdfWords <- as.matrix(sparseMatrix) %>%
  as.data.frame() %>%
  mutate(id = as.numeric(row.names(.))) %>%
  select(id, everything()) %>%
  arrange(id)
saveRDS(tfIdfWords, "tfIdfWords.RDS")

# reduce binaryWords with correspondence analysis (only train set!)
# bwReduced <- binaryWords %>%
#   filter(id %in% trainIds) %>%
#   select(-id)
# library(FactoMineR)
# library(factoextra)
# bwCa <- CA(bwReduced, ncp = 20, graph = TRUE)
# fviz_screeplot(bwCa, addlabels = TRUE, ylim = c(0, 50))





# nbWords <- wordsN %>%
#   filter(!word %in% stop_words_sb$word) %>%
#   mutate(
#     word = wordStem(word, language = "en"),
#     correct = hunspell_check(word)
#   ) %>%
#   group_by(word) %>%
#   summarise(n = n()) %>%
#   filter(n > 10 & word %in% colnames(binaryWords)) %>%
#   arrange(word) %>%
#   pull(word)

# naive bayes tests
# x <- dplyr::select(binaryWords, all_of(nbWords))
# y <- as.factor(data$idea)
#
#
# library(klaR)
# model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10))


library("stringr")
library("NLP")
library("openNLP")
library("openNLPdata")

text <- readLines("https://slcladal.github.io/data/english.txt")
# convert character to string
s <- as.String(text)
# define sentence and word token annotator
sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
# apply sentence and word annotator
a2 <- NLP::annotate(s, list(sent_token_annotator, word_token_annotator))
# define syntactic parsing annotator
parse_annotator <- openNLP::Parse_Annotator()
p <- parse_annotator(s, a2)
# extract parsed information
ptexts <- sapply(p$features, '[[', "parse")
ptexts

# read into NLP Tree objects.
wtrees <- lapply(ptexts, Tree_parse)
# show frist tree
ptrees[[1]]
ptrees
length(ptrees[[1]])


library(purrr)

map_int(x, vec_depth)

max(map_int(ptrees, vec_depth))


library(NLP)
library(igraph)

# load function
source("https://slcladal.github.io/rscripts/parsetgraph.R")
# generate syntax tree
parse2graph(parsedInfo[1], leaf.color='red',
            # to put sentence in title (not advisable for long sentences)
            #title = stringr::str_squish(stringr::str_remove_all(ptexts[1], "\\(\\,{0,1}[A-Z]{0,4}|\\)")),
            margin=-0.05,
            vertex.color=NA,
            vertex.frame.color=NA,
            vertex.label.font=2,
            vertex.label.cex=.75,
            asp=.8,
            edge.width=.5,
            edge.color='gray',
            edge.arrow.size=0
)

x <- list(
  list(),
  list(list()),
  list(list(list(1)))
)
vec_depth(x)
map_int(x, vec_depth)
library(plotrix)
plotrix::list_depth(x)
library(collapse)
collapse::ldepth(ptrees[[1]])

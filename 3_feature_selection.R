library(caret)
library(Boruta)
library(tidyverse)

# Boruta Search based on UNpreprocessed training set ###########################
trainT <- readRDS("trainFeatures.RDS") %>%
  left_join(readRDS("ideaScores.RDS"), by = "id") %>%
  left_join(readRDS("orgaScores.RDS"), by = "id") %>%
  left_join(readRDS("styleScores.RDS"), by = "id") %>%
  left_join(readRDS("convScores.RDS"), by = "id") %>%
  mutate(
    idea1 = ifelse(idea == 1, 1, 0),
    idea3 = ifelse(idea == 3, 1, 0),
    orga1 = ifelse(orga == 1, 1, 0),
    orga3 = ifelse(orga == 3, 1, 0),
    style1 = ifelse(style == 1, 1, 0),
    style3 = ifelse(style == 3, 1, 0),
    conv1 = ifelse(conv == 1, 1, 0),
    conv3 = ifelse(conv == 3, 1, 0)
    )

# Boruta Plot function
borPlot <- function(bor) {
  bor %>%
    rename(Prädiktor = feature, `Mittlere Itemwichtigkeit (Z-Wert)` = meanImp) %>%
    ggplot(
      aes(x = Prädiktor, y = `Mittlere Itemwichtigkeit (Z-Wert)`, fill = `Allg. prädiktiv`, color =  `Allg. prädiktiv`, shape = `Prädiktor für`)) +
    geom_point(size = 3, alpha = 0.75) +
    scale_shape_manual(values=c(25, 24)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      legend.position = "top"
    )
}


#  Boruta search separately on each target and score

# idea
borutaIdea1 <- Boruta(idea1 ~ .,
                      select(trainT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), idea1),
                      doTrace=2,
                      maxRuns = 500)
borutaIdea1 <- TentativeRoughFix(borutaIdea1)
fBorI1 <- getSelectedAttributes(borutaIdea1)

borutaIdea3 <- Boruta(idea3 ~ .,
                      select(trainT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), idea3),
                      doTrace=2,
                      maxRuns = 500)
borutaIdea3 <- TentativeRoughFix(borutaIdea3)
fBorI3 <- getSelectedAttributes(borutaIdea3)

# orga
borutaOrga1 <- Boruta(orga1 ~ .,
                      select(trainT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), orga1),
                      doTrace=2,
                      maxRuns = 500)
borutaOrga1 <- TentativeRoughFix(borutaOrga1)
fBorO1 <- getSelectedAttributes(borutaOrga1)

borutaOrga3 <- Boruta(orga3 ~ .,
                      select(trainT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), orga3),
                      doTrace=2,
                      maxRuns = 500)
borutaOrga3 <- TentativeRoughFix(borutaOrga3)
fBorO3 <- getSelectedAttributes(borutaOrga3)

# style
borutaStyle1 <- Boruta(style1 ~ .,
                      select(trainT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), style1),
                      doTrace=2,
                      maxRuns = 500)
borutaStyle1 <- TentativeRoughFix(borutaStyle1)
fBorS1 <- getSelectedAttributes(borutaStyle1)

borutaStyle3 <- Boruta(style3 ~ .,
                      select(trainT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), style3),
                      doTrace=2,
                      maxRuns = 500)
borutaStyle3 <- TentativeRoughFix(borutaStyle3)
fBorS3 <- getSelectedAttributes(borutaStyle3)

# conv
borutaConv1 <- Boruta(conv1 ~ .,
                       select(trainT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), conv1),
                       doTrace=2,
                       maxRuns = 500)
borutaConv1 <- TentativeRoughFix(borutaConv1)
fBorC1 <- getSelectedAttributes(borutaConv1)

borutaConv3 <- Boruta(conv3 ~ .,
                       select(trainT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), conv3),
                       doTrace=2,
                       maxRuns = 500)
borutaConv3 <- TentativeRoughFix(borutaConv3)
fBorC3 <- getSelectedAttributes(borutaConv3)

# plot(borutaOrga1)
# plot(borutaStyle1)
# plot(borutaConv1)

# Boruta Search based on preprocessed training set #############################
trainPPT <- readRDS("train_scaled_transformed_preprocessed.RDS") %>%
  left_join(readRDS("ideaScores.RDS"), by = "id") %>%
  left_join(readRDS("orgaScores.RDS"), by = "id") %>%
  left_join(readRDS("styleScores.RDS"), by = "id") %>%
  left_join(readRDS("convScores.RDS"), by = "id") %>%
  mutate(
    idea1 = ifelse(idea == 1, 1, 0),
    idea3 = ifelse(idea == 3, 1, 0),
    orga1 = ifelse(orga == 1, 1, 0),
    orga3 = ifelse(orga == 3, 1, 0),
    style1 = ifelse(style == 1, 1, 0),
    style3 = ifelse(style == 3, 1, 0),
    conv1 = ifelse(conv == 1, 1, 0),
    conv3 = ifelse(conv == 3, 1, 0)
  )

# idea
borutaIdea1 <- Boruta(idea1 ~ .,
                      select(trainPPT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), idea1),
                      doTrace=2,
                      maxRuns = 500)
borutaIdea1 <- TentativeRoughFix(borutaIdea1)
fBorIPP1s <- attStats(borutaIdea1) %>%
  filter(decision == "Confirmed") %>%
  mutate(`Prädiktor für` = as.factor("2 Punkte"), feature = rownames(.))
fBorIPP1 <- getSelectedAttributes(borutaIdea1)

borutaIdea3 <- Boruta(idea3 ~ .,
                      select(trainPPT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), idea3),
                      doTrace=2,
                      maxRuns = 500)
borutaIdea3 <- TentativeRoughFix(borutaIdea3)
fBorIPP3s <- attStats(borutaIdea3) %>%
  filter(decision == "Confirmed") %>%
  mutate(`Prädiktor für` = as.factor("0 Punkte"), feature = rownames(.))
fBorIPP3 <- getSelectedAttributes(borutaIdea3)

fBorIPPs <- rbind(fBorIPP1s, fBorIPP3s) %>%
  group_by(feature) %>%
  mutate(n = n(),
         `Allg. prädiktiv` = ifelse(n > 1, "Ja", "Nein"),
         meanImp2 = mean(meanImp)) %>%
  ungroup() %>%
  arrange(desc(meanImp2))

feats <- unique(fBorIPPs$feature)
fBorIPPs$feature <- factor(fBorIPPs$feature, levels = feats)
borPlot(fBorIPPs)
saveRDS(fBorIPPs, "borPlotIdea")


# orga
borutaOrga1 <- Boruta(orga1 ~ .,
                      select(trainPPT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), orga1),
                      doTrace=2,
                      maxRuns = 500)
borutaOrga1 <- TentativeRoughFix(borutaOrga1)
fBorOPP1s <- attStats(borutaOrga1) %>%
  filter(decision == "Confirmed") %>%
  mutate(`Prädiktor für` = as.factor("0 Punkte"), feature = rownames(.))
fBorOPP1 <- getSelectedAttributes(borutaOrga1)

borutaOrga3 <- Boruta(orga3 ~ .,
                      select(trainPPT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), orga3),
                      doTrace=2,
                      maxRuns = 500)
borutaOrga3 <- TentativeRoughFix(borutaOrga3)
fBorOPP3s <- attStats(borutaOrga3) %>%
  filter(decision == "Confirmed") %>%
  mutate(`Prädiktor für` = as.factor("2 Punkte"), feature = rownames(.))
fBorOPP3 <- getSelectedAttributes(borutaOrga3)

fBorOPPs <- rbind(fBorOPP1s, fBorOPP3s) %>%
  group_by(feature) %>%
  mutate(n = n(),
         `Allg. prädiktiv` = ifelse(n > 1, "Ja", "Nein"),
         meanImp2 = mean(meanImp)) %>%
  ungroup() %>%
  arrange(desc(meanImp2))

feats <- unique(fBorOPPs$feature)
fBorOPPs$feature <- factor(fBorOPPs$feature, levels = feats)
borPlot(fBorOPPs)
saveRDS(fBorOPPs, "borPlotOrga")



# style
borutaStyle1 <- Boruta(style1 ~ .,
                       select(trainPPT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), style1),
                       doTrace=2,
                       maxRuns = 500)
borutaStyle1 <- TentativeRoughFix(borutaStyle1)
fBorSPP1s <- attStats(borutaStyle1) %>%
  filter(decision == "Confirmed") %>%
  mutate(`Prädiktor für` = as.factor("0 Punkte"), feature = rownames(.))
fBorSPP1 <- getSelectedAttributes(borutaStyle1)

borutaStyle3 <- Boruta(style3 ~ .,
                       select(trainPPT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), style3),
                       doTrace=2,
                       maxRuns = 500)
borutaStyle3 <- TentativeRoughFix(borutaStyle3)
fBorSPP3s <- attStats(borutaStyle3) %>%
  filter(decision == "Confirmed") %>%
  mutate(`Prädiktor für` = as.factor("2 Punkte"), feature = rownames(.))
fBorSPP3 <- getSelectedAttributes(borutaStyle3)

fBorSPPs <- rbind(fBorSPP1s, fBorSPP3s) %>%
  group_by(feature) %>%
  mutate(n = n(),
         `Allg. prädiktiv` = ifelse(n > 1, "Ja", "Nein"),
         meanImp2 = mean(meanImp)) %>%
  ungroup() %>%
  arrange(desc(meanImp2))

feats <- unique(fBorSPPs$feature)
fBorSPPs$feature <- factor(fBorSPPs$feature, levels = feats)
borPlot(fBorSPPs)
saveRDS(borPlot(fBorSPPs), "borPlotStyle")


# conv
borutaConv1 <- Boruta(conv1 ~ .,
                      select(trainPPT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), conv1),
                      doTrace=2,
                      maxRuns = 500)
borutaConv1 <- TentativeRoughFix(borutaConv1)
fBorCPP1s <- attStats(borutaConv1) %>%
  filter(decision == "Confirmed") %>%
  mutate(`Prädiktor für` = as.factor("0 Punkte"), feature = rownames(.))
fBorCPP1 <- getSelectedAttributes(borutaConv1)

borutaConv3 <- Boruta(conv3 ~ .,
                      select(trainPPT, -c(id, starts_with(c("orga", "style", "conv", "idea"))), conv3),
                      doTrace=2,
                      maxRuns = 500)
borutaConv3 <- TentativeRoughFix(borutaConv3)
fBorCPP3s <- attStats(borutaConv3) %>%
  filter(decision == "Confirmed") %>%
  mutate(`Prädiktor für` = as.factor("2 Punkte"), feature = rownames(.))
fBorCPP3 <- getSelectedAttributes(borutaConv3)

fBorCPPs <- rbind(fBorCPP1s, fBorCPP3s) %>%
  group_by(feature) %>%
  mutate(n = n(),
         `Allg. prädiktiv` = ifelse(n > 1, "Ja", "Nein"),
         meanImp2 = mean(meanImp)) %>%
  ungroup() %>%
  arrange(desc(meanImp2))

feats <- unique(fBorCPPs$feature)
fBorCPPs$feature <- factor(fBorCPPs$feature, levels = feats)
borPlot(fBorCPPs)
saveRDS(fBorCPPs, "borPlotConv")

# Number of features per criteria and step
length(fBorIPP1)
length(fBorIPP3)
length(fBorOPP1)
length(fBorOPP3)
length(fBorSPP1)
length(fBorSPP3)
length(fBorCPP1)
length(fBorCPP3)

# arrange by importance
fBorIPP1s %>% arrange(desc(meanImp)) %>% pull(feature)

bla <- unique(c(fBorIPP1, fBorIPP3, fBorOPP1, fBorOPP3, fBorSPP1, fBorSPP3, fBorCPP1, fBorCPP3))
length(bla)
ncol(featuresPP)

# save selected feature vectors
saveRDS(fBorIPP1s %>% arrange(desc(meanImp)) %>% pull(feature), "sfIdea1")
saveRDS(fBorIPP3s %>% arrange(desc(meanImp)) %>% pull(feature), "sfIdea3")
saveRDS(fBorOPP1s %>% arrange(desc(meanImp)) %>% pull(feature), "sfOrga1")
saveRDS(fBorOPP3s %>% arrange(desc(meanImp)) %>% pull(feature), "sfOrga3")
saveRDS(fBorSPP1s %>% arrange(desc(meanImp)) %>% pull(feature), "sfStyle1")
saveRDS(fBorSPP3s %>% arrange(desc(meanImp)) %>% pull(feature), "sfStyle3")
saveRDS(fBorCPP1s %>% arrange(desc(meanImp)) %>% pull(feature), "sfConv1")
saveRDS(fBorCPP3s %>% arrange(desc(meanImp)) %>% pull(feature), "sfConv3")


# Overall importance df
fBorS <- fBorIPPs %>%
  mutate(Kriterium = "Inhalt") %>%
  bind_rows(mutate(fBorOPPs, Kriterium = "Textaufbau")) %>%
  bind_rows(mutate(fBorSPPs, Kriterium = "Sprachangemessenheit")) %>%
  bind_rows(mutate(fBorCPPs, Kriterium = "Sprachrichtigkeit")) %>%
  group_by(feature) %>%
  mutate(nCrit = n(),
         meanImp3 = mean(meanImp),
         Identifiziert = case_when(
           both == "Beidseitig" ~ "Beides",
           both != "Beidseitig" & score == "0 Punkte" ~ "0 Punkte",
           both != "Beidseitig" & score == "2 Punkte" ~ "2 Punkte",
           TRUE ~ as.character(NA)
         )) %>%
  ungroup() %>%
  arrange(desc(nCrit))

ggplot(fBorS,
       aes(x = feature, y = meanImp, color = Kriterium, shape = score)) +
  geom_point(size = 3, alpha = 0.75) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))











# Boruta Search based on preprocessed training set with binary words ###########
trainPPTW <- readRDS("train_scaled_transformed_preprocessed_with_binaryWords.RDS") %>%
  left_join(readRDS("ideaScores.RDS"), by = "id") %>%
  left_join(readRDS("orgaScores.RDS"), by = "id") %>%
  left_join(readRDS("styleScores.RDS"), by = "id") %>%
  left_join(readRDS("convScores.RDS"), by = "id") %>%
  mutate(
    idea1 = ifelse(idea == 1, 1, 0),
    idea3 = ifelse(idea == 3, 1, 0),
    orga1 = ifelse(orga == 1, 1, 0),
    orga3 = ifelse(orga == 3, 1, 0),
    style1 = ifelse(style == 1, 1, 0),
    style3 = ifelse(style == 3, 1, 0),
    conv1 = ifelse(conv == 1, 1, 0),
    conv3 = ifelse(conv == 3, 1, 0)
  )

# idea
borutaIdea1 <- Boruta(idea1 ~ .,
                      select(trainPPTW, -c(id, starts_with(c("orga", "style", "conv", "idea"))), idea1),
                      doTrace=2,
                      maxRuns = 500)
borutaIdea1 <- TentativeRoughFix(borutaIdea1)
fBorIPPW1 <- getSelectedAttributes(borutaIdea1)

borutaIdea3 <- Boruta(idea3 ~ .,
                      select(trainPPTW, -c(id, starts_with(c("orga", "style", "conv", "idea"))), idea3),
                      doTrace=2,
                      maxRuns = 500)
borutaIdea3 <- TentativeRoughFix(borutaIdea3)
fBorIPPW3 <- getSelectedAttributes(borutaIdea3)

# orga
borutaOrga1 <- Boruta(orga1 ~ .,
                      select(trainPPTW, -c(id, starts_with(c("orga", "style", "conv", "idea"))), orga1),
                      doTrace=2,
                      maxRuns = 500)
borutaOrga1 <- TentativeRoughFix(borutaOrga1)
fBorOPPW1 <- getSelectedAttributes(borutaOrga1)

borutaOrga3 <- Boruta(orga3 ~ .,
                      select(trainPPTW, -c(id, starts_with(c("orga", "style", "conv", "idea"))), orga3),
                      doTrace=2,
                      maxRuns = 500)
borutaOrga3 <- TentativeRoughFix(borutaOrga3)
fBorOPPW3 <- getSelectedAttributes(borutaOrga3)

# style
borutaStyle1 <- Boruta(style1 ~ .,
                       select(trainPPTW, -c(id, starts_with(c("orga", "style", "conv", "idea"))), style1),
                       doTrace=2,
                       maxRuns = 500)
borutaStyle1 <- TentativeRoughFix(borutaStyle1)
fBorSPPW1 <- getSelectedAttributes(borutaStyle1)

borutaStyle3 <- Boruta(style3 ~ .,
                       select(trainPPTW, -c(id, starts_with(c("orga", "style", "conv", "idea"))), style3),
                       doTrace=2,
                       maxRuns = 500)
borutaStyle3 <- TentativeRoughFix(borutaStyle3)
fBorSPPW3 <- getSelectedAttributes(borutaStyle3)

# conv
borutaConv1 <- Boruta(conv1 ~ .,
                      select(trainPPTW, -c(id, starts_with(c("orga", "style", "conv", "idea"))), conv1),
                      doTrace=2,
                      maxRuns = 500)
borutaConv1 <- TentativeRoughFix(borutaConv1)
fBorCPPW1 <- getSelectedAttributes(borutaConv1)

borutaConv3 <- Boruta(conv3 ~ .,
                      select(trainPPTW, -c(id, starts_with(c("orga", "style", "conv", "idea"))), conv3),
                      doTrace=2,
                      maxRuns = 500)
borutaConv3 <- TentativeRoughFix(borutaConv3)
fBorCPPW3 <- getSelectedAttributes(borutaConv3)

borutaFeatures <- list(
  fBorI1,
  fBorO1,
  fBorS1,
  fBorC1,
  fBorIPP1,
  fBorOPP1,
  fBorSPP1,
  fBorCPP1,
  fBorIPPW1,
  fBorOPPW1,
  fBorSPPW1,
  fBorCPPW1,
  fBorI3,
  fBorO3,
  fBorS3,
  fBorC3,
  fBorIPP3,
  fBorOPP3,
  fBorSPP3,
  fBorCPP3,
  fBorIPPW3,
  fBorOPPW3,
  fBorSPPW3,
  fBorCPPW3
)

saveRDS(borutaFeatures, "borutaFeatures2.RDS")

bli <- readRDS("borutaFeatures.RDS")
bli[[9]]









# boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
# print(boruta_signif)
# roughFixMod <- TentativeRoughFix(boruta_output)
# boruta_signif <- getSelectedAttributes(roughFixMod)
# print(boruta_signif)
# imps <- attStats(roughFixMod)
# imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
# head(imps2[order(-imps2$meanImp), ])  # descending sort
# plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

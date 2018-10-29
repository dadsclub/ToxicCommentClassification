# Clear Console and Previous Plots
cat("\014")
dev.off(dev.list()["RStudioGD"])


# Import Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)


# Import data into environment if not already imported
if (!exists("train")) {
  assign("train", read.csv("data/train.csv"))
}

# Turn Binary Columns into Logical Columns
train$toxic = as.logical(train$toxic)
train$severe_toxic = as.logical(train$severe_toxic)
train$obscene = as.logical(train$obscene)
train$threat = as.logical(train$threat)
train$insult = as.logical(train$insult)
train$identity_hate = as.logical(train$identity_hate)


# Create a Data Frame with the Count of every classification
F <- T <- c(0, 0, 0, 0, 0, 0)
ClassificationSummary <- data.frame(T, F)
ClassificationSummary$T <- t(train %>% 
  summarise(Toxic = sum(toxic), 
            SevereToxic = sum(severe_toxic),
            Obscene = sum(obscene),
            Threat = sum(threat),
            Insult = sum(insult),
            IdentityHate = sum(identity_hate)))
ClassificationSummary$F <- t(train %>% 
    summarise(nToxic = n() - sum(toxic), 
              nSevereToxic = n() - sum(severe_toxic),
              nObscene = n() - sum(obscene),
              nThreat = n() - sum(threat),
              nInsult = n() - sum(insult),
              nIdentityHate = n() - sum(identity_hate)))
colnames(ClassificationSummary) <- c("T", "F")



# Plot Bar Graph of each type of classification
ClassificationSummary %>%
  ggplot() +
  geom_bar(mapping = aes(x = c("Toxic", "Severe Toxic", "Obscene", "Threat", "Insult", "Identity Hate"), y = T), stat = "identity") +
  labs(title = "Number of Comments with Each Classification",
       x = "Classification",
       y = "Number of Comments")


# Clear Console and Previous Plots
cat("\014")
dev.off(dev.list()["RStudioGD"])


# Import Libraries
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(gridExtra)


# Import data into environment if not already imported
if (!exists("train")) {
  assign("train", read.csv("data/train.csv"))
}

train$comment_text <- as.character(train$comment_text)

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

remove(F)
remove(T)


# Split comments into separate rows with separate words and remove stop words
tidy_train <- train %>%
unnest_tokens(word, comment_text) %>%
  anti_join(stop_words)

# Remove observations with no bad comments
tidy_train <- tidy_train %>%
  filter(toxic + severe_toxic + obscene + threat + insult + identity_hate > 0)

# Make words into factors
tidy_train$word <- as.factor(tidy_train$word)

# Plot Bar Graph of each type of classification
ClassificationSummary %>%
  ggplot() +
  geom_bar(mapping = aes(x = c("Toxic", "Severe Toxic", "Obscene", "Threat", "Insult", "Identity Hate"), y = T), stat = "identity") +
  labs(title = "Number of Comments with Each Classification",
       x = "Classification",
       y = "Number of Comments")



# Plot most common words within comments flagged as toxic
(plotTop10Toxic <- tidy_train %>%
  group_by(word) %>%
  filter(toxic == TRUE) %>%
  summarise(NumOfOccurances = n()) %>%
  arrange(desc(NumOfOccurances)) %>%
  head(10) %>%
  ggplot() +
    geom_bar(mapping = aes(x = word, y = NumOfOccurances), stat = "identity") +
    labs(title = "Top 10 Words Within Comments Flagged as Toxic",
         x = "Word",
         y = "Number of Occurances"))


# Plot most common words within comments flagged as severe_toxic
(plotTop10SevereToxic <- tidy_train %>%
  group_by(word) %>%
  filter(severe_toxic == TRUE) %>%
  summarise(NumOfOccurances = n()) %>%
  arrange(desc(NumOfOccurances)) %>%
  head(10) %>%
  ggplot() +
  geom_bar(mapping = aes(x = word, y = NumOfOccurances), stat = "identity") +
  labs(title = "Top 10 Words Within Comments Flagged as Severe Toxic",
       x = "Word",
       y = "Number of Occurances"))


# Plot most common words within comments flagged as obscene
(plotTop10Obscene <- tidy_train %>%
  group_by(word) %>%
  filter(obscene == TRUE) %>%
  summarise(NumOfOccurances = n()) %>%
  arrange(desc(NumOfOccurances)) %>%
  head(10) %>%
  ggplot() +
  geom_bar(mapping = aes(x = word, y = NumOfOccurances), stat = "identity") +
  labs(title = "Top 10 Words Within Comments Flagged as Obscene",
       x = "Word",
       y = "Number of Occurances"))


# Plot most common words within comments flagged as threat
(plotTop10Threat <- tidy_train %>%
  group_by(word) %>%
  filter(threat == TRUE) %>%
  summarise(NumOfOccurances = n()) %>%
  arrange(desc(NumOfOccurances)) %>%
  head(10) %>%
  ggplot() +
  geom_bar(mapping = aes(x = word, y = NumOfOccurances), stat = "identity") +
  labs(title = "Top 10 Words Within Comments Flagged as Threat",
       x = "Word",
       y = "Number of Occurances"))


# Plot most common words within comments flagged as Insult
(plotTop10Insult <- tidy_train %>%
  group_by(word) %>%
  filter(insult == TRUE) %>%
  summarise(NumOfOccurances = n()) %>%
  arrange(desc(NumOfOccurances)) %>%
  head(10) %>%
  ggplot() +
  geom_bar(mapping = aes(x = word, y = NumOfOccurances), stat = "identity") +
  labs(title = "Top 10 Words Within Comments Flagged as Insult",
       x = "Word",
       y = "Number of Occurances"))


# Plot most common words within comments flagged as identity_hate
(plotTop10IdentityHate <- tidy_train %>%
  group_by(word) %>%
  filter(identity_hate == TRUE) %>%
  summarise(NumOfOccurances = n()) %>%
  arrange(desc(NumOfOccurances)) %>%
  head(10) %>%
  ggplot() +
  geom_bar(mapping = aes(x = word, y = NumOfOccurances), stat = "identity") +
  labs(title = "Top 10 Words Within Comments Flagged as Identity Hate",
       x = "Word",
       y = "Number of Occurances"))


# Plot most common words within comments flagged as each classification in a grid
grid.arrange(plotTop10Toxic, plotTop10SevereToxic, plotTop10Obscene, plotTop10Threat, plotTop10Insult, plotTop10IdentityHate)



# Plot most common words flagged
tidy_train %>%
    group_by(word) %>%
    summarise(NumOfOccurances = n()) %>%
    arrange(desc(NumOfOccurances)) %>%
    head(10) %>%
    ggplot() +
    geom_bar(mapping = aes(x = word, y = NumOfOccurances), stat = "identity") +
    labs(title = "Top 10 Words Within Comments Flagged",
         x = "Word",
         y = "Number of Occurances")


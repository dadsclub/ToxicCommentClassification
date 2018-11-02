
# Import Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidytext)
library(lettercase)

# Import data into environment if not already imported
train <- read_csv("data/train.csv")

# Create a tokenized table, splitting comments by word
train <- as_tibble(train)
tokenized <- unnest_tokens(train, word, comment_text) %>% anti_join(stop_words)
bad_words <- tokenized %>%
  group_by(word) %>%
  dplyr::summarise(
    count = n(),
    toxic_avg = mean(toxic),
    severe_toxic = mean(severe_toxic),
    obscene_avg = mean(obscene),
    threat_avg = mean(threat),
    insult_avg = mean(insult),
    identity_hate_avg = mean(identity_hate),
    avgTox = sum(toxic_avg, severe_toxic, obscene_avg, threat_avg,
                 insult_avg, identity_hate_avg)  
  ) %>% filter(count>300) %>%  arrange(desc(avgTox))
bad_words %>% top_n(20)

bad_words$zipf <- (bad_words$count/(sum(bad_words$count)))^(-1) 
#bad_words$bing <- bad_words[,1] %>% left_join(get_sentiments('bing'))
#isbad_words$nrc <- bad_words[,1] %>% left_join(get_sentiments('nrc'))
bad_words$afinn <- bad_words[,1] %>% left_join(get_sentiments('afinn'))


# Summary Engineering -----------------------------------------------------

train$allcap <- is_all_caps(train$comment_text)
train$toxScore <- (train$toxic+train$severe_toxic+train$obscene+train$threat+train$insult+train$identity_hate) 
train$toxScore[train$toxScore==-Inf] <- 0
train %>% ggplot() + geom_col(aes(x = allcap, y = toxScore))



# Import Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidytext)
library(lettercase)


# Import data into environment if not already imported
train <- read_csv("train.csv")

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
  ) %>% filter(count>350) %>%  arrange(desc(avgTox))
bad_words %>% top_n(20)

bad_words$zipf <- (bad_words$count/(sum(bad_words$count)))^(-1) 
#bad_words$bing <- bad_words[,1] %>% left_join(get_sentiments('bing'))
#isbad_words$nrc <- bad_words[,1] %>% left_join(get_sentiments('nrc'))
bad_words$afinn <- bad_words[,1] %>% left_join(get_sentiments('afinn'))


# Summary Engineering -----------------------------------------------------

train$allcap <- is_all_caps(train$comment_text)


# Bad Word Reference ------------------------------------------------------

# Here we are having a dictionary of 100 top worst words based on their overall toxicity.
# We will count the number of bad words in each comment, and use the assigned weight to determine the output

bad_words_dict <- bad_words[,1:8] %>% head(50)


count_baddies <- function(text){
  return(str_count(text, bad_words_dict$word) %>% sum())
}
train$badwordcount <- sapply(train$comment_text, count_baddies)

train %>% ggplot() + geom_smooth(aes(x = toxic , y = badwordcount))








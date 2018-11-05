
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(caret)



# Importing Data ----------------------------------------------------------
trainingset <- read_csv("all/train.csv")
testset <- read_csv("all/test.csv")
labels_set <-  read_csv("all/test_labels.csv")



# Tidying -----------------------------------------------------------------

#stop words to be filtered out
data(stop_words)

#tokenization and removal of stop words
ind_words <- trainingset %>% 
  unnest_tokens(word, comment_text) %>% 
  anti_join(stop_words) %>% 
  ungroup()




# Feature Engineering -----------------------------------------------------

##main function for any kind of feature engineering that is going to be done
features <- c("toxic",
              "severe_toxic",
              "obscene",
              "threat",
              "insult",
              "identity_hate",
              "total_toxicity"
)


feature_eng <- function(data){
  fea <- data[ ,features]
  fea$total_toxicity <- rowSums(data[ , c(3,4,5,7,8)])
  
  return(fea)
}


# Graphs and things -------------------------------------------------------

###****this is here temporarily and will be added to the feature_eng function above later on***
trainingset$total_toxicity <- rowSums(trainingset[ , c(3,4,5,6,7,8)])

##sorting by most toxic comments 
sort_total <- arrange(trainingset, desc(total_toxicity)) %>% 
  select('id', 'comment_text', 'total_toxicity')



# dic ---------------------------------------------------------------------

avg_resummarize <- ind_words %>%
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




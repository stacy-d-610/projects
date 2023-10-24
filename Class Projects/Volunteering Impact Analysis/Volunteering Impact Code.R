### Loading cleaned data file
data <- read.csv("final_data.csv")

### Wordclouds

## install.packages("wordcloud")
library(wordcloud)
##install.packages("RColorBrewer")
library(RColorBrewer)
##install.packages("wordcloud2")
library(wordcloud2)
##install.packages("tm")
library(tm)
##install.packages("dplyr")
library(dplyr) ## loading all required packages

## impact wordcloud and top 10 words

impact <- data$ImpactOnYou_Text
impact <- na.omit(impact)
impact_docs <- Corpus(VectorSource(impact)) ## making corpus of impact

impact_docs <- impact_docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
impact_docs <- tm_map(impact_docs, content_transformer(tolower))
impact_docs <- tm_map(impact_docs, removeWords, stopwords("english")) ## removing unnecessary words/punctuation

dtm_impact <- TermDocumentMatrix(impact_docs) 
matrix_impact <- as.matrix(dtm_impact) 
words_impact <- sort(rowSums(matrix_impact),decreasing=TRUE) 
df_impact <- data.frame(word = names(words_impact),freq=words_impact) ## making dtm of words and frequency

set.seed(1234) 
wordcloud(words = df_impact$word, freq = df_impact$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
df_impact[1:10,]

## tasks wordcloud and top 10 words

## repeat above steps but with tasks column
tasks <- data$Tasks_Text
tasks <- na.omit(tasks)
tasks_docs <- Corpus(VectorSource(tasks))

tasks_docs <- tasks_docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
tasks_docs <- tm_map(tasks_docs, content_transformer(tolower))
tasks_docs <- tm_map(tasks_docs, removeWords, stopwords("english"))

dtm_tasks <- TermDocumentMatrix(tasks_docs) 
matrix_tasks <- as.matrix(dtm_tasks) 
words_tasks <- sort(rowSums(matrix_tasks),decreasing=TRUE) 
df_tasks <- data.frame(word = names(words_tasks),freq=words_tasks)

set.seed(1234) 
wordcloud(words = df_tasks$word, freq = df_tasks$freq, min.freq = 1,max.words=99, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))

df_tasks[1:10,]

## population helped wordcloud and top 10 words

population <- data$HelpPopulation_Text
population <- na.omit(population)
population_docs <- Corpus(VectorSource(population))

population_docs <- population_docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
population_docs <- tm_map(population_docs, content_transformer(tolower))
population_docs <- tm_map(population_docs, removeWords, stopwords("english"))

dtm_population <- TermDocumentMatrix(population_docs) 
matrix_population <- as.matrix(dtm_population) 
words_population <- sort(rowSums(matrix_population),decreasing=TRUE) 
df_population <- data.frame(word = names(words_population),freq=words_population)

set.seed(1234) 
wordcloud(words = df_population$word, freq = df_population$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))

df_population[1:10,]

## goals wordcloud and top 10 words

goal <- data$GoalOutcomeAchieved_Text
goal <- na.omit(goal)
goal_docs <- Corpus(VectorSource(goal))

goal_docs <- goal_docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
goal_docs <- tm_map(goal_docs, content_transformer(tolower))
goal_docs <- tm_map(goal_docs, removeWords, stopwords("english"))

dtm_goal <- TermDocumentMatrix(goal_docs) 
matrix_goal <- as.matrix(dtm_goal) 
words_goal <- sort(rowSums(matrix_goal),decreasing=TRUE) 
df_goal <- data.frame(word = names(words_goal),freq=words_goal)

set.seed(1234) 
wordcloud(words = df_goal$word, freq = df_goal$freq, min.freq = 1,max.words=178, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))

print(df_goal[1:10,])

### bigram analysis

library(FactoMineR)
library(factoextra)
library(GGally)
library(ggdendro)
library(igraph)
library(network)
library(Matrix)
library(quanteda)
library(dplyr)
library(stringr)
library(tm)
library(tidyr)
library(tidytext)

# read in text
impact <- impact %>%
  paste0(collapse = " ") %>%
  stringr::str_squish() %>%
  stringr::str_remove_all("- ")
# further processing
impact_split <- impact %>% 
  as_tibble() %>%
  tidytext::unnest_tokens(words, value)
# no stopwords
impact_tidy <- impact_split %>%
  filter(!(words %in% stopwords()))

impact_words <- impact_split %>% ## creating trigrams
  dplyr::rename(word1 = words) %>%
  dplyr::mutate(word2 = c(word1[2:length(word1)], NA)) %>%
  dplyr::mutate(word3 = c(word2[2:length(word2)], NA)) %>%
  na.omit()

impact_words_tidy <- impact_tidy %>% ## bigram
  dplyr::rename(word1 = words) %>%
  dplyr::mutate(word2 = c(word1[2:length(word1)], NA)) %>%
  na.omit()

impact2grams <- data.frame(word1=impact_words_tidy$word1, word2=impact_words_tidy$word2)
impact2grams <- impact2grams %>%
  group_by(word1, word2) %>%
  mutate(frequency = n()) ## bigram sort by n

impact2grams <- impact2grams %>% distinct()

## visualising correlation between bigrams (no stopwords) using directed graph
library(igraph)

bigram_graph <- impact2grams %>%
  filter(frequency >= 5) %>%
  graph_from_data_frame()

library(ggraph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = frequency), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                 end_cap = circle(1, "mm")) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), size = 3) +
  theme_void()

### further text analysis

#Part 1: analyze tri-grams
library(tidyverse) 
library(stringr)
library(ggplot2)

#look at top tri-grams to find key words
impact <- read.csv("impact_trigram_frequency.csv")
vol <- read.csv("final_data.csv")
impact[impact$frequency >= 10,]

#find proportion of responses that use key words
tot_responses <- length(which(!is.na(vol$ImpactOnYou_Text)))
realize <- vol[grep("realize",vol$ImpactOnYou_Text), "ImpactOnYou_Text"]
help <- vol[grep("help",vol$ImpactOnYou_Text), "ImpactOnYou_Text"]
learn <- vol[grep("learn",vol$ImpactOnYou_Text), "ImpactOnYou_Text"]
future <- vol[grep("future",vol$ImpactOnYou_Text), "ImpactOnYou_Text"]
community <- vol[grep("community",vol$ImpactOnYou_Text), "ImpactOnYou_Text"]

length(realize)/tot_responses
length(help)/tot_responses
length(learn)/tot_responses
length(future)/tot_responses
length(community)/tot_responses

#create bar plot of proportions
words <- c("realize", "help", "learn", "future", "community")
percent <- c(6.39, 23.83, 11.74, 2.59, 10.88)

data <- data.frame(words, percent)

ggplot(data, aes(x = words, y = percent)) + geom_bar(stat = "identity", fill = "lightgreen", color = "darkgreen") + ggtitle("Proportion of responses containing key words") +
  xlab("") + ylab("Percent of Responses") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=percent),color="darkgreen",size=2.5,position=position_stack(vjust=0.5))

#part 2: create word cloud 
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

#get words that directly follow feel or feels using regex 
feelings <- str_match(vol$ImpactOnYou_Text, "feels* (\\w+)")[,2]
feelings <- feelings[!is.na(feelings)] 
feelings <- feelings[feelings != "like"]
feelings <- feelings[feelings != "very"]
feelings <- feelings[feelings != "a"]
feelings <- feelings[feelings != "as"]

#Create a vector containing only the text
text <- feelings
# Create a corpus  
docs <- Corpus(VectorSource(text))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
#create word cloud 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=20, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

### sentiment analysis

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(stringr)

library(tm)
library(RColorBrewer)
library(textdata)
library(syuzhet)

# Making Separate Data Frames by Category

#Impact General (all 707 columns)
impact <- data$ImpactOnYou_Text

# Motive Categories
m_personal <- data[data$Motive_PersInterest == 1,]
m_similar <- data[data$Motive_DidSimilarBefore == 1,]
m_req <- data[data$Motive_Req == 1,]
m_pop <- data[data$Motive_AddressIssueHelpPop == 1,]
m_friend <- data[data$Motive_FriendsParticipating == 1,]

# All motive categories in a list
m_impact <- list(m_personal, m_similar, m_req, m_pop, m_friend)

# Find Out Categories
f_online <- data[data$FindOut_Online == 1,]
f_social <- data[data$FindOut_SocialMedia == 1,]
f_frenfam <- data[data$FindOut_FriendFamily == 1,]
f_staff <- data[data$FindOut_StaffFaculty == 1,]
f_email <- data[data$FindOut_Email == 1,]
f_affil <- data[data$FindOut_UCLAaffil == 1,]
f_sturun <- data[data$FindOut_CampusStudentRun == 1,]
f_other <- data[data$FindOut_OtherOutsideUCLA == 1,]

# All find out categories in a list
f_impact <- list(f_online, f_social, f_frenfam, f_staff, f_email, f_affil, f_sturun, f_other)

# Setting Categories
s_office <- data[data$Setting_Office == 1,]
s_hospitals <- data[data$Setting_Hospital == 1,]
s_schools <- data[data$Setting_K12Schools == 1,]
s_govt <- data[data$Setting_GovtBldg == 1,]
s_outdoors <- data[data$Setting_Outdoors == 1,]
s_residence <- data[data$Setting_MyResidence == 1,]
s_ucla <- data[data$Setting_UCLA == 1,]
s_other <- data[data$Setting_OtherOutsideUCLA == 1,]

# All setting categories in a list
s_impact <- list(s_office, s_hospitals, s_schools, s_govt, s_outdoors, s_residence, s_ucla, s_other)

# Sentiment Analysis of Impact (General)
# get all words from impact column
text_words <- get_tokens(impact)
head(text_words)
length(text_words)

# measure sentiment scores 
sentiment_scores <- get_nrc_sentiment(text_words)
head(sentiment_scores)

summary(sentiment_scores)

# graph of sentiments + emotions
barplot(
  colSums(sentiment_scores),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  col = brewer.pal(n = 10, name = "Set3"),
  main = "Emotions and Sentiments of Impact",
  xlab="Emotions (8) /Sentiments (2)", ylab = NULL)

# Sentiment Analysis of Impact based on Motive_FriendsParticipating
text_words <- get_tokens(m_friend$ImpactOnYou_Text)
head(text_words)
length(text_words)

sentiment_scores <- get_nrc_sentiment(text_words)
head(sentiment_scores)

summary(sentiment_scores)

barplot(
  colSums(sentiment_scores),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  col = brewer.pal(n = 10, name = "Set3"),
  main = "Emotions and Sentiments of Motive_FriendsParticipating",
  xlab="Emotions (8) /Sentiments (2)", ylab = NULL)

# Sentiment Analysis of Impact based on FindOut_UCLAaffil
text_words <- get_tokens(f_affil$ImpactOnYou_Text)
head(text_words)
length(text_words)

sentiment_scores <- get_nrc_sentiment(text_words)
head(sentiment_scores)

summary(sentiment_scores)

barplot(
  colSums(sentiment_scores),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  col = brewer.pal(n = 10, name = "Set3"),
  main = "Emotions and Sentiments of FindOut_UCLAaffil",
  xlab="Emotions (8) /Sentiments (2)", ylab = NULL)

# Sentiment Analysis of Impact based on Setting_Hospital
text_words <- get_tokens(s_hospitals$ImpactOnYou_Text)
head(text_words)
length(text_words)

sentiment_scores <- get_nrc_sentiment(text_words)
head(sentiment_scores)

summary(sentiment_scores)

barplot(
  colSums(sentiment_scores),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  col = brewer.pal(n = 10, name = "Set3"),
  main = "Emotions and Sentiments of Setting_Hospital",
  xlab="Emotions (8) /Sentiments (2)", ylab = NULL)

# Code for all 21 sentiment analysis plots

# # Sentiment Analysis Function
# sentiment_analysis <- function(data_list, column_name) {
#   sentiment_scores_list <- list()
#   for (i in seq_along(data_list)) {
#     data <- data_list[[i]]
#     column <- data %>% pull({{column_name}})
#     text_words <- get_tokens(column)
#     sentiment_scores <- get_nrc_sentiment(text_words)
# 
#     barplot(
#       colSums(sentiment_scores),
#       space = 0.2,
#       horiz = FALSE,
#       las = 1,
#       cex.names = 0.6,
#       col = brewer.pal(n = 10, name = "Set3"),
#       main = paste0("Emotions and Sentiments of Impact in Data Frame", i),
#       xlab="Emotions (8) /Sentiments (2)", ylab = NULL)
#     
#     sentiment_scores_list[[i]] <- sentiment_scores
#   }
#   
#   return(sentiment_scores_list)
# }
# 
# # Sentiment Analysis of Impact by Motive
# sentiment_analysis(m_impact, "ImpactOnYou_Text")
# 
# # Sentiment Analysis of Impact by Find Out
# sentiment_analysis(f_impact, "ImpactOnYou_Text")
# 
# # Sentiment Analysis of Impact by Setting
# sentiment_analysis(s_impact, "ImpactOnYou_Text")

### ANOVA

################
# MOTIVE
#################

library(tidyverse)
library(ggplot2)

cleaned.data <- read.csv("final_data.csv")

names(cleaned.data)

# Make Impact Length Variable 
impact.length <- c()
for(i in 1:nrow(cleaned.data)){
  if(is.na(cleaned.data$ImpactOnYou_Text[i])){
    impact.length[i] <- 0 
  } else{
    impact.length[i] <- nchar(cleaned.data$ImpactOnYou_Text[i])
  }
  impact.length <- as.matrix(impact.length)
}
cleaned.data$impact.length <- impact.length

#create variables with only motive
motive <- cleaned.data[,c(28:33,36)]

num_motives <- as.factor(rowSums(motive[,c(1:6)]))
motive <- cbind(motive, num_motives)

motive

#run anova
motive_aov <- aov(impact.length ~ as.factor(impact.length), data=motive)
summary(motive_aov)

#create plot of means
data <- motive %>% group_by(num_motives) %>% summarize(mean = mean(impact.length))
data
ggplot(data, aes(x = num_motives, y = mean)) + geom_bar(stat = "identity", fill = "lightgreen", color = "darkgreen") + ggtitle("Mean Impact Length Based on Number of Motives Selected") +
  xlab("Number of Motives Selected") + ylab("Mean") + theme(plot.title = element_text(hjust = 0.5))

library(tidyr)

################
# SETTING
#################

# Remove people who chose multiple settings
sum(rowSums(cleaned.data[,15:22]) > 1)
indset <- rowSums(cleaned.data[,15:22]) <= 1
set_cd <- cleaned.data[,c(1, 15:22, 36)]
ob_set <- set_cd[indset, ]

# Pivot to make setting a categorical variable 
data.setting <- pivot_longer(data = ob_set, names_to = "Setting", values_to = "Yes", Setting_Office:Setting_OtherOutsideUCLA)
data.setting

data.set.1 <- data.setting[data.setting$Yes == 1, ]
data.set.1 <-data.set.1[,1:3]

# Run ANOVA AND LM 
setting.new.anova <- aov(impact.length ~ Setting, data=data.set.1)

summary(setting.new.anova)

setting.new.lm <- lm(impact.length ~ Setting, data=data.set.1)

summary(setting.new.lm)

##################
# Find Out
##################

# Remove people who chose multiple settings
sum(rowSums(cleaned.data[,2:9]) > 1)
ind2 <- rowSums(cleaned.data[,2:9]) <= 1
fo_cd <- cleaned.data[,c(1:9, 36)]
ob <- fo_cd[ind2, ]

# Pivot to make FindOut a categorical variable 
data.findout <- pivot_longer(data = ob, names_to = "FindOut", values_to = "Yes", FindOut_Online:FindOut_OtherOutsideUCLA)
data.findout

data.find.1 <- data.findout[data.findout$Yes == 1, ]
data.find.1 <-data.find.1[,1:3]

# Run ANOVA AND LM 
find.new.anova <- aov(impact.length ~ FindOut, data=data.find.1)

summary(find.new.anova)

find.new.lm <- lm(impact.length ~ FindOut, data=data.find.1)

summary(find.new.lm)

# Frequency # Not Significant 

freq <- cleaned.data[, c(1, 24, 36)]
ind <- freq$Frequency == "Other (please describe below)"
freq_cestchique <- freq[, -ind]

freq.new.anova <- aov(impact.length ~ Frequency, data=freq_cestchique)

summary(freq.new.anova)

freq.new.lm <- lm(impact.length ~ Frequency, data=freq_cestchique)

summary(freq.new.lm)

### Plot for Mean Impact Length vs. Setting
library(dplyr)
set_means <- data.set.1 %>% group_by(Setting) %>% 
  summarise(Mean_Impact_Length = mean(impact.length))

setting_means <- data.frame(Setting = c("GovtBldg", "Hospital", "K12Schools", "MyResidence",
                                        "Office", "OtherOutsideUCLA", "Outdoors", "UCLA"),
                            set_means[,2])

ggplot(setting_means, aes(x = Setting, y = Mean_Impact_Length)) +
  geom_bar(stat= "identity", fill = "lightgreen", color = "darkgreen") +
  ggtitle("Mean Impact Length Based on Setting") + xlab("Setting") + ylab("Mean Impact Length")


### Plot for Mean Impact Length vs. FindOut
findout_means <- data.find.1 %>% group_by(FindOut) %>% 
  summarise(Mean_Impact_Length = mean(impact.length))

find_means <- data.frame(FindOut = c("CampusStudentRun", "Email", 
                                     "FriendFamily", "Online", "OtherOutsideUCLA",
                                     "SocialMedia", "StaffFaculty", "UCLAaffil"), 
                         findout_means[,2])

ggplot(find_means, aes(x = FindOut, y = Mean_Impact_Length)) +
  geom_bar(stat= "identity", fill = "lightgreen", color = "darkgreen") +
  ggtitle("Mean Impact Length Based on How Student Found Out") + xlab("FindOut") + ylab("Mean Impact Length")






#Select top 50 cities of Turkey:
grepl("İzmit|İstanbul|Ankara|Türkiye|Istanbul|İzmir|Turkey|Izmir|Bursa|Antalya|Konya|
        Adana|Gaziantep|Şanlıurfa|Mersin|Diyarbakır|Kayseri|Samsun|Denizli|Eskişehir|
        Adapazarı|Malatya|Kahramanmaraş|Erzurum|Van|Batman|Elazığ|Manisa|Sivas|Gebze|
        Balıkesir|Tarsus|Kütahya|Trabzon|Çorum|Çorlu|Adıyaman|Osmaniye|Kırıkkale|Antakya|
        Aydın|İskenderun|Uşak|Aksaray|Afyon|Isparta|İnegöl|Tekirdağ|Edirne|Darıca|Ordu|
        Karaman|Gölcük|Siirt|Siirt|Körfez", d$user_location)

# —------------------------------------------------
#Merve’s code

library(readr)
data <- read_csv("~/Desktop/turkey_earthquake_tweets.csv")

# creating a new dataframe subset of variables we will use 
data2 <- data[c("id", "user_name", "user_location", "date", "text", "hashtags", "source","retweets", "favorites")]
# removed is_retweet because it only shows "FALSE" or "NA"

library(lubridate)
# creating new variables for specific day in February 2023 and hour tweet was sent (24hr format)
data2$day <- day(data2$date) # all days are in February 2023, the days range from 7th to 21st
data2$hour <- hour(data2$date)

library(countrycode)
city_country<-read.csv("https://raw.githubusercontent.com/girijesh18/dataset/master/City_and_province_list.csv")

# custom_dict for countrycode cannot have duplicate origin codes
city_country <- city_country[!duplicated(city_country$City), ]

# creating new variable for cities/countries listed in “user_location”
data2$country<- countrycode(data2$user_location, "City", "Country", custom_dict = city_country)

# number of tweets sent from each country 
table(data2$country)

# creating new dataframe for tweets sent from only Turkey
data3 <- data2[which(data2$country == "Turkey"),]

# creating new dataset for country value not NA
data4 <- data2[!is.na(data2$country),]

# removing unnecessary words 
library(tm)

unnecessary<-c("the","in","to","of","a","and","for","https","is","from","you","are","this", 
               "have", "i", "by", "that", "after", "on", "we", "httpstco", "amp", "with", 
               "their", "who", "at", "t", "co", "//", "their", "our", "sr", "we", "your", 
               "was", "has", "all", "as", "it", "can")

new_text_data2 <- removeWords(tolower(data2$text), unnecessary)
new_text_data3 <- removeWords(tolower(data3$text), unnecessary)
new_text_data4 <- removeWords(tolower(data4$text), unnecessary)

# installing every package below was too much but I followed this site: https://cran.r-project.org/web/packages/RSCAT/readme/README.html#:~:text=For%20Mac%20OS,them%20and%20run%20it%20again. 
library(RSCAT)
library(rJava)
library(qdap)

# finding 15 most frequent words used in the tweets from Turkey
frequent_terms_turkey <- freq_terms(new_text_data3, 15)
frequent_terms_turkey

# finding 15 most frequent words used in the tweets from all countries
frequent_terms_all_countries <- freq_terms(new_text_data4, 15)
frequent_terms_all_countries

# finding 15 most frequent words used in the tweets from entire dataset
frequent_terms_all <- freq_terms(new_text_data2, 15)
frequent_terms_all

# nicer plot
library(ggplot2)

ggplot(frequent_terms_all, aes(x = FREQ, y = WORD, fill=WORD)) +
  geom_bar(stat = "identity", width=0.5) +
  labs(x = "Frequency", y = "Word") +
  ggtitle("Top 10 Words Used in the Tweets from Entire Dataset") +
  theme_minimal() + theme(legend.position="none")

ggplot(frequent_terms_all_countries, aes(x = FREQ, y = WORD, fill=WORD)) +
  geom_bar(stat = "identity", width=0.5) +
  labs(x = "Frequency", y = "Word") +
  ggtitle("Top 10 Words Used in the Tweets by Country") +
  theme_minimal() + theme(legend.position="none")

ggplot(frequent_terms_turkey, aes(x = FREQ, y = WORD, fill=WORD)) +
  geom_bar(stat = "identity", width=0.5) +
  labs(x = "Frequency", y = "Word") +
  ggtitle("Top 10 Words Used in the Tweets from Turkey") +
  theme_minimal() + theme(legend.position="none")

# adding onto Leo's bar chart
fre<-table(data2$country)
fre_data<-tibble("country"= names(fre), "fre"= unname(fre))
ggplot(data=fre_data,aes(x=fre, y=country, fill=country)) +
  geom_bar(stat = "identity")+
  labs(y = "Country", x = "Number of Tweets") + 
  ggtitle("Number of Tweets Per Country") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() + theme(legend.position="none")

# idea from https://bookdown.org/yann_ryan/r-for-newspaper-data/term-frequencies.html 
# tokenizing to find the frequent words in text
library(tidytext)

data("stop_words")

tokenised_data2 = data2 %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  filter(!word %in% c('superscript', 
                      'style', 
                      'de', 
                      'thle', 
                      'tile', 
                      'tie', 
                      'tire', 
                      'tiie', 
                      'tue',
                      'amp')) %>% 
  filter(!str_detect(word, '[0-9]{1,}')) %>% 
  filter(nchar(word) > 2)

tokenised_data2 %>% 
  group_by(word) %>% 
  tally() %>% 
  arrange(desc(n)) %>% head(20)

# top 5 words for each day in dataset
tokenised_data2 %>% 
  group_by(day, word) %>% 
  tally() %>% 
  arrange(day, desc(n)) %>% 
  group_by(day) %>% 
  top_n(5) %>% head(100)

# Check the top words per retweet count?
tokenised_data2 %>% 
  group_by(retweets, word) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  group_by(retweets) %>% 
  top_n(5)

#words over time
tokenised_data2 %>%
  filter(word == 'turkeyearthquake') %>% 
  group_by(day, word) %>% 
  tally() %>% ggplot() + geom_col(aes(x = day, y = n))

This one is nice: 
  # Check the top words per retweet and day
  nice_df = tokenised_data2 %>% 
  group_by(retweets, day, word) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  group_by(retweets) %>% 
  top_n(5)


# Leo’s Code
#Create bar chart for tweets in each country:

fre<-table(data2$country)

fre_data<-data_frame("country"= names(fre), "fre"= unname(fre))

ggplot(data=fre_data,aes(x=fre, y=country))+geom_bar(stat = "identity")+labs(
  title = "Number of Tweets for Each Country",
  y = "Country", x = "Number of Tweets"
)


#Bar chart for the percteage of tweets in Turkey:
library(dplyr)
fre<-table(data3$user_location)
fre_data<-tibble("city"= names(fre), "fre"= unname(fre))


pct <- round(fre_data$fre/sum(fre_data$fre)*100)
lbls <- paste(pct,"%",sep="") # ad % to labels

pie(fre_data$fre, labels = lbls, main = "Pie Chart for the Number of Tweets in Turkey",col = rainbow(length(fre_data$fre)))

legend("topright", fre_data$city, cex = 0.5,fill = rainbow(length(fre_data$fre)))

#Bar chart for the percteage of likes in Turkey(Leo):
um_likes_tbl <- data3 %>% group_by(user_location) %>% 
summarise(sum_likes = sum(favorites),
            .groups = 'drop')
sum_likes_tbl

pct <- round(sum_likes_tbl$sum_likes/sum(sum_likes_tbl$sum_likes)*100)
lbls <- paste(pct,"%",sep="") 
pie(sum_likes_tbl$sum_likes, labels = lbls, main = "Pie Chart for the Number of Favorites in Turkey",col = rainbow(length(sum_likes_tbl$sum_likes)))
legend("topright", sum_likes_tbl$user_location, cex = 0.5,
       fill = rainbow(length(sum_likes_tbl$sum_likes)))


#Mianbo’s code

library(tm)
library(wordcloud)

custom_stopwords <- c("the","in","to","of","a","and","for","https","is","from","you","are",
                      "this", "have", "i", "by", "that", "after", "on", "we", "httpstco", 
                      "amp", "with", "their", "who", "at", "t", "co", "//", "their", "our", 
                      "sr", "we", "your", "was", "has", "all", "as", "it", "can")

words <- unlist(strsplit(tolower(paste(data$text, collapse = " ")), "\\W+"))
words <- words[!(words %in% stopwords("english") | words %in% custom_stopwords)]

word_counts <- table(words)

wordcloud(names(word_counts), freq = word_counts, scale=c(8,0.5), min.freq = 3, 
          random.order = FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(purrr)
library(readxl)
library(readr)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(wordcloud2)

# Import Tweets Dataset
ds <- read.csv("D:/Documents/Codes/Twitter/Others/cat.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";")

# Create Corpus Object
word = ds %>% select(username, full_text)
word
head(word$full_text, 5)

# Delete "http" element
word$stripped_text1 <- gsub("http\\S+","",word$full_text)

# Convert to Lowercase, Deleting Punctuations, and ID for every tweets
word_stem <- word %>% select(stripped_text1) %>% unnest_tokens(word, stripped_text1)
head(word_stem, 5)

# Delete stopwords for every tweets that is stored in word_stem
cleaned_tweet <- word_stem %>% anti_join(stop_words)
head(cleaned_tweet, 5)

# Filter words
cleaned <- cleaned_tweet %>% filter(cleaned_tweet!="di"&cleaned_tweet!="yg"&
                                    cleaned_tweet!="ini"&cleaned_tweet!="dia"&
                                    cleaned_tweet!="aja"&cleaned_tweet!="pak"&
                                    cleaned_tweet!="sun"&cleaned_tweet!="dan"&
                                    cleaned_tweet!="yang"&cleaned_tweet!="ada"&
                                    cleaned_tweet!="ke"&cleaned_tweet!="itu")

# Count the 20 most unique words
cleaned %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x="Unique Words",
       y="Count",
       title="Word Frequency Graph for Tweets containing 'Kucing'")

# Create wordcloud
# Create corpus object
docs <- Corpus(VectorSource(cleaned))

# Delete numbers, punctuations, white spaces
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

# Create term document Matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.25,
          colors=brewer.pal(8, "Dark2"))

wordcloud2(data=df, size = 0.4, shape = 'circle')

# Perform sentiment analysis using Bing 
# This command below returns a tibble
bing_kata = cleaned_tweet %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()

# Word counts visualization
# Perform Filtering and Plotting on every tweets to compare
# tweets with positive and negative emotions
bing_kata %>% group_by(sentiment) %>% top_n(30) %>% ungroup() %>% 
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment))+ 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(title = "Tweets containing 'Kucing'", y = "Contribution to sentiment", 
       x = NULL) + coord_flip() + theme_bw()

# Create a function to get sentiment scores for every tweets
sentiment_bing = function(twt){
  twt_tbl = tibble(text = twt) %>%
    mutate(
      stripped_text = gsub("http\\S+","",text)
    )%>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment, sort = TRUE) %>%
    ungroup() %>%
    #buat kolom "skor", yang menetapkan -1 untuk semua kata negatif, 
    #dan 1 untuk kata positif
    mutate(
      score = case_when(
        sentiment == 'negative'~n*(-1),
        sentiment == 'positive'~n*1)
    )
  #menghitung total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #jika tidak ada kata, skor adalah 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #selainnya, jumlah positif dan negatif
  )
  #untuk melacak tweet mana yang tidak mengandung kata sama sekali dari daftar bing
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", #Type 1: tidak ada kata sama sekali, zero = no
    nrow(twt_tbl)>0~"Type 2" #Type 2: nol berarti jumlah kata = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

# Implement lapply() function
# lappy() function returns a list of every sentiment scores, types, and tweets tables
kata_sent = lapply(word$full_text, function(x){sentiment_bing(x)})   
kata_sent

# Create tibble that determines words, scores, and those kinds
kata_sentiment = bind_rows(tibble(word = 'kucing',
                                  score = unlist(map(kata_sent, 'score')),
                                  type = unlist(map(kata_sent, 'type'))))

# We could see the sentiment characteristics in every groups
# Create a histogram graph of tweets sentiments
ggplot(kata_sentiment, aes(x=score, fill = word)) + geom_histogram(bins = 15, alpha= .6) +
  facet_grid(~word) + theme_bw() + ggtitle("Histogram Plot of Kucing Sentiments Analysis")

# Count how many Tweets with negative emotions toward Kucing
count_neg <- sum(kata_sentiment$score < 0)
cat("Jumlah Tweets dengan emosi NEGATIF terhadap Kucing:", count_neg)

# Count how many Tweets with positive emotions toward Kucing
count_pos <- sum(kata_sentiment$score > 0)
cat("Jumlah Tweets dengan emosi POSITIF terhadap Kucing:", count_pos)
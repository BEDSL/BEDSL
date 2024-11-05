# Installing and loading the packages

if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}
if(!require("quanteda.textstats")) {install.packages("quanteda.textstats"); library("quanteda.textstats")}
if(!require("readtext")) {install.packages("readtext"); library("readtext")}
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if(!require("RColorBrewer")) {install.packages("RColorBrewer"); library("RColorBrewer")}
if(!require("corpus")) {install.packages("corpus"); library("corpus")}
if(!require("tm")) {install.packages("tm"); library("tm")}
if(!require("RWeka")) {install.packages("RWeka"); library("RWeka")}
if(!require("tidyr")) {install.packages("tidyr"); library("tidyr")}
if(!require("textmineR")) {install.packages("textmineR"); library("textmineR")}
if(!require("tidytext")) {install.packages("tidytext"); library("tidytext")}
if(!require("topicmodels")) {install.packages("topicmodels"); library("topicmodels")}
if(!require("reshape2")) {install.packages("reshape2"); library("reshape2")}
if(!require("wordcloud")) {install.packages("wordcloud"); library("wordcloud")}
if(!require("wordcloud2")) {install.packages("wordcloud2"); library("wordcloud2")}
if(!require("igraph")) {install.packages("igraph"); library("igraph")}
if(!require("ggraph")) {install.packages("ggraph"); library("ggraph")}
if(!require("factoextra")) {install.packages("factoextra"); library("factoextra")}
if(!require("widyr")) {install.packages("widyr"); library("widyr")}
if(!require("stats")) {install.packages("stats"); library("stats")}
if(!require("readr")) {install.packages("readr"); library("readr")}
if(!require("ldatuning")) {install.packages("ldatuning"); library("ldatuning")}
if(!require("stm")) {install.packages("stm"); library("stm")}
if(!require("readxl")) {install.packages("readxl"); library("readxl")}
if(!require("visNetwork")) {install.packages("visNetwork"); library("visNetwork")}
if(!require("networkD3")) {install.packages("networkD3"); library("networkD3")}

# Setting the working directory
getwd()
setwd("C:/Users/RUXI/Desktop/X/files_fara_jobs")


# Reading the files
data <- readtext("*.txt") 
data <- unique(data[,2])
data <- gsub("[^\x01-\x7F]", "", data)
texts = data

data <- as.data.frame(data)
data$doc_id <- 1:nrow(data)
colnames(data)[1] ="text"
View(data)

my.corpus <- corpus(texts)
docvars(my.corpus, "Number") <- sprintf("%02d", 1:ndoc(my.corpus))
my.corpus


# Create the summary statistics of corpus file
my.corpus.stats <- summary(my.corpus, n = ndoc(my.corpus))
my.corpus.stats$Text <- reorder(my.corpus.stats$Text, 1:ndoc(my.corpus), order = T)
my.corpus.stats %>%
  arrange(desc(Types))

# Document Feature Matrices (DFMs)
# Create dfm from corpus + data preparation + stop words
my.dfm <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove =c(stopwords("english")))
my.dfm <- dfm(my.dfm,remove = c( "us", "will", "can", "also", "sustainability", "initiative"))
# DFM sorted by prevalent features
head(dfm_sort(my.dfm, decreasing = TRUE, margin = "both")) 
topfeatures(my.dfm) # basic word frequencies
word.frequencies <- textstat_frequency(my.dfm) # more elaborate frequencies
head(word.frequencies, 20)


#-------Visualizing DFMs---------
wordcloud2(word.frequencies, color = "random-dark", backgroundColor = "white",size = 0.5, shape = "triangle")
wordcloud2(word.frequencies)


#-------Tidy the data set-----------
data_continut <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Customize some stopwords 
my_stop_words <- bind_rows(stop_words, 
                           tibble(word = c("us", "will", "can", "also", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), 
                                  lexicon = "custom"))

data_continut<- data_continut %>%
  anti_join(my_stop_words)

rownames(data_continut) <- NULL



#-------BI-GRAMS-------------------
linkedin_bigrams <- data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
linkedin_bigrams   #pairs of common words

#Eliminating stop words
bigrams_separated <- linkedin_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united    #pairs of uncommon words


#Bigram counts: most used word pairs 
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

#Trigram counts
trigrams = data %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% my_stop_words$word,
         !word2 %in% my_stop_words$word,
         !word3 %in% my_stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
trigrams

#Graphics of bigrams and trigrams
library(ggplot2)
library(dplyr)
library(forcats)


bigram_counts$pair <- paste(bigram_counts$word1,bigram_counts$word2)
x <- data.frame(bigram_counts[,4], bigram_counts[,3])[1:10,]
colnames(x) <- c("bigram","freq")

ggplot(x, aes(x = freq , y = reorder(bigram, -freq), fill=freq )) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="oldlace", high="dodgerblue2") + #gray88
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
  theme(axis.text = element_text(face="bold"))+
  theme(legend.position="none") +
  xlab("Count") + 
  ylab("Bigrams") 

trigrams$pair <- paste(trigrams$word1,trigrams$word2, trigrams$word3)
x <- data.frame(trigrams[,5], trigrams[,4])[1:10,]
colnames(x) <- c("trigram","freq")

ggplot(x, aes(x = freq , y = reorder(trigram, -freq), fill=freq )) + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="oldlace", high="dodgerblue2") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
  theme(axis.text = element_text(face="bold"))+
  theme(legend.position="none") +
  xlab("Count") + 
  ylab("Trigrams") 


#Analysing bigrams
bigrams_filtered %>%
  filter(word2 == "energy") %>%
  count( word1, sort = TRUE)  #what type of energy is wanted
bigrams_filtered %>%
  filter(word1 == "solar") %>%
  count( word2, sort = TRUE) #what can be said about the solar market
bigrams_filtered %>%
  filter(word1 == "water") %>%
  count( word2, sort = TRUE) #what can be said about the water market

bigrams_filtered %>%
  filter(word2 == "management") %>%
  count( word1, sort = TRUE) #what to manage

bigrams_filtered %>%
  filter(word1 == "promote") %>%
  count( word2, sort = TRUE) #what is being promoted
bigrams_filtered %>%
  filter(word1 == "support") %>%
  count( word2, sort = TRUE) #what to support

bigrams_filtered %>%
  filter(word1 == "green") %>%
  count( word2, sort = TRUE) #what is green


bigrams_filtered %>%
  filter(word2 == "industry") %>%
  count( word1, sort = TRUE) #what industries are most talked about

bigrams_filtered %>%
  filter(word2 == "company") %>%
  count( word1, sort = TRUE) #what types of companies

bigrams_filtered %>%
  filter(word2 == "challenge") %>%
  count( word1, sort = TRUE) #challenges

bigrams_filtered %>%
  filter(word2 == "technology") %>%
  count( word1, sort = TRUE) #what is expected of technology?

bigrams_filtered %>%
  filter(word2 == "development") %>%
  count( word1, sort = TRUE) #things under development



#Visualizing a network of bigrams with ggraph
bigram_graph <- bigram_counts %>%
  filter(n > 13) %>%
  graph_from_data_frame()

bigram_graph
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()



#-------Content word pairs ----------------
continut_word_pairs <- data_continut %>% 
  pairwise_count(word, doc_id, sort = TRUE, upper = FALSE)

continut_word_pairs %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))


# Content word correlations
continut_word_cors <- data_continut %>% 
  anti_join(my_stop_words) %>%
  group_by(word) %>%
  filter(n() >= 130) %>%
  pairwise_cor(word, doc_id, sort = TRUE, upper = FALSE)

continut_word_cors %>%
  filter(correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()



#-------Topic modelling----------------------
my_stop_words <- bind_rows(stop_words, 
                           tibble(word = c( 'global','business','2023','world', 'environmental','esg', 'future', 'climate', "energy","saudi", "initiative", "us", "will", "can", "also", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "green", "initiatives", "sustainable", "sustainability"), 
                                  lexicon = "custom"))

word_counts <- data_continut %>%
  anti_join(my_stop_words) %>%
  count(doc_id, word, sort = TRUE) %>%
  ungroup()

desc_dtm <- word_counts %>%
  cast_dtm(doc_id, word, n)

desc_dtm

desc_lda <- LDA(desc_dtm, k = 3, control = list(seed = 1234))
desc_lda


tidy_lda <- tidy(desc_lda)
tidy_lda

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms


top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 3, scales = "free")



#-------Sentiment Analysis-----

library(tidytext)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

bing_word_counts <- my.dfm %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

#-------------Try-----------------














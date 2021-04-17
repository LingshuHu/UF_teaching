
################# install required packages #############
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("wordcloud", "ggplot2", "ggthemes", "rtweet", "dplyr", "quanteda", "tokenizers"))

################ Obtain tweets about florida gators ###############
rt <- rtweet::search_tweets(
  "football Florida Gators OR gator", # key words for searching
  n = 5000, 
  include_rts = TRUE, 
  lang = "en", 
  retryonratelimit = FALSE
)

## save tweets
#saveRDS(rt, "UF_teaching/data/florida_gators_football.rds")

###################### Word clouds #########################

### preprocess data
texts <- unlist(rtweet::plain_tweets(rt$text)) # obtain texts
texts <- gsub("@|#", "", texts)
# tokenize
wds <- tokenizers::tokenize_tweets(texts, lowercase = TRUE, stopwords = stopwords::stopwords("en"), 
                                   strip_punct = TRUE, strip_url = FALSE, simplify = FALSE)

dfs <- lapply(wds, data.frame, stringsAsFactors = F)
dfs <- dplyr::bind_rows(dfs)

words <- table(dfs)
words <- tibble::data_frame(
  word = names(words),
  n = as.integer(words)
)

words <- as.data.frame(words)
words <- words[!words$word %in% c("florida", "#Florida", "football", "gators", "gator", "uf") ,]

### display word cloud

wordcloud::wordcloud(
  words$word, words$n, 
  min.freq = 1,
  max.words=200,
  rot.per=0.35, 
  #max.words = 20, 
  random.color = FALSE, 
  colors=brewer.pal(8, "Dark2")
  #colors = c("grey", "skyblue", "green3", "orange", "tomato")
  #scale = c(4, 0.75)
)


######################## Emotion Detection ##########################

## obtain dictionary
data(data_dictionary_NRC, package = "quanteda.dictionaries")

dict <- dfm(rt$text) %>%
  dfm_lookup(dictionary = data_dictionary_NRC) 

dict <- convert(dict, to = "data.frame")

dictsum <- as.data.frame(apply(dict[, 2:11], 2, sum))
dictsum_df <- data.frame(emotion = rownames(dictsum), value = dictsum[, 1])

ggplot(dictsum_df, aes(x = emotion, y = value[order(value)])) + 
  geom_bar(stat = "identity", fill = "tan1", width = 0.5) + 
  ggthemes::theme_gdocs() +
  theme(axis.title.x = element_blank()) +
  coord_flip()

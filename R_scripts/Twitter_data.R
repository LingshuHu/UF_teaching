
################# install required packages #############
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

check.packages(c("wordcloud", "ggplot2", "ggthemes", "rtweet", "dplyr", "quanteda", "tokenizers"))


################ Obtain tweets ###################
## get tweets via keywords
rt <- rtweet::search_tweets(
  "COVID", # key words for searching
  n = 1000, 
  include_rts = TRUE, 
  lang = "en", 
  retryonratelimit = FALSE
)

## save tweets
write.csv(rt, "data/COVID_Tweets.csv")
## read dataset
rt <- read.csv("data/COVID_Tweets.csv")

## get tweets via user names
usr <- rtweet::get_timeline(
  "nytimes", 
  n = 1000, 
  token = rtweet::bearer_token(), 
  check = F
)

################# Explore Twitter data ######################

View(rt)
colnames(rt)

rt$text[1:5]

rt$location[1:5]

rt[1:3, 1:5]

rt2 <- subset(rt, retweet_count > 1000 & is_retweet)[, c("retweet_count", "text")]
## sort colums
rt2[order(rt2$retweet_count, decreasing = TRUE),]

## regular language
rt[rt$retweet_count > 1000, ]
## 18 years old; 18 year old; 18-year-old; 18years old; 18yearsold
rt_age <- subset(rt, grepl("[0-9]+(\\s|-)?years?(\\s|-)?old", text, ignore.case = TRUE))
rt_age$text <- sub("coronavirus|COVID\\s+|COVID19", "COVID-19", rt_age$text)
rt_age$text

## group by
group_by(rt, is_retweet) %>% summarise(mean = mean(retweet_count))
group_by(rt, is_retweet) %>% summarise(n = n(retweet_count))

## visualization
ggplot(rt, aes(x = is_retweet, fill = is_retweet)) + 
  geom_bar(stat="count", width = .5)

ggplot(rt, aes(x = friends_count)) + 
  geom_density(fill = "lightblue", alpha = 0.7)

ggplot(subset(usr, favorite_count > 0 & retweet_count > 0), 
       aes(x = log(favorite_count), y = log(retweet_count))) + 
  geom_point(color = "tomato2") +
  geom_smooth(se = F, method = "lm", formula = "y ~ x", color = "skyblue")

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
  random.color = FALSE, 
  colors=brewer.pal(8, "Dark2")
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

######################### latent semantic analysis #######################

wv <- quanteda::dfm(rt$text)

quanteda::textstat_simil(wv[, c("covid", "india")],  method = "cosine", margin = "features")

quanteda::textstat_simil(wv[, c("covid", "china")],  method = "cosine", margin = "features")





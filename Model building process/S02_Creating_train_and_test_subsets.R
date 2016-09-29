###############################################################################
# Creating the text corpus for training our models and the text subset for
# evauating performance. We start from the clean source files
##############################################################################
#
news <- readRDS("clean_news.rds")
blogs <- readRDS("clean_blogs.rds")
twitter <- readRDS("clean_twitter.rds")

# Aggregating sources in one corpus
text <- c(news, blogs, twitter)

# Shuffling elements for avoiding any ulterior bias due to their positions
set.seed(150658)
text <- text[sample(1:length(text), length(text))]

# Generating the train and test corpus
set.seed(15061958)
train <- sample(1:length(text), length(text) * 0.8)

text_corpus <- text[train]
text_test  <- text[-train]

# Saving the subsets
saveRDS(text_corpus, file = "text_corpus.RDS")
saveRDS(text_test, file = "text_test.RDS")
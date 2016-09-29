################################################################################
# Read the text from one of the three different sources (blogs,
# news, and twitter), and:
#
# 1. Cleaning the text from non-latin, numbers, urls, profanities, etc.
#
# 2. As we are not concerned with analyzing each particular element of the text, 
#    pasting all of them in a reduced set, with a bigger, similar average 
#    number of character. This will make easier the genaration process of 
#    ngrams (futher step) with limited CPU/RAM resources.
#
# 3. Generating and saving the clean corpus for the selected source.
# 
################################################################################

##############################################################
# Parameters that control the operations
###########################################################

# Operation sample mode = yes|no
sample_mode = "no"

# Choosing the type of source
type = "twitter"

# Average block size in number of characters
blockSize <- 10000000

#######################################################

library(tm)

# Loading the cleaning function
source("F01_1_Cleaning_source_steps.R")

# Reading the source file
cat("\n Loading", type, "file ...")
source <- paste0("../../data/en_US/en_US.", type, ".txt")
text_source <- readLines(source, encoding = "UTF-8")

# If sample mode is active, get a sample of 20% of the data set
if (sample_mode == "yes"){
  set.seed(150658)
  text_source <- text_source[sample(1:length(text_source), 
                                    floor(length(text_source) * 0.2))]
}

# Cleaning the source
cat("\n Cleaning", type, "texts")
text <- cleaningSource(text_source)

# Saving the cleaned source file
if (sample_mode == "yes"){
  file <- paste0("clean_", type, "_sample.rds")
} else {
  file <- paste0("clean_", type, ".rds")
}
saveRDS(text, file)

# Cleaning the environment
rm(list = ls()); gc(verbose = FALSE)
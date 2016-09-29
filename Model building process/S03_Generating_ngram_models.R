################################################################################
# This generate the corresponding Ngram model for a given corpus with the
# option of doing a pruning for ngrams whose frequency is below a minimum.
#
# It uses de 'stylo' package, faster and more covenient than 'tm' when you are
# not analyzing frequency per document in a corpus, but total frequency
################################################################################

rm(list = ls()); gc(verbose = FALSE)

library(stringr)
library(dplyr)
library(data.table)

source("F03_1_proc_text.R")
source('F03_2_cond_prob.R')
source("F03_3_smooth_prob.R")


########################################################
# Parameters that control the tokenizing operation
########################################################

# the size of the ngram (uni-, bi-, tri-, tetra-)
ngramSize <- 4

# the minimum frequency to keep a term in the pruning process (1 means no
# intermediate pruning at all)
minFreq <- 2

# Percentage of corpus to be procesed
samplePercentage <- 100

# The process could be splited in fragments: many fragments could help to
# reduce the growth of memory but if pruning is activated many ngrams could be
# removed
numFrag <- 50

# Number of fragments where intermediate pruning is activated. If equal to
# numFrag is executed one time at the end
pruning <- 25

# Starting the process
cat("\nGenerating ngrams of size", ngramSize, "over", 
    samplePercentage, "%", "of the corpus")
cat("\nPruning frequencies below", minFreq)
totTime <- Sys.time()

# Loading corpus data
iniTime <- Sys.time()
cat("\n Loading data... ")
text <- readRDS("text_corpus.RDS")

# Selecting the percentage of elements established
set.seed(15061958)
text <- text[sample(1:length(text), 
                    ceiling(length(text) * samplePercentage / 100))]

#########################################################
# 1. Creating the ngram frequency table
#######################################################

# value of number of elements en each fragment 
ind <- ceiling(length(text)/numFrag)

# Initializing ngram frequency table
ngramFreqTot <- data.table(ngram = character(), freq = integer())

# Processing each block (fragment) of corpus
for (i in 1:numFrag){
    
    # Calculating indices
    ini <- (i - 1) * ind + 1
    if (i == numFrag) {
        fin <- length(text)
    } else {
        fin <- i * ind 
    }
    
    # Selecting elements of the block
    text_frag <- text[ini:fin]
    
    # Reporting status
    cat("\n\nProcessing block", i, "of", numFrag, 
        "/ Memory used:", memory.size(TRUE), "MB.")
    
    # Generating frequency table of the block
    fragFreqTot <- generateNgramFreq(text_frag, ngramSize)
    
    # Merging this block with the table so far
    cat("\nMerging blocks...")
    iniTime <- Sys.time()
    ngramFreqTot <- 
        merge(ngramFreqTot, fragFreqTot, by = c("ngram"), all = TRUE)[, 
        .SD[, .(freq = sum(freq.x, freq.y, na.rm = TRUE)), by = ngram]]
    
    # Clening memory and reporting status
    rm(text_frag, fragFreqTot); gc(verbose = FALSE)
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
    
    # Pruning the frequency table
    if (i %% pruning == 0){
        cat("\nPruning table...")
        iniTime <- Sys.time()
        ngramFreqTot <- ngramFreqTot[freq >= minFreq]
        procTime <- difftime(Sys.time(), iniTime, units = "secs")
        cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
    }
    
    # Saving partial negram table. Just in case the program chrases due to
    # memory problems at least a high percentage of frequencies can be
    # recovered
    cat("\nSaving partial ngrams frequency table...")
    iniTime <- Sys.time()
    fileName <- paste0("ngramFreq_", ngramSize, "_partial.rds")
    saveRDS(ngramFreqTot, file = fileName)
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
}

# Last pruning
cat("\nLast pruning ...")
iniTime <- Sys.time()
ngramFreqTot <- ngramFreqTot[freq >= minFreq]
procTime <- difftime(Sys.time(), iniTime, units = "secs")
cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")

############################################################################
# 2. Assigning probabilities to the ngrams. In the case of bi-, tri- y 4-grams
# the probabilty is the conditioned probability to the root words of the
# sequence. In the case of unigrams is the absolute probability smoothed
# with the Good Turing algorithm
##############################################################################

# Creating the root words or base of each ngram for high-order ngrams and 
# adding the column to the frequency table
if (ngramSize > 1) {
    iniTime <- Sys.time()
    cat("\n Adding ngram bases... ")
    
    base <- unlist(lapply(str_split(ngramFreqTot$ngram, " "), 
                          function(x){paste(x[1:(ngramSize - 1)], 
                                            collapse = " ")}))
    ngramFreqTot$base <- base
    
    rm(base); gc(verbose = FALSE)
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
}

ngramFreqTot <- ngramFreqTot[order(ngram)]

# Adding conditioned (>1) or smoothed probabilties (=1) to the
# ngrams
iniTime <- Sys.time()
cat("\n Adding probabilities... ")

if (ngramSize > 1) {
    ngramFreqTot <- addCondProb(ngramFreqTot)
} else {
    ngramFreqTot <- addSmoothProb(ngramFreqTot)
}

procTime <- difftime(Sys.time(), iniTime, units = "secs")
cat("\n Adding probabilities... ", procTime, "s.")


totTime <- difftime(Sys.time(), totTime, units = "mins")
cat("\n\n Total time", totTime, "m.")

# Saving the ngram model
fileName <- paste0("ngram_", ngramSize, ".rds")
saveRDS(ngramFreqTot, file = fileName)
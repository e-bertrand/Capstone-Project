###############################################################
# Create Good Turing table of count-adjusted values for unigrams
# In order to calculate the probabilty of a unigram incuding unseen
# words we need a count-adjusted value (c*) using the Good-Turing algorithm
# In practical terms is not necesary to calculate the c* for every unigram
# but only for those what appear with less frequency. This determines a k value 
# as the threshold (see Chamber: SI485i: NLP course)
# Above this threshold value we assume that c* = c
# The result is a table for those c value that will be smoothed
####################################################################
library(data.table)

############################################################################
# Creating the smoothed frequencies of each unigram using the Good Turing
# algorithm
#############################################################################

createGoodTuring <- function(unigram){
    
    # Determining the k value. We put the limit in the first 5 percent of
    # different counting frequencies
    k <- floor(length(table(unigram$freq)) * 5 / 100)
    
    # Get the count of unigrams that appear 1, 2, 3,....k times. We must add
    # 1 to k to apply some formulas that requires N[k + 1] value
    Nc <- sapply(1:(k + 1), function(x){nrow(unigram[freq == x])})
    
    # We have to assign a value for how many unigrams appear 0. The criteria
    # is to consider the number of unigram tokens (= sum of all frequencies) in the
    # corpus
    No <- sum(unigram$freq)
    
    # This is the full list of unigrams that appear 0, 1, 2, 3,...,k
    Nc <- c(No, Nc)
    
    # Building the GT table with c, N and cadj (c*)
    GT <- data.frame(c = integer(), N = integer(), cadj = numeric())
    
    # Calculating the first value of cadj (without considering the impact of k)
    # Take into account that index in R start in 1 and not in 0
    for (i in 1:(length(Nc) - 1)){
        cadj <- i *  Nc[i + 1] / Nc[i]
        GT <- rbind(GT, data.frame(c = i - 1, N = Nc[i], cadj = cadj))
    }
    
    # Considering now the impact of limiting the smoothig process only to
    # the first k values
    cadj2 <- vector(mode = "numeric", length = k)
    
    # The first value (equivalento to c = 0) is the same in both cases
    cadj2[1] <- GT$cadj[1]
    
    # For the rest apply the correction formula
    for (i in 2:k){
        cadj2[i] <- (((GT$c[i] + 1) * GT$N[i + 1] / GT$N[i]) -
                         (GT$c[i] * (k + 1) *  GT$N[k + 1] / GT$N[2])) /
            (1 - ((k + 1) *  GT$N[k + 1] / GT$N[2]))
    }
    
    # Deleting the last value (as we need k + 1 elements computing cadj2 gives NA)
    GT <- GT[1:k, ]
    
    # Adding the new adjuted values to the table
    GT$cadj2 <- cadj2
    
    return(GT)
}

################################################################
# Caculating absoluted smoothed probabilities for unigrams
################################################################

addSmoothProb <- function(unigram){
    
    # Adjusting original frequencies through Good Turing
    GT <- as.data.table(createGoodTuring(unigram))
    
    # Max frequency included in GT table
    GTLimit <- max(GT$c)
    
    # Total of tokens
    N <- sum(unigram$freq)
    ini <- Sys.time()
    
    # Calculating the probabilities
    prob <- vector(mode = "numeric", length = nrow(unigram))
    
    for (i in 1:nrow(unigram)){
        
        freq <- unigram[i]$freq
        
        # If the frecuency is in the GT table we used the adjusted count 
        # as frequency
        if (freq <= GTLimit) {
            prob[i] <- GT[c == freq]$cadj2 / N
            
            # If not in GT table probability uses observed count
        } else {
            prob[i] <- freq / N
        }
        
        if (i %% 10000 == 0){
            cat("\n    - Assigned smoothed probability to", i, "of", 
                nrow(unigram), "unigrams in", 
                difftime(Sys.time(), ini, units = "secs"), "s.")
            ini <- Sys.time()    
        }
    }
    
    # Adding and saving as new table
    unigram$prob <- prob
    
    # Adding the OOV probability row
    prob_OOV <- GT$cadj2[1]
    unigram <- rbind(unigram, data.frame(ngram = "<OOV>", 
                                         freq = 0, 
                                         prob = prob_OOV))
    
    # Cheking correct smoothing
    cat("\n Calculated probability of OOV words", GT$cadj2[1])
    cat("\n Probability left by the ngrams", 1 - sum(unigram$prob), "\n\n")
    
    return(unigram)
}

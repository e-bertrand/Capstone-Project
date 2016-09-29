##############################################################################
# Getting the ngrams of a particular order from a string of words using
# 'stylo' packages
###############################################################################

library(stylo)
library(data.table)

getNgrams <- function(text, ngSize){

    # Pasting all de elements of a text vector in a string, removing duplicated
    # end of sentences marks
    iniTime <- Sys.time()
    cat("\n Collapsing data elements... ")
    
    text_all <- paste("<s/>", paste(text, collapse = " <s/> "))
    text_all <- gsub("<s/>\\s*<s/>(\\s*<s/>)*", "<s/> ", text_all)
    
    rm(text); gc(verbose = FALSE)
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.")

    # Getting the sequence of words from a text string. English contractions are
    # preserved as single words
    iniTime <- Sys.time()
    cat("\n Extracting words from data... ")
    
    ttw <- txt.to.words.ext(text_all, language = "English.all", 
                            splitting.rule = "[[:space:]]+")
    
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.")
    rm(text_all); gc(verbose = FALSE)

    # Producing the list of ngrams of the requested order
    iniTime <- Sys.time()
    cat("\n Making ngrams... ")
    
    ng <- make.ngrams(ttw, ngram.size = ngSize)
    
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.")
    rm(ttw); gc(verbose = FALSE)
    
    return(ng)
}
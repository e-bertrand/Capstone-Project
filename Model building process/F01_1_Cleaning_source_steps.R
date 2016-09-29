################################################################################
# This function clean a source of text (blogs, news, or twitter) using
# standard and ad-hoc basic cleaning functions. The standard cleaning functions
# came from R base and the 'tm' package. The ad-hoc functions are available in
# a separate script (F01_Cleaning_text_functions.R).
#
# The result is a normalized version of the source, keeping its structure
################################################################################

library(tm)

# Loading set of ad-hoc cleaning functions
source("F01_2_Cleaning_text_functions.R")

cleaningSource <- function (text_source){

    # Step 1 - Removing non-English text
    cat("\n   -> Step 1 - Removing non-English text")

    text_source <- removeNonEnglish(text_source)

    # Step 2 - Removing links
    cat("\n   -> Step 2 - Replacing links")

    text_source <- removeLinks(text_source)

    # Step 3 - Normalizing quotation marks and hyphens/dashes
    cat("\n   -> Step 3 - Normalizing quotation/hyphens")

    text_source <- normalizeQuotesHyphens(text_source)

    # Step 4 - Removing numbers
    cat("\n   -> Step 4 - Removing numbers")

    text_source <- removeNumbers(text_source)

    # Step 5 - Marking end of sentences
    cat("\n   -> Step 5 - Marking end of sentences")

    text_source <- markingEoS(text_source)

    # Step 6 - Removing punctuation except intra-word dashes
    cat("\n   -> Step 6 - Removing punctuation")

    text_source <- removePunctuation(text_source, 
                                     preserve_intra_word_dashes = TRUE)

    # Step 7 - Changing reserved characters
    cat("\n   -> Step 7 - Changing reserved characters")
    
    text_source <- changingReservedChars(text_source)

    # Step 8 - Remove all left non-latin characters
    cat("\n   -> Step 8 - Remove non-latin characters")

    text_source <- removeNonLatin(text_source)

    # Step 9 - converting to lower case
    cat("\n   -> Step 9 - Converting to lower case")

    text_source <- tolower(text_source)

    # Step 10 - Deleting profanities
    cat("\n   -> Step 10 - Deleting profanities")
    
    text_source <- removeProfanities(text_source)

    # Step 11 - Removing non-functional marks (superfluous apostrophes/hyphens)
    cat("\n   -> Step 11 - Removing non-functional marks")

    text_source <- removeNonFunctionalMarks(text_source)

    # Step 12 - Strip streams of <s/>
    cat("\n   -> Step 12 - Strip streams of <s/>")
    
    text_source <- stripEoS(text_source)

    # Step 13 Normalizing white space
    cat("\n   -> Step 13 - Normalizing white space")

    text_source <- stripWhitespace(text_source)
    text_source <- trimws(text_source)

return(text_source)
}

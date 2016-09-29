###########################################################################
# Set of special cleaning text functions used for normalizing text that
# is to be used to predict next words. These functions are called when
# necessary from the cleaning script, where they are combinend with other
# standars cleaning functions implemented in "tm" package.
#
# A basic criteria that has been followed is trying to maintain real
# sentences separated: that is, not putting together words that are not; 
# for instance, words separated by end points, colons or semicolons.
# In this case an end of sentence mark (<s/>) is generated.
# As this may enter in conflict with other criteria, a compromise
# must be reached. That is the case with twitter where there are not
# blanks after end points or colons for saving characters.
#######################################################################

library(tm)

###############################################################################
# Function for cleaning  non-English lines in a text. English basically is
# based on the latin-1 UNicode subset. If the majority of the string uses
# other unicode characters for sure it is in a foreign languege
###############################################################################

removeNonEnglish <- function(text, cut = 0.5){
  
  foreign_text = vector(mode = "numeric")
  
  for (i in 1:length(text)){
    
    # Reporting the progress (costly process)
    if (i %% 100000 == 0){
      cat("\n      · Processing", i, "elements")
    }
    
    # All unicode non latin characters are removed
    text_reduced <- gsub("[\U0080-\UFFFF]", "", text[i])
    
    # If the new length is substantially shorter (cut criteria can be changed)
    # the text is not english and the element is registered
    if(nchar(text_reduced) < nchar(text[i]) * cut){
      foreign_text <- c(foreign_text, i)
    }
  }
  
  # All the elements registered as non-english are deleted
  if (length(foreign_text > 0)){
    text <- text[-foreign_text]
  }
  
  return(text)
}

###############################################################################
# Function for removing links and references: URLs, mails, web sites/pages, etc.
# and replace them by double vertical bar special character (\U2016)
# The special character will be replaced later by a <s/> mark. The reason
# for using that character is to "protected" it against other cleaning 
# operations until the rigth moment
###############################################################################

removeLinks <- function(text){
    
  # Patterns for detecting references
  mail_pattern <- 
  "\\b(([a-zA-Z0-9_\\.-]+)@([\\da-zA-Z\\.-]+)\\.([a-zA-Z\\.]{2,6}))\\b"
  
  url_pattern <- 
  "\\b(((http|ftp)s?://)([\\da-zA-Z\\.-]+)\\.([a-zA-Z\\.]{2,6})(/[\\da-zA-Z\\.-]+)*)\\b"
  
  web_pattern <- 
  "\\b(((www|WWW)\\.)([\\da-zA-Z\\.-]+)\\.([a-zA-Z\\.]{2,6})(/[\\da-zA-Z\\.-]+)*)\\b"
  
  # Replacing links by end of sentence mark
  text <- gsub(mail_pattern, " \U2016 ", text)
  text <- gsub(url_pattern, " \U2016 ", text)
  text <- gsub(web_pattern, " \U2016 ", text)
  
  return(text)
}

###############################################################################
# Function for normalizing all the different ways that quotation marks and
# hyphen/dashes are used.
# There are there type of quotations: \U2019, \U0092 and \U0027 (apostrophe)
# that are converted to \U2019 (to preserve the quotation against further
# operations).
# There are many types of hyphens/dashes, all converted to \U002D (simple hyphen)
################################################################################

normalizeQuotesHyphens <- function(text){
    
    # Patterns for detectin quotes and hyphens
    quote_pattern <- "\U0027|\U0092"
    dash_pattern <- "[\U2010-\U2015\U2212\UFE58\UFE63\UFF0D]"

    text <- gsub(quote_pattern, "\U2019", text)
    text <- gsub(dash_pattern, "\U002D", text)

    return(text)
}

################################################################################
# Function for converting some punctuation in end of sentence markers
# Before we remove all punctuations, some of them must be interpreted as 
# end of sentence markers in order to avoid spureous ngrams.
# In the case of twitter there is a conflict with the fact that usually they are
# no space after punctuation. In this case punctuation will not be preserved
# in the form of end of sentences marks
###############################################################################

markingEoS <- function(text){
    
    # Patterns for detecting end of sentence, including sentences inside
    # dashes and inside parenthesis
    eos_pattern1 <- "(\\w)[!.:;?)]\\s+|(\\w)[!.:;?)]$|(\\w)\U002D\\s+|(\\w)\U002D$"
    eos_pattern2 <- "\\s+\\((\\w)|^\U002D(\\w)|\\s+\U002D(\\w)"
   
    # Replacing punctuations by end of sentence
    text <- gsub(eos_pattern1, "\\1\\2\\3\\4 \U2016 ", text)
    text <- gsub(eos_pattern2, " \U2016 \\1\\2\\3", text)

    return(text)
}

################################################################################
# Function for changing reserved characters apostrophe and end of sentence to
# latin subset characters after all punctuation not relevant has been removed
# and before all non latin characters be cleaned
################################################################################

changingReservedChars <- function(text){
    
    # Changing normalized quotation marks by apostrophes
    text <- gsub("\U2019", "\U0027", text)
    
    # Changing end of sentence by <s/> marks
    text <- gsub("\U2016", "<s/>", text)
    
    return(text)
}

################################################################################
# Function for removing remaining non-latin character once all relevant special 
# characters have been standarized and preserved. Particularly relevant for 
# emoticons symbols in twitter, special editing marks in news, etc.
################################################################################

removeNonLatin <- function(text){
    
    # Changing non-latin subset of characters
    text <- gsub("[\U0080-\UFFFF]", " ", text)
    
    return(text)
}

################################################################################
# Function for removing swear words, bad words, and other possible profanities 
# from text. It uses a list of profanities (text file with one word per line)
# that must be loaded and the function removeWords() from "tm" package.
# The list that has been used is the "unofficial" list of banned words managed 
# by Google and published in: 
# http://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/
# There are different sources with divergences but this list seems reasonable.
# Apparently there is a bigger one used by Android Google Keyboard
# (http://www.wired.com/2013/12/banned-android-words/), but based on criteria
# sometimes bizarre, so we will keep the basic list.
###############################################################################

removeProfanities <- function(text){
    
    # Loading and normalizing the list
    badwords <- readLines("../../data/bad-words_1.txt")
    badwords <- stripWhitespace(badwords)
    badwords <- trimws(badwords)
    
    # Remove words from the list
    text <- removeWords(text, badwords)
    
    return(text)
}

################################################################################
# Function for removing all "free" apostrophes and hyphens. After removing steps
# some apostrophes and hyphens remain "isolated". We are interested in
# preserving these characters in possesive forms, contracted verbal formas and
# intra-word hyphens. All of them share the fact that the character is between
# word characters. The rest of uses will be removed.
################################################################################

removeNonFunctionalMarks <- function(text){
    
    # Detecting apostrophes or hyphens not fully surrounded by word characters
    bad_pattern1 <- "^['-](\\w)|\\s+['-](\\w)"
    bad_pattern2 <- "(\\w)['-]\\s+|(\\w)['-]$"
    bad_pattern3 <- "\\s+['-]\\s+|''+|--+|^['-] | ['-]$"
    
    # Removing superfluous apostrophes and hyphens
    text <- gsub(bad_pattern1, " \\1\\2", text)
    text <- gsub(bad_pattern2, "\\1\\2 ", text)
    text <- gsub(bad_pattern3, " ", text)
    
    return(text)
}

################################################################################
# Function to strip streams of end of sentence (<s/>) marks in only one
# ocurrence. After many removing processes sometimes there are no words between
# two (or more) <s/> marks: they must be compressed in one.
################################################################################

stripEoS <- function(text){
    
    # Pattern to detect streams of two or more consecutive <s/>
    streamsEoS_pattern <- "<s/>\\s*<s/>(\\s*<s/>)*"
     
    # Collapsing streams of EoS in only one ocurrence
    text <- gsub(streamsEoS_pattern, "<s/> ", text)
    
    return(text)
}

#MICROSTRATEGY_BEGIN
#
#RVAR Text -input -string -vector
#
#RVAR FileName -parameter -string
#RVAR PlotWordCloud -parameter -boolean
#RVAR PlotHistogram -parameter -boolean
#RVAR RemoveRetweets -parameter -boolean
#RVAR SaveCSV -parameter -boolean
#
#RVAR Score -output -numeric -vector           # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Score", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Grade -output -string -vector            # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Grade", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Anger -output -string -vector            # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Anger", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Anticipation -output -string -vector     # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Anticipation", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Disgust -output -string -vector          # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Disgust", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Fear -output -string -vector             # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Fear", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Joy -output -string -vector              # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Joy", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Negative -output -string -vector         # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Negative", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Positive -output -string -vector         # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Positive", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Sadness -output -string -vector          # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Sadness", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Surprise -output -string -vector         # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Surprise", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR Trust -output -string -vector            # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="Trust", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR TotalWords -output -numeric -vector      # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="TotalWords", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR WordCount -output -numeric -vector       # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="WordCount", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#RVAR WordScore -output -numeric -vector       # Metric Expression: RScriptU<[_RScriptFile]="SentimentAnalysis.R", [_OutputVar]="WordScore", _Params="FileName='SA_mstr', PlotWordCloud=TRUE, PlotHistogram=TRUE, RemoveRetweets=TRUE, SaveCSV=TRUE">(Text)
#
#MICROSTRATEGY_END

mstr.ErrMsg<-tryCatch({

  timer0 <- proc.time()                                        #Start a timer to measure duration of this execution

  ripEnv <- environment()                                      # Remember the R environment of this execution

  # save(list=ls(), file="SA.Rdata")

  ###################
  ###  FUNCTIONS  ###
  ###################

  if(exists("mstr.WorkingDir")) { setwd(mstr.WorkingDir) }     # Set Working Directory, if specified

  ### Function:  LogTask
  ###            Accumulates tasks for performance and debug
  ###            into "TaskLog" variable
  LogTask <- function(task, silent=FALSE) {
    #save.image(paste0("LogTask_", make.names(task)))
    task <- paste(format(Sys.time(), format="%H:%M:%S"), task)
    if(!silent) { print(task) }
    if(exists("TaskLog")) {
      TaskLog <- c(TaskLog, task)
    } else {
      TaskLog <- task
    }
    assign("TaskLog", TaskLog, ripEnv)
  }  #END-LogTask


  ### Function:  CheckInstall Packages
  ###            Check if package(s) are installed, install if not, and then load each package
  ###            Requires that the machine executing this script has access to the internet
  CheckInstallPackages <- function(pkgs) {                     # pkgs is a vector of strings with length >= 1
    timer <- proc.time(); funcName <- "CheckInstallPackages"
    LogTask(paste0(funcName, " - Started..."))
    x <- lapply(pkgs, function(pkg){                           # For each pkg in pkgs (attempt to load each package one at a time):
      if(!do.call("require", list(pkg))) {                     #   Load the package if available,
        try(install.packages(pkg, lib=.Library,
                             repos="http://cran.rstudio.com")) #   Silently attempt to install into the default library
        tryCatch(do.call("library", list(pkg)),                #   Now attempt to load the package, catch error if it wasn't installed
                 error = function(err) {                       #    Catch if we're unable to install into the default library
                   if(!interactive()) {                        #      If non-interactive, install into this user's personal library
                     usrLib <- Sys.getenv("R_LIBS_USER")       #        Get the path to this user's personal library
                     usrLib <- gsub("\\\\", "/", usrLib)       #        Replace any double back slashes with forward slashes
                     if(is.na(match(usrLib, .libPaths()))) {   #        If the personal library is not in the list of libraries
                       dir.create(usrLib, recursive = TRUE)    #          Then create the personal library
                       .libPaths(usrLib)                       #          And add the personal library to the list of libraries
                     }
                     install.packages(pkg, lib=usrLib,         #        Attempt to install the package into the personal library
                              repos="http://cran.rstudio.com") #          if this fails, raise the error back to the report
                     do.call("library", list(pkg,
                                             lib.loc=usrLib))  #        Finally, attempt to load the package from the personal library
                   }
                 }
        )
      }
    })
    LogTask(paste0(funcName, " - Finshed (",format((proc.time() - timer)[3], nsmall=3), "sec)"))
  }  #END-CheckInstall Packages

  ### Function:  GetWords
  ###            Process text into words while removing punction and stop words
  ###            If not analyzing tweets, comment out the first filter and mutate that follows
  GetWords <- function(tblText) {
    timer <- proc.time(); funcName <- "GetWords"
    LogTask(paste0(funcName, " - Started..."))
    reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"              # RegEx that converts text
   if(RemoveRetweets) {                                       # If we're to remove Retweets
      words <- tblText %>%                                     # Start with the input text
        dplyr::filter(!str_detect(tblText$Text, '^"')) %>%     # Remove retweets (text that start with a double quote -- for twitter only)
        dplyr::mutate(Text=str_replace_all(tblText$Text,
                        "https://t.co/[A-Za-z\\d]+|&amp;","") )# Remove any URLs (for twitter only)
    } else {
      words <- tblText
    }
    stopWords <- setdiff(stop_words$word,
                         intersect(stop_words$word,
                                   tblLexicon$word))           # Exclude stop words that have sentiment
    words <-  words %>%                                        # Start with the input text
      unnest_tokens(word, Text, token = "regex",
                    pattern = reg) %>%                         # Break the text into words
      # dplyr::filter(!word %in% stop_words$word,
      dplyr::filter(!word %in% c("rt", stopWords),
                    str_detect(word, "[a-z]")) %>%             # Remove any stop words
      select(word, Id)                                           # Return the words along with the Id of the Text the word belongs to
    LogTask(paste0(funcName, " - Finshed (",format((proc.time() - timer)[3], nsmall=3), "sec)"))
    return(words[, c("Id", "word")])
  } #END-GetWords

  ### Function:  GetLexicon
  ###            Convert the tidytext package's sentiments of english words into a table of words with of 10 sentiments plus a score
  GetLexicon <- function() {
    timer <- proc.time(); funcName <- "GetLexicon"
    LogTask(paste0(funcName, " - Started..."))
    tblLex <- get_sentiments("nrc") %>%                 # First, process sentiments from the non AFINN lexicons (nrc & bing)
      select(word, sentiment) %>%                              # Get the words and their sentiments
      distinct() %>%                                           # Delete any duplicate rows (when the same word-sentiment appears in more than one lexicon)
      mutate(qty=1) %>%                                        # Add the word count qty
      group_by(word, sentiment) %>%                            # Group by word and sentiment
      summarize(qty=max(qty)) %>%                              # Aggregate so each row is a unique word
      ungroup() %>%                                            # Ungroup the table
      spread(sentiment, qty, fill=0) %>%                       # Spread the sentiment column in the a column for each sentiment
      full_join(get_sentiments("afinn") %>%
                  select(word, value))                         # Join in the words from the AFINN lexicon along with their scores
    x <- is.na(tblLex$value)                                      # Find all words with no score
    tblLex$value[x] <- 2*(tblLex$positive[x] - tblLex$negative[x])      # For words with no score, set score using positive / negative
    colnames(tblLex)[match("value", colnames(tblLex))] <- "WordScore"# Rename score to WordScore (since Score is that average of the WordScores for each text element)
    colOrder <- c("word", "WordScore", "positive", "negative", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
    LogTask(paste0(funcName, " - Finshed (",format((proc.time() - timer)[3], nsmall=3), "sec)"))
    return(tblLex[, colOrder])                                    # Return the lexicon table
  } #END-GetLexicon

  ### Function:  WordCloud Plotter
  WordCloudPlotter <- function(word, n, minFreg=2, maxWords=100, colors=brewer.pal(9, "RdBu"), rotPer=.1) {
    timer <- proc.time(); funcName <- "WordCloudPlotter"
   LogTask(paste0(funcName, " - Started..."))
    wordcloud(word,                                            #     Plot these words in the wordcloud
              n,                                               #       Count for each word
              min.freq=2,
              max.words=100,                                   #       Only this many words will be plutted
              colors = brewer.pal(9,"RdBu"),                   #       Number of colors, palette used for the wordcloud
              rot.per = .1)

    LogTask(paste0(funcName, " - Finshed (",format((proc.time() - timer)[3], nsmall=3), "sec)"))
  }  #END-WordCloudPlotter

  #############################
  ###  Main Execution Flow  ###
  #############################

  LogTask("Begin Main Execution Flow")
  Sys.setlocale(category = "LC_COLLATE", locale = "Arabic")

 Sys.setlocale(category = "LC_CTYPE", locale = "Arabic")



  ### Load required packages
  CheckInstallPackages(c("tidytext", "wordcloud",
                         "dplyr", "stringr", "tidyr","utf8"))         # Check and install if not present, then load packages

  ##remotes::install_github("ChristopherLucas/translateR",force=TRUE)
  library(translateR)

  ### Data Prep:  Use "Dual Execution Mode" so this single script can be run manaully from the R Console and also automated by MicroStrategy
  LogTask("Preparing Data")
  if(exists("mstr.ExFlag")) {                                  # If executed by MicroStrategy
    if(!exists(FileName) || (nchar(FileName)==0)) {            #   Then if there is no file name
      FileName <- "SA_MSTR"                                    #      Then use a default name indicating this came from a MicroStrategy execution
    }
  } else {                                                     # Otherwise, this is executed from the console,
    #For the Text input, use the 10 sentences of Lincoln's Gettysburg Address
    Text <- c("Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.",
              "Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure.",
              "We are met on a great battle-field of that war.",
              "We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live.",
              "It is altogether fitting and proper that we should do this.",
              "But, in a larger sense, we can not dedicate-we can not consecrate-we can not hallow-this ground.",
              "The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract.",
              "The world will little note, nor long remember what we say here, but it can never forget what they did here.",
              "It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced.",
              "It is rather for us to be here dedicated to the great task remaining before us-that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion-that we here highly resolve that these dead shall not have died in vain-that this nation, under God, shall have a new birth of freedom-and that government of the people, by the people, for the people, shall not perish from the earth")
    FileName <- "SA_console"                                   #   Set file name when executed from the console
    PlotHistogram <- TRUE                                      #   Yes, include the Sentiment histogram plot
    RemoveRetweets <- TRUE                                     #   Yes, remove any elements that look like a re-tweet
    PlotWordCloud <- TRUE                                      #   Yes, include the Word Cloud for all words
    SaveCSV <- TRUE                                            #   Yes, save the sentiment data as a CSV file
  }

  ### Prepare Text and Analyze Sentiments
  LogTask("Analyzing Sentiment")
  tblLexicon <- GetLexicon()                                   # Get the Lexicon table with words and their sentiments/scores
  
  transText <-(translate(content.vec=Text,microsoft.api.key='XXXXXXXXXXXXXXX',source.lang='ar', target.lang='en'))
  LogTask(transText)
  tblText <- tbl_df(data.frame(Id=seq(1, length(transText)),
                               Text=(transText),
                               stringsAsFactors=FALSE))        # Convert add an Id for each Text element and save to a table
  tblWords <- GetWords(tblText)                                # Get the words
  tblSentsWords <- left_join(tblText %>%
                               mutate(WordCount=1),            # Join the table of input text (on rows) with
                            left_join(tblWords, tblLexicon,
                                      by="word"))              #  The table of words and the lexicon table
  tblText$TotalWords <- sapply(seq_along(tblText$Id), FUN=function(i) {
    length(which(tblSentsWords$Id==tblText$Id[i]))
  })                                                           #  Get the number of words left after stop words/etc are removed
  hasSent <- apply(tblSentsWords[, -c(1:4)], 1, FUN=function(x) {
    !all(is.na(x))
  })                                                           # Find all rows with words that didn't match any in the Lexicon
  tblSentsWords <- tblSentsWords[hasSent, ] %>%
                     group_by(Id, Text, word) %>%
                     summarise_each(funs(sum)) %>%
                     ungroup()                                 # Remove non-sentiment word/rows and aggregate (when on word appears more than once in an element)

  tblSentsWords[is.na(tblSentsWords)] <- 0                     # Replace any NA with 0

  tblSents <- tblSentsWords %>% group_by(Id, Text) %>%         # Build the final output table
    select(-word) %>%
    summarise_each(funs(sum)) %>%
    ungroup() %>%
    mutate(Score=WordScore/WordCount) %>%                 # Aggregate (when on word appears more than once in an element)
    left_join(tblText %>% select(Id, TotalWords))

  LogTask("Assigning Grades")
  GRADES <- c("Extremely Negative", "Very Negative",
              "Negative", "Somewhat Negative", "Slightly Negative",
              "Neutral",
              "Slightly Positive", "Somewhat Positive", "Positive",
              "Very Positive", "Extremely Positive",
              "Barely Negative", "Barely Positive")            # Provide text descriptions, a grade for each AFINN positive/negative polarity sentiment measure
  tblSents$Grade <- GRADES[unlist(sapply(tblSents$Score,
                                  FUN=function(x) {            # Assign a grade to each Text Id
    grd= round(max(min(x, 5), -5))+6                           # Map numeric Score to the text grades
    grd=ifelse((x<0.5)&(x>0.1), 13,
               ifelse((x>=(-0.5))&(x<(-0.1)), 12, grd))        # Handle so "Neutral" is only -0,1 to +0.1
  }))] 
                                                         # Assign text grade
  

  colOrder <- c("Id", "Text", "TotalWords", "WordCount", "WordScore", "Score", "Grade", "positive", "negative",
                "anger", "anticipation", "disgust", "fear", "joy", "sadness",
                "surprise", "trust")
  tblSents <- tblSents[, colOrder]                              # Put the columns in the preferred order

  tblSentsWords <- rbind(tblSentsWords %>% mutate(Score=0,  Grade=""),
                         tblSents %>% select(-TotalWords) %>%
                           mutate(word="*Total"))              # Add the text element level results to the text-word results
  tblSentsWords <- tblSentsWords[order(tblSentsWords$Id,
                                       tblSentsWords$word), ]  # Sort in order of Id and Word

  ### Build final dataset with sentiments, positive/negative polarity scores & grades, plus word counts
  ### and ensure variable names start with capital letters and the rest lower case
  LogTask("Preparing Outputs")
  # tblSents <- cbind(tblSents,
  #                     Grade=as.character(Grade), WordCount)    # Combine Grade and WordCount with the table of Sentiments
  colnames(tblSents)[-c(1, 2, 14, 15)] <- sapply(colnames(tblSents)[-c(1, 2, 14, 15)],
                               FUN=function(n) {               # For each column in the table of sentiments
    x <- sub("_sum", "", n)                                    #   Remove any "_sum" suffix from column names (added during "summarize_each")

    x <- paste0(toupper(substr(x, 1, 1)),
                substr(x, 2, nchar(x)))                        #   Capitialize the only the first character of the column name

    assign(x, unname(unlist(tblSents[, n])), ripEnv)           #   Add this column as an output variable in the environment
    return(x)
  })

  ### Count Words
  LogTask("Counting Words")
  tblWordCount <- count_(tblWords, "word")                     # Create a table that tallies the count of each word
  WordCount <-  sapply(unique(tblSents$Id), FUN=function(Id) { # Count the words in each Text id
    return(nrow((tblWords[tblWords$Id==Id, ])))
  })
  totalWordCount <- sum(unlist(WordCount))                             # Get the total count of words from all ids

  ### Return Out-of-Band byproducts:  WordCloud, Histogram, Dataset CSV, and .Rdata of the environment
  LogTask("Returning Out-of-Band Byproducts")
  if(exists("mstr.ExFlag")) {                                  # If executed by MicroStrategy
    if(PlotWordCloud){                                         #   If requested to Persist the WordCloud as file
      jpeg(paste0(FileName, "_WordCloud.jpg"))                 #     Set the WordCloud image file name
      WordCloudPlotter(tblWordCount$word, tblWordCount$n)      #     Plot the Word Cloud
      dev.off()                                                #     Save the WordCloud as a JPEG file
    }
    if(PlotHistogram){                                         #   If requested to Plot the Histogram of sentiment scores
      jpeg(paste0(FileName, "_Histogram.jpg"))                 #     Set the histogram image file name
      hist(tblSents$Score)                                     #     Plot the histogram
      dev.off()                                                #     Save the histogram as a JPEG file
    }
  } else {                                                     # Otherwise, this is executed from the R Console
    if(PlotWordCloud) {                                        #   If requested to Plot the WordCloud
      WordCloudPlotter(tblWordCount$word, tblWordCount$n)      #     Plot the Word Cloud
    }
    if(PlotHistogram){                                         #   If requested to Plot the Histogram of sentiment scores
      hist(tblSents$Score)                                     #     Plot the histogram
    }
  }
  if(SaveCSV) {                                                #  If requestsed to save a CSV of the sentiment data
    tryCatch({
      write.csv(tblSents, file=paste0(FileName, ".csv"),
              row.names=FALSE)                                 #    Save a CSV of the Sentiment data
      write.csv(tblSentsWords, file=paste0(FileName, "_words.csv"),
                row.names=FALSE)                                 #    Save a CSV of the Sentiment data
    }, error = function(err) {                                 # Catch block to report an error
      LogTask(err$message)
    })
  }

  ### Finish Execution
  duration <- paste0("*** Success! Total Elasped Time=",
                     format((proc.time() - timer0)[3],
                            nsmall=3), "sec")                  # Prepare success message
  LogTask(duration)                                            # Display Success Message
  save(list=ls(all=TRUE),
       file=paste0(FileName, "_FINAL.Rdata"))                  #  Finally, save all the objects in this environment to .Rdata file

  mstr.ErrMsg <- ""                                            # If we've reach here, then we're done and no errors were caught

  ### Error Handler

}, error = function(err) {                                     # Catch block to report an error
  try(print(err))                                              #  Print error message to console (using try to continue on any print error)
  save(list=ls(ripEnv, all.names=TRUE),
       file=paste0(ifelse(!exists("FileName") || nchar(FileName)==0,
                          "SentimentAnalysis",
                          FileName),
                   "_ERROR.Rdata"))                            #  Save the objects in this environment
  return(err$message)                                          #  Return error Message
})

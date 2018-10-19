# This document shows how to create the database for predicting next word. Also, it will show the exmaple of predicting algorithum used in shiny app.

# Getting tools
library(quanteda)
library(stringr)
library(data.table)

# Load the data
blog <- readLines("en_US.blogs.txt", skipNul = TRUE)

# clean the data
clean <- function(x){
    x <- iconv(x, "latin1", "UTF-8")
    x <- gsub("a\u0080\u0099", "'", x, fixed=TRUE)
    x <- gsub("a\u0080\u0093", " ", x, fixed=TRUE)
    x <- gsub("a\u0080\u0098", " ", x, fixed=TRUE)
    x <- gsub("a\u0080\u009c", " ", x, fixed=TRUE)
    x <- gsub("a\u0080\u009d", " ", x, fixed=TRUE)
    x <- gsub("a\u0080\u0094", " ", x, fixed=TRUE)
    x <- gsub("a\u0080", " ", x, fixed=TRUE)
    x <- gsub("<", " ", x)
    x <- gsub(">", " ", x)
    x <- gsub("\\. |\\.$", " <EOS> ", x)
    x <- gsub("\\? |\\?$", " <EOS> ", x)
    x <- gsub("\\! |\\!$", " <EOS> ", x)
    x <- gsub("?", " ", fixed = TRUE, x)
    x <- gsub("???Ts", " ", fixed = TRUE, x)
    x <- gsub(" [b-hj-z] ", " ", x)
    x <- gsub(" [B-HJ-Z] ", " ", x)
    x <- gsub("[^[:alnum:][:space:]'<>]", " ", x)
    x <- gsub("^ *'| +'|' +", " ", x) # remove apostrophes except the apostrophes in the contraction words
    return(x)
}

blogs1 <- clean(blog)

# Turn the dataset into corpus
blogs <- corpus(blogs1)
docvars(blogs, "source") <- "blogs"
docvars(blogs, "line") <- 1:ndoc(blogs)

# Function to tokenize the corpus into N-grams
ToTokenize <- function(object, n){
    tokensAll <- tokens(object, remove_numbers = TRUE,
                        remove_symbols = TRUE, remove_separators = TRUE,
                        remove_twitter = FALSE, remove_hyphens = TRUE, remove_url = TRUE)
    NoBadWord <- tokens_select(tokensAll, c(profanity), selection = "remove", case_insensitive = TRUE)
    ng <- tokens_ngrams(NoBadWord, n, concatenator = " ")
    newDfm <- dfm(ng)
    newDfm <- dfm_select(newDfm, "^[e][o][s]|[e][o][s]$| [e][o][s] ", selection="remove", valuetype = "regex")
    return(newDfm)
}

# Applying bi-grams for blog data set
profanity <- NULL
dfm2 <- ToTokenize(blogs, 2)
saveRDS(dfm2, "dfm2.rds")

# Convert dfm to data table
ToDT <- function(object, n){
    df <- data.frame(feature = featnames(object), frequency = colSums(object),
                     row.names = NULL, stringsAsFactors = FALSE)
    df$base <- word(string = df$feature, start = 1, end = n-1, sep = fixed(" "))
    df$predict <- word(string = df$feature, start = n, end = n, sep = fixed(" "))
    DT <- as.data.table(df)
    DT <- DT[, c("feature") := NULL][order(-frequency)]
    return(DT)
}

n2 <- readRDS("dfm2.rds")
DT2 <- ToDT(n2, 2)
saveRDS(DT2, "DT2.rds")

# Prediction app example 
PrediictNext1 <- reactive({
    input <- tolower(input$text)
    input <- unlist(strsplit(as.character(input), ' '))
    n <- length(input)
    if(n >= 1 ){
        New <- DT2[DT2$base == input[n],]
        return(New$predict[1])
    } else{
        return("Unknown")
    }
})
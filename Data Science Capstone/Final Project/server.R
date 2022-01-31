#load library

library(shiny)
library(tm)
library(stringr)

# load data


quadgram <-  readRDS(url("https://github.com/Shadi-Sedi/Capstone-Project/raw/master/quadgram.RData"));
trigram <-  readRDS(url("https://github.com/Shadi-Sedi/Capstone-Project/raw/master/trigram.RData"));
bigram <-  readRDS(url("https://github.com/Shadi-Sedi/Capstone-Project/raw/master/bigram.RData"));
#mesg <<- ""

# prediction method
word_predict <- function(x) {
    input_clean <- removeNumbers(removePunctuation(tolower(x)))
    user_input <- strsplit(input_clean, " ")[[1]]
    
    if (length(user_input)>= 3) {
        user_input <- tail(user_input,3)
        if (identical(character(0),head(quadgram[quadgram$unigram == user_input[1] & quadgram$bigram == user_input[2] & quadgram$trigram == user_input[3], 4],1))){
            word_predict(paste(user_input[2],user_input[3],sep=" "))
        }
        else {mesg <<- "4-grams has been used"; head(quadgram[quadgram$unigram == user_input[1] & quadgram$bigram == user_input[2] & quadgram$trigram == user_input[3], 4],1)}
    }
    else if (length(user_input) == 2){
        user_input <- tail(user_input,2)
        if (identical(character(0),head(trigram[trigram$unigram == user_input[1] & trigram$bigram == user_input[2], 3],1))) {
            word_predict(user_input[2])
        }
        else {mesg<<- "3-grams has been used"; head(trigram[trigram$unigram == user_input[1] & trigram$bigram == user_input[2], 3],1)}
    }
    else if (length(user_input) == 1){
        user_input <- tail(user_input,1)
        if (identical(character(0),head(bigram[bigram$unigram == user_input[1], 2],1))) {mesg<<-"The word with the highest frequency is returned"}
        else {mesg <<- "2-grams has been used."; head(bigram[bigram$unigram == user_input[1],2],1)}
    }
}




## Server



shinyServer(function(input, output) {

    output$prediction <- renderPrint({
        result <- word_predict(input$inputString)
        output$method <- renderText({mesg})
        result
    });
    
    output$answer <- renderText({
        input$inputString});
}
)



library(magrittr)
library(stringi)
library(ggplot2)

blogfile<- "en_US.blogs.txt"
newsfile<- "en_US.news.txt"
twitterfile<- "en_US.twitter.txt"

blogs<-readLines("en_US.blogs.txt",encoding="UTF-8", skipNul = TRUE)
news<-readLines("en_US.news.txt",encoding="UTF-8", skipNul = TRUE)
twitter<-readLines("en_US.twitter.txt",encoding="UTF-8", skipNul = TRUE)

library(tm)
library(RWeka)

##Summary
words_blogs <- stri_count_words(blogs)
words_news <- stri_count_words(news)
words_twitter <- stri_count_words(twitter)
size_blogs <- file.info("en_US.blogs.txt")$size/1024^2
size_news <- file.info("en_US.news.txt")$size/1024^2
size_twitter <- file.info("en_US.twitter.txt")$size/1024^2
summary_table <- data.frame(filename = c("blogs","news","twitter"),
                            file_size_MB = c(size_blogs, size_news, size_twitter),
                            num_lines = c(length(blogs),length(news),length(twitter)),
                            num_words = c(sum(words_blogs),sum(words_news),sum(words_twitter)),
                            mean_num_words = c(mean(words_blogs),mean(words_news),mean(words_twitter)))
summary_table


##  Histogram

set.seed(1)
blogsSample <- sample(blogs, length(blogs)*0.01)
newsSample <- sample(news, length(news)*0.01)
twitterSample <- sample(twitter, length(twitter)*0.01)

wpl <- lapply(list(blogs, news, twitter), function(x) stri_count_words(x))


plot1 <- qplot(wpl[[1]],
               geom = "histogram",
               main = "US Blogs",
               xlab = "Words per Line",
               ylab = "Frequency",
               binwidth = 5,
               colour = I("green"))

plot2 <- qplot(wpl[[2]],
               geom = "histogram",
               main = "US News",
               xlab = "Words per Line",
               ylab = "Frequency",
               colour = I("Yellow") ,
               binwidth = 5)

plot3 <- qplot(wpl[[3]],
               geom = "histogram",
               main = "US Twitter",
               xlab = "Words per Line",
               ylab = "Frequency",
               binwidth = 1,
               colour = I("blue"))

plot1
plot2
plot3
rm(plot1, plot2, plot3)

### data prepare

sample.corpus  <- c(blogsSample,newsSample,twitterSample)

corpus <- Corpus(VectorSource(list(sample.corpus)))



    # Helper function to preprocess corpus
    corpus <- tm_map(corpus, toSpace, "/|@|\\|")
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, removeWords, profanities)
    corpus <- tm_map(corpus, stripWhitespace)
   


    
    library(RWeka)
    
    unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
    bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    # create term document matrix for the corpus
    unigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = unigramTokenizer))
    
    # eliminate sparse terms for each n-gram and get frequencies of most common n-grams
    unigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(unigramMatrix, 0.99))), decreasing = TRUE)
    unigramMatrixFreq <- data.frame(word = names(unigramMatrixFreq), freq = unigramMatrixFreq)
    
    # generate plot
    g <- ggplot(unigramMatrixFreq[1:20,], aes(x = reorder(word, -freq), y = freq))
    g <- g + geom_bar(stat = "identity", fill = I("grey50"))
    g <- g + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
    g <- g + xlab("")
    g <- g + ylab("Frequency")
    g <- g + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
                   axis.text.x = element_text(hjust = 1.0, angle = 45),
                   axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
    g <- g + ggtitle("20 Most Common Unigrams")
    print(g)
    
    
    
    
    source("Ngrams_Tokenizer.R")
    unigram.tokenizer <- ngram_tokenizer(1)
    wordlist <- unigram.tokenizer(corpus)
    unigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
    names(unigram.df) <- c("word","freq")
    unigram.df <- unigram.df[with(unigram.df, order(-unigram.df$freq)),]
    row.names(unigram.df) <- NULL
    save(unigram.df, file="unigram.Rda")
    
    
    
    
freq_frame <- function(tdm){
    # Helper function to tabulate frequency
    freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
    freq_frame <- data.frame(word=names(freq), freq=freq)
    return(freq_frame)
}

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))



text_sample <- VCorpus(VectorSource(text_sample))
text_sample <- preprocessCorpus(text_sample)

tdm1a <- TermDocumentMatrix(text_sample)
tdm1 <- removeSparseTerms(tdm1a, 0.99)
freq1_frame <- freq_frame(tdm1)

tdm2a <- TermDocumentMatrix(text_sample, control=list(tokenize=BigramTokenizer))
tdm2 <- removeSparseTerms(tdm2a, 0.999)
freq2_frame <- freq_frame(tdm2)

tdm3a <- TermDocumentMatrix(text_sample, control=list(tokenize=TrigramTokenizer))
tdm3 <- removeSparseTerms(tdm3a, 0.9999)
freq3_frame <- freq_frame(tdm3)

tdm4a <- TermDocumentMatrix(text_sample, control=list(tokenize=QuadgramTokenizer))
tdm4 <- removeSparseTerms(tdm4a, 0.9999)
freq4_frame <- freq_frame(tdm4)


##  Analysis
#For each Term Document Matrix, we list the most common unigrams, bigrams, trigrams and fourgrams.

a<-wordcloud(freq1_frame$word, freq1_frame$freq, min.freq=200)
ggplot(freq1_frame, aes(x=reorder(word,freq), y=freq, fill=freq)) +
    geom_bar(stat="identity") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank()) +
    labs(y="Frequency", title="Most common unigrams in text sample")




source("Ngrams_Tokenizer.R")
unigram.tokenizer <- ngram_tokenizer(1)
wordlist <- unigram.tokenizer(my.corpus)
unigram.df <- data.frame(V1 = as.vector(names(table(unlist(wordlist)))), V2 = as.numeric(table(unlist(wordlist))))
names(unigram.df) <- c("word","freq")
unigram.df <- unigram.df[with(unigram.df, order(-unigram.df$freq)),]
row.names(unigram.df) <- NULL
save(unigram.df, file="unigram.Rda")


library(RWeka)



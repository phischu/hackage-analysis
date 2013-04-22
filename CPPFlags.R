library(wordcloud)
data <- read.table(file="CPPFlags.data",col.names=c("word","frequency"))
svg(filename="CPPFlags.svg")
wordcloud(data$word,data$frequency)

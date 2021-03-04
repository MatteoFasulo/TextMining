############## INSTALL ##############
install.packages("word2vec")
install.packages("udpipe")
install.packages("philentropy")
############## LOAD ##############
library(word2vec)
library(udpipe)
library(philentropy)
############## APPROACH ##############
data("brussels_reviews", package = "udpipe")
x <- subset(brussels_reviews, language="es") #Not working actually
which(x$language=="es")
xes <- x$feedback[1:500]
xes <- tolower(xes)
class(xes)
model <- word2vec(x = xes, type = "skip-gram",dim = 15, iter = 20)
vet_parola <- predict(model,"turismo",type = "embedding")
############## NESPRESSO ##############
nespresso <- read.csv(file = "20181110-inissia28_10.csv",sep = ";",stringsAsFactors = F)
names(nespresso)
class(nespresso)
xness <- nespresso$Review.Text
xness <- tolower(xness)
modelness <- word2vec(xness, "skip-gram",dim = 10, iter = 30)
matness <- as.matrix(modelness)
vetaroma <- predict(modelness,"aroma",type = "embedding")
vetgusto <- predict(modelness,"gusto",type = "embedding")
aromagusto <- rbind(vetaroma, vetgusto)
distance(aromagusto, method = "euclidean")
near_aroma <- predict(modelness,"aroma",type = "nearest")
near_gusto <- predict(modelness,"gusto",type = "nearest")
########################################################

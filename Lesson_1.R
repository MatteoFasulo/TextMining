############## INSTALL #########################################################
install.packages("word2vec")
install.packages("udpipe")
install.packages("philentropy")
install.packages("stopwords")
install.packages("syuzhet")
install.packages("tm")
install.packages("textclean")
############## LOAD ############################################################
library(word2vec)
library(udpipe)
library(philentropy)
library(stopwords)
library(syuzhet)
library(tm)
library(textclean)
############## APPROACH ########################################################
data("brussels_reviews", package = "udpipe")
x <- subset(brussels_reviews, language="es") #Not working actually
which(x$language=="es")
xes <- x$feedback[1:500]
xes <- tolower(xes)
class(xes)
model <- word2vec(x = xes, type = "skip-gram",dim = 15, iter = 20)
vet_parola <- predict(model,"turismo",type = "embedding")
############## NESPRESSO #######################################################
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
#########################CLEAN DATA#############################################
nespresso$Colour <- gsub("Stile: InissiaColore: ", "", nespresso$Colour)
nespresso$Colour <- gsub(" Utile Segnala un abuso", "", nespresso$Colour)
nespresso$Colour <- gsub("Stile: Inissia & AeroccinoColore: ", "", nespresso$Colour)
unique(nespresso$Colour)
nespresso$Colour <- gsub("Formato: Cucina", "NA", nespresso$Colour)
unique(nespresso$Colour)
table(nespresso$Colour)
barplot(prop.table(table(nespresso$Colour)))
colori <- as.matrix(table(nespresso$Colour))
nespresso_frasi <- get_sentences(nespresso$Review.Text)
################################################################################
nespressocorpus <- VCorpus(VectorSource(nespresso_frasi)) #OGGETTO PARTICOLARE 
stopwords("it")
nespressopulito <- tm_map(nespressocorpus, removeWords, stopwords("it"))

nespressopulito[[1]]$content
nespresso_frasi[1] #removed "tutto"
nespressopulito[[2]]$content

nespresso2 <- tm_map(nespressocorpus, tolower) #lower
nespresso2[[3318]]$content
nespresso_frasi[3318]

nespresso2 <- tm_map(nespressopulito, stripWhitespace) #no white spaces
nespresso2[[2]]$content

nespresso2 <- tm_map(nespressopulito, removePunctuation) #removed Punctuation
nespresso2[[3315]]$content
nespresso_frasi[3315]

nespresso2 <- tm_map(nespressopulito, removeNumbers) #removed Numbers
nespresso2[[3315]]$content

nespresso2 <- tm_map(nespressopulito, stemDocument) #cambia le forme verbali
nespresso2[[3300]]$content

################################################################################
ndf <- data.frame(text=unlist(sapply(nespressocorpus,'[',"content"))) #RICONVERTIRE A DATA FRAME
class(ndf)
names(ndf)

x <- read.csv(file = "20181110-inissia28_10.csv",sep = ";",stringsAsFactors = F)
x$Colour
x$Colour <- gsub(pattern = "Stile", replacement = "TTT", x$Colour)
x$Colour

ps <- c(": InissiaColore: ", "TTT", " Utile Segnala un abuso", "Formato: Cucina", "Inissia & AeroccinoColore: ", ": ") #multi pattern
x$Colour <- mgsub(pattern = ps, replacement = "", x$Colour) #gsup globale
unique(x$Colour)

pp <- stopwords("italian")
class(pp)

x$Review.Text <- tolower(x$Review.Text) #minuscolo
x$Review.Text

parole <-strsplit(x$Review.Text, " ",fixed =T) #manteniamo corrispondenza delle parole alla loro recensione originale
class(parole)
parole[[1]] #parole nella prima recensione
parole[[1]][2] #seconda parola della prima recensione
par <- unlist(parole) #unlist le parole 40k parole singole
class(par)

conti <- table(par) #analisi in frequenza
class(conti)
conti[10]
mean(conti) #ogni parola appare x volte
conti <- sort(conti, decreasing =TRUE) #ordinate in decrescente e 1246
conti[1:10] #10 parole più frequenti

library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(quanteda)
library(quanteda.textmodels)
library(caret)
library(readtext)
library(tm)
library(textcat)

# Setting the working directory
getwd()
setwd("C:/Users/RUXI/Desktop/X/files_fara_jobs")

# Reading the files and clean a little
data <- readtext("*.txt") 
data <- unique(data[,2])
data <- gsub("[^\x01-\x7F]", "", data)
data <- gsub('[[:punct:] ]+',' ',data)
data <- gsub("\r?\n|\r", " ", data)
data <- as.data.frame(data)
data$doc_id <- 1:nrow(data)
colnames(data)[1] ="text"


#Eliminating texts in other languages 
data$language <- sapply(data$text, textcat::textcat)
data <- data[data$language == "english", ]
data <- data[, !(names(data) %in% "language")]
data <- na.omit(data)
View(data)


# Eliminate stopwords and cleaning corpus
custom_stopwords <- c("as", "a", "his", "her", "read", 'more', 're', 'all', 'an', 'way', 'its', 'not', 'you', 'your', 'into', 'us', 'my', 's')
for(i in 1:length(data$text)){ 
  corpus <- Corpus(VectorSource(data$text[i])) %>% 
    tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(tolower)) %>%  
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map( removeWords, custom_stopwords) %>%
    tm_map(stripWhitespace) #%>% 
    #tm_map(stemDocument) 
  data$text[i] <- as.character(corpus[[1]]) # convert the cleaned corpus back to a text string   
  }
view(data)


# A function that takes the name of a file and returns the nr of postive
# sentiment words, negative sentiment words, the difference 
GetSentiment <- function(text, count){
  
  file <- gsub("\\$", "", text)  # remove any dollar signs (they're special characters in R)
  tokens <- data_frame(text = file) %>% unnest_tokens(word, text)  # tokenize
  
  positive = 0
  negative = 0
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(positive =  positive) %>%
    mutate(negative =  negative) %>%
    mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
    mutate(text = file) %>%  # add the text of our file
    mutate(Nr = count )
  
  return(sentiment) # return our sentiment dataframe
}

# Obtaining sentiments dataset
sentiments <- data_frame() 
count = 0
for(i in data$text){ # get the sentiments for each file in our datset
  count = count + 1
  sentiments <- rbind(sentiments, GetSentiment(i, count))
}
summary(sentiments) #-7 44


# Putting labels
neg <- sentiments[sentiments$sentiment  < 0 , ]
pos <- sentiments[sentiments$sentiment  > 5 , ]
results = rbind(pos,neg)
results$label <- ifelse(results$sentiment < 0, 0, 1)

classCounts <- table(results[, "label"])
print(classCounts)

# Build corpus
corpus <- corpus(results$text, docnames = results$Nr)
docvars(corpus, "sentiment") <- results$label
docvars(corpus, "id_numeric") <- 1:ndoc(corpus)
summary(corpus,5)

#Split corpus into train and test
set.seed(1234)
id_train <- sample(1:ndoc(corpus), 70/100*ndoc(corpus) , replace=F)
head(id_train, 10)
dfmat_train <- corpus_subset(corpus, id_numeric %in% id_train) %>% tokens() %>% dfm() #%>% dfm_weight(scheme="boolean")
dfmat_test <- corpus_subset(corpus, !(id_numeric %in% id_train)) %>% tokens %>% dfm() #%>% dfm_weight(scheme="boolean")

dfmat_matched <- dfm_match(dfmat_test, features=featnames(dfmat_train))
actual_class <- docvars(dfmat_matched, "sentiment")
id_test <- !((1:ndoc(corpus)) %in% id_train)

#----------Naive Bayes--------------------------------
sentmod.nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "sentiment"), distribution = "Bernoulli")
summary(sentmod.nb)

predicted_class <- predict(sentmod.nb, newdata=dfmat_matched)
tab_class <- table(actual_class,predicted_class)
tab_class

caret::confusionMatrix(tab_class, mode="everything", positive="1")
sentmod.nb$param[,1:10]

#Most positive words
sort(sentmod.nb$param["1",]/colSums(sentmod.nb$param),dec=T)[1:20]
#Most negative words
sort(sentmod.nb$param["0",]/colSums(sentmod.nb$param),dec=T)[1:20]


# Plot weights of words
post.pos <- sentmod.nb$param["1",]/colSums(sentmod.nb$param)
plot(colSums(dfmat_train),post.pos, pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Posterior Probabilities, Naive Bayes Classifier", ylab="<--- Negative  --- Positive  --->", xlab="Total Appearances")
text(colSums(dfmat_train),post.pos, colnames(dfmat_train),pos=4,cex=5*abs(.5-post.pos), col=rgb(0,0,0,1.5*abs(.5-post.pos)))

# Closer look at the negative
plot(colSums(dfmat_train),post.pos, pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Posterior Probabilities, Naive Bayes Classifier", ylab="<--- Negative  --- Positive  --->", xlab="Total Appearances", xlim=c(10,1000),ylim=c(0,.25))
text(colSums(dfmat_train),post.pos, colnames(dfmat_train),pos=4,cex=5*abs(.5-post.pos), col=rgb(0,0,0,1.5*abs(.5-post.pos)))

#  Closer look at the positive
plot(colSums(dfmat_train),post.pos, pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Posterior Probabilities, Naive Bayes Classifier, IMDB", ylab="<--- Negative Reviews --- Positive Reviews --->", xlab="Total Appearances", xlim=c(10,1000),ylim=c(0.75,1.0))
text(colSums(dfmat_train),post.pos, colnames(dfmat_train),pos=4,cex=5*abs(.5-post.pos), col=rgb(0,0,0,1.5*abs(.5-post.pos)))

#Evaluating predictions
predicted_prob <- predict(sentmod.nb, newdata=dfmat_matched, type="probability")
plot(density(predicted_prob[,"1"]), main = "Predicted Probabilities from Naive Bayes classifier", xlab="", ylab = "")
rug(predicted_prob[,"1"])
head(predicted_prob)
summary(predicted_prob)


#What's the most positive text in the test set according to this?
sort.list(predicted_prob[,"0"], dec=F)[1]
writeLines(as.character(corpus)[id_test][54])


#----------Ridge regression (Logistic with L2-regularization)--------------
library(glmnet)
library(doMC)

registerDoMC(cores=2) # parallelize to speed up
sentmod.ridge <- cv.glmnet(x=dfmat_train,
                           y=docvars(dfmat_train)$sentiment,
                           family="binomial", 
                           alpha=0,  # alpha = 0: ridge regression
                           nfolds=5, # 5-fold cross-validation
                           parallel=TRUE, 
                           intercept=TRUE,
                           type.measure="class")
plot(sentmod.ridge)

# actual_class <- docvars(dfmat_matched, "sentiment")
predicted_value.ridge <- predict(sentmod.ridge, newx=dfmat_matched,s="lambda.min")[,1]
predicted_class.ridge <- rep(NA,length(predicted_value.ridge))
predicted_class.ridge[predicted_value.ridge>0] <- "1"
predicted_class.ridge[predicted_value.ridge<0] <- "0"
tab_class.ridge <- table(actual_class,predicted_class.ridge)
tab_class.ridge


# Plot weights of words
plot(colSums(dfmat_train),coef(sentmod.ridge)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Ridge Regression Coefficients", ylab="<--- Negative--- Positive  --->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),coef(sentmod.ridge)[-1,1], colnames(dfmat_train),pos=4,cex=200*abs(coef(sentmod.ridge)[-1,1]), col=rgb(0,0,0,75*abs(coef(sentmod.ridge)[-1,1]))) # inexplicable error when knitting of alpha not in [0,1] range when it absolutely is

# Plot impact wighted
plot(colSums(dfmat_train),log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Ridge Regression Coefficients (Impact Weighted)", ylab="<--- Negative  --- Positive  --->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1], colnames(dfmat_train),pos=4,cex=50*abs(log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1]), col=rgb(0,0,0,25*abs(log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1])))

#Most positive and negative features by impact:
sort(log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1],dec=T)[1:20]
sort(log(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1],dec=F)[1:20]


# Plot error wighted
plot(colSums(dfmat_train),sqrt(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Ridge Regression Coefficients (Error Weighted)", ylab="<--- Negative --- Positive --->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),sqrt(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1], colnames(dfmat_train),pos=4,cex=30*abs(sqrt(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1]), col=rgb(0,0,0,10*abs(sqrt(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1])))

#Most positive and negative terms by error:
sort(sqrt(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1],dec=T)[1:20]
sort(sqrt(colSums(dfmat_train))*coef(sentmod.ridge)[-1,1],dec=F)[1:20]


#----------LASSO (Logistic with L1-regularization)-------------------------------------------------------
sentmod.lasso <- cv.glmnet(x=dfmat_train,
                           y=docvars(dfmat_train)$sentiment,
                           family="binomial", 
                           alpha=1,  # alpha = 1: LASSO
                           nfolds=5, # 5-fold cross-validation
                           parallel=TRUE, 
                           intercept=TRUE,
                           type.measure="class")
plot(sentmod.lasso)


# actual_class <- docvars(dfmat_matched, "Sentiment")
predicted_value.lasso <- predict(sentmod.lasso, newx=dfmat_matched,s="lambda.min")[,1]
predicted_class.lasso <- rep(NA,length(predicted_value.lasso))
predicted_class.lasso[predicted_value.lasso>0] <- "pos"
predicted_class.lasso[predicted_value.lasso<0] <- "neg"
tab_class.lasso <- table(actual_class,predicted_class.lasso)
tab_class.lasso

# Plot wights of words
plot(colSums(dfmat_train),coef(sentmod.lasso)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="LASSO Coefficients", ylab="<--- Negative --- Positive --->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),coef(sentmod.lasso)[-1,1], colnames(dfmat_train),pos=4,cex=2*abs(coef(sentmod.lasso)[-1,1]), col=rgb(0,0,0,.3))
#the vast majority of our coefficients are zero ... most features have no influence on the predictions

# Plot impact wighted
plot(colSums(dfmat_train),log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1], pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="LASSO Coefficients (Impact Weighted)", ylab="<--- Negative--- Positive--->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1], colnames(dfmat_train),pos=4,cex=.8*abs(log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1]), col=rgb(0,0,0,.25*abs(log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1])))

#Most positive and negative features by impact:
sort(log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1],dec=T)[1:20]
sort(log(colSums(dfmat_train))*coef(sentmod.lasso)[-1,1],dec=F)[1:20]

#----------Support vector machine---------------------------
library(e1071)
sentmod.svm <- svm(x=dfmat_train,
                   y=as.factor(docvars(dfmat_train)$sentiment),
                   kernel="linear", 
                   cost=10,  # arbitrary regularization cost
                   probability=TRUE)

# actual_class <- docvars(dfmat_matched, "Sentiment")
predicted_prob.svm <- attr(predict(sentmod.svm, newdata=dfmat_matched, probability=TRUE),"probabilities")[,"1"]
predicted_class.svm <- predict(sentmod.svm, newdata=dfmat_matched)
tab_class.svm <- table(actual_class,predicted_class.svm)
tab_class.svm

caret::confusionMatrix(tab_class.svm, mode="everything", positive="1")
beta.svm <- drop(t(sentmod.svm$coefs)%*%dfmat_train[sentmod.svm$index,])

# Plot weights for words
plot(colSums(dfmat_train),beta.svm, pch=19, col=rgb(0,0,0,.3), cex=.5, log="x", main="Support Vector Machine Coefficients (Linear Kernel)", ylab="<--- Negative sentiment ---  Positive sentiment --->", xlab="Total Appearances", xlim = c(1,50000))
text(colSums(dfmat_train),beta.svm, colnames(dfmat_train),pos=4,cex=10 * abs(beta.svm))


sort(beta.svm,dec=T)[1:20] #negative
sort(beta.svm,dec=F)[1:20] #positive

#----------Random Forests---------------------------------------------

library(randomForest)
dfmat.rf <- corpus %>%
  tokens() %>%
  dfm() %>%
  dfm_trim(min_docfreq=80,max_docfreq=300,verbose=TRUE)

dfmatrix.rf <- as.matrix(dfmat.rf)

set.seed(1234)
sentmod.rf <- randomForest(dfmatrix.rf[id_train,], 
                           y=as.factor(docvars(dfmat.rf)$sentiment)[id_train],
                           xtest=dfmatrix.rf[id_test,],
                           ytest=as.factor(docvars(dfmat.rf)$sentiment)[id_test],
                           importance=TRUE,
                           mtry=20,
                           ntree=100
)

predicted_class.rf <- sentmod.rf$test[['predicted']]
tab_class.rf <- table(actual_class,predicted_class.rf)
caret::confusionMatrix(tab_class.rf, mode="everything", positive="1")

varImpPlot(sentmod.rf)

#----------ROC Curve-----------------------------
library(ROCit)
roc_empirical.nb <- rocit(score =predict(sentmod.nb, newdata=dfmat_matched, type="probability")[,"1"] , class = actual_class,negref = "0")
roc_empirical.ridge <- rocit(score = predicted_value.ridge, class = actual_class,negref = "0")
roc_empirical.lasso <- rocit(score = predicted_value.lasso, class = actual_class,negref = "0")
roc_empirical.svm <- rocit(score = predicted_prob.svm, class = actual_class,negref = "0")
roc_empirical.rf <- rocit(score = sentmod.rf$test$votes[,2], class = actual_class,negref = "0")

plot(c(0,1),c(0,1), type="n",main = "ROC Curves",ylab = "Sensitivity (TPR)", xlab = "1-Specificity (FPR)")
lines(c(0,1),c(0,1), col="grey", lty=2)
#lines(roc_empirical.nb$FPR,roc_empirical.nb$TPR,col="red",lwd=2)
#lines(roc_empirical.ridge$FPR,roc_empirical.ridge$TPR,col="blue",lwd=2)
#lines(roc_empirical.lasso$FPR,roc_empirical.lasso$TPR,col="green",lwd=2)
lines(roc_empirical.svm$FPR,roc_empirical.svm$TPR,col="orange",lwd=2)
lines(roc_empirical.rf$FPR,roc_empirical.rf$TPR,col="purple",lwd=2)
legend(.6,.2,legend=c("Support Vector Machine","Random Forests"), col=c("orange","purple"),lty=1,lwd=2)
#"Naive Bayes","Ridge Regression","LASSO",
#"red","blue","green",

print(paste("AUC, Lexicoder: ",sprintf("%1.3f",roc_empirical.lsd$AUC)))
print(paste("AUC, Naive Bayes: ",sprintf("%1.3f",roc_empirical.nb$AUC)))
print(paste("AUC, Ridge Regression: ",sprintf("%1.3f",roc_empirical.ridge$AUC)))
print(paste("AUC, LASSO: ",sprintf("%1.3f",roc_empirical.lasso$AUC)))
print(paste("AUC, SVM: ",sprintf("%1.3f",roc_empirical.svm$AUC)))
print(paste("AUC, Random Forests: ",sprintf("%1.3f",roc_empirical.rf$AUC)))



#Conform Confusion Matrix, Random Forest si Support Vector Machine dau mai bine si restul sunt groaznice


#----------------Make Predictions-------------------------------
text1= "There will come a day when sustainable practices will be encouraged by all schools. Today, our team has managed to create a succesful forestation project that will encourage children of al ages to join the innitiative for a greener future. As of now, 400 saplings have been planted by the children of Emigrand School alone."
text2 = "The sight of the landfill near the outskirts of town... is horrific! If only the authorities would do something about this ecological disaster. The only hope is that the city council would put an end to all this waste and close the landfill once and for all, ending the air, water and soil pollution in the area. It is horrible. Can somebody please do something?"
text <- c(text1, text2)

# Rebuild corpus
corpusnew <- corpus( c(results$text,text), docnames = c(results$Nr, c(1282,1283)))
docvars(corpusnew, "sentiment") <- c(results$label, c(1,0))
docvars(corpusnew, "id_numeric") <- 1:ndoc(corpusnew)
summary(corpusnew,5)


#Split corpus into train and test
#id_train  si setul de training raman cele vechi
dfmat_testnew <- corpus_subset(corpusnew, !(id_numeric %in% id_train)) %>% tokens %>% dfm() 

dfmat_matched <- dfm_match(dfmat_testnew, features=featnames(dfmat_train))
actual_class <- docvars(dfmat_matched, "sentiment")
id_test <- !((1:ndoc(corpusnew)) %in% id_train)


#RF
library(randomForest)
dfmat.rf <- corpusnew %>%
  tokens() %>%
  dfm() %>%
  dfm_trim(min_docfreq=50,max_docfreq=300,verbose=TRUE)

dfmatrix.rf <- as.matrix(dfmat.rf)

set.seed(1234)
sentmod.rf <- randomForest(dfmatrix.rf[id_train,], 
                           y=as.factor(docvars(dfmat.rf)$sentiment)[id_train],
                           xtest=dfmatrix.rf[id_test,],
                           ytest=as.factor(docvars(dfmat.rf)$sentiment)[id_test],
                           importance=TRUE,
                           mtry=20,
                           ntree=100
)

predictions <- sentmod.rf$test[['predicted']]
predictions[(length(predictions)-1):length(predictions)] 


#SVM
library(e1071)
sentmod.svm <- svm(x=dfmat_train,
                   y=as.factor(docvars(dfmat_train)$sentiment),
                   kernel="linear", 
                   cost=10,  # arbitrary regularization cost
                   probability=TRUE)

predicted_class.svm <- predict(sentmod.svm, newdata=dfmat_matched)
predicted_class.svm[(length(predicted_class.svm)-1):length(predicted_class.svm)]




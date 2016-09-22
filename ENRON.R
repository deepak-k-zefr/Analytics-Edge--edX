
emails=read.csv("energy_bids.csv",stringsAsFactors = FALSE);
str(emails)
#855 emails and  if it has something to do with energy schedules or bids response= 1 or else 0 
emails$email[1]
strwrap(emails$email[1])
strwrap(emails$email[2])
emails$responsive[2]
table(emails$responsive)

#preposses the corpus
library(tm)
corpus=Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
tm_map(corpus,tolower)
tm_map(corpus,removePunctuation)
tm_map(corpus,removeWords,stopwords("english"))
tm_map(corpus,stemDocument)

strwrap(corpus[[1]])


#Bag of Words
dtm=DocumentTermMatrix(corpus)
dtm=removeSparseTerms(dtm,0.97)
dtm
labeledTerms=as.data.frame(as.matrix(dtm))
labeledTerms$responsive=emails$responsive
str(labeledTerms)

#testing different models
library(caTools)
set.seed(144)
spl=sample.split(labeledTerms$responsive,0.7)
train=subset(labeledTerms,spl==TRUE)
test=subset(labeledTerms,spl==FALSE)
library(rpart)
library(rpart.plot)
emailCART=rpart(responsive~.,data=train,method="class")
prp(emailCART)
pred=predict(emailCART,newdata = test)
pred[1:10,]
pred.prob=pred[,2]
table(test$responsive,pred.prob>=.5)
(197+18)/(197+18+23+19)
# CART model=83%
#baseline model
table(test$responsive)
215/257
#83% for baseline model

#ROC curve
library(ROCR)
predROCR=prediction(pred.prob,test$responsive)
perfROCR=performance(predROCR,"tpr","fpr")
plot(perfROCR,colorize=TRUE)
performance(predROCR,"auc")@y.values

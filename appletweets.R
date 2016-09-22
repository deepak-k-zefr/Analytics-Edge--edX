#apple tweets sentiment detection
#NLP-understanding human language eg SIRI,googlenow
#Understand public perception.
#twitter data is publicly avalibale or use API
# outocme variable=+ve,_ve or nyetral
# Amazone mechanial turk- people claasifying tweets.
#people judge sentiments
#options= strongly -ve,+ve nuetral or +ve or -ve


#Bag of words tranforms text to independent variables


#bag of words for text analytics
# convert  words into features
# count of each word  and uses count as independent variable
# one feature for each word.
# bag of words-baseline of text analytics
# prepoocessing increases bag of words accuracy

#preprocessing the text- clean up irregularities for better predictions
#TOLOWER-eg Apple and apple APPPle are different- SOLUTION:convert all to lower case
#PUNCTUATIONS- remove punctuations- useless. but sometimes @apple #aPPLe are  different.
#STOPWORDS- the,this.that which- will nor affect predictive quality. but "The who" is a useful stopwrd that may be removed
#STEMMING- arguing, argues ,argued,argue are the same word. STEM= argu. using a daabase of words and stems.
#P0rter stemmer



tweets=read.csv("tweets.csv",stringsAsFactors = FALSE)

str(tweets)
# two types of data- the tweet and the score
head (tweets)

# we want to detect neagtive sentiment
# all tweets with score<1 are negative tweets
tweets$Negative=as.factor(tweets$Avg<=-1)

############### preprocessing 
library(tm)
library(SnowballC)
table(tweets$Negative)
# corpus is a collection of documents
corpus=Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
#corpus = tm_map(corpus, PlainTextDocument)
# change to all lower course. like we pass to tapply function
#corpus=tm_map(corpus,tolower)
corpus = tm_map(corpus,content_transformer(tolower))
corpus[[1]]
corpus=tm_map(corpus,removePunctuation)
corpus[[1]]
corpus=tm_map(corpus,removeWords,c("apple",stopwords("english")))
corpus[[1]]
corpus=tm_map(corpus,stemDocument)
corpus[[1]]

#preprocessing data over
       
#SECTION 2
#extract frequency using bag of words
       frequencies=DocumentTermMatrix(corpus)# rows=documents #column= word in the tweets  value=grequency
       frequencies
       # documents 1000:1005 and word 500:505
       inspect(frequencies[1000:1005,505:515])
       # very sparse data many zeroes in  matrix
       findFreqTerms(frequencies,lowfreq = 20)# number of words>20 frequencies will only be dispalyed
       #0nly 56 words/3289 words>20 times
#Remove terms that dont appear very often
       #0.995 is the sparsity threshold. 0.0995= only words that appear in 0.5% of tweets
       sparse=removeSparseTerms(frequencies,0.995)       
       sparse
       # convert spoarse onto data frame tweet sparse
       tweetsSparse=as.data.frame(as.matrix(sparse))
       colnames(tweetsSparse)=make.names(colnames(tweetsSparse)) #convert all of our names=to appropriate variable names. some  names are numbers
       
       
       
       #dependent variable tweetSparse$Negative
       tweetsSparse$Negative=tweets$Negative #
       library(caTools)
       #
       # split data to training and test
       set.seed(123)
       # 70 % in training set
      split= sample.split(tweetsSparse$Negative,SplitRatio = 0.7)
       trainSparse=subset(tweetsSparse,split==TRUE)
       testSparse=subset(tweetsSparse,split==FALSE)
     # data is ready.
       
#section
       #build predictive model now
#Predict sentiment
       # use CART and logistic regression to predict -ve sntiment
       #rpart-Recursive partitioning for classification, regression and survival trees. 
       library(rpart)
       library(rpart.plot)
       #Plotting rpart trees with prp
       tweetCART=rpart(Negative~.,data=trainSparse,method="class") #negative=dependent all variables= independent
       #class- Classidification model
       prp(tweetCART)
       #TRUE- negative sentiment
       #false=non negative sentiment
       
       #evealutate numnerical performance on tests set
       predictCART=predict(tweetCART,newdata =testSparse,type="class")
       table(testSparse$Negative,predictCART)
       #confision matrix  Diagonal are correct preidctions
       table(testSparse$Negative)
#986+67/(13+115+986+67)       
#89% accurate       
     #If you were to use cross-validation to pick the cp       parameter for the CART model, the accuracy would increase to about the same as the random forest model.
       #
#baseline model
       table(testSparse$Negative)
       #999/(999+182)
       #84.5%
#Random Forrest model
       library(randomForest)
       set.seed(123)
       tweetRF=randomForest(Negative~.,data=trainSparse)
       predictRf=predict(tweetRF,newdata = testSparse)
       table(testSparse$Negative,predictRf)
       #(990+125)/(990+125+9+57)
#94.4% ACCURACY
       
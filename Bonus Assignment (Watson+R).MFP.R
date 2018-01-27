#Farees Patel
#Bonus Assignment
#IST687 - Watson Analysis Lab

library(ggplot2)
install.packages('httr')
library(httr)

#Step-1 (1,2)
#credential for AlchemyAPI
YOUR_API_KEY<- '***********************************'

#Step-1(3)
#Load the Cognizer package hosted on Github

install.packages('devtools')
library(devtools)
install.packages("https://github.com/jeroenooms/curl/archive/master.tar.gz", repos = NULL)
devtools::install_github("ColumbusCollaboratory/cognizer")
library(cognizer)

#Step-1(4)
#	Passing authentication using the API key. Status 200 in reponse indicates the authentication is successfully

GET(url=paste("http://gateway-a.watsonplatform.net/calls/info/GetAPIKeyInfo?apikey=YOUR_API_KEY&outputMode=json"))

#Step-2(5)
#Reading the SBA speech text and cleaning the speech text

install.packages('readr')
library(readr)

speech<-read_file("SBA-speech.txt")
speech<-gsub("\r\n\r","",speech) 
speech<-gsub("\n","",speech)
speech<-gsub('["]','',speech)
speech

#Step-2(6)
#Using the function in WatsonR to get the overall sentiment score for SBA speech.

result <- text_sentiment(speech, YOUR_API_KEY)
result[[1]]$docSentiment$type
result[[1]]$docSentiment$score

#Step-2(7)
#Dividing the SBA-speech into multiple seperate sentences

p<-strsplit(speech,"[.]")
p<-p[[1]]
file<- p
file
#	Calculate sentiment score for each Quarter of the speech

num<-4
increment <- length(file)/num
Finalresult<-NULL
FinalType<- NULL

for (i in 1:num){
  
  start <- (i-1)*increment
  end <- start+increment
  TEXT<- (file[start:end])
  q<- paste(TEXT, collapse = ". ")
  result <- text_sentiment(q, YOUR_API_KEY)
  Finalresult[i]<-result[[1]]$docSentiment$score
  FinalType[i]<- result[[1]]$docSentiment$type
}
Values<- as.numeric(Finalresult)
Quarter<- c("Q1","Q2","Q3","Q4")
sentimentAnalysis<- data.frame(Quarter,Values,FinalType)
sentimentAnalysis #Sentiment scores for each Quarter

#Step-2(8)
#	Plotting the comparison results for the 4 numbers via bar chart.

g<- ggplot(data=sentimentAnalysis, aes(x=Quarter, y=Values, fill=Values))
g<- g+ geom_bar(stat="identity")
g<- g+ geom_text(aes(label=Values), vjust = ifelse(Values >= 0, -0.25, 1))
g<- g+ ylab("Sentiment Scores")
g<-g+ ggtitle('Sentiment scores for each Quarter')
g #plot of Sentiment scores for each Quarter

#Step-3: Comparing the results

#(1)	How do the results compare with what you saw when we did the text mining work (i.e., using the 'tm' package)

#Answer: 
#As we analysed the sentiment scores using the 'tm' package in the class, we observed that SBA speech is just less than 7% negative (-0.068)
#however, using IBM Watson's Alchemy package we analysed the SBA speech to be 70% negative (-0.7039)

#(a) : Where the results the same or different?  If different, can you explain the difference?

#Answer:
#The results are definately different because of the following 2 reasons:
#(i) : The 'tm' package compare words in the speech with the pre-defined sentiment scores of positive and negative words to calculate the overall sentiment score of the speech
      #This may be deceptive sometimes because for instance 'the movie is not bad' will give a negative sentiment but the actual sentiment is positive

#(ii) : The IMB Watson's Alchemy package has an advanced algorithm looks for words that carry a positive or negative connotation then figures out which person, place or thing they are referring to.
        #It also understands negations (i.e. "this car is good" vs. "this car is not good") and modifiers (i.e. "this car is good" vs. "this car is really good").
        #Hence, the Alchemy package algorithm predicts the sentiments very close to the actual sentiments.

#(b) :	Was one method easier than the other (i.e. using Watson vs the 'tm' package)?

#Answer
#Using Watson was easy for me because we just need to load the API and there are pre-defined functions to find sentiments of text
#Whereas, in the 'tm' package we need to first prepare the text file by performing transformations and then need to compare it with the positive and negative wordlist to find the sentiment score of the text.


#Step-4
#	Finding the dominant emotion for each Quarter of the speech

EmotionalAnalysis=NULL

for (i in 1:num){
  
  start <- (i-1)*increment
  end <- start+increment
  TEXT<- (file[start:end])
  q<- paste(TEXT, collapse = ". ")
  result <- text_emotion(q, YOUR_API_KEY)
  Anger<-result[[1]]$docEmotions$anger
  Disgust<-result[[1]]$docEmotions$disgust
  Joy<-result[[1]]$docEmotions$joy
  Sadness<-result[[1]]$docEmotions$sadness
  add<- data.frame(Anger,Disgust,Joy,Sadness)
  EmotionalAnalysis<- rbind(EmotionalAnalysis,add)
}


Emotion<-colnames(EmotionalAnalysis)[apply(EmotionalAnalysis,1,which.max)]
Value<- apply(EmotionalAnalysis,1,max)
EmotionalAnalysis<- data.frame(Quarter,EmotionalAnalysis)

EmotionalAnalysis #Emotional Scores for every Quarter

Emotion<- paste(Quarter,"-",Emotion)

DominantEmotion<- data.frame(Emotion, Value) 
DominantEmotion # Dominant emotion for each quarter

g<- ggplot(data=DominantEmotion, aes(x=Emotion, y=Value, fill=Value))
g<- g+ geom_bar(stat="identity")
g<- g+ geom_text(aes(label=Value), vjust =-0.25)
g<- g+ ylab("Emotional Scores")
g<- g+ ggtitle('Dominant emotion for each Quarter of the speech')
g # plot of Dominant emotion for each quarter

#End of assignment
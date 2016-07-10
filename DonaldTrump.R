library(ggplot2)
library(twitteR)
library(ROAuth)
library(httr)
library(ggmap)
library(dplyr)
library(curl)
library(RCurl)
library(maps)
library(dismo)
library(XML)
library(streamR)

#Likes and retweets: positive donald trump sentiment
#@RealDonalTrump References: sentimentality alogorithm to see if positive or negative
# Set API Keys
api_key <- "7HQoP73cvwueWpMxsoDIeG9ve"
api_secret <- "Ty0jwrDdpd0BQTMhlbRe5OiI22NqnbuBXdc0gdY0NbKaknPaq4"
access_token <- "750683362650566656-RiPSDKN4Q7pYvZgzz9U7m06aHEMSg45"
access_token_secret <- "D47dlD1P0kRgJhnKOlDqzEg74vfwHKyW2Lt9LBs6m3AB9"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
setwd("/Users/DaraLadje/Documents/TwitterBot")
tweets_donny <- searchTwitter('@realDonaldTrump', n=500,lang = 'en', since=as.character(Sys.Date()-1)) #since 24 hours ago
library(plyr)
tweets_donny.df <- twListToDF(tweets_donny)

#Above code connects to Twitter account. Takes in tweets that have Donald Trump's twitter handle


TrumpRetweet<-filter(tweets_donny.df,isRetweet==TRUE)
#separate retweets and actual comments
TrumpComment<-filter(tweets_donny.df,isRetweet==FALSE)



RetweetNames<-dplyr::select(TrumpRetweet,screenName)


feed_donny = laply(tweets_donny, function(t) t$getText())

yay = scan('positive-words.txt',
           what='character', comment.char=';')
boo = scan('negative-words.txt',
           what='character', comment.char=';')
# Add a few twitter-specific negative phrases
bad_text = c(boo)
good_text = c(yay)

#sentimentality algorithm to separate the positive tweets from the negative ones
score.sentiment = function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, good_text, bad_text) 
  {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('[-0-9]\\S+', '', sentence)
    #to remove emojis
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)        
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
# Call the function and return a data frame
MakeAmericaGreat <- score.sentiment(feed_donny, good_text, bad_text, .progress='text')

# Cut the text, just gets in the way
# Remove neutral values of 0

MakeAmericaGreat<-filter(MakeAmericaGreat,MakeAmericaGreat$score != 0) #neutral scores not needed
MakeAmericaGreat<-MakeAmericaGreat[!grepl("RT ", MakeAmericaGreat$text),]

proTrump<-filter(MakeAmericaGreat,score>0) #trump supporters
antiTrump<-filter(MakeAmericaGreat,score<0) #trump haters



proTrump$text<-as.character(proTrump$text)
antiTrump$text<-as.character(antiTrump$text)

proTrumpName<-dplyr::select(merge(proTrump,TrumpComment),screenName) #add the username to the tweets
antiTrumpName<-dplyr::select(merge(antiTrump,TrumpComment),screenName)


#get the location on someone's profile
#this location is only available if the user has a public profile
#the location may not even be real
#geocode determines the coordinates to the best of its ability
prolocations<-sapply(proTrumpName$screenName,function(x){getUser(x)$location})
proTrumpName$location<-prolocations
proTrumpName<-filter(proTrumpName, location!="",location!="NULL")
proTrumpName<-proTrumpName[tolower(gsub(",.*$", "", proTrumpName$location)) %in% coordinates$City,]
proLocations<-dplyr::select(proTrumpName,location)
proDots<-dplyr::select(geocode(proLocations$location),longitude,latitude)

antilocations<-sapply(antiTrumpName$screenName,function(x){getUser(x)$location})
antiTrumpName$location<-antilocations
antiTrumpName<-filter(antiTrumpName, location!="",location!="NULL")
antiTrumpName<-antiTrumpName[tolower(gsub(",.*$", "", antiTrumpName$location)) %in% coordinates$City,]
antiLocations<-dplyr::select(antiTrumpName,location)
antiDots<-dplyr::select(geocode(antiLocations$location),longitude,latitude)

Retweetlocations<-sapply(RetweetNames$screenName,function(x){getUser(x)$location})
RetweetNames$location<-Retweetlocations
RetweetNames<-filter(RetweetNames, location!="",location!="NULL")
RetweetNames<-RetweetNames[tolower(gsub(",.*$", "", RetweetNames$location)) %in% coordinates$City,]
RetweetLocations<-dplyr::select(RetweetNames,location)
RetweetDots<-dplyr::select(geocode(RetweetLocations$location),longitude,latitude)

proDots$Trump2016<-"Trump Supporter"
antiDots$Trump2016<-"Trump Hater"
RetweetDots$Trump2016<-"Trump Retweet"
finalDot<-rbind(proDots,antiDots,RetweetDots)


#map of all points
usa_center = as.numeric(geocode("United States"))
USAMap = ggmap(get_googlemap(center=c(lon = usa_center[3],lat = usa_center[4]), scale=2, zoom=4), extent="device")
finalMap<-USAMap +
geom_point(aes(x=longitude, y=latitude,color = Trump2016), data=finalDot, alpha=0.8, size=2) +
  theme(legend.title = element_text(colour="black", size=10, 
                                    face="bold")) +
  scale_color_manual(values=c('Blue','Orange','Red'))


png(filename = '/Users/DaraLadje/Documents/TwitterBot/finalMap.png', width = 1000, height = 600)
finalMap
dev.off()

captions <- c("Who agrees with Donald Trump's tweets?", "Hey America! Big election coming up. Checkout the locations of people who post positively and negatively about Donald Trump.", "Where do Donald Trump Supporters live? Find out now.", "#FuckDonalTrump", "#ILoveMexicans", "See where people agree with what Trump says. Shocking.", "Upcoming ELection is huge.", "Why do people like Trump?", "Who would even retweet Donald Trump.")
tweet(sample(captions, 1), mediaPath = '/Users/DaraLadje/Documents/TwitterBot/finalMap.png')

quit(save="no")

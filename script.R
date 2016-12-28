#Read the base file
dataset1 <- read.delim("C:/Users/jkesavan/Documents/CH43_20160908_104658.csv", header = TRUE, sep = ",")
subset = head(dataset, 50000)
View(subset)
ncol(dataset1)
nrow(dataset1)


#Read the file from Qlik
dataset2 <- read.delim("C:/Users/jkesavan/Documents/Navision transaction overrides - July 2016.csv", header = TRUE, sep = ",")
subset = head(dataset, 50000)
View(dataset2)
ncol(dataset2)
nrow(dataset2)

dataset3 = result = dataset1[dataset1$Posting.Date %in% dataset2$Posting.Date &
    dataset1$Firm.Code %in% dataset2$Firm.Code &
    dataset1$Document.No %in% dataset2$Document.No &
    dataset1$Vendor.No_..Pay.To. %in% dataset2$Vendor.No_..Pay.To.
,]
View(dataset3)
nrow(dataset3)

write.table(dataset3,
            file = "C:/Users/jkesavan/Documents/MatchingSet.csv",
            row.names = FALSE,
            na = "",
            col.names = TRUE,
            sep = ",")

library(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM dataset3 EXCEPT SELECT * FROM dataset2')
View(a1NotIna2)

cname <- file.path("C:/Users/jkesavan/Documents/VSS/JRLibs", "SampleTexts")
cname
dir(cname)

library(tm)
docs <- Corpus(DirSource(cname))
summary(docs)

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)

library(SnowballC)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)
inspect(dtm)
tdm <- TermDocumentMatrix(docs)
tdm

freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq)
m <- as.matrix(dtm)
dim(m)
write.csv(m, file = "C:/Users/jkesavan/Documents/VSS/JRLibs/dtm.csv")

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)

freq[head(ord)]
freq[tail(ord)]

freq <- colSums(as.matrix(dtms))
freq

wf <- data.frame(word = names(freq), freq = freq)
head(wf)

library(ggplot2)
p <- ggplot(subset(wf, freq > 20), aes(word, freq))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

library(wordcloud)
wordcloud(names(freq), freq, min.freq = 5)
set.seed(142)
wordcloud(names(freq), freq, min.freq = 3, scale = c(5, .1), colors = brewer.pal(6, "Dark2"))


#Read the base file
dataset1 <- read.delim("C:/Users/jkesavan/Documents/CH43_20160908_104658.csv", header = TRUE, sep = ",")
subset = head(dataset, 50000)
View(subset)
ncol(dataset1)
nrow(dataset1)


#Read the file from Qlik
dataset2 <- read.delim("C:/Users/jkesavan/Documents/Navision transaction overrides - July 2016.csv", header = TRUE, sep = ",")
subset = head(dataset, 50000)
View(dataset2)
ncol(dataset2)
nrow(dataset2)

dataset3 = result = dataset1[dataset1$Posting.Date %in% dataset2$Posting.Date &
    dataset1$Firm.Code %in% dataset2$Firm.Code &
    dataset1$Document.No %in% dataset2$Document.No &
    dataset1$Vendor.No_..Pay.To. %in% dataset2$Vendor.No_..Pay.To.
,]
View(dataset3)
nrow(dataset3)

write.table(dataset3,
            file = "C:/Users/jkesavan/Documents/MatchingSet.csv",
            row.names = FALSE,
            na = "",
            col.names = TRUE,
            sep = ",")

library(sqldf)
a1NotIna2 <- sqldf('SELECT * FROM dataset3 EXCEPT SELECT * FROM dataset2')
View(a1NotIna2)

cname <- file.path("C:/Users/jkesavan/Documents/VSS/JRLibs", "SampleTexts")
cname
dir(cname)

library(tm)
docs <- Corpus(DirSource(cname))
summary(docs)

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)

library(SnowballC)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)
inspect(dtm)
tdm <- TermDocumentMatrix(docs)
View(tdm)
inspect(tdm)

tdm = as.matrix(tdm)
colnames(tdm) = c("ATT", "Verizon")

library(wordcloud)

comparison.cloud(tdm, random.order = FALSE,
colors = c("#00B2FF", "red"),
title.size = 1.5, max.words = 500)

my.df <- as.data.frame(inspect(tdm))
my.df.scale <- scale(my.df)
d <- dist(my.df.scale, method = "euclidean")
fit <- hclust(d, method = "ward")
plot(fit)

x <- list(a = 1:10, b = 2:25, b = 100:1090)
View(x)



freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq)
m <- as.matrix(dtm)
dim(m)
write.csv(m, file = "C:/Users/jkesavan/Documents/VSS/JRLibs/dtm.csv")

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)

freq[head(ord)]
freq[tail(ord)]

freq <- colSums(as.matrix(dtms))
freq

wf <- data.frame(word = names(freq), freq = freq)
head(wf)

library(ggplot2)
p <- ggplot(subset(wf, freq > 20), aes(word, freq))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

library(wordcloud)
wordcloud(names(freq), freq, min.freq = 5)
set.seed(142)
wordcloud(names(freq), freq, min.freq = 3, scale = c(5, .1), colors = brewer.pal(6, "Dark2"))


library(tidyjson) # this library
library(dplyr)
purch_json <- '
[
  {
    "name": "bob", 
    "purchases": [
      {
        "date": "2014/09/13",
        "items": [
          {"name": "shoes", "price": 187},
          {"name": "belt", "price": 35}
        ]
      }
    ]
  },
  {
    "name": "susan", 
    "purchases": [
      {
        "date": "2014/10/01",
        "items": [
          {"name": "dress", "price": 58},
          {"name": "bag", "price": 118}
        ]
      },
      {
        "date": "2015/01/03",
        "items": [
          {"name": "shoes", "price": 115}
        ]
      }
    ]
  }
] '

purch_items <- purch_json %>%
  gather_array %>% # stack the users 
spread_values(person = jstring("name")) %>% select(person) # extract the user name
select(person)
enter_object("purchases") %>% gather_array %>% # stack the purchases
spread_values(purchase.date = jstring("date")) %>% # extract the purchase date
enter_object("items") %>% gather_array %>% # stack the items
spread_values(# extract item name and price
    item.name = jstring("name"),
    item.price = jnumber("price")
  ) %>%
select(person), purchase.date, item.name, item.price) # select only what is needed
q()


respjs = '{ "status":["OK"], 
"usage":["By accessing AlchemyAPI or using information generated by AlchemyAPI, you are agreeing to be bound by the AlchemyAPI Terms of Use: http://www.alchemyapi.com/company/terms.html"], 
"url":[""], 
"language":["english"], 
"entities":[{ "type":"Person", "relevance":"0.33", "count":"2", "text":"Donald Trump", "disambiguated": { "subType":["AwardNominee", "AwardWinner", "Celebrity", "CompanyFounder", "TVPersonality", "TVProducer", "FilmActor", "TVActor"], "name":"Donald Trump", "website":"http://www.trumponline.com/", "dbpedia":"http://dbpedia.org/resource/Donald_Trump", "freebase":"http://rdf.freebase.com/ns/m.0cqt90", "opencyc":"http://sw.opencyc.org/concept/Mx4rv0ncIZwpEbGdrcN5Y29ycA", "yago":"http://yago-knowledge.org/resource/Donald_Trump" }}, { "type":"Person", "relevance":"0.33", "count":"1", "text":"Bill Clinton", "disambiguated": { "subType":["MusicalArtist", "Politician", "Appointer", "AwardWinner", "Celebrity", "MusicalGroupMember", "OfficeHolder", "PoliticalAppointer", "TVPersonality", "FilmActor", "FilmWriter", "USPresident"], "name":"Bill Clinton", "website":"http://www.clintonlibrary.gov/", "dbpedia":"http://dbpedia.org/resource/Bill_Clinton", "freebase":"http://rdf.freebase.com/ns/m.0157m", "opencyc":"http://sw.opencyc.org/concept/Mx4rwQBp5JwpEbGdrcN5Y29ycA", "yago":"http://yago-knowledge.org/resource/Bill_Clinton", "musicBrainz":"http://zitgist.com/music/artist/a11bd200-7f0b-43a6-b7fe-4ea04929a42b" }}, { "type":"Person", "relevance":"0.33", "count":"1", "text":"Hillary Clintons", "disambiguated": { "subType":{ }}}] }'



alchemyJson = '{
    "status":"OK",
    "usage":"By accessing AlchemyAPI or using information generated by AlchemyAPI, you are agreeing to be bound by the AlchemyAPI Terms of Use: http://www.alchemyapi.com/company/terms.html",
    "url":"",
    "totalTransactions":"3",
    "language":"english",
    "entities":[{
    "type":"Person",
      "relevance":"0.89066",
      "sentiment": {
        "type":"positive",
        "score":"0.234428",
        "mixed":"1"
    },
      "emotions": {
        "anger":"0.195003",
        "disgust":"0.20833",
        "fear":"0.303655",
        "joy":"0.3299",
        "sadness":"0.233234"
    },
      "count":"7",
      "text":"Elliot Turner"
    }, {
        "type":"PrintMedia",
      "relevance":"0.252507",
      "sentiment": {
            "type":"neutral"
        },
      "emotions": {
            "anger":"0.147791",
        "disgust":"0.219509",
        "fear":"0.363833",
        "joy":"0.332399",
        "sadness":"0.163618"
        },
      "count":"1",
      "text":"PR Newswire"
    }, {
        "type":"Company",
      "relevance":"0.233257",
      "sentiment": {
            "type":"positive",
        "score":"0.260944"
        },
      "emotions": {
            "anger":"0.214484",
        "disgust":"0.192031",
        "fear":"0.118489",
        "joy":"0.561376",
        "sadness":"0.160291"
        },
      "count":"1",
      "text":"Walmart"
    }, {
        "type":"Company",
      "relevance":"0.211686",
      "sentiment": {
            "type":"positive",
        "score":"0.274219"
        },
      "emotions": {
            "anger":"0.172225",
        "disgust":"0.072838",
        "fear":"0.289085",
        "joy":"0.486758",
        "sadness":"0.277583"
        },
      "count":"1",
      "text":"AlchemyAPI"
    }, {
        "type":"Technology",
      "relevance":"0.163204",
      "sentiment": {
            "type":"neutral"
        },
      "emotions": {
            "anger":"0.187185",
        "disgust":"0.211998",
        "fear":"0.211838",
        "joy":"0.314706",
        "sadness":"0.22456"
        },
      "count":"1",
      "text":"iPhone."
    }, {
        "type":"Quantity",
      "relevance":"0.163204",
      "sentiment": {
            "type":"neutral"
        },
      "count":"1",
      "text":"three years"
    }
    ]
}
'

alchItems = respjs %>%
  #gather_array %>% # stack the users 
enter_object("entities") %>% gather_array %>%
spread_values(type = jstring("type"),
              relevance = jstring("relevance"),
              count = jstring("count"),
text = jstring("text")) %>%
#enter_object("sentiment")
enter_object("emotions") %>%
spread_values(anger = jstring("anger"),
            disgust = jstring("disgust"))

alchItems = respjs %>%
  #gather_array %>% # stack the users 
enter_object("entities") %>% gather_array %>%
spread_values(type = jstring("type"),
              relevance = jstring("relevance"),
              count = jstring("count"),
text = jstring("text"))


#enter_object("sentiment")
enter_object("emotions") %>%
spread_values(anger = jstring("anger"),
            disgust = jstring("disgust"))



df = as.data.frame(alchItems)

View(df)



#%>%
#spread_values(typesenti = jstring("type"),
#    scoresenti = jstring("score")) %>%
enter_object("emotions") %>%
spread_values(anger = jstring("anger"))


##Load the required Libraries
##http://www.rdocumentation.org/packages/jsonlite/functions/stream_in
loadBlueMixLibs = function() {
    library(RCurl)
    library(jsonlite)
}

##Call BlueMixAlchemyTextEntityExtraction
callBlueMixAlchemy = function(blueMixText) {
    ##Create the URL.
    blueMixAlchecmyEntityURL = "http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities?extract=entity"
    blueMixAlchecmyKey = "apikey=75c6700778eda02ffce454aed26b51f1faad5f54"
    blueMixAlchecmyOPMode = "outputMode=json"
    blueMixAlchecmyDataURL = paste("text=", blueMixText, sep = "")

    ##Default for paste is single space separator. Explicity provide - sep = no space.
    blueMixAlchemyFinalURL = paste(blueMixAlchecmyEntityURL, "&",
                                 blueMixAlchecmyKey, "&",
                                 blueMixAlchecmyOPMode, "&",
                                 blueMixAlchecmyDataURL, sep = "")

    ##Encode the URL
    blueMixAlchemyFinalURL = URLencode(blueMixAlchemyFinalURL)
    ##Invoke BlueMix
    blueMixAlchemyResponseDF <- fromJSON(blueMixAlchemyFinalURL,
                                       flatten = TRUE)

    return(blueMixAlchemyResponseDF)
}

loadBlueMixLibs()
bMixDf = callBlueMixAlchemy("On Monday Republican presidential candidate Donald Trump assured people that he will bring up Bill Clinton his history with women who are not his wife and Hillary Clintons role as a political adviser and defender during these crises.")
##This works
test = "BRASÍLIA - After an all-night debate, Brazil's Senate voted Thursday morning to suspend President Dilma Rousseff and begin an impeachment trial against her, ousting a deeply unpopular leader whose sagging political fortunes have come to embody widespread public anger over systemic corruption and a battered economy.
In a vote of 55 to 22, lawmakers accepted the charges against Ms. Rousseff, accusing her of borrowing from state banks to conceal a looming deficit, a budgetary sleight of hand that critics say was aimed at securing her re-election two years ago.
" We could no longer ignore these crimes and thus voted for
    impeachment, " Álvaro Dias, a senator from the Green Party, said shortly before casting his vote. " Having been assaulted by incompetence and wrongdoing, Brazilians expect punishment. "
During her impeachment trial, which could last six months, Ms. Rousseff will be replaced by a onetime ally, Vice President Michel Temer, who has been convicted of violating campaign finance limits and will now be under tremendous pressure to stem Brazil's worst economic crisis in decades.
"
bMixDf = callBlueMixAlchemy(test)
View(bMixDf)

#############Ignore Below#################
names(bMixDf$entities)
output = bMixDf$entities
View(output)
output$disambiguated.subType
x <- vapply(output$disambiguated.subType, length, 1L) ## How many items per list element
output <- output[rep(rownames(output), x),] ## Expand the data frame
output$Title <- unlist(output$disambiguated.subType, use.names = FALSE)
write.table(bMixDf, file = "C:\\Users\\jkesavan\\Documents\\bMixEntity.csv", row.names = FALSE)
write.csv(bMixDf, "C:\\Users\\jkesavan\\Documents\\bMixEntity.csv", qmethod = 'escape', quote = TRUE)


library(RCurl)
library(jsonlite)


more test for
    sampling
##Below is the Actual CURL Request Enity Analyzer
##curl -G "http://gateway-a.watsonplatform.net/calls/url/URLGetCombinedData?extract=entity,doc-sentiment,author,concept&apikey=[YOUR_API_KEY]&sentiment=1&outputMode=json&url=http://heidloff.net/article/worker-safety-demo-watson-iot-platform"
con <- stream_in(url("http://gateway-a.watsonplatform.net/calls/text/URLGetCombinedData?extract=entity&apikey=75c6700778eda02ffce454aed26b51f1faad5f54&outputMode=json&text=more%20test%20for%20sampling"))


library("jsonlite")
# compare formats
x <- iris[1:3,]
toJSON(x)
stream_out(x)

# Trivial example
mydata <- stream_in(url("https://data.ny.gov/api/views/9a8c-vfzj/rows.json?accessType=DOWNLOAD"))

View(con)

closeAllConnections()

testPOST = postForm(
  uri = 'http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities',
  .opts = list(
    postFields = list('extract' = "entity",
                     'apikey' = "75c6700778eda02ffce454aed26b51f1faad5f54",
                     'outputMode' = "json",
                     'text' = "On Monday Republican presidential candidate Donald Trump assured people that he will bring up Bill Clinton his history with women who are not his wife, and Hillary Clinton's role as a political adviser and defender during these crises."),
    httpheader = c('Content-Type' = 'application/json', 'Accept' = 'application/json'),
    httpauth = 1L,
    ssl.verifyhost = 0L, ssl.verifypeer = 0L
  ),
  style = "POST"
)


r2 <- GET("http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities", query = list('extract' = "entity",
                                                                                                   'apikey' = "75c6700778eda02ffce454aed26b51f1faad5f54",
                                                                                                   'outputMode' = "json",
                                                                                                   'text' = "On Monday Republican presidential candidate Donald Trump assured people that he will bring up Bill Clinton his history with women who are not his wife, and Hillary Clinton's role as a political adviser and defender during these crises."))


r2$content
json12 = content(r2$content, "text")
validate(json12)

extract <- function(x) {
    if (inherits(x, "response")) {
        return(content(x))
    } else if (length(x)) {
        lapply(x, extract) # apply `extract` recursively
    }
}

out <- unlist(lapply(r2$content, extract))

# Now we can parse
object <- jsonlite::fromJSON(json12)

View(r2)
print(r)
typeof(r)

test = toJSON(r)

h = getCurlHandle()
getURL("http://www.dice.com", curl = h)

encodedtext = URLencode("http://gateway-a.watsonplatform.net/calls/text/TextGetCombinedData?extract=entity&apikey=75c6700778eda02ffce454aed26b51f1faad5f54&outputMode=json&text=On Monday, Republican presidential candidate Donald Trump assured people that he will bring up Bill Clinton, his history with women who are not his wife, and Hillary Clinton's role as a political adviser and defender during these crises.")

encodedtext1 = URLencode("http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities?extract=entity&apikey=75c6700778eda02ffce454aed26b51f1faad5f54&outputMode=json&text=Expertise in Java and JEE Technologies, Spring/Hibernate Strong knowledge of Design in Java/Service layer technologies Desirable knowledge of RDBMS (Sybase Preferred )and SQL Desirable knowledge of AngularJS/HTML5 and other UI technologies Desirable knowledge of Good client facing and communication skills")


con <- stream_in(url("http://gateway-a.watsonplatform.net/calls/text/TextGetCombinedData?extract=entity&apikey=75c6700778eda02ffce454aed26b51f1faad5f54&outputMode=json&text=On Monday, Republican presidential candidate Donald Trump assured people that he will bring up Bill Clinton, his history with women who are not his wife, and Hillary Clinton's role as a political adviser and defender during these crises."))

con2134 <- fromJSON(encodedtext, flatten = TRUE)

con <- fromJSON(encodedtext, flatten = TRUE)
View(con2134)

sample = URLencode("4.Language detection is performed on the retrieved document before attempting named entity extraction. A minimum of 15 characters of text must exist within the requested HTTP document to perform language detection.")
con2 <- fromJSON("http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities?extract=entity&apikey=75c6700778eda02ffce454aed26b51f1faad5f54&outputMode=json&text=Key Skills: Core Java, Java, J2EE, API Automation, QA testing methodologies, C++, SQL, Angular JS, REST, HTML/HTML5, CSS/CSS3, Garret, Git, Jenkins, API, Weblogic, Websphere, Tomcat, MQ, Rabbit MQ, MVC, Spring, Hibernate, Struts, Core Java, Multi threading, JSP, Garbage collection, Design Pattern, Sql, SOAP, Web services, Java, Java, JVM, Sql, JMS, Maven, Sun Solaris.")
con2$entities

con312 <- fromJSON("http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities?extract=entity&apikey=75c6700778eda02ffce454aed26b51f1faad5f54&outputMode=json&text=On Monday, Republican presidential candidate Donald Trump assured people that he will bring up Bill Clinton, his history with women who are not his wife, and Hillary Clinton's role as a political adviser and defender during these crises.", flatten = TRUE)

postForm("http://gateway-a.watsonplatform.net/calls/text/TextGetRankedNamedEntities",
         "extract" = "entity",
         "apikey = " 75 c6700778eda02ffce454aed26b51f1faad5f54 ",
         " outputMode " = " json ",
         " text " = " On Monday Republican presidential candidate Donald Trump assured people that he will bring up Bill Clinton his history with women who are not his wife and Hillary Clintons role as a political adviser and defender during these crises. ")

##Load the required Libraries
loadBlueMixLibs = function(){
  ## These are the only libararies needed.
  library(RCurl)
  library(jsonlite)
  
}

##Call BlueMix

callBlueMixAlchemy = function(blueMixText){

 #encodedtext1 = URLencode(" http: / / gateway - a.watsonplatform.net / calls / text / TextGetRankedNamedEntities ? extract = entity & apikey = 75 c6700778eda02ffce454aed26b51f1faad5f54 & outputMode = json & text = Expertise in Java and JEE Technologies, Spring / Hibernate Strong knowledge of Design in Java / Service layer technologies Desirable knowledge of RDBMS(Sybase Preferred) and SQL Desirable knowledge of AngularJS / HTML5 and other UI technologies Desirable knowledge of Good client facing and communication skills ")


  ##Create the URL.
  blueMixAlchecmyEntityURL = " http: / / gateway - a.watsonplatform.net / calls / text / TextGetRankedNamedEntities ? extract = entity "
  blueMixAlchecmyKey = " apikey = 75 c6700778eda02ffce454aed26b51f1faad5f54 "
  blueMixAlchecmyOPMode = " outputMode = json "
  #blueMixAlchecmyDataURL = " url = https: / / www.dice.com / jobs / detail / Java - Developer - Collabera - Warren - NJ - 07059 / 10208346 / 0205 _JavaDevel_?icid=sr3-1p&q=java&l=New%20York,%20NY"
  ##Encode the text before calling BlueMix via Get - This will add the & etc of what is needed.
  #blueMixText = URLencode(blueMixText)
  blueMixText = "Expertise in Java and JEE Technologies, Spring/Hibernate Strong knowledge of Design in Java/Service layer technologies Desirable knowledge of RDBMS (Sybase Preferred )and SQL Desirable knowledge of AngularJS/HTML5 and other UI technologies Desirable knowledge of Good client facing and communication skills"
  #blueMixText = "https://www.dice.com/jobs/detail/Java-Developer-Collabera-Warren-NJ-07059/10208346/0205_JavaDevel_?icid=sr3-1p&q=java&l=New%20York,%20NY" 
  blueMixAlchecmyDataURL = paste("text=", blueMixText, sep = "")

  ##Default for paste is single space separator. Explicity provide - sep = no space.
  blueMixAlchemyFinalURL = paste(blueMixAlchecmyEntityURL, "&",
                                 blueMixAlchecmyKey, "&",
                                 blueMixAlchecmyOPMode, "&",
                                 blueMixAlchecmyDataURL, sep = "")

blueMixAlchemyFinalURL = URLencode(blueMixAlchemyFinalURL)
blueMixAlchemyResponseDF <- fromJSON(blueMixAlchemyFinalURL,
                                         flatten = TRUE)

  #bluemixresposne = fromJSON(blueMixAlchemyFinalURL)


  View(blueMixAlchemyResponseDF)
print(blueMixText)
print(blueMixAlchemyFinalURL)
return(blueMixAlchemyResponseDF)
}

loadBlueMixLibs()
callBlueMixAlchemy("On Monday Republican presidential candidate Donald Trump assured people that he will bring up Bill Clinton his history with women who are not his wife and Hillary Clintons role as a political adviser and defender during these crises.")








##Below is the Actual CURL Request for Tone Analyzer
##curl -u "{username}":"{password}" -H "Content-Type: application/json" -d "{\"text\": \"A word is dead when it is said, some say. Emily Dickinson\"}" "https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11"
testGet1 = fromJSON(getURL("https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11&text=A%20word%20is%20dead%20when%20it%20is%20said,%20some%20say.%20Emily%20Dickinson",
                       userpwd = "fa045f8e-1c8d-4a25-a337-97d10391741f:SgNXKzZympzW",
                       httpauth = 1L,
                       ssl.verifyhost = 0L,
                       ssl.verifypeer = 0L), flatten = TRUE)

typeof(testGet)
View(testGet1)
library(plyr)
matchData = testGet
matchData <- rbind.fill(lapply(matchData,
                               function(x) do.call("data.frame", as.list(x))
))
View(matchData)

postForm("http://www.speakeasy.org/~cgires/perl_form.cgi",
         "some_text" = "Duncan",
         "choice" = "Ho",
         "radbut" = "eep",
         "box" = "box1, box2"
)

library(RCurl)
library(RJSONIO)
##text = 'A word is dead when it is said, some say. Emily Dickinson',
testPOST = postForm(
  uri = 'https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone',
  .opts = list(
    postFields = toJSON(list(version = "2016-02-11", text = "A word is dead when it is said, some say. Emily Dickinson")),
    httpheader = c('Content-Type' = 'application/json', 'Accept' = 'application/json'),
    httpauth = 1L,
    ssl.verifyhost = 0L, ssl.verifypeer = 0L,
    userpwd = 'fa045f8e-1c8d-4a25-a337-97d10391741f:SgNXKzZympzW'
  ),
  style = "POST"
)

testPOST = postForm(
  uri = 'https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?',
  .opts = list(
  postFields = toJSON(list(version = "2016-02-11", text = "I would lt")),
  httpheader = c('Content-Type' = 'application/json', 'Accept' = 'application/json'),
  httpauth = 1L,
  ssl.verifyhost = 0L, ssl.verifypeer = 0L,
  userpwd = 'fa045f8e-1c8d-4a25-a337-97d10391741f:SgNXKzZympzW'
  ),
  style = "POST"
)


#Create your own appication key at https://dev.twitter.com/apps
consumer_key = "fa045f8e-1c8d-4a25-a337-97d10391741f";
consumer_secret = "SgNXKzZympzW";
#basic auth
library(httr)
secret <- RCurl::base64(paste(consumer_key, consumer_secret, sep = ":"));
req <- POST("https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone",
            config(httpheader = c(
              "Authorization" = paste("Basic", secret),
              "Content-Type" = "application/json"
            )),
            body = toJSON(list(version = "2016-02-11", text = "I would lt")

));

toJSON(c(text = "I would lt"))

library(httr)
httr::set_config(config(ssl_verifypeer = 0L))
headers <- list(
  "Accept" = "application/json"
)
req = POST("https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11",
           config = authenticate("fa045f8e-1c8d-4a25-a337-97d10391741f", "SgNXKzZympzW", type = "basic"),
           body = "{\"text\": \"A word is dead when it is said, some say. Emily Dickinson\"}",
           encode = 'multipart',
           add_headers(.headers = character(headers)),
     verbose())
req$content

/ / this works
req1 = POST("https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11",
           config = authenticate("fa045f8e-1c8d-4a25-a337-97d10391741f", "SgNXKzZympzW", type = "basic"),
           body = "{\"text\": \"A word is dead when it is said, some say. Emily Dickinson\"}",
           add_headers("Content-Type" = "application/json",
                       "Authorization" = paste(consumer_key, consumer_secret, sep = ":")),
           verbose(), handle = NULL)
req1$content
req1$cookies


content(req1, "text")
toJSON("A word is dead when it is said, some", auto_unbox = TRUE)

req = POST("https://gateway.watsonplatform.net/tone-analyzer-beta/api/v3/tone?version=2016-02-11",
           config = authenticate("fa045f8e-1c8d-4a25-a337-97d10391741f", "SgNXKzZympzW", type = "basic"),
           body = toJSON("A word is dead when it is said, some", auto_unbox = FALSE),
           add_headers("Content-Type" = "application/json",
                       "Authorization" = paste(consumer_key, consumer_secret, sep = ":")),
           verbose())
req$content
content(req, "text")


"{\n \"text\": \"I would lt\" \n}"
"{\"text\": \"A word is dead when it is said, some say. Emily Dickinson\"}"

View(test1)
test
typeof(test)
typeof(t)
typeof(x)

t = toJSON(test)
x = fromJSON(test, flatten = TRUE)
View(x)
View(fromJSON(t), flatten = TRUE)

##OpenData
library(RJSONIO)
foodMarketsRaw <- fromJSON("https://data.ny.gov/api/views/9a8c-vfzj/rows.json?accessType=DOWNLOAD", flatten = TRUE)
View(foodMarketsRaw)

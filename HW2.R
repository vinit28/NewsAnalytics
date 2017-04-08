library(topicmodels)
library(tm)
library(rvest)
library(stringr)
########################## Step 1. Fit a topic model to the existing news data archive ##########################

#Preprocess the archival data. That is, transform the data into a corpus, clean the text, and generate a document-term 
#matrix. This step is very similar to exercises we have done in class in earlier sessions.
cnbc = as.data.frame(read.csv("NewsArticles.csv"))

corp.original = VCorpus(VectorSource(cnbc$content))

corp <- tm_map(corp.original, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, content_transformer(removeWords), stopwords("SMART"), lazy=TRUE)  
corp <- tm_map(corp, content_transformer(removeWords), stopwords("english"), lazy=TRUE)  
corp <- tm_map(corp, content_transformer(tolower), lazy=TRUE) 
corp <- tm_map(corp, content_transformer(stemDocument), lazy=TRUE) 
corp <- tm_map(corp, stripWhitespace)
writeLines(as.character(corp[[3]]))

content_dtm = DocumentTermMatrix(corp)
content_dtm = removeSparseTerms(content_dtm[1:1000,], .995)

#Use the LDA() function in the topicmodels package to train a topic model with 10-15 topics. With 20,000 documents, 
#this process may take a while (it is computationally intensive), so try beginning with only about 1,000 documents, and 
#then slowly increase until the model takes no more than five minutes to run. Using more documents provides better 
#accuracy, so use as many as your computer can handle in about five minutes, but please try to use no less than 1000.

lda1 = LDA(content_dtm, 12, method = "Gibbs")

#List out 10 words that appear in each of these topics. By eyeballing the words in each topic, provide a name for each 
#topic category. This requires human judgment. Do your best to assign a sensible category name to each group based on the
#words assigned to that topic.

words = as.data.frame(terms(lda1, 10))
colnames(words) = c("Technology", "Capital Markets", "Healthcare", "Cars", "Savings and Lending", "Municipal", "Equities", "Election", "Housing","Environment", "Macroeconomics", "Telecom")
names(words)
words
####################################### Step 2. Retrieve new articles #######################################

#Scrape article URLs from this following page: CNBC-NEWS.
newslinks = data.frame()
for (i in 1:10) {
  url = paste("http://www.cnbc.com/us-news/?page=", toString(i), sep = "")
  page = read_html(url)
  page = html_nodes(page, "#pipeline_assetlist_0 .headline a")
  page_links = as.data.frame(html_attr(page, "href"))
  newslinks = rbind(newslinks, page_links)
}

#Collect and clean the text from just one of these news article.
#Once you have this working for a single news article, create a for loop that puts the above pieces togethers. In other words, 
#it should automatically scrape and clean the text for each of the 40 (or more) URLs you collected. The end result should be the 
#cleaned text from 40 articles, where each single element is the cleaned text from one of these articles.

articlecontent = NULL

for (x in 1:40) {
  url = paste("http://www.cnbc.com", newslinks[x,], sep="")
  article = read_html(url)
  article = html_nodes(article, "p")
  article = html_text(article)
  article = paste(article, collapse = "")
  
  article = gsub("\n", "", article)
  article = gsub("\t", "", article)
  article = gsub("\"", "", article)
  article = gsub("[[:punct:]]", "", article)
  articlecontent = rbind(articlecontent, as.data.frame(article))
}

################################### Step 3. Classify news articles using your topic model ################################### 

corp.original1 = VCorpus(VectorSource(articlecontent$article))
corp1 <- tm_map(corp.original1, removePunctuation)
corp1 <- tm_map(corp1, removeNumbers)
corp1 <- tm_map(corp1, content_transformer(tolower), lazy=TRUE)
article_dtm = DocumentTermMatrix(corp1)

# Specify this dictionary when creating the dtm for the new articles, which will limit the dtm it creates to only the words that
#also appeared in the archive.  In the example below, 'ldaOut' would be the name assigned to the topic model you created in Step 1.

dic = Terms(content_dtm)
new_dtm = DocumentTermMatrix(corp1, control=list(dictionary = dic))
new_dtm = new_dtm[rowSums(as.matrix(new_dtm))!=0,]
topic_probabilities = posterior(lda1, new_dtm)

#Then the probability of a document appearing in each topic will be located in topic_probabilities$topics. Using these data, 
#generate a vector that assigns to each document the topic for which it has the highest probability of appearing.

colnames(topic_probabilities$topics) = c("Technology", "Capital Markets", "Healthcare", "Cars", "Savings and Lending", "Municipal", "Equities", "Election", "Housing","Environment", "Macroeconomics", "Telecom")
topic_probabilities1 = topic_probabilities$topics
topic.articles = colnames(topic_probabilities1)[apply(topic_probabilities1,1,which.max)]
topic_probabilities$topics
topic.articles

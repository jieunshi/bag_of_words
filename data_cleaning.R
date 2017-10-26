
library(ggplot2)
library(tm)
library(NLP)
library(lsa)
library(igraph)
library(SnowballC)
library(wordcloud)
library(proxy)
library(igraph)
library(scales)


big_rumor <-read.csv(file="big_rumor_v7.csv", header=TRUE, sep="\t", colClasses=c("id"="character", "actor_id"="character", "original_actor"="character", "object_id"="character", "rumor_id"="factor", "long_body"="character", "short_body"="character"))
big_rumor <-subset(big_rumor, belief=="e")
big_rumor$posted_time <-as.Date(big_rumor$posted_time)
big_rumor$long_body[(big_rumor$long_body=="")] <- big_rumor$short_body[(big_rumor$long_body)==""]

rumor1 <- subset(big_rumor, rumor_id ==1)
rumor1_1 <-subset(rumor1, rumor1$posted_time =="2012-11-06")
rumor <- rumor1_1

rumor$long_body <- tolower(rumor$long_body )
rumor$long_body <-gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", rumor$long_body)

rumor$long_body <-gsub("@\\w+", "", rumor$long_body)
rumor$long_body <-gsub("http[[:alnum:][:punct:]]*", "", rumor$long_body)

rumor$long_body <-gsub("[[:punct:]]", " ", rumor$long_body)
rumor$long_body <-gsub(pattern = " {2, }", replacement = " ", rumor$long_body)

rumor$long_body <-gsub("^ ", "", rumor$long_body)
rumor$long_body <-gsub(" $", "", rumor$long_body)

======matrix =============
  
corpus1 <-Corpus(VectorSource(rumor$long_body))

corpus1 <-tm_map(corpus1, stemDocument)
corpus2 <-tm_map(corpus1, removeWords, my.stops)


tdm <-TermDocumentMatrix(corpus2, control=list(wordLengths=c(2, 30)))
tdm
tdm <-removeSparseTerms(tdm, 0.91)
tdm.mx <-as.matrix(tdm)
dim(tdm.mx)
tdm.mx[1:18, 1:18]

termMatrix <-tdm.mx %*% t(tdm.mx)
termMatrix[1:18, 1:18]
m <-graph.adjacency(termMatrix, weighted=T, mode="undirected")
m<-simplify(m)
class(m)

V(g)$degree <-igraph::degree(g)
V(g)$category =as.character(words$category[match(V(g)$name, words$name)])
V(g)$color =V(g)$category
V(g)$color=gsub("0", "red", V(g)$color)
V(g)$color=gsub("2", "yellow", V(g)$color)
V(g)$color=gsub("1", "skyblue", V(g)$color)

tiff("rumor_graph.tiff", width=4, height=4, units= 'in', res=300)
V(g)$label <-V(g)$name
V(g)$size <-igraph::degree(g)
V(g)$label.cex <- (V(g)$degree/ max(V(g)$degree)+.2)
V(g)$label.color <- "black"
V(g)$frame.color <-NA
E(g)$color <-adjustcolor("grey", alpha=.5)
E(g)$width <-0.5
layout=layout.fruchterman.reingold(g, area=30*vcount(g)^2, niter=1000)
plot(g, layout=layout, vertex.color=V(g)$color, edge.arrow.size=0.1, vertex.size=V(g)$size)
title("June 11, 2012")
dev.off()


==== wordcloud ======
  wordcloud(corpus, min.freq=3, scale=c(4,0.2), colors=brewer.pal(8, "Dark2"),random.color=TRUE, random.order=FALSE, max.words=150)

==== another aproach wordcloud ====
  r30.matrix <-as.matrix(r30.tdm)
wordFreq.sort <-sort(rowSums(r30.matrix), decreasing=T)
set.seed(1234)
grayLevels <-gray((wordFreq.sort +10) / (max(wordFreq.sort)+10))
word.cloud<- wordcloud(words=names(wordFreq.sort), freq=wordFreq.sort, min.freq=3, random.order=F, colors=grayLevels)

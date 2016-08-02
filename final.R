library(data.table)
library(igraph)
library(ggplot2)
library(brainGraph)

#import the table of links
links <- fread("Adjacency.txt",header=TRUE,sep="\t")
colnames(links)
data<-data.frame(links)
#Get vertices
vertices<-union(unique(data[,12]),unique(data[,13]))
vertices
#ids reid the vertices(cause vertices are not consequent numbers)
ids<-1:length(vertices)
names(ids)<-vertices;

#Get edges
from<-as.character(data[,12])
to<-as.character(data[,13])
edges<-matrix(c(ids[from],ids[to]),ncol=2)
edges

#Generate original graph
g<-graph.empty(directed=F)
g<-add.vertices(g,length(vertices))
g<-set.vertex.attribute(g,"id",index=V(g),value=names(ids))
g<-add.edges(g,t(edges))

#Decompose the original graph
dg <- decompose.graph(g)
#Get the number of individual networks
sizes<-clusters(g)$csize

#Get measures
aveBet<-c() #average betweenness centrality
aveEBet<-c() #average edge betweenness centrality
aveD<-c() #average degree
aveClo<-c()  #average closeness centrality
gCC<-c()  #gloabal clustering coefficient
aveLCC<-c()  #local clustering coefficient
avePL<-c()  #average path length
den<-c()  #density
dia<-c()  #diameter
subCen<-c()  #subgraph centrality scores
richCC<-c()    # rich club coefficient
cenEigen<-c()   #graph level centralization index (eigenvector)

for (i in 1:length(dg)){
  aveD[i]<-mean(degree(dg[[i]]))
  aveBet[i] <- mean(betweenness(dg[[i]]))
  aveEBet[i]<-mean(edge_betweenness(dg[[i]]))
  aveClo[i]<-mean(closeness(dg[[i]]))
  gCC[i]<-transitivity(dg[[i]],type=c("globalundirected"))
  aveLCC[i]<-mean(na.omit(transitivity(dg[[i]],type=c("localundirected"))))
  avePL[i]<-average.path.length(dg[[i]])
  den[i]<-edge_density(dg[[i]])
  dia[i]<-diameter(dg[[i]])
  subCen[i]<-mean(subgraph.centrality(dg[[i]]))
  richCC[i]<-rich.club.coeff(dg[[i]],k=5)$phi
  cenEigen[i]<-mean(eigen_centrality(dg[[i]])$vector)
}
#build a data frame df for measures results
df <-data.frame(aveBet,aveEBet,aveD,aveClo,gCC,aveLCC,avePL,den,dia,subCen,richCC,cenEigen)
#add conditions to each network
conditions<-c(rep("ESC",4),rep("EpiSC",1),rep("2i",11))
df <- data.frame(df, conditions)


#Generate plots to present resutls of each measure
boxplot(df$aveD~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Average Degree",main="Average Degree~Conditions")
points(df$conditions,df$aveD,type='p',col=df$conditions,pch=16)
boxplot(df$aveBet~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Average Betweenness",main="Average Betweenness~Conditions")
points(df$conditions,df$aveBet,type='p',col=df$conditions,pch=16)
boxplot(df$aveEBet~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Average Edge Betweenness",main="Average Edge Betweenness~Conditions")
points(df$conditions,df$aveEBet,type='p',col=df$conditions,pch=16)
boxplot(df$aveClo~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Average Closeness",main="Average Closeness~Conditions")
points(df$conditions,df$aveClo,type='p',col=df$conditions,pch=16)
boxplot(df$cenEigen~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Average Eigenvector",main="Average Eigenvector~Conditions")
points(df$conditions,df$cenEigen,type='p',col=df$conditions,pch=16)
boxplot(df$gCC~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Global Clustering Coefficient",main="Global Clustering Coefficient~Conditions")
points(df$conditions,df$gCC,type='p',col=df$conditions,pch=16)
boxplot(df$aveLCC~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Average Local Clustering Coefficient",main="Average Local Clustering Coefficient~Conditions")
points(df$conditions,df$aveLCC,type='p',col=df$conditions,pch=16)
boxplot(df$den~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Density",main="Density~Conditions")
points(df$conditions,df$den,type='p',col=df$conditions,pch=16)
boxplot(df$dia~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Diameter",main="Diameter~Conditions")
points(df$conditions,df$dia,type='p',col=df$conditions,pch=16)
boxplot(df$avePL~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Average Path Length",main="Average Path Length~Conditions")
points(df$conditions,df$avePL,type='p',col=df$conditions,pch=16)
boxplot(df$subCen~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Subgraph Centrality",main="Subgraph Centrality~Conditions")
points(df$conditions,df$subCen,type='p',col=df$conditions,pch=16)
boxplot(df$richCC~df$conditions,col=c("pink","yellow","lightblue"),xlab="Conditions",ylab="Rich Club Coefficient",main="Rich club Coefficient~Conditions")
points(df$conditions,df$richCC,type='p',col=df$conditions,pch=16)

#Generate interactive parallel coordinate chart 
#This part requires rChart Package
#deletion of missing and standardization
df2<-na.omit(df) 
df2[, -c(13)] <- scale(df2[, -c(13)])
drops<-c("conditions")
# initialize chart and set path to parcoords library
p1 <- rCharts$new()
p1$setLib("libraries/widgets/parcoords")
# add more details to the plot
p1$set(
  padding = list(top = 24, left = 0, bottom = 48, right = 0)
)
p1$set(
  data = toJSONArray(df2[ , !(names(df2) %in% drops)], json = F), 
  colorby = 'aveBet', 
  range = range(df2$aveBet),
  colors = c('steelblue','brown')
)
p1

#Generate scatter plots to show relationships of different measures with unscaled dataframe
y1=df$dia
x1=(df$aveD)/(df$den)
plot(y1 ~ x1,xlab="aveD/den",ylab="dia",col = df$conditions,main="dia~aveD/den",type='p',pch=16)
with(df,text(x = x1, y = y1, pos = 1))
y2=(df$aveD)/(df$aveClo)
x2=(df$richCC)
plot(y2 ~ x2,xlab="richCC",ylab="aveD/aveClo",col = df$conditions,main="aveD/aveClo~richCC",type='p',pch=16)
with(df,text(x = x2, y = y2, pos = 1))
y3=df$aveBet
x3=(df$avePL)/(df$gCC)
plot(y3 ~ x3,xlab="avePL/gCC",ylab="aveBet",col = df$conditions,main="aveBet~avePL/gCC",type='p',pch=16)
with(df,text(x = x3, y = y3, pos = 1))
x4=df$aveD
y4=(df$avePL)/(df$aveClo)
plot(y4 ~ x4,ylab="avePL/aveClo",xlab="aveD",col = df$conditions,main="avePL/aveClo~aveD",type='p',pch=16)
with(df,text(x = x4, y = y4, pos = 1))

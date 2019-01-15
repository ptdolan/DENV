library(ggplot2)
library(data.table)

load("~/GitHub/DENV/DENV/fitnesstable_All.Rdata")

#Generate data sets for modeling(found in GitHub directory):

#castFitness<-(dcast(Wtable,ID~host+set,value.var="wrel",fun.aggregate = mean))
#write.csv(castFitness,file = "~/GitHub/DENV/DENV/fitnessAlleleByHost-Rep.csv")
#write.csv(Wtable,file = "~/GitHub/DENV/DENV/fitnessTableAll_Long.csv")

Points<-data.frame(dcast(Wtable,ID+host+set~passage,value.var = "freq"))

moments<-Points
for (i in ncol(moments):5){
  moments[,i]<-Points[,i]-Points[,(i-1)]
}

#Analyzing PCA of passage-to-passage changes in frequency

tall<-melt(moments)
output<-dcast(tall,ID~variable+host+set)
PC<-princomp(output[,c(-1,-33,-37)])
PCmoment<-princomp(scale(output[,c(-1,-33,-37)]))$loadings[,1:34]
PCScoremoment<-princomp(scale(output[,c(-1,-33,-37)]))$scores[,1:34]
DF<-data.frame(PCmoment)
DF$name<-limma::strsplit2(rownames(DF),split = "_")
DF$passage<-as.factor(DF$name[,1])
DF$host<-as.factor(DF$name[,2])
DF$set<-as.factor(DF$name[,3])


ggplot(DF)+
  geom_point(mapping = aes(shape=set,Comp.1,Comp.2,col=host))+
#  geom_path(mapping = aes(group=paste(set,host),Comp.1,Comp.2,col=host))+
  geom_text(aes(Comp.1,Comp.2,label=passage))+facet_grid(host~set)

ggplot(DF)+
  geom_point(mapping = aes(shape=set,passage,Comp.1,col=host))+
  geom_path(mapping = aes(group=paste(set,host),passage,Comp.1,col=host))

ggplot(DF)+
  geom_point(mapping = aes(shape=set,passage,Comp.2,col=host))+
  geom_path(mapping = aes(group=paste(set,host),passage,Comp.2,col=host))

ggplot(DF)+
  geom_point(mapping = aes(shape=set,passage,Comp.3,col=host))+
  geom_path(mapping = aes(group=paste(set,host),passage,Comp.3,col=host))

ggplot(DF)+
  geom_point(mapping = aes(shape=set,passage,Comp.4,col=host))+
  geom_path(mapping = aes(group=paste(set,host),passage,Comp.4,col=host))

DFs<-data.frame(PCScoremoment)
DFs$ID<-output$ID
DFs$pos<-unlist(lapply(output$ID,FUN = function(X){
  spl=strsplit(X,split = "")
  as.numeric(paste(spl[[1]][2:(length(spl[[1]])-1)],collapse = ""))
})
)

ggplot(DFs)+
  geom_point(mapping = aes(pos,Comp.1))

ggplot(DFs)+
  geom_point(mapping = aes(pos,Comp.3))

ggplot(DFs)+
  geom_point(mapping = aes(pos,Comp.2))

ggplot(DFs)+
  geom_point(mapping = aes(pos,Comp.4))

plot(PC)

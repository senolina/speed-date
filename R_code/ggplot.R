##install.packages("ggplot2");install.packages("reshape2")

library(ggplot2);library(reshape2);library(tree) ; library(caret) ; library(e1071) ;library(randomForest)
library(dplyr)


train<-read.csv('C:\\Users\\HOME\\Desktop\\train_v9.3.csv',header=T)
str(train)


set.seed(1234)
t<-train[,-c(1,2,3,6,17,18,19,20,34)]
intrain<-createDataPartition(y=t$match, p=0.7, list=FALSE)
train70<-t[intrain, ];str(train70)
test30<-t[-intrain, ]

train70_1<-train70[,c(1,2,3,4,13,22)];str(train70_1)
train70_2<-train70[,-c(1,2,4,13,22)];str(train70_2)




tot_var1<- melt(train70_1, id.var = "match")

gga<-ggplot(data = tot_var1, aes(x=variable, y=value)) + 
   geom_boxplot(aes(fill=as.factor(match)),outlier.shape=NA) +
   theme_bw() + # white background
   coord_flip() # flip the x & y-axis



windows()


tot_var2<- melt(train70_2, id.var = "match")

ggb<-ggplot(data = tot_var2, aes(x=value, y=variable)) + 
   geom_boxplot(aes(fill=as.factor(match)),outlier.shape=NA) +
   theme_bw() + # white background
   coord_flip() # flip the x & y-axis



ggc<-ggplot(data = tot_var2, aes(x=variable, y=value)) + 
   geom_boxplot(aes(fill=as.factor(match)),outlier.shape=NA) +
   theme_bw() + # white background
   coord_flip() # flip the x & y-axis


ggsave(device="pdf",filename=gga)
ggsave(device="pdf",filename=ggb)



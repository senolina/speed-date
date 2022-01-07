##데이터 불러오기. 2월 9일 -> train9로 train_v6.0 데이터를 불러오겠음

train9<-read.csv("C:/Users/HOME/Desktop/train_v9.1.csv")
head(train9)
str()
#train9<-train9[,-c(19,68)] 
###train9<-train9[,-c(88,89)]

train9$match<-as.factor(train9$match)
train9$from<-as.factor(train9$from)
train9$gender<-as.factor(train9$gender)
train9$samerace<-as.factor(train9$samerace)
train9$goal<-as.factor(trai9n$goal)
train9$met<-as.factor(train9$met)
train9$met_o<-as.factor(train9$met_o)
train9$field_cd<-as.factor(train9$field_cd)
train9$race<-as.factor(train9$race)
train9$race_o<-as.factor(train9$race_o)
train9$career_c<-as.factor(train9$career_c)
train9$numdat_2<-as.factor(train9$numdat_2)



##데이터 구조 확인

str(train9)

##install.packages("caret")
##install.packages("qqplot2")
library(caret)
set.seed(100) ##매번 같은 난수값이 랜덤하게 나오도록 한다

intrain<-createDataPartition(y=train9$match, p=0.7, list=FALSE)
train70<-train9[intrain,]
test30<-train9[-intrain,]


##install.packages("tree")
library(tree)


matchtrain<-tree(match~. , data=train70)
plot(matchtrain)
text(matchtrain)


?cv.tree
##이거 안해도 되나...?
##cv.trees<-cv.tree(treemod, FUN=prune.tree )  #분산이 가장 낮은 것으로 가지치기..?
##plot(cv.trees)

##prune.trees<-prune.tree(treemod, best=5)
##plot(prune.trees)
##text(prune.trees, pretty=0)


##install.packages("e1071")
library(e1071)


pred <-predict(matchtrain, test30, type='class')
confusionMatrix(pred, test30$match)####여기서 막혔음

sum(train9$match==1)


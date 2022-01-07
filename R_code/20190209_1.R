library(tree) ; library(caret) ; library(e1071)

train9<-read.csv('C:/Users/user/Desktop/윤슬/대학교/연구/학회/speed_dating/train_v9.0.csv',header=T)

str(train9)
train9$match<-as.factor(train9$match)
train9$condtn<-as.factor(train9$condtn)
train9$from<-as.factor(train9$from)
train9$gender<-as.factor(train9$gender)
train9$samerace<-as.factor(train9$samerace)
train9$goal<-as.factor(train9$goal)
train9$met<-as.factor(train9$met)
train9$met_o<-as.factor(train9$met_o)
train9$field_cd<-as.factor(train9$field_cd)
train9$race<-as.factor(train9$race)
train9$race_o<-as.factor(train9$race_o)
train9$career_c<-as.factor(train9$career_c)
train9$numdat_2<-as.factor(train9$numdat_2)

train9<-train9[,-c(37:53)]

set.seed(100) #set.seed() 함수는 매번 같은 난수값이 랜덤하게 나오도록 한다
intrain<-createDataPartition(y=train9$match, p=0.7, list=FALSE)
train70<-train9[intrain, ]
test30<-train9[-intrain, ]

matchtrain<-tree(match~. , data=train70)
plot(matchtrain)
text(matchtrain)

pred <- predict(matchtrain,test30,type='class')
confusionMatrix(pred,test30$match)   #0.8427

############################
##           		   ##
##   종속변수와 범주형변수     ##
##      table그리기         ##
##              		   ##
############################

#samerace~match
with(train9,table(samerace,match))
print(chisq.test(train9$samerace,train9$match))   #p-value = 0.1879 > 0.05 독립O

#race_o ~match
with(train9,table(race_o,match))
print(chisq.test(train9$race_o,train9$match))   #0.0001232 < 0.05 독립X 연관O

#met_o~match
with(train9,table(met_o,match))
print(chisq.test(train9$met_o,train9$match))   # 2.2e-16 < 0.05 독립X 연관O

#field_cd~match
with(train9,table(field_cd,match))
print(chisq.test(train9$field_cd,train9$match))   #5.855e-06 < 0.05 독립X 연관O

#race~match
with(train9,table(race,match))
print(chisq.test(train9$race,train9$match))   #0.001145 < 0.05 독립X 연관O

#from~match
with(train9,table(from,match))
print(chisq.test(train9$from,train9$match))   #9.763e-05 < 0.05 독립X 연관O 

#goal~match
with(train9,table(goal,match))
print(chisq.test(train9$goal,train9$match))   #0.4389 > 0.05 독립O

#met ~match
with(train9,table(met,match))
print(chisq.test(train9$met,train9$match))   #2.2e-16 < 0.05 독립X 연관O

#career_c~match
with(train9,table(career_c,match))
print(chisq.test(train9$career_c,train9$match))   # 0.009921 < 0.05 독립X 연관O

#numdat_2~match
with(train9,table(numdat_2,match))
print(chisq.test(train9$numdat_2,train9$match))   #0.1284 >0.05 독립O

#condtn~match
with(train9,table(condtn,match))
print(chisq.test(train9$condtn,train9$match))   #0.0001117 < 0.05 독립X 연관O

#결론 : samerace, goal, numdat_2 는 뺍니다.

############################
##              ##
##   종속변수와 수치형변수     ##
##      boxplot그리기     ##
##              ##
############################

## round~match
with(train9, boxplot(round~match))      #연관X

## order~match
with(train9, boxplot(order~match))      #연관X

## int_corr~match
with(train9, boxplot(int_corr~match))   #애매 0.03 버려

## age_o~match
with(train9, boxplot(age_o~match))      #연관X

## pf_o_att~match
with(train9, boxplot(pf_o_att~match))   #연관X

## pf_o_sin~match
with(train9, boxplot(pf_o_sin~match))   #연관X

## pf_o_sin~match
with(train9, boxplot(pf_o_sin~match))   #연관X

## pf_o_int~match
with(train9, boxplot(pf_o_int~match))   #애매 버려
windows()
## pf_o_fun~match
with(train9, boxplot(pf_o_fun~match))   #애매 버려

## pf_o_amb~match
with(train9, boxplot(pf_o_amb~match))   #애매 버려

## pf_o_sha~match
with(train9, boxplot(pf_o_sha~match))   #차이O

## attr_o~match
with(train9, boxplot(attr_o~match))      #차이O

## sinc_o~match
with(train9, boxplot(sinc_o~match))      #차이O

## intel_o~match
with(train9, boxplot(intel_o~match))   #차이O

## fun_o ~match
with(train9, boxplot(fun_o ~match))      #차이O

## amb_o~match
with(train9, boxplot(amb_o~match))      #차이O

## shar_o~match
with(train9, boxplot(shar_o~match))      #차이O

## like_o~match
with(train9, boxplot(like_o~match))      #차이O

## prob_o~match
with(train9, boxplot(prob_o~match))      #차이O

## age~match
with(train9, boxplot(age~match))      #연관X

## imprace~match
with(train9, boxplot(imprace~match))   #연관X

## imprelig~match
with(train9, boxplot(imprelig~match))   #연관X

## date~match
with(train9, boxplot(date~match))      #연관X

## go_out~match
with(train9, boxplot(go_out~match))      #연관X

## exphappy~match
with(train9, boxplot(exphappy~match))   #연관X

## attr1_1~match
with(train9, boxplot(attr1_1~match))   #연관X

## sinc1_1~match
with(train9, boxplot(sinc1_1~match))   #연관X

## intel1_1~match
with(train9, boxplot(intel1_1~match))   #연관X

## fun1_1~match
with(train9, boxplot(fun1_1~match))      #연관X

## amb1_1~match
with(train9, boxplot(amb1_1~match))      #연관X

## shar1_1~match
with(train9, boxplot(shar1_1~match))   #애매 차이 O

## attr3_1~match
with(train9, boxplot(attr3_1~match))   #애매 버려

## sinc3_1~match
with(train9, boxplot(sinc3_1~match))   #연관X

## fun3_1~match
with(train9, boxplot(fun3_1~match))      #연관X

## intel3_1~match
with(train9, boxplot(intel3_1~match))   #연관X

## amb3_1~match
with(train9, boxplot(amb3_1~match))      #연관X

boxplot(train9$inside~train9$match)    #애매 버려
boxplot(train9$outside~train9$match)   #연관ㅇ
boxplot(train9$heal~train9$match)      #연관x
boxplot(train9$cult~train9$match)       #o
boxplot(train9$newsport~train9$match)   #x
boxplot(train9$amb3_2~train9$match)     #x
train9$match_ess<-train9$match_es/train9$round
hist(match_ess)   #match_ess : 매치될것같은확률
with(train9,boxplot(match_ess~match))   #O
with(train9,boxplot(fun3_2~match))       #x
with(train9,boxplot(intel3_2~match))     #x
with(train9,boxplot(sinc3_2~match))      #x
with(train9,boxplot(attr3_2~match))     #o
with(train9,boxplot(shar1_2~match))    #o
with(train9,boxplot(amb1_2~match))     #애매 버려
with(train9,boxplot(fun1_2~match))     #애매 버려
with(train9,boxplot(intel1_2~match))   #x
with(train9,boxplot(sinc1_2~match))       #애매 버려
with(train9,boxplot(attr1_2 ~match))      #x
with(train9,boxplot(match_ess~match))     #o
with(train9,boxplot(prob~match))          #o
with(train9,boxplot(like~match))          #o
with(train9,boxplot(shar~match))          #o
with(train9,boxplot(amb~match))           #o
with(train9,boxplot(fun~match))          #o
with(train9,boxplot(intel~match))        #o
with(train9,boxplot(sinc~match))          #o
with(train9,boxplot(attr~match))          #O

##--> pf_o_sha,attr_o,sinc_o/intel_o/fun_o/amb_o/shar_o/like_o/prob_o/shar1_1/
##attr/sinc/intel/fun/amb/shar/like/prob/match_ess/shar1_2/attr3_2/cult/outside

#결론 : samerace, goal, numdat_2 는 뺍니다.

############################
##              ##
##   합이 100이 아닌     ##
##              ##
############################

#pf_o_att
train9$pfosum<-apply(train9[,12:17],1,sum)
table(train9$pfosum)

#attr1_1~
train9$sum1<-apply(train9[,38:43],1,sum)
table(train9$sum1)

#attr1_2~
train9$sum2<-apply(train9[,60:65],1,sum)
table(train9$sum2)


####합으로 나눠서 곱하기 100
#pf_o문항
train9$pf_o_att<-train9$pf_o_att/train9$pfosum*100
train9$pf_o_sin<-train9$pf_o_sin/train9$pfosum*100
train9$pf_o_int<-train9$pf_o_int/train9$pfosum*100
train9$pf_o_fun<-train9$pf_o_fun/train9$pfosum*100
train9$pf_o_amb<-train9$pf_o_amb/train9$pfosum*100
train9$pf_o_sha<-train9$pf_o_sha/train9$pfosum*100

table(apply(train9[,12:17],1,sum))

#1_1문항
train9$attr1_1<-train9$attr1_1/train9$sum1*100
train9$sinc1_1<-train9$sinc1_1/train9$sum1*100
train9$intel1_1<-train9$intel1_1/train9$sum1*100
train9$fun1_1<-train9$fun1_1/train9$sum1*100
train9$amb1_1<-train9$amb1_1/train9$sum1*100
train9$shar1_1<-train9$shar1_1/train9$sum1*100

table(apply(train9[,38:43],1,sum))

#1_2문항
train9$attr1_2<-train9$attr1_2/train9$sum2*100
train9$sinc1_2<-train9$sinc1_2/train9$sum2*100
train9$intel1_2<-train9$intel1_2/train9$sum2*100
train9$fun1_2<-train9$fun1_2/train9$sum2*100
train9$amb1_2<-train9$amb1_2/train9$sum2*100
train9$shar1_2<-train9$shar1_2/train9$sum2*100

table(apply(train9[,60:65],1,sum))

train91 <- train9[,-c(8,9,10,11,12,13,14,15,16,26,27,30,31,33,34,35,37,38,39,40,41,42,44:48,57:64,67:71,73,75:78)]


#write.csv(train91,file='train_v9.3.csv',row.names=F)

##########version 9.3로 해본다.#############

################################
##                 ##
##   9.3로 random forest     ##
##                 ##
################################

#install.packages('randomForest')
library(randomForest)

set.seed(1234)
t91<-train91[,-c(1,6)]
intrain<-createDataPartition(y=t91$match, p=0.7, list=FALSE)
train70<-t91[intrain, ]
test30<-t91[-intrain, ]

matchtrain<-randomForest(match~. , data=train70)
plot(matchtrain)
text(matchtrain)

pred <- predict(matchtrain,test30,type='class')
confusionMatrix(pred,test30$match)   #0.8493

#outside:   dining shopping clubbing
#cult     :   museum art reading  theater movies concert music

############################
##              ##
##     로지스틱 회귀     ##
##              ##
############################
set.seed(Sys.time())
t92<-t91[,-18]
intrain<-createDataPartition(y=t92$match, p=0.7, list=FALSE)
train70<-t92[intrain, ]
test30 <-t92[-intrain, ]

logic <- glm(match ~ . ,family = "binomial", data = train70)
summary(logic)
vif(logic)

#install.packages('ROCR')
library(ROCR)
 p <- predict(final, newdata=test30, type="response")
pr <- prediction(p, test30$match)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc                  #0.8679929

##
p2 <- predict(logic, test30, type="response")
pred2<-rep(0,nrow(test30))
pred2[p2>0.5]=1
pred2<-as.factor(pred2)
confusionMatrix(pred2,test30$match)   #0.8611 
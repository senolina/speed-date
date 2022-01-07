library(tree) ; library(caret) ; library(e1071)

train9<-read.csv('C:/Users/HOME/Desktop/train_v9.1.csv',header=T)

str(train9)
train9$match<-as.factor(train9$match)
train9$from<-as.factor(train9$from)
train9$gender<-as.factor(train9$gender)
train9$condtn<-as.factor(train9$condtn)
train9$samerace<-as.factor(train9$samerace)
train9$goal<-as.factor(train9$goal)
train9$met<-as.factor(train9$met)
train9$met_o<-as.factor(train9$met_o)
train9$field_cd<-as.factor(train9$field_cd)
train9$race<-as.factor(train9$race)
train9$race_o<-as.factor(train9$race_o)
train9$career_c<-as.factor(train9$career_c)
train9$numdat_2<-as.factor(train9$numdat_2)

set.seed(100) #set.seed() 함수는 매번 같은 난수값이 랜덤하게 나오도록 한다
intrain<-createDataPartition(y=train9$match, p=0.7, list=FALSE)
train70<-train9[intrain, ]
test30<-train9[-intrain, ]

######################여기까지 통일됨#########################



############################
##                        ##
##     종속변수와 범주형     ##
##       table 그리기      ##
############################


##카이검정->범주형인 변수들 검정//////연속형일때-> t,z test임

#samerace~match
with(train9,table(samerace,match))
print(chisq.test(train9$samerace,train9$match))   #연관이 없다.

#gender~match
with(train9,table(gender,match))
print(chisq.test(train9$gender,train9$match))     #연관이 없다.

#race_o~match
with(train9,table(race_o,match))
print(chisq.test(train9$race_o,train9$match))    #연관이 있다.


#met_o~match
with(train9,table(met_o,match))
print(chisq.test(train9$met_o,train9$match))     #연관이 있다.

#field_cd~match
with(train9,table(field_cd,match))
print(chisq.test(train9$field_cd,train9$match))  #연관이 있다.

#race~match
with(train9,table(race,match))
print(chisq.test(train9$race,train9$match))    #연관이 있다.
 
#from~match
with(train9,table(from ,match))
print(chisq.test(train9$from ,train9$match))   #연관이 있다.

#goal~match
with(train9,table(goal,match))
print(chisq.test(train9$goal,train9$match))   #연관이 없다.

#condtn~match
with(train9,table(condtn,match))
print(chisq.test(train9$condtn,train9$match))  #연관이 있다.

#career_c~match
with(train9,table(career_c,match))
print(chisq.test(train9$career_c,train9$match)) #연관이 있다.

# met ~match
with(train9,table( met ,match))
print(chisq.test(train9$met ,train9$match))   #연관이 있다.

#numdat_2~match
with(train9,table(numdat_2,match))
print(chisq.test(train9$numdat_2,train9$match))  #연관이 없다.

####samerace numdat_2 goal 제거



############################
##                        ##
##   종속변수와 수치형변수     ##
##      boxplot 그리기     ##
############################

##수치형변수끼리 match와의 분포를 보기 위해


boxplot(train9$inside~train9$match)  #애매

boxplot(train9$outside~train9$match) #연관ㅇ

boxplot(train9$heal~train9$match)    #연관x

boxplot(train9$cult~train9$match)    #o

boxplot(train9$newsport~train9$match)#x

boxplot(train9$amb3_2~train9$match)  #x

#이렇게 할 수 있음

match_ess<-train9$match_es/train9$round
hist(match_ess)


with(train9,boxplot(fun3_2~match))     #x

with(train9,boxplot(intel3_2~match))   #x

with(train9,boxplot(sinc3_2~match))    #x

with(train9,boxplot(attr3_2~match))    #o

with(train9,boxplot(shar1_2~match))    #o

with(train9,boxplot(amb1_2~match))     #애매

with(train9,boxplot(fun1_2~match))     #애매

with(train9,boxplot(intel1_2~match))   #x

with(train9,boxplot(sinc1_2~match))    #애매

with(train9,boxplot(attr1_2 ~match))   #x

with(train9,boxplot(match_ess~match))  #o

with(train9,boxplot(prob~match))       #o

with(train9,boxplot(like~match))       #o

with(train9,boxplot(shar~match))       #o

with(train9,boxplot(amb~match))        #o

with(train9,boxplot(fun~match))        #o

with(train9,boxplot(intel~match))      #o

with(train9,boxplot(sinc~match))       #o

with(train9,boxplot(attr~match))       #애매

## round~match
with(train9, boxplot(round~match))      #연관X

## order~match
with(train9, boxplot(order~match))      #연관X

## int_corr~match
with(train9, boxplot(int_corr~match))   #애매

## age_o~match
with(train9, boxplot(age_o~match))      #연관X

## pf_o_att~match
with(train9, boxplot(pf_o_att~match))   #연관X

## pf_o_sin~match
with(train9, boxplot(pf_o_sin~match))   #연관X

## pf_o_sin~match
with(train9, boxplot(pf_o_sin~match))   #연관X

## pf_o_int~match
with(train9, boxplot(pf_o_int~match))   #애매

## pf_o_fun~match
with(train9, boxplot(pf_o_fun~match))   #애매

## pf_o_amb~match
with(train9, boxplot(pf_o_amb~match))   #애매

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
with(train9, boxplot(shar1_1~match))   #애매

## attr3_1~match
with(train9, boxplot(attr3_1~match))   #애매

## sinc3_1~match
with(train9, boxplot(sinc3_1~match))   #연관X

## fun3_1~match
with(train9, boxplot(fun3_1~match))      #연관X

## intel3_1~match
with(train9, boxplot(intel3_1~match))   #연관X

## amb3_1~match
with(train9, boxplot(amb3_1~match))      #연관X


##창하나 더 열기
windows()









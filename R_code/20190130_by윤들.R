###### 2019-01-30 

train30<-read.csv('C:/Users/HOME/Desktop/train_v1.0.csv',header=T)

dim(train30)

colSums(is.na(train30))

train30<-train30[-which(is.na(train30$pid)==T),]   #pid가 NA인 6개 삭제

#age_o가 missing인 애들 평균을 wave내 평균으로 넣기

missage<-subset(train30,is.na(age_o))
missage$age_o[58<=missage$pid & missage$pid<=59] <- 25
missage$age_o[missage$pid==129] <- 21
missage$age_o[missage$pid==136] <- 25
missage$age_o[339<=missage$pid & missage$pid<=340] <- 27
missage$age_o[missage$pid==346] <- 27
missage$age_o[missage$pid==512] <- 25
sum(is.na(missage$age_o))   # 0됨 ㅎ

train30$age_o[is.na(train30$age_o)] <- missage$age_o

#사전조사 안한 아이들 ex) 28, 58 ... ==> int_corr이 NA인 데이터 지운다.

train30<-train30[-which(is.na(train30$int_corr)==T),]   

#pf_o_fun, pf_o_amb, pf_o_sha 이 NA인 애는 나머지를 더해서 100이 되기 때문에 0을 넣는다

train30$pf_o_fun[is.na(train30$pf_o_fun)] <- 0
train30$pf_o_amb[is.na(train30$pf_o_amb)] <- 0
train30$pf_o_sha[is.na(train30$pf_o_sha)] <- 0

#상대도 나도 평가 안함 ? NA -> 0
#_o가 NA 나는 평가함 ? NA -> 0
   # outlier인 것 1개 있음 근데 이건 지울거야.
#상대->나 X,선택 O : 6개
#나->상대 X,선택 O : 4개 얘네 지울거야 

dim(train30)

train30[is.na(train30$attr_o)==T & is.na(train30$sinc_o)==T & train30$dec_o==1,]
train30<- train30[-c(285,370,4103,4232,5608,6534),]

train30[is.na(train30$attr)==T & is.na(train30$sinc)==T & train30$dec==1,]
train30<- train30[-c(541,2010,5744,6173),]

#이제 attr_o ~ shar_o 가 NA인 것 0넣으면됨

train30$attr_o[is.na(train30$attr_o)] <- 0
train30$sinc_o[is.na(train30$sinc_o)] <- 0
train30$intel_o[is.na(train30$intel_o)] <- 0
train30$fun_o[is.na(train30$fun_o)] <- 0
train30$amb_o[is.na(train30$amb_o)] <- 0
train30$shar_o[is.na(train30$shar_o)] <- 0

#attr_o가 10이 최대인데 10.5인것 있어서 변경함 
train30$attr_o[train30$attr_o > 10] <-10

colSums(is.na(train30))

#_o 이렇게 여섯가지 점수 더한 것 total_o라고 할거임. 그리고 얘에 범주에 따라 like_o NA채울것임

train30$total <- train30$attr_o+train30$sinc_o+train30$intel_o+train30$fun_o+train30$amb_o+train30$shar_o
windows()
hist(train30$total)
hist(train30$like_o)

misslike<-subset(train30, is.na(like_o))

misslike$like_o[0<=misslike$total & misslike$total<10] <- round(mean(train30$like_o[0<=train30$total & train30$total<10],na.rm=T),0)
misslike$like_o[10<=misslike$total & misslike$total<20] <- round(mean(train30$like_o[10<=train30$total & train30$total<20],na.rm=T),0)
misslike$like_o[20<=misslike$total & misslike$total<30] <- round(mean(train30$like_o[20<=train30$total & train30$total<30],na.rm=T),0)
misslike$like_o[30<=misslike$total & misslike$total<40] <- round(mean(train30$like_o[30<=train30$total & train30$total<40],na.rm=T),0)
misslike$like_o[40<=misslike$total & misslike$total<50] <- round(mean(train30$like_o[40<=train30$total & train30$total<50],na.rm=T),0)
misslike$like_o[50<=misslike$total & misslike$total<60] <- round(mean(train30$like_o[50<=train30$total & train30$total<60],na.rm=T),0)
misslike$like_o[misslike$total==60] <- round(mean(train30$like_o[train30$total==60]),0)

train30$like_o[is.na(train30$like_o)] <- misslike$like_o

#prob_o도 like_o랑 똑같은 방법으로 할 것임
hist(train30$prob_o)

missprob<-subset(train30, is.na(prob_o))

missprob$prob_o[0<=missprob$total & missprob$total<10] <- round(mean(train30$prob_o[0<=train30$total & train30$total<10],na.rm=T),0)
missprob$prob_o[10<=missprob$total & missprob$total<20] <- round(mean(train30$prob_o[10<=train30$total & train30$total<20],na.rm=T),0)
missprob$prob_o[20<=missprob$total & missprob$total<30] <- round(mean(train30$prob_o[20<=train30$total & train30$total<30],na.rm=T),0)
missprob$prob_o[30<=missprob$total & missprob$total<40] <- round(mean(train30$prob_o[30<=train30$total & train30$total<40],na.rm=T),0)
missprob$prob_o[40<=missprob$total & missprob$total<50] <- round(mean(train30$prob_o[40<=train30$total & train30$total<50],na.rm=T),0)
missprob$prob_o[50<=missprob$total & missprob$total<60] <- round(mean(train30$prob_o[50<=train30$total & train30$total<60],na.rm=T),0)
missprob$prob_o[missprob$total==60] <- round(mean(train30$prob_o[train30$total==60]),0)

train30$prob_o[is.na(train30$prob_o)] <- missprob$prob_o

##met_o

a<-subset(train30, met_o >2)   #이건 삭제~~
train30<-train30[-c(756,1176,2006,2036,2045,2682,5249,6422),]

b<-subset(train30, is.na(met_o))
train30$met_o[is.na(train30$met_o)]<-2

##age

train30$age[train30$iid==129] <-21
train30$age[train30$iid==512] <-25

##field_cd

#40번이 field가 operation research인데 이것은 social science 인 3번에 넣는다.
train30$field_cd[train30$iid==40] <-3

##X... , mn_sat 은 NA때문에 지움

train30<- train30[,-c(30:31)]

boxplot(train30$date)
windows()
boxplot(train30$go_out)

##exphappy

table(train30$exphappy)
train30$exphappy[train30$iid==512] <-5




####20190131_#####
train31<-read.csv('C:/Users/HOME/Desktop/train_v4.0.csv',header=T)


#416번은 3_1의 결측치를 3_2의 값으로 넣는다.
train31$attr3_1[is.na(train31$attr3_1) & train31$iid==416] <- 6
train31$sinc3_1[is.na(train31$sinc3_1) & train31$iid==416] <- 7
train31$fun3_1[is.na(train31$fun3_1) & train31$iid==416] <- 8
train31$intel3_1[is.na(train31$intel3_1) & train31$iid==416] <- 8
train31$amb3_1[is.na(train31$amb3_1) & train31$iid==416] <- 7

#414번은 3_1의 결측치를 성별 같은 나이 쁠마2의 평균으로 넣는다
train31$attr3_1[is.na(train31$attr3_1) & train31$iid==414] <- 7
train31$sinc3_1[is.na(train31$sinc3_1) & train31$iid==414] <- 8
train31$fun3_1[is.na(train31$fun3_1) & train31$iid==414] <- 8
train31$intel3_1[is.na(train31$intel3_1) & train31$iid==414] <- 8
train31$amb3_1[is.na(train31$amb3_1) & train31$iid==414] <- 8

colSums(is.na(train31))



#attr~shar 이렇게 여섯가지 점수 더한 것 total2라고 할거임. 그리고 얘에 범주에 따라 like NA채울것임

train31$total2 <- train31$attr+train31$sinc+train31$intel+train31$fun+train31$amb+train31$shar

misslike<-subset(train31, is.na(like))

misslike$like[0<=misslike$total2 & misslike$total2<10] <- round(mean(train31$like[0<=train31$total2 & train31$total2<10],na.rm=T),0)
misslike$like[10<=misslike$total2 & misslike$total2<20] <- round(mean(train31$like[10<=train31$total2 & train31$total2<20],na.rm=T),0)
misslike$like[20<=misslike$total2 & misslike$total2<30] <- round(mean(train31$like[20<=train31$total2 & train31$total2<30],na.rm=T),0)
misslike$like[30<=misslike$total2 & misslike$total2<40] <- round(mean(train31$like[30<=train31$total2 & train31$total2<40],na.rm=T),0)
misslike$like[40<=misslike$total2 & misslike$total2<50] <- round(mean(train31$like[40<=train31$total2 & train31$total2<50],na.rm=T),0)
misslike$like[50<=misslike$total2 & misslike$total2<60] <- round(mean(train31$like[50<=train31$total2 & train31$total2<60],na.rm=T),0)
misslike$like[misslike$total2==60] <- round(mean(train31$like[train31$total2==60]),0)



#prob도 like랑 똑같은 방법으로 할 것임

missprob<-subset(train31, is.na(prob))

missprob$prob[0<=missprob$total2 & missprob$total2<10] <- round(mean(train31$prob[0<=train31$total2 & train31$total2<10],na.rm=T),0)
missprob$prob[10<=missprob$total2 & missprob$total2<20] <- round(mean(train31$prob[10<=train31$total2 & train31$total2<20],na.rm=T),0)
missprob$prob[20<=missprob$total2 & missprob$total2<30] <- round(mean(train31$prob[20<=train31$total2 & train31$total2<30],na.rm=T),0)
missprob$prob[30<=missprob$total2 & missprob$total2<40] <- round(mean(train31$prob[30<=train31$total2 & train31$total2<40],na.rm=T),0)
missprob$prob[40<=missprob$total2 & missprob$total2<50] <- round(mean(train31$prob[40<=train31$total2 & train31$total2<50],na.rm=T),0)
missprob$prob[50<=missprob$total2 & missprob$total2<60] <- round(mean(train31$prob[50<=train31$total2 & train31$total2<60],na.rm=T),0)
missprob$prob[missprob$total2==60] <- round(mean(train31$prob[train31$total2==60]),0)

train31$prob[is.na(train31$prob)] <- missprob$prob
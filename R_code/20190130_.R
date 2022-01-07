##변수제거한 데이터 불러오기/오늘 날짜가 1월 30일 => train30으로 불러옴

train30<-read.csv("C:/Users/HOME/Desktop/train_v1.0.csv",header=T);train30



######변수별로 결측치가 몇 개 인지######

colSums(is.na(train30))



##pid 결측치 6개는 삭제

train30<-train30[-which(is.na(train30$pid)==T),]



##age_o가 missing인 애들 평균을 wave내 평균으로 넣기

missage<-subset(train30,is.na(age_o))
missage$age_o[58<=missage$pid & missage$pid<=59] <- 25
missage$age_o[missage$pid==129] <- 21
missage$age_o[missage$pid==136] <- 25
missage$age_o[339<=missage$pid & missage$pid<=340] <- 27
missage$age_o[missage$pid==346] <- 27
missage$age_o[missage$pid==512] <- 25
sum(is.na(missage$age_o))

train30$age_o[is.na(train30$age_o)] <- missage$age_o



##int_corr 의 결측치 삭제
train30<-train30[-which(is.na(train30$int_corr)==T),]


##pf_o_fun, pf_o_amb, pf_o_sha 이 NA인 애는 나머지를 더해서 100이 되기 때문에 0을 넣는다

train30$pf_o_fun[is.na(train30$pf_o_fun)] <- 0
train30$pf_o_amb[is.na(train30$pf_o_amb)] <- 0
train30$pf_o_sha[is.na(train30$pf_o_sha)] <- 0




##상대가 나를 평가X,나를 선택/내가 상대를 평가X,상대 선택=>10개 삭제까지

##정리된 데이터

train30<-read.csv("C:/Users/HOME/Desktop/train_v2.0.csv",header=T);train30

train30[which(sum(train30[,58:63])!=100)]

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


train30





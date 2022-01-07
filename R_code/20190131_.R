#####20190131_####

train31<-read.csv("C:/Users/HOME/Desktop/train_v4.2.csv",header=T)
head(train31)

train31$met[train31$met==0]<-2    ######met에 0인 값에 2를 넣음
 
colSums(is.na(train31))


par(mfrow=c(1,2))
plot(train31$attr1_1);plot(train31$attr1_2)


plot(train31$like,train31$prob)



##설문지 1_2의 결측을 1_1로 채움

train31$attr1_2[is.na(train31$attr1_2)] <- train31$attr1_1[is.na(train31$attr1_2)]
train31$sinc1_2[is.na(train31$sinc1_2)] <- train31$sinc1_1[is.na(train31$sinc1_2)] 
train31$intel1_2[is.na(train31$intel1_2)] <- train31$intel1_1[is.na(train31$intel1_2)]
train31$fun1_2[is.na(train31$fun1_2)] <- train31$fun1_1[is.na(train31$fun1_2)]
train31$amb1_2[is.na(train31$amb1_2)] <- train31$amb1_1[is.na(train31$amb1_2)]
train31$shar1_2[is.na(train31$shar1_2)] <- train31$shar1_1[is.na(train31$shar1_2)] 


 
##설문 3_2의 결측을 3_1로 채움

train31$attr3_2[is.na(train31$attr3_2)] <- train31$attr3_1[is.na(train31$attr3_2)]
train31$sinc3_2[is.na(train31$sinc3_2)] <- train31$sinc3_1[is.na(train31$sinc3_2)] 
train31$intel3_2[is.na(train31$intel3_2)] <- train31$intel3_1[is.na(train31$intel3_2)]
train31$fun3_2[is.na(train31$fun3_2)] <- train31$fun3_1[is.na(train31$fun3_2)]
train31$amb3_2[is.na(train31$amb3_2)] <- train31$amb3_1[is.na(train31$amb3_2)]
train31$shar3_2[is.na(train31$shar3_2)] <- train31$shar3_1[is.na(train31$shar3_2)] 




#####wave내 미팅은 전반적으로 만족스러웠으나 1:1 상대는 맘에 들지 않았을 경우가 발생 가능=> 일단 match가 1인 것이 많지 않음
###그럴 경우 match에 영향을 주는 변수는 아님!










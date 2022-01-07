train<-read.csv("C:/Users/HOME/Desktop/Speed_train 1차.csv",header=T)
train<-train[order(train$iid),]
head(train)


train$missrow<-rowSums(is.na(train))   #행별 NA값 뽑아내기
sum(train$missrow>47)            #행의 정보가 50%이상 NA인게 28개
dim(train)
train<-subset(train,missrow<48)      #28개 제거

train<-train[,-95]
#dim(train)하면 6674*94입니다.
missc<-subset(train,is.na(career_c)) #carrer code가 missing인 애들만 따로 뽑은거
missc$career_c[missc$career=='lawyer'|missc$career=='law'] <- 1
missc$career_c[missc$career=='Economist'] <- 2
missc$career_c[missc$career=='tech professional'] <- 5
missc$career_c[is.na(missc$career_c)] <- 15
head(missc)

train$career_c[is.na(train$career_c)] <- missc$career_c  ##원래 데이터에 다시 넣기
head(train)

train<-train[,-39]
head(train)


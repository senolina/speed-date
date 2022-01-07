train7<-read.csv("C:/Users/HOME/Desktop/train_v7.0.csv",header=T);

library("car")

train7$gender<-as.factor(train7$gender)
train7$samerace<-as.factor(train7$samerace)
train7$goal<-as.factor(train7$goal)
train7$dec_o<-as.factor(train7$dec_o)
train7$dec<-as.factor(train7$dec)
train7$met<-as.factor(train7$met)
train7$met_o<-as.factor(train7$met_o)
train7$field_cd<-as.factor(train7$field_cd)
train7$race<-as.factor(train7$race)
train7$race_o<-as.factor(train7$race_o)
train7$career_c<-as.factor(train7$career_c)
train7$numdat_2<-as.factor(train7$numdat_2)
train7$match<-as.factor(train7$match)

##´ÙÁß°ø¼±¼º ¾È¹ß»ýÇÏµµ·Ï ¸¸µë

train71<-train7

train71<-train71[,-c(18,67)]   #dec¶û dec_o»­
train71<-train71[,-c(88,89)]

train71$match<-as.factor(train71$match)
train71$from<-as.factor(train71$from)
train71$gender<-as.factor(train71$gender)
train71$samerace<-as.factor(train71$samerace)
train71$goal<-as.factor(train71$goal)
train71$met<-as.factor(train71$met)
train71$met_o<-as.factor(train71$met_o)
train71$field_cd<-as.factor(train71$field_cd)
train71$race<-as.factor(train71$race)
train71$race_o<-as.factor(train71$race_o)
train71$career_c<-as.factor(train71$career_c)
train71$numdat_2<-as.factor(train71$numdat_2)

str(train71)

glm7 <- glm(match ~ . ,family = "binomial", data = train71)
summary(glm7)
train71<-train71[]

a<-vif(glm7)


glm7 <- glm(match ~ .- career_c17, family = "binomial", data = train7)
a<-vif(glm7)
a[a>10]
a




log.Quality <- glm(PoorCare ~ ., data = train, family = 'binomial')

summary(log.Quality)      
Call:
glm(formula = PoorCare ~ ., family = "binomial", data = train)


log.Quality <- glm(PoorCare ~ . - OfficeVisits , data = train, family = 'binomial')
summary(log.Quality)

Call:
glm(formula = PoorCare ~ . - OfficeVisits, family = "binomial", 
    data = train)


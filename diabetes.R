full <- read.csv("diabetes.csv",header = T)
str(full)
full$Outcome=as.factor(full$Outcome)
table(full$Outcome)

colnames(full)=c("Pregnancies","Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age","Outcome")

#replace all 0,s with nas except for pregnancies and outcome
full %>%
  filter(Glucose==0 | BloodPressure==0 | SkinThickness==0 | Insulin==0 |BMI==0 | DiabetesPedigreeFunction==0 | Age==0)%>%
  tally()

table(is.na(full))

colnames1 <- colnames(full)[!colnames(full) %in% c("Pregnancies","Outcome")]
replace <- full[colnames1]==0
full[colnames1][replace] <- NA

print(apply(replace,2,sum))

#missing value imputation
library(mice)
md.pattern(full)

library(VIM)
aggr_plot <- aggr(full, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(full), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#since bmi,glucose,bp have lesser missing values
#replace them with median
full$BloodPressure[is.na(full$BloodPressure)]<-median(full$BloodPressure,na.rm =TRUE)
full$Glucose[is.na(full$Glucose)]<-median(full$Glucose,na.rm =TRUE)
full$BMI[is.na(full$BMI)]<-median(full$BMI,na.rm =TRUE)

#now mice imputation for insulin and skin thickness
impute_data <- mice(full[,c("Insulin","SkinThickness")],method="rf")
impute_data$imp$Insulin
#let us consider the 3rd iteration
complete_data<-complete(impute_data)

full$Insulin <- complete_data$Insulin
full$SkinThickness <- complete_data$SkinThickness

par(mfrow=c(2,2))

hist(full$Insulin,ylim =c(0,0.004) )
hist(complete_data$Insulin,ylim=c(0,0.004))
hist(full$SkinThickness,ylim=c(0,0.04))
hist(complete_data$SkinThickness,ylim=c(0,0.04))

library(ggplot2)
ggplot(full,aes(x=Age,fill=Outcome))+
  geom_bar(binwidth = 0.5)

sum(is.na(full$BloodPressure))  

ggplot(full,aes(x=Age,y=Pregnancies,fill=Outcome))+
  geom_bar(stat = "identity")+
  xlab("Age")+
  ylab("Pregnancies")+
  labs(fill="Outcome")

ggplot(full,aes(x=Pregnancies,fill=Outcome))+
  geom_bar()+
  xlab("Pregnancies")+
  labs(fill="Outcome")

#by above plot we observe that if pregnancy is between 0-6 less possiblity of dia.

full$Lessthan6 <- sapply(full$Pregnancies,function(x) {if(x>=0 & x<=6) return(1) else return(0)})
as.factor(full$Lessthan6)

boxplot(full$Pregnancies,ylab="Pregnancies",las=1,col=2)

ggplot(full,aes(x=Pregnancies,fill=Outcome))+
  geom_bar()+
  facet_wrap(~Lessthan6)+
  xlab("Pregnancies")+
  labs(fill="Outcome")

  #gucose
  summary(full$Glucose)

  boxplot(full$Glucose ~ full$Outcome,ylab="glucose",las=1)
  
  #can be observed that diabetic people has higher glucose level.
  
  pregnancy_cuts <- cut(full$Pregnancies,breaks = c(0,5,10,17),labels = c("<5","5-10","10-17"),include.lowest = T)
  full$pregnancy_cuts <-pregnancy_cuts
  
  boxplot(full$Glucose[full$Pregnancies>7] ~ full$Outcome[full$Pregnancies>7],ylab="glucose",las=1)
  
  ggplot(full,aes(x=Glucose,fill=Outcome))+
    geom_bar()+
    facet_wrap(~pregnancy_cuts)
  xlab("glucose")+
    labs(fill="Outcome")
 
  #blood pressure
  boxplot(full$BloodPressure ~ full$Outcome,ylab="blood pressure",las=1)
  boxplot(full$BloodPressure[full$Pregnancies>7] ~ full$Outcome[full$Pregnancies>7],ylab="bloodpressure",las=1)
  
  ggplot(full,aes(x=BloodPressure,fill=Outcome))+
    geom_bar()+
    facet_wrap(~pregnancy_cuts)+
  xlab("blood pressure")+
    labs(fill="Outcome")
  
  boxplot(full$BloodPressure ~ full$Outcome,ylab="blood pressure",las=1,notch=T)
  
  bpcut <- cut(full$BloodPressure,breaks = c(0,20,40,60,80,100),labels = c("<20","20-40","40-60","60-80",">80"),include.lowest = T)
  table(bpcut)
  full$bpcut<-bpcut
  as.factor(full$bpcut)
  
  ggplot(full,aes(x=BloodPressure,fill=Outcome))+
    geom_bar()+
    facet_wrap(~bpcut)+
    xlab("blood pressure")+
    labs(fill="Outcome")
 
  #skin thickness
  full %>%
    group_by(Outcome) %>%
    summarise(mean(SkinThickness))
    
  full %>%
    filter(Pregnancies==0|Glucose==0| BloodPressure==0 | SkinThickness==0 | Insulin==0|BMI==0 |DiabetesPedigreeFunction==0|Age==0) %>%
    tally()
  
  ggplot(full,aes(Outcome,SkinThickness))+
    geom_boxplot()+
    labs(fill="Outcome")
    
  #insulin
  ggplot(full,aes(x=Outcome,y=Insulin))+
    geom_boxplot()
  
  boxplot(full$Insulin ~ full$Outcome,ylab="insulin",las=1)
  
  ggplot(full,aes(x=full$Insulin,fill=Outcome))+
    geom_histogram()
    
  full %>%
    filter(Insulin<140) %>%
    select(Outcome) %>%
    group_by(Outcome) %>%
    tally()
 # so the above calculation imply insulin should be less than 140  
  
  ggplot(full,aes(x=full$Insulin,fill=Outcome))+
    facet_wrap(~pregnancy_cuts)+
    geom_histogram()
  
  ggplot(full,aes(y=full$Insulin,x=Outcome,fill=Outcome))+
    facet_wrap(~pregnancy_cuts)+
    geom_boxplot()
  
    #BMI
  ggplot(full,aes(x=full$BMI,fill=Outcome))+
    geom_histogram(binwidth = 20)
  
  #so higher bmi higher risk of diabetes
  max(full$BMI)
  min(full$BMI)
  mean(full$BMI)
  
  full %>%
    filter(BMI>40) %>%
    select(Outcome) %>%
    group_by(Outcome) %>%
    tally()
  
  full %>%
    filter(BMI>40) %>%
    group_by(pregnancy_cuts) %>%
    tally()
  
  #diabetes pedegree function
  ggplot(full,aes(x=full$DiabetesPedigreeFunction,fill=Outcome))+
    geom_histogram()
#age
  ggplot(full,aes(x=full$Age,fill=Outcome))+
    geom_histogram()
  
  ggplot(full,aes(y=full$Age,x=Outcome,fill=Outcome))+
    geom_boxplot()
  
  
  #model
  set.seed(3)
  id<-sample(2,nrow(full),prob = c(0.7,0.3),replace = T)
  full_train <- full[id==1,]
  full_test <- full[id==2,]
  library(rpart)
  library(party)
  full_model <- ctree(Outcome~.,data=full_train)
  full_model
plot(full_model)  

full_pred<-predict(full_model,full_test)
full_pred
library(caret)
confusionMatrix(full_pred,full_test$Outcome)

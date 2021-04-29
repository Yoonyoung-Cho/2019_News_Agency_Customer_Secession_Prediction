setwd("G:/내 드라이브/대학원/통분방/보고서") # laptop
setwd("I:/내 드라이브/대학원/통분방/보고서") # Desktop
library(survival) 
library(plyr)
library(fmsb)
library(e1071)
library(kernlab)
library(VGAM)
library(KMsurv)
library(nlme)
library(rpart)
library(ipred)
library(randomForest)
library(corrplot)
library(survMisc)
# install.packages("MLmetrics")
library(MLmetrics)
# library(ROCR)

# library(scatterplot3d)

# data Cleaning ####
data <- read.csv("Telco-Customer-Churn.csv")
names(data)
data$censor <- rep(0,nrow(data))
data[data$tenure<=72 & data$Churn=="No",22] <- 1
data <- data[,-c(1,8,10,11,12,13,14,15)] # customer ID 제거, 안쓰는 사람들 제
data <- data[!is.na(data$TotalCharges),] # 가입한지 한달 이내 고객 제거

# factorization
data$SeniorCitizen <- factor(data$SeniorCitizen,levels=c(1,0))
data$SeniorCitizen <- revalue(data$SeniorCitizen, c("0"="No", "1"="Yes"))
# data$Churn <- revalue(data$Churn, c("No"=0, "Yes"=1))
# 생존분석 카플란 마이어할때 이거 안됨. . 
# data$censor <- factor(data$censor) #, c("No"=0, "Yes"=1))
# data$censor <- revalue(data$censor, c("1"="Yes", "0"="No"))
str(data)
# data$Partner <- revalue(data$Partner, c("No"=0, "Yes"=1))
# data$Dependents <- revalue(data$Dependents, c("No"=0, "Yes"=1))
# data$PhoneService <- revalue(data$PhoneService, c("No"=0, "Yes"=1))
# data$PaperlessBilling <- revalue(data$PaperlessBilling, c("No"=0, "Yes"=1))
str(data)

summary(data) # no NA
names(data)
summary(data$tenure)
boxplot(data$tenure)
boxplot(data$Churn)
hist(data$tenure)
# 1. 분할표 분석 ####
# [참고] 
# 3.1 #
# install.packages('fmsb')
# oddsratio(5,8,1,82) 
# Senior인것과 payment method가 연관성이 있다.
# paperless Billing 인것과 상관있다.

table(data$PaperlessBilling, data$SeniorCitizen)
S.P <- matrix(table(data$PaperlessBilling, data$SeniorCitizen),ncol=2)
colnames(S.P)<-c("Not PaperBilling","PaperBilling") 
rownames(S.P)<-c("Senior Citizen","Not Senior Citizen") 
oddsratio(S.P)  # association between 2.


# Churn 과 다른 변수들 확인. 
temp <- matrix(rep(0,4),2,2)
# 7,8,10 removed Since its not 2x2
for (i in c(1:4,6,9)){
temp <- matrix(table(data[,i],data$Churn),ncol=length(unique(levels(data[,i]))))
print(i)
print(names(data)[i])
print(oddsratio(temp)$estimate)
}

# Paperless Billing 이 너무 큼
addmargins(table(data$PaperlessBilling,data$Churn))
(1400/1869)/(469/1869) 
(2768/5136)/(2395/5163) # 
((1400/1869)/(469/1869) )/ ((2768/5136)/(2395/5163))
# churn한 집단에서 paper billing을 한 사람들이 churn안한 사람들보다 2.6배 많다.  
matrix(table(data$PaperlessBilling,data$Churn))
# Churn과 아무 관련 없는 것은 gender와 Phone Service이었다. 

# 여러 범주 인 애들 카이제곱 검정 ####
# Internet Service
I.C <- table(data$InternetService,data$Churn)
chisq.test(I.C)
# Contract
C.C <- table(data$Contract,data$Churn)
chisq.test(C.C)
# Payment Method
P.C <- table(data$PaymentMethod,data$Churn)
chisq.test(P.C)
# Gender
G.C <- table(data$gender,data$Churn)
chisq.test(G.C)
#### 층을 넣고 연관성 확인 ####
# Senior를 제어했을 때, internet Service를 이용하는 것과 Churn과 상관이 있다. 
# 인터넷을 이용하면 churn 할수도? 인터넷이 너무 느려서.
# 질병의 심각도를 층변수로 제어했을 때 혈액 기증자들의 성별과 환자들의 생존 상태 사이에는 유의한 연관성
# 3.8 
str(data$PaperlessBilling)
table(data$PaperlessBilling,data$Churn, data$SeniorCitizen)
b <- as.vector(table(data$PaperlessBilling,data$Churn, data$SeniorCitizen))

# H0 : paper billing 과 churn과는 독립이다 - 기각
S.P.C <- array(c(188,478,78,398,2207,2290,391,1002),
               dim=c(2,2,2),dimnames=list(PaperlessBilling=c("No","Yes"),
                                          Churn=c("No","Yes"),SeniorCitizen=c("Yes","No")))

mantelhaen.test(S.P.C , correct=FALSE)

#### Internet Service ####
# H0: internet service와 Churn이 독립이다 (when controlling Senior Citizen) - 기각 
table(data$InternetService,data$Churn, data$SeniorCitizen)
I.C.S <- array(c(181,438,47,78,393,5,1776,1361,1360,381,904,108), dim=c(4,2,2),
               dimnames=list(InternetService=c("DSL", "Fiber", "optic" ,"No"),
                             Churn=c("No", "Yes"),SeniorCitizen=c("Yes","No")))
mantelhaen.test(I.C.S , correct=FALSE)

# H0: internet service와 Churn이 독립이다 (when controlling Dependents) - 기각 
table(data$InternetService,data$Churn, data$Dependents)
I.C.D <- array(c(1253,1339,798,363,1095,85,704,460,609,96,202,28), dim=c(4,2,2),
               dimnames=list(InternetService=c("DSL", "Fiber", "optic" ,"No"),
                             Churn=c("No", "Yes"),Dependents=c("No","Yes")))
mantelhaen.test(I.C.D , correct=FALSE)

# H0: internet service와 senior citizen이 독립이다 (when controlling gender) - 기각 
table(data$InternetService,data$SeniorCitizen, data$gender)
I.S.G <- array(c(123,424,21,1061,1129,725,136,407,31,1096,1136,743), dim=c(4,2,2),
               dimnames=list(InternetService=c("DSL", "Fiber", "optic" ,"No"),
                             gender=c("Female", "Male"),SeniorCitizen=c("Yes","No")))
mantelhaen.test(I.S.G , correct=FALSE)


#### Contract ####

# H0: Contract와 Churn이 독립이다. (when controlling Partner) - 기각 
table(data$Contract,data$Churn, data$Partner)
C.C.P <- array(c(1380,575,484,1115,68,17,840,731,1153,540,98,31), dim=c(3,2,2),
               dimnames=list(Contract=c("Month-to-month", "One year", "Two year"),
                             Churn=c("No", "Yes"),Partner=c("No","Yes")))
mantelhaen.test(C.C.P , correct=FALSE)

# H0: Contract와 Churn이 독립이다. (when controlling Dependents) - 기각 
table(data$Contract,data$Churn, data$Dependents)
C.C.D <- array(c(1690,825,875,1396,117,30,530,481,762,259,29,18), dim=c(3,2,2),
               dimnames=list(Contract=c("Month-to-month", "One year", "Two year"),
                             Churn=c("No", "Yes"),Dependents=c("No","Yes")))
mantelhaen.test(C.C.D , correct=FALSE)





# H0: Phone service와 Senior Citizen이 독립이다 (when controlling gender) - not reject
table(data$PhoneService,data$SeniorCitizen, data$gender)
P.S.G <- array(c(45,523,284,2631,59,515,292,2683), dim=c(2,2,2),
               dimnames=list(PhoneService=c("No","Yes"),
                             gender=c("Female", "Male"),SeniorCitizen=c("Yes","No")))
mantelhaen.test(P.S.G , correct=FALSE)

# H0: Dependents가 있는것과 Internet Service는 독립이다 (when controlling Senior) - 기각
table(data$InternetService,data$Dependents, data$SeniorCitizen)
D.I.S <- array(c(230,29,772,59,49,3,1386,1662,834,771,603,634), dim=c(3,2,2),
               dimnames=list(InternetService=c("DSL","Fiber optic","No"),
                             Dependents=c("No", "Yes"),SeniorCitizen=c("Yes","No")))
mantelhaen.test(D.I.S , correct=FALSE)




# 2. 로그 선형 분석 ####
data <- read.csv("Telco-Customer-Churn.csv")
names(data)
data$censor <- rep(0,nrow(data))
data[data$tenure<=72 & data$Churn=="No",22] <- 1
data <- data[,-c(1,8,10,11,12,13,14,15)] # customer ID 제거, 안쓰는 사람들 제
data <- data[!is.na(data$TotalCharges),] # 가입한지 한달 이내 고객 제거

# factorization
data$SeniorCitizen <- factor(data$SeniorCitizen,levels=c(1,0))
data$SeniorCitizen <- revalue(data$SeniorCitizen, c("0"="No", "1"="Yes"))
# data$Churn <- revalue(data$Churn, c("No"=0, "Yes"=1))
data$censor <- factor(data$censor) #, c("No"=0, "Yes"=1))
data$censor <- revalue(data$censor, c("1"="Yes", "0"="No"))
str(data)
dataL <- dataL

# data partition
V = 2
n =  NROW(dataL)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
data.train = dataL[ii,]
data.test  = dataL[-ii,]


### 전체 넣고 glm ####
fit=glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
          InternetService+Contract+PaperlessBilling+PaymentMethod+
          MonthlyCharges+TotalCharges, data=data.train, family=binomial)
summary(fit)

# Variable selection
stepwise <- step(fit)
fit2=glm(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + 
           InternetService + Contract + PaperlessBilling + PaymentMethod + 
           TotalCharges, data=data.train, family=binomial)
summary(fit2)


pred = predict(fit2, newdata=data.test, type="response") #prediction
head(pred)
yhat = ifelse(pred > cutoff, 1, 0)
ctable = table(data.test$Churn, yhat, dnn=c("Actual", "Predicted")); ctable #classification table
Accuracy(yhat, as.numeric(data.test$Churn)-1) 
Precision(yhat, as.numeric(data.test$Churn)-1)
F1_Score(yhat, as.numeric(data.test$Churn)-1)
Recall(yhat, as.numeric(data.test$Churn)-1)




# pchisq(fit$deviance, fit$residual, lower.tail = F)
# 3. 로짓 모형 ####
### Churn & Internet service & dependents ####
a <- array(table(data$InternetService,data$Churn, data$Dependents))
(table(data$InternetService,data$Churn, data$Dependents))
data.log.li =data.frame(expand.grid(InternetService = factor(c("DSL","Fiber optic","No") , levels=c("DSL","Fiber optic","No")),
                                    Churn = factor(c("No","Yes") , levels=c("No","Yes")),
                                    Dependents = factor(c("No","Yes") , levels=c("No","Yes"))),
                        count=a)

fit = glm(formula= count~InternetService*Churn*Dependents,family=poisson(link=log), data=data.log.li)
summary(fit)

# 3 factor 제거 # Homogeneous
fit.reduced = glm(formula = count~InternetService*Churn + InternetService*Dependents + Dependents*Churn, family = poisson ,
                  data=data.log.li)
summary(fit.reduced)

# 2int and 1
# Conditional Independent
fit.reduced1 = glm(formula = count~ Dependents*InternetService + Dependents*Churn, family = poisson , data=data.log.li)
summary(fit.reduced1)

fit.reduced2 = glm(formula = count~ Dependents*Churn + InternetService*Churn, family = poisson ,
                   data=data.log.li)
summary(fit.reduced2)

fit.reduced3 = glm(formula = count~ InternetService*Dependents + InternetService*Churn, family = poisson , data=data.log.li)
summary(fit.reduced3)

# 1 int and 1
# Joint independent 
fit.reduced4 = glm(formula = count~ Dependents + InternetService*Churn, family = poisson , data=data.log.li)
summary(fit.reduced4)
fit.reduced5 = glm(formula = count~ Churn + Dependents*InternetService, family = poisson , data=data.log.li)
summary(fit.reduced5)
fit.reduced6 = glm(formula = count~ InternetService + Dependents*Churn, family = poisson , data=data.log.li)
summary(fit.reduced6)

# Independent 
fit.indep = glm(formula = count~ Dependents+Churn+InternetService, family = poisson , data=data.log.li)
summary(fit.indep)
#### Logistic1 ####
fit.logistic1 = glm(Churn~Dependents+InternetService, family=binomial(link=logit), weight=count ,data=data.log.li)
fit.logistic1 = glm(Churn~Dependents*InternetService, family=binomial(link=logit), weight=count ,data=data.log.li)
summary(fit.logistic1)


### Churn & Internet service & Contract ####

a <- array(table(data$InternetService,data$Churn, data$Contract))
(table(data$InternetService,data$Churn, data$Contract))
data.log.li =data.frame(expand.grid(InternetService = factor(c("DSL","Fiber optic","No") , levels=c("DSL","Fiber optic","No")),
                                    Churn = factor(c("No","Yes") , levels=c("No","Yes")), 
                                    Contract = factor(c("Month-to-Month","One year", "Two year") , levels=c("Month-to-Month","One year", "Two year"))),
                        count=a)

fit = glm(formula= count~InternetService*Churn*Contract,family=poisson(link=log), data=data.log.li)
summary(fit)
# 3 factor 제거 # Homogeneous
fit.reduced = glm(formula = count~InternetService*Churn + InternetService*Contract + Contract*Churn, family = poisson ,
                  data=data.log.li)
summary(fit.reduced)

qchisq(0.95,8)
# 2int and 1
# Conditional Independent
fit.reduced1 = glm(formula = count~ Contract*InternetService + Contract*Churn, family = poisson , data=data.log.li)
summary(fit.reduced1)

fit.reduced2 = glm(formula = count~ Contract*Churn + InternetService*Churn, family = poisson ,
                   data=data.log.li)
summary(fit.reduced2)

fit.reduced3 = glm(formula = count~ InternetService*Contract + InternetService*Churn, family = poisson , data=data.log.li)
summary(fit.reduced3)

# 1 int and 1
# Joint independent 
fit.reduced4 = glm(formula = count~ Contract + InternetService*Churn, family = poisson , data=data.log.li)
summary(fit.reduced4)
fit.reduced5 = glm(formula = count~ Churn + Contract*InternetService, family = poisson , data=data.log.li)
summary(fit.reduced5)
fit.reduced6 = glm(formula = count~ InternetService + Contract*Churn, family = poisson , data=data.log.li)
summary(fit.reduced6)

# Independent 
fit.indep = glm(formula = count~ Contract+Churn+InternetService, family = poisson , data=data.log.li)
summary(fit.indep)
#### Logistic 2 ####
fit.logistic2 = glm(Churn~Contract+InternetService, family=binomial(link=logit), weight=count ,data=data.log.li)
summary(fit.logistic2)
### Churn & Senior & gender #####

a <- array(table(data$SeniorCitizen,data$Churn, data$gender))
(table(data$SeniorCitizen,data$Churn, data$gender))
data.log.li =data.frame(expand.grid(SeniorCitizen = factor(c("Yes","No") , levels=c("Yes","No")),
                                    Churn = factor(c("No","Yes") , levels=c("No","Yes")), 
                                    gender = factor(c("Female","Male") , levels=c("Female","Male"))),
                        count=a)

fit = glm(formula= count~SeniorCitizen*Churn*gender,family=poisson(link=log), data=data.log.li)
summary(fit)
# 3 factor 제거 # Homogeneous
fit.reduced = glm(formula = count~SeniorCitizen*Churn + SeniorCitizen*gender + gender*Churn, family = poisson ,
                  data=data.log.li)
summary(fit.reduced)

# 2int and 1
# Conditional Independent
fit.reduced1 = glm(formula = count~ gender*SeniorCitizen + gender*Churn, family = poisson , data=data.log.li)
summary(fit.reduced1)

fit.reduced2 = glm(formula = count~ gender*Churn + SeniorCitizen*Churn, family = poisson ,
                   data=data.log.li)
summary(fit.reduced2)

fit.reduced3 = glm(formula = count~ SeniorCitizen*gender + SeniorCitizen*Churn, family = poisson , data=data.log.li)
summary(fit.reduced3)

# 1 int and 1
# Joint independent 
fit.reduced4 = glm(formula = count~ gender + SeniorCitizen*Churn, family = poisson , data=data.log.li)
summary(fit.reduced4)
fit.reduced5 = glm(formula = count~ Churn + gender*SeniorCitizen, family = poisson , data=data.log.li)
summary(fit.reduced5)
fit.reduced6 = glm(formula = count~ SeniorCitizen + gender*Churn, family = poisson , data=data.log.li)
summary(fit.reduced6)

# Independent 
fit.indep = glm(formula = count~ gender+Churn+SeniorCitizen, family = poisson , data=data.log.li)
summary(fit.indep)
#### Logisitc 3 ####
fit.logistic3 = glm(Churn~gender*SeniorCitizen, family=binomial(link=logit), weight=count ,data=data.log.li)
summary(fit.logistic3)
summary(fit.logistic1)
summary(fit.logistic2)
summary(fit.logistic3)
# 4. 생존분석 ####
## 생명표 방법 ####
# install.packages("nlme")
# install.packages("KMsurv")
library(KMsurv)
library(nlme)

data <- read.csv("Telco-Customer-Churn.csv")
names(data)
data$censor <- rep(0,nrow(data))
data[data$tenure<=72 & data$Churn=="No",22] <- 1
data <- data[,-c(1,8,10,11,12,13,14,15)] # customer ID 제거, 안쓰는 사람들 제
data <- data[!is.na(data$TotalCharges),] # 가입한지 한달 이내 고객 제거

# factorization
data$SeniorCitizen <- factor(data$SeniorCitizen,levels=c(1,0))
data$SeniorCitizen <- revalue(data$SeniorCitizen, c("0"="No", "1"="Yes"))


# 생명표 방법 12개월 기준으로 나눔. 
a <- data[data$tenure<=12,] #1
b <- data[12<data$tenure & data$tenure<=24,] #2
c <- data[24<data$tenure & data$tenure<=36,] #3
d <- data[36<data$tenure & data$tenure<=48,] #4
e <- data[48<data$tenure & data$tenure<=60,] #5
f <- data[60<data$tenure & data$tenure<=72,] #6
g <- data[data$tenure==72,] #censored more than 6 years.
nrow(data)
time <- c(0:6, NA)
death <- c(nrow(a[a$Churn=="Yes",]),nrow(b[b$Churn=="Yes",]),nrow(c[c$Churn=="Yes",]),
           nrow(d[d$Churn=="Yes",]),nrow(e[e$Churn=="Yes",]),nrow(f[f$Churn=="Yes",]),
           nrow(g[g$Churn=="Yes",]))
censor<- c(nrow(a[a$Churn=="No",]),nrow(b[b$Churn=="No",]),nrow(c[c$Churn=="No",]),
           nrow(d[d$Churn=="No",]),nrow(e[e$Churn=="No",]),nrow(f[f$Churn=="No",]),
           nrow(g[g$Churn=="No",]))
sur <- lifetab(tis=time, ninit = nrow(data),
               nlost=censor, nevent = death)
sur
par(mfrow=c(1,1))
plot(time[1:7], sur[,5], type = "s", xlab = "survival time in every one year", ylab = "Proportion Surviving", main="Life table method")

## 누적한계추정법-카플란 마이어 ####
# install.packages("survival")
# library(survival)
fit_K <- with(data, survfit(Surv(tenure, censor) ~ 1, conf.type = "none")) 
summary(fit_K) 
plot(fit_K, xlab = "Time", ylab = "Survival Probability", main="Kaplan Meier")



## log-rank test ####
names(data)
windows(10,10)
par(mfrow=c(3,3))
plot(survfit(Surv(tenure,censor)~gender,data=data), main = "Kaplan Meier Gender", xlab = "",ylab="Survival Probability",col=c("blue","red"))
survdiff(Surv(tenure, censor)~gender, data = data) # do not reject
fit = survfit(Surv(tenure, censor) ~ gender, data = data)
comp(ten(fit))$tests$lrTests

plot(survfit(Surv(tenure,censor)~SeniorCitizen,data=data), main = "Kaplan Meier SeniorCitizen", xlab = "",ylab="Survival Probability",col=c("blue","red"))
survdiff(Surv(tenure, censor)~SeniorCitizen, data = data) # reject

plot(survfit(Surv(tenure,censor)~Partner,data=data), main = "Kaplan Meier Partner", xlab = "",ylab="Survival Probability",col=c("blue","red"))
survdiff(Surv(tenure, censor)~Partner, data = data) # reject

plot(survfit(Surv(tenure,censor)~Dependents,data=data), main = "Kaplan Meier Dependents", xlab = "",ylab="Survival Probability",col=c("blue","red"))
survdiff(Surv(tenure, censor)~Dependents, data = data) # reject

plot(survfit(Surv(tenure,censor)~PhoneService,data=data), main = "Kaplan Meier PhoneService", xlab = "",ylab="Survival Probability",col=c("blue","red"))
survdiff(Surv(tenure, censor)~PhoneService, data = data) # do not reject

plot(survfit(Surv(tenure,censor)~InternetService,data=data), main = "Kaplan Meier Internet Service", xlab = "",ylab="Survival Probability",col=c("blue","red","orange"))
survdiff(Surv(tenure, censor)~InternetService, data = data) # reject

plot(survfit(Surv(tenure,censor)~Contract,data=data), main = "Kaplan Meier Contract", xlab = "",ylab="Survival Probability",col=c("blue","red","orange"))
survdiff(Surv(tenure, censor)~Contract, data = data) # reject

plot(survfit(Surv(tenure,censor)~PaperlessBilling,data=data), main = "Kaplan Meier Paperless Billing", xlab = "",ylab="Survival Probability",col=c("blue","red"))
survdiff(Surv(tenure, censor)~PaperlessBilling, data = data) # reject

plot(survfit(Surv(tenure,censor)~PaymentMethod,data=data), main = "Kaplan Meier Payment Method", xlab = "",ylab="Survival Probability",col=c("blue","red","orange","green"))
survdiff(Surv(tenure, censor)~PaymentMethod, data = data) # reject


### senior와 non_senior  ###
library(KMsurv)
library(nlme)

data_S <- data[data$SeniorCitizen=="Yes",]
data_NS <- data[data$SeniorCitizen=='No',]

## Senior ###
a <- data_S[data_S$tenure<=12,] #1
b <- data_S[12<data_S$tenure & data_S$tenure<=24,] #2
c <- data_S[24<data_S$tenure & data_S$tenure<=36,] #3
d <- data_S[36<data_S$tenure & data_S$tenure<=48,] #4
e <- data_S[48<data_S$tenure & data_S$tenure<=60,] #5
f <- data_S[60<data_S$tenure & data_S$tenure<=72,] #6
g <- data_S[data_S$tenure==72,] #censored more than 6 years.
time <- c(0:6, NA)
death <- c(nrow(a[a$Churn=="Yes",]),nrow(b[b$Churn=="Yes",]),nrow(c[c$Churn=="Yes",]),
           nrow(d[d$Churn=="Yes",]),nrow(e[e$Churn=="Yes",]),nrow(f[f$Churn=="Yes",]),
           nrow(g[g$Churn=="Yes",]))
censor<- c(nrow(a[a$Churn=="No",]),nrow(b[b$Churn=="No",]),nrow(c[c$Churn=="No",]),
           nrow(d[d$Churn=="No",]),nrow(e[e$Churn=="No",]),nrow(f[f$Churn=="No",]),
           nrow(g[g$Churn=="No",]))
sur <- lifetab(tis=time, ninit = nrow(data_S),
               nlost=censor, nevent = death)
sur
plot(time[1:7], sur[,5], type = "s", col="red", ylim=c(0,1),xlab = "survival time in every one year", ylab = "Proportion Surviving", main="Life table method - Senior & non_Senior")


## non_senior ###

a <- data_NS[data_NS$tenure<=12,] #1
b <- data_NS[12<data_NS$tenure & data_NS$tenure<=24,] #2
c <- data_NS[24<data_NS$tenure & data_NS$tenure<=36,] #3
d <- data_NS[36<data_NS$tenure & data_NS$tenure<=48,] #4
e <- data_NS[48<data_NS$tenure & data_NS$tenure<=60,] #5
f <- data_NS[60<data_NS$tenure & data_NS$tenure<=72,] #6
g <- data_NS[data_NS$tenure==72,] #censored more than 6 years.
nrow(data_NS)
time <- c(0:6, NA)
death <- c(nrow(a[a$Churn=="Yes",]),nrow(b[b$Churn=="Yes",]),nrow(c[c$Churn=="Yes",]),
           nrow(d[d$Churn=="Yes",]),nrow(e[e$Churn=="Yes",]),nrow(f[f$Churn=="Yes",]),
           nrow(g[g$Churn=="Yes",]))
censor<- c(nrow(a[a$Churn=="No",]),nrow(b[b$Churn=="No",]),nrow(c[c$Churn=="No",]),
           nrow(d[d$Churn=="No",]),nrow(e[e$Churn=="No",]),nrow(f[f$Churn=="No",]),
           nrow(g[g$Churn=="No",]))
sur2 <- lifetab(tis=time, ninit = nrow(data_NS),
               nlost=censor, nevent = death)
sur2

### Log rank test & plot ###
plot(time[1:7], sur[,5], type = "s", col="red", ylim=c(0.4,1),xlab = "survival time in every one year", ylab = "Proportion Surviving", main="Life table method - Senior & non_Senior")
par(new=T)
plot(time[1:7], sur2[,5], type = "s", col="blue", ylim=c(0.4,1), xlab = "", ylab = "")
# plot(time[1:7], sur2[,5], type = "s", xlab = "survival time in every one year", ylab = "Proportion Surviving", main="Life table method - Non_Senior")
legend(0.2, 0.5, legend = c("Senior","non-Senior"), col = c("red","blue"), lty = c(1,1), lwd = 2)

# Kaplan-Meier method plot 
fit_S <- with(data_S, survfit(Surv(tenure, censor) ~ 1, conf.type = "none")) 
plot(fit_S, xlab = "", ylab = "", col="red")
par(new=T)
fit_NS <- with(data_NS, survfit(Surv(tenure, censor) ~ 1, conf.type = "none")) 
plot(fit_NS, xlab = "Time", col="blue", ylab = "Survival Probability", main="Kaplan Meier")
legend(0.2, 0.5, legend = c("Senior","non-Senior"), col = c("red","blue"), lty = c(1,1), lwd = 2)

# log rank test ##
survdiff(Surv(tenure, censor)~SeniorCitizen, data = data)



### gender ###
data_F <- data[data$gender=="Female",]
data_M <- data[data$gender=="Male",]



## Female ###
a <- data_F[data_F$tenure<=12,] #1
b <- data_F[12<data_F$tenure & data_F$tenure<=24,] #2
c <- data_F[24<data_F$tenure & data_F$tenure<=36,] #3
d <- data_F[36<data_F$tenure & data_F$tenure<=48,] #4
e <- data_F[48<data_F$tenure & data_F$tenure<=60,] #5
f <- data_F[60<data_F$tenure & data_F$tenure<=72,] #6
g <- data_F[data_F$tenure==72,] #censored more than 6 years.
nrow(data_F)
time <- c(0:6, NA)
death <- c(nrow(a[a$Churn=="Yes",]),nrow(b[b$Churn=="Yes",]),nrow(c[c$Churn=="Yes",]),
           nrow(d[d$Churn=="Yes",]),nrow(e[e$Churn=="Yes",]),nrow(f[f$Churn=="Yes",]),
           nrow(g[g$Churn=="Yes",]))
censor<- c(nrow(a[a$Churn=="No",]),nrow(b[b$Churn=="No",]),nrow(c[c$Churn=="No",]),
           nrow(d[d$Churn=="No",]),nrow(e[e$Churn=="No",]),nrow(f[f$Churn=="No",]),
           nrow(g[g$Churn=="No",]))
sur <- lifetab(tis=time, ninit = nrow(data_F),
               nlost=censor, nevent = death)
sur
plot(time[1:7], sur[,5], type = "s", col="red", ylim=c(0,1),xlab = "survival time in every one year", ylab = "Proportion Surviving", main="Life table method - Female & non_Female")


## Male ###

a <- data_M[data_M$tenure<=12,] #1
b <- data_M[12<data_M$tenure & data_M$tenure<=24,] #2
c <- data_M[24<data_M$tenure & data_M$tenure<=36,] #3
d <- data_M[36<data_M$tenure & data_M$tenure<=48,] #4
e <- data_M[48<data_M$tenure & data_M$tenure<=60,] #5
f <- data_M[60<data_M$tenure & data_M$tenure<=72,] #6
g <- data_M[data_M$tenure==72,] #censored more than 6 years.
nrow(data_M)
time <- c(0:6, NA)
death <- c(nrow(a[a$Churn=="Yes",]),nrow(b[b$Churn=="Yes",]),nrow(c[c$Churn=="Yes",]),
           nrow(d[d$Churn=="Yes",]),nrow(e[e$Churn=="Yes",]),nrow(f[f$Churn=="Yes",]),
           nrow(g[g$Churn=="Yes",]))
censor<- c(nrow(a[a$Churn=="No",]),nrow(b[b$Churn=="No",]),nrow(c[c$Churn=="No",]),
           nrow(d[d$Churn=="No",]),nrow(e[e$Churn=="No",]),nrow(f[f$Churn=="No",]),
           nrow(g[g$Churn=="No",]))
sur2 <- lifetab(tis=time, ninit = nrow(data_M),
                nlost=censor, nevent = death)
sur2

### Log rank test & plot ###
plot(time[1:7], sur[,5], type = "s", col="red", ylim=c(0.4,1),xlab = "survival time in every one year", ylab = "Proportion Surviving", main="Life table method - Female & Male")
par(new=T)
plot(time[1:7], sur2[,5], type = "s", col="blue", ylim=c(0.4,1), xlab = "", ylab = "")
# plot(time[1:7], sur2[,5], type = "s", xlab = "survival time in every one year", ylab = "Proportion Surviving", main="Life table method - Non_Female")
legend(0.2, 0.5, legend = c("Female","Male"), col = c("red","blue"), lty = c(1,1), lwd = 2)

# Kaplan-Meier method plot 
data_F <- with(data_F, survfit(Surv(tenure, censor) ~ 1, conf.type = "none")) 
plot(data_F, xlab = "", ylab = "", col="red")
par(new=T)
data_M <- with(data_M, survfit(Surv(tenure, censor) ~ 1, conf.type = "none")) 
plot(data_M, xlab = "Time", col="blue", ylab = "Survival Probability", main="Kaplan Meier")
legend(0.2, 0.5, legend = c("Female","Male"), col = c("red","blue"), lty = c(1,1), lwd = 2)

par(mfrow=c(1,1))
plot(survfit(Surv(tenure,censor)~gender,data=data), main = "Kaplan Meier Gender", xlab = "",ylab="Survival Probability",col=c("blue","red"))
legend("topright", legend=c("femal", "male"),fill=c("blue","red"),bty="n")


# log rank test ##
survdiff(Surv(tenure, censor)~gender, data = data) # 이건 안한다고? - 평행하긴한데....거의 똑같아서 의미 X
survdiff(Surv(tenure, censor)~SeniorCitizen, data = data) # 이건 기각하고 - 평행이 아니라서 그런가? 

fit = survfit(Surv(tenure, censor) ~ SeniorCitizen, data = data)
comp(ten(fit))$tests$lrTests
fit = survfit(Surv(tenure, censor) ~ gender, data = data)
comp(ten(fit))$tests$lrTests
par(mfrow=c(1,1))
plot(survfit(Surv(tenure,censor)~Dependents,data=data), main = "Kaplan Meier Dependents", xlab = "",ylab="Survival Probability",col=c("blue","red"))
# legend("topright", legend=c("femal", "male"),fill=c("blue","red"),bty="n")
fit = survfit(Surv(tenure, censor) ~ Dependents, data = data)
comp(ten(fit))$tests$lrTests
plot(survfit(Surv(tenure,censor)~InternetService,data=data), main = "Kaplan Meier Dependents", xlab = "",ylab="Survival Probability",col=c("blue","red","orange"))
# legend("topright", legend=c("female", "male"),fill=c("blue","red"),bty="n")
fit = survfit(Surv(tenure, censor) ~ InternetService, data = data)
comp(ten(fit))$tests$lrTests


## Parametric Model ####
# diabetes <- data.frame(time = c(3.6, 15.4, 11.3, 10.3, 5.8, 8.0, 14.6,
#                                 11.4, 7.2, 5.5, 11.1, 16.5, 10.9, 2.5,
#                                 10.8, 4.7, 5.5, 4.5, 9.0, 6.8, 3.6, 
#                                 12.1, 8.1, 11.1, 7.0, 1.5, 11.7, 0.3), 
#                        censor = c(1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,0,1,1,
#                                   1,0,1,1,1,0,1,1,1), 
#                        age = c(63, 71, 51, 59, 50, 66, 42, 40, 67, 86,
#                                52, 42, 60, 75, 81, 60, 60, 63, 62, 57,
#                                71, 58, 42, 45, 66, 61, 48, 82), 
#                        BMI = c(25.1, 26, 32, 28.1, 26.1, 45.3, 30, 35.7,
#                                28.1, 32.9, 37.6, 43.4, 25.4, 49.7, 35.2,
#                                37.3, 26, 21.8, 18.2, 34.1, 25.6, 35.1, 
#                                32.5, 44.1, 29.7, 29.2, 25.2, 25.3), 
#                        diag_age = c(46, 59, 49, 57, 49, 49, 41, 36, 61,
#                                     61, 46, 37, 60, 57, 81, 39, 42, 60,
#                                     43, 41, 54, 45, 28, 40, 59, 54, 30, 50), 
#                        smoking = c(1,0,1,1,1,0,1,1,0,0,1,0,0,1,0,0,0,1,0,
#                                    1,1,0,1,0,1,0,1,0))
library(survival) 
# fit.null <- survfit(Surv(time, censor) ~ 1, data = diabetes) 
fit.null <- survfit(Surv(tenure, censor) ~ 1, data = data) 
time <- fit.null$time 
logtime <- log(fit.null$time) 
lls <- log(-log(fit.null$surv)) 
ls <- -log(fit.null$surv) 
logls <- log((1-fit.null$surv)/fit.null$surv) 
lnorm <- qnorm(1-fit.null$surv)
windows(10,10)
par(mfrow=c(2,2)) 
plot(logtime, lls, ylab = "log[-log(Survival Probability)]", xlab = "log(time)", main = "Weibull") 
lines(logtime, lls) 
plot(time, ls, ylab = "-log(Survival Probability", main = "Exponential") 
lines(time, ls) 
plot(logtime, lnorm, type = "l", main = "Log normal") 
plot(logtime, logls, type = "l", main = "Log Logistic")


fit.weibull <- with(data, survreg(Surv(tenure,censor) ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+
                                                        InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+
                                                        StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+
                                                        MonthlyCharges+TotalCharges, 
                                  dist = 'weibull', scale = 0.4185)) 
fit.weibull <- with(data, survreg(Surv(tenure,censor) ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                                                        InternetService+Contract+PaperlessBilling+PaymentMethod+
                                                        MonthlyCharges+TotalCharges, 
                                  dist = 'weibull', scale = 0.4185)) 
summary(fit.weibull)

## COX ####
# cox 는 factor 안받고 다 numerix이어야 한다.
str(data)
data1  <- data 
for (i in 1:ncol(data)){
data1[,i] <- as.numeric(data[,i])
}
# 비례 모형 가정 검정 + Plot
cox.zph(fit.cox) #Test of Proportinal Hazard model
windows(10,10)
par(mfrow=c(3,3))
plot(survfit(Surv(tenure,censor)~gender,data=data), 
     main = "Kaplan Meier Gender", 
     xlab = "",ylab="Survival Probability",
     fun="cloglog",col=c("blue","red"))
plot(survfit(Surv(tenure,censor)~SeniorCitizen,data=data), main = "Kaplan Meier SeniorCitizen", xlab = "",ylab="Survival Probability",fun="cloglog",col=c("blue","red"))
plot(survfit(Surv(tenure,censor)~Partner,data=data), main = "Kaplan Meier Partner", xlab = "",ylab="Survival Probability",fun="cloglog",col=c("blue","red"))
plot(survfit(Surv(tenure,censor)~Dependents,data=data), main = "Kaplan Meier Dependents", xlab = "",ylab="Survival Probability",fun="cloglog",col=c("blue","red"))
plot(survfit(Surv(tenure,censor)~PhoneService,data=data), main = "Kaplan Meier PhoneService", xlab = "",ylab="Survival Probability",fun="cloglog",col=c("blue","red"))
plot(survfit(Surv(tenure,censor)~InternetService,data=data), main = "Kaplan Meier Internet Service", xlab = "",ylab="Survival Probability",fun="cloglog",col=c("blue","red","orange"))
plot(survfit(Surv(tenure,censor)~Contract,data=data), main = "Kaplan Meier Contract", xlab = "",ylab="Survival Probability",fun="cloglog",col=c("blue","red","orange"))
plot(survfit(Surv(tenure,censor)~PaperlessBilling,data=data), main = "Kaplan Meier Paperless Billing", xlab = "",ylab="Survival Probability",fun="cloglog",col=c("blue","red"))
plot(survfit(Surv(tenure,censor)~PaymentMethod,data=data), main = "Kaplan Meier Payment Method", xlab = "",ylab="Survival Probability",fun="cloglog",col=c("blue","red","orange","green"))


# 적합 
fit.cox <- coxph(Surv(tenure, censor) ~ gender+SeniorCitizen+Partner+Dependents+PhoneService
                 +InternetService+PaperlessBilling+PaymentMethod+MonthlyCharges+Contract,
                 ties = "breslow", data = data1)
summary(fit.cox)
par(mfrow = c(2,2))u
plot(cox.zph(fit.cox)) # 어디에 쓰이는고? 

stepwise <- step(fit.cox)

fit.cox2 <- coxph(Surv(tenure, censor) ~ SeniorCitizen + Partner + Dependents + 
                    PhoneService + InternetService + PaperlessBilling + PaymentMethod + 
                    MonthlyCharges + Contract, ties="breslow", data= data)
summary(fit.cox2)
cox.zph(fit.cox2)

fit.cox3 <- coxph(Surv(tenure, censor) ~ Dependents +PhoneService ,
                  ties="breslow", data= data)
summary(fit.cox3)
stepwise <- step(fit.cox3)
cox.zph(fit.cox3) #Test of Proportinal Hazard model
# 5. SVM ####
# SVM ####
library(e1071)
library(kernlab)
# install.packages("MLmetrics")
library(MLmetrics)
svm.model <- svm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                   InternetService+Contract+PaperlessBilling+PaymentMethod+
                   MonthlyCharges+TotalCharges, data=data, gamma=1, cost=1) 
summary(svm.model) 
addmargins(table(data$Churn, svm.model$fitted))

# Train Test split 
n <- nrow(data)
sub <- sample(1:n, round(0.75*n))
data.1 <- data[sub,]
data.2 <- data[-sub,]

svm.model.1 <- svm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                     InternetService+Contract+PaperlessBilling+PaymentMethod+
                     MonthlyCharges+TotalCharges, data=data.1, gamma=1, cost=1)
summary(svm.model.1) 
addmargins(table(data.1$Churn, svm.model.1$fitted))

svm.predict.2 <- predict(svm.model.1, newdata=data.2)
Accuracy(svm.predict.2, data.2$Churn) 
Precision(svm.predict.2, data.2$Churn)
F1_Score(svm.predict.2, data.2$Churn)
Recall(svm.predict.2, data.2$Churn)


addmargins(table(data.2$Churn, svm.predict.2))


# tuning
p.time <- proc.time()
tune.svm <- tune(svm, Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                   InternetService+Contract+PaperlessBilling+PaymentMethod+
                   MonthlyCharges+TotalCharges,  data=data.1, ranges=list(gamma=c(0.1,1,10),cost=c(0.1,1,10)))
summary(tune.svm)  # best parameters are 0.1 and 10
proc.time()-p.time

# tuned model
svm.model.2 <- svm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                     InternetService+Contract+PaperlessBilling+PaymentMethod+
                     MonthlyCharges+TotalCharges, data=data.1, gamma=0.1, cost=10)
summary(svm.model.2) 
addmargins(table(data.1$Churn, svm.model.2$fitted))

svm.predict.3 <- predict(svm.model.2, newdata=data.2)
addmargins(table(data.2$Churn, svm.predict.3))
Accuracy(svm.predict.3, data.2$Churn) 
Precision(svm.predict.3, data.2$Churn)
F1_Score(svm.predict.3, data.2$Churn)
Recall(svm.predict.3, data.2$Churn)


# class weights
class.wts <- (n/2)/table(data$Churn)
svm.model.3 <- svm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                     InternetService+Contract+PaperlessBilling+PaymentMethod+
                     MonthlyCharges+TotalCharges, data=data, gamma=0.1, cost=10, class.weights=class.wts) 
summary(svm.model.3) 
addmargins(table(data$Churn, svm.model.3$fitted))

# class weight tuning
p.time <- proc.time()
tune.svm2 <- tune(svm, Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                    InternetService+Contract+PaperlessBilling+PaymentMethod+
                    MonthlyCharges+TotalCharges,  data=data.1, ranges=list(gamma=c(0.1,1,10),cost=c(0.1,1,10)), class.weights=class.wts)
summary(tune.svm2)  # best parameters are 0.1 and 10
proc.time()-p.time

# weight final model
svm.model.4 <- svm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                     InternetService+Contract+PaperlessBilling+PaymentMethod+
                     MonthlyCharges+TotalCharges, data=data.1, gamma=1, cost=0.1,class.weights=class.wts)
summary(svm.model.4) 
addmargins(table(data.1$Churn, svm.model.4$fitted))

svm.predict.4 <- predict(svm.model.4, newdata=data.2)
addmargins(table(data.2$Churn, svm.predict.4))
Accuracy(svm.predict.4, data.2$Churn) 
Precision(svm.predict.4, data.2$Churn)
F1_Score(svm.predict.4, data.2$Churn)
Recall(svm.predict.4, data.2$Churn)


# Comparing 3 models
addmargins(table(data.2$Churn, svm.predict.2))
addmargins(table(data.2$Churn, svm.predict.3))
addmargins(table(data.2$Churn, svm.predict.4))



# end

# 6. Tree Model ####
data2 <- data[,-14]
names(data) 

### Data partition
set.seed(12)
V = 2
n =  NROW(data)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
data.train = data2[ii,]
data.test  = data2[-ii,]

### without pruning ####
tree.2 <- rpart(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                  InternetService+Contract+PaperlessBilling+PaymentMethod+
                  MonthlyCharges+TotalCharges, data = data.train, parms = list(split = "information"))
tree.2
par(mar=c(1,1,1,1), xpd = TRUE); plot(tree.2)
text(tree.2, use.n = TRUE)
plot(tree.2, margin = 0.1);text(tree.2, use.n=TRUE, all = T,cex=0.6)

table(data.test$Churn, predict(tree.2, type="class",newdata=data.test))
yhat <- predict(tree.2, type="class",newdata=data.test)
Accuracy(yhat, data.test$Churn) 
Precision(yhat, data.test$Churn)
F1_Score(yhat, data.test$Churn)
Recall(yhat, data.test$Churn)


# ROC curve
### ROC and AUC

library(ROCR)
windows(10,5)
par(mfrow = c(1,2))
pred2 = predict(tree.2, newdata=data.train, type="prob") #prediction
pred = prediction(pred2[,2], data.train$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(tree.2, newdata=data.test, type="prob") #prediction
pred = prediction(pred2[,2], data.test$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

### With Pruning ####

### Grow a tree

fit = rpart(Churn ~., data=data.train, method="class", control = rpart.control(xval=10, cp=0))
fit
summary(fit)
plot(fit);  text(fit)


### Prune the tree

tmp = printcp(fit)
cp.tmp = tmp[which.min(tmp[,"xerror"]),"CP"]
fit.pruned = prune(fit, cp=cp.tmp)
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE, all = T,cex=0.6)


### Prediction

cutoff = 0.5
pred = predict(fit.pruned, newdata=data.test, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(data.test$Churn, yhat, dnn=c("Actual", "Predicted")); ctable #classification table
Accuracy(yhat, as.numeric(data.test$Churn)-1) 
Precision(yhat, as.numeric(data.test$Churn)-1)
F1_Score(yhat, as.numeric(data.test$Churn)-1)
Recall(yhat, as.numeric(data.test$Churn)-1)



### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


### ROC and AUC

library(ROCR)
windows(10,5)
par(mfrow = c(1,2))
pred2 = predict(fit.pruned, newdata=data.train, type="prob") #prediction
pred = prediction(pred2[,2], data.train$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit.pruned, newdata=data.test, type="prob") #prediction
pred = prediction(pred2[,2], data.test$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


### Solve data imbalance with pruning ####
data2 <- data[,-14]
names(data) 

### Data partition

data2$wts <- ifelse(data2$Churn=="1", 0.27, 0.73)

set.seed(12)
V = 2
n =  NROW(data)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
data.train = data2[ii,]
data.test  = data2[-ii,]

### Grow a tree

fit = rpart(Churn ~., data=data.train, method="class", control = rpart.control(xval=10, cp=0),weights=wts)
fit
summary(fit)
plot(fit);  text(fit)


### Prune the tree

tmp = printcp(fit)
cp.tmp = tmp[which.min(tmp[,"xerror"]),"CP"]
fit.pruned = prune(fit, cp=cp.tmp)
par(mfrow=c(1,1))
plot(fit.pruned, margin = 0.1);text(fit.pruned, use.n=TRUE, all = T,cex=0.6)


### Prediction

cutoff = 0.5
pred = predict(fit.pruned, newdata=data.test, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(data.test$Churn, yhat, dnn=c("Actual", "Predicted")); ctable #classification table
Accuracy(yhat, as.numeric(data.test$Churn)-1) 
Precision(yhat, as.numeric(data.test$Churn)-1)
F1_Score(yhat, as.numeric(data.test$Churn)-1)
Recall(yhat, as.numeric(data.test$Churn)-1)

### ROC and AUC

library(ROCR)
windows(10,5)
par(mfrow = c(1,2))
pred2 = predict(fit.pruned, newdata=data.train, type="prob") #prediction
pred = prediction(pred2[,2], data.train$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit.pruned, newdata=data.test, type="prob") #prediction
pred = prediction(pred2[,2], data.test$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC




### bagging using ipred package ####
# data imbalance and partitioning
data2.Churn <- data2[data2$Churn=="Yes",]
data2.noChurn <- data2[data2$Churn=="No",]
data2.balanced <- rbind(data2.Churn, data2.Churn, data2.Churn, data2.Churn, data2.noChurn)
V = 2
n =  NROW(data2.balanced)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
data.train = data2.balanced[ii,]
data.test  = data2.balanced[-ii,]

library(ipred)
bag.data2 <- bagging(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                       InternetService+Contract+PaperlessBilling+PaymentMethod+
                       MonthlyCharges+TotalCharges, data = data.train, coob=T)
bag.data2

### Prune the tree

tmp = printcp(bag.data2)
cp.tmp = tmp[which.min(tmp[,"xerror"]),"CP"]
bag.data2.pruned = prune(bag.data2, cp=cp.tmp)
plot(bag.data2.pruned, margin = 0.1);text(bag.data2.pruned, use.n=TRUE, all = T,cex=0.6)


### Prediction
cutoff = 0.5
pred = predict(bag.data2, newdata=data.test, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(data.test$Churn, yhat, dnn=c("Actual", "Predicted")); ctable #classification table
Accuracy(yhat, as.numeric(data.test$Churn)-1) 
Precision(yhat, as.numeric(data.test$Churn)-1)
F1_Score(yhat, as.numeric(data.test$Churn)-1)
Recall(yhat, as.numeric(data.test$Churn)-1)


### ROC and AUC

library(ROCR)
windows(10,5)
par(mfrow = c(1,2))
pred2 = predict(bag.data2, newdata=data.train, type="prob") #prediction
pred = prediction(pred2[,2], data.train$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(bag.data2, newdata=data.test, type="prob") #prediction
pred = prediction(pred2[,2], data.test$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


### Random Forest ####
data2.Churn <- data2[data2$Churn=="Yes",]
data2.noChurn <- data2[data2$Churn=="No",]
data2.balanced <- rbind(data2.Churn, data2.Churn, data2.Churn, data2.Churn, data2.noChurn)
V = 2
n =  NROW(data2.balanced)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
data.train = data2.balanced[ii,]
data.test  = data2.balanced[-ii,]

library(randomForest)
RF.1 <- randomForest(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+
                       InternetService+Contract+PaperlessBilling+PaymentMethod+
                       MonthlyCharges+TotalCharges, data=data.train, var.importance=T)

### Prediction
cutoff = 0.5
pred = predict(RF.1, newdata=data.test, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(data.test$Churn, yhat, dnn=c("Actual", "Predicted")); ctable #classification table
Accuracy(yhat, as.numeric(data.test$Churn)-1) 
Precision(yhat, as.numeric(data.test$Churn)-1)
F1_Score(yhat, as.numeric(data.test$Churn)-1)
Recall(yhat, as.numeric(data.test$Churn)-1)

i <- order(RF.1$importance,decreasing = T)
RF.1$importance[i,]


### ROC and AUC

library(ROCR)
windows(10,5)
par(mfrow = c(1,2))
pred2 = predict(RF.1, newdata=data.train, type="prob") #prediction
pred = prediction(pred2[,2], data.train$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(RF.1, newdata=data.test, type="prob") #prediction
pred = prediction(pred2[,2], data.test$Churn)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC







## 공변량 0이라는 검정일까? ####
fit7.5_weibull <- with(diabetes, survreg(Surv(time,censor) ~ age + BMI + diag_age + smoking, dist = 'weibull', scale = 0.4185)) 
summary(fit7.5_weibull)




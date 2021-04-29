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
data$censor <- factor(data$censor) #, c("No"=0, "Yes"=1))
data$censor <- revalue(data$censor, c("1"="Yes", "0"="No"))
str(data)
# data$Partner <- revalue(data$Partner, c("No"=0, "Yes"=1))
# data$Dependents <- revalue(data$Dependents, c("No"=0, "Yes"=1))
# data$PhoneService <- revalue(data$PhoneService, c("No"=0, "Yes"=1))
# data$PaperlessBilling <- revalue(data$PaperlessBilling, c("No"=0, "Yes"=1))


summary(data) # no NA
names(data)
summary(data$tenure)
summary(data[data$Churn=="Yes",]) # internet 쓰는 사람 중 fiber optic 이탈 
summary(data[data$Churn=="No",])
par(mfcol = c(1, 2))
# [Fail] pair plot ####
pairs(data[,c(11,12)])
pairs(data[ , c(5,11,12)],
      col = c( "cornflowerblue", "purple")[data$Churn],   # Change color by group
      pch = c(8, 18, 1)[data$Churn],                            # Change points by group
      labels = c("Tenure", "Monthly Charges", "Total Charges"),
      main = "This is an even nicer pairs plot in R")
library(scatterplot3d)
windows(10,5)
scatterplot3d(data$tenure,data$MonthlyCharges,data$TotalCharges,color=c("cornflowerblue","red")[data$Churn],pch=20)
# install.packages("rgl")
library(rgl)
persp3d(data$tenure,data$MonthlyCharges,data$TotalCharge, color=c("cornflowerblue","red")[data$Churn],pch=20)



# 각 변수별 churn 별 막대그림 #####
# Internet Service
par(mfcol = c(1, 2))
barplot(table(data[data$Churn=="No",7]),ylim=c(0,2000),main="Internet Service when not churn",col="#4077ed")
barplot(table(data[data$Churn=="Yes",7]),ylim=c(0,2000),main="Internet Service when churn",col="#4077ed")
# gender
barplot(table(data[data$Churn=="No",1]),ylim=c(0,3000),main="Gender")
barplot(table(data[data$Churn=="Yes",1]),ylim=c(0,3000))
# Senior Citizen
barplot(table(data[data$Churn=="No",2]),ylim=c(0,5000),main="Senior Citizen")
barplot(table(data[data$Churn=="Yes",2]),ylim=c(0,5000),main="Senior Citizen")
#Partner
barplot(table(data[data$Churn=="No",3]),ylim=c(0,3000),main="Partner")
barplot(table(data[data$Churn=="Yes",3]),ylim=c(0,3000))
# Dependents
barplot(table(data[data$Churn=="No",4]),ylim=c(0,5000),main="Dependents")
barplot(table(data[data$Churn=="Yes",4]),ylim=c(0,5000))
# Phone Service
barplot(table(data[data$Churn=="No",6]),ylim=c(0,5000),main="Phone Service")
barplot(table(data[data$Churn=="Yes",6]),ylim=c(0,5000))
# Contract
barplot(table(data[data$Churn=="No",8]),ylim=c(0,2500),main="Contract when not churn",col="#4ecf8a")
barplot(table(data[data$Churn=="Yes",8]),ylim=c(0,2500),main="Contract when churn",col="#4ecf8a")
windows(10,5)
par(mfcol=c(1,3))
hist(data[data$Churn=="Yes"&data$Contract=="Month-to-month",5],xlab="tenure",main="Month-to-month & Churn 's tenure",col="#e8c848")
hist(data[data$Churn=="Yes" & data$Contract=="One year",5],xlab="tenure", main="One year & Churn 's tenure",col="#e8c848")
hist(data[data$Churn=="Yes" & data$Contract=="Two year",5],xlab="tenure", main="Two year & Churn 's tenure",col="#e8c848")
summary(data[data$Churn=="Yes"  &data$Contract=="Month-to-month",5])
count(data[data$Churn=="Yes" & data$Contract=="Month-to-month",5])
# Paperless Billing
windows(10,5)
par(mfcol=c(1,2))
barplot(table(data[data$Churn=="No",9]),ylim=c(0,3000),main="Paperless Billing when not Churn")
barplot(table(data[data$Churn=="Yes",9]),ylim=c(0,3000),main="Paperless Billing when Churn")



attach(data)
# windows(10, 5)
par(mfcol = c(1, 2))
hist(tenure)
names(data)

# 각 변수별 tenure에 hist그림 ####
windows(10,5)
par(mfcol=c(1,2))
hist(data[data$Churn=="Yes",5],nclass=15, xlim=c(0,75),ylim=c(0,1000),col="cornflowerblue")
hist(data[data$Churn=="No",5],nclass=15, xlim=c(0,75),ylim=c(0,1000),col="purple")

hist(data[data$gender=="Female",5])
hist(data[data$gender=="Male",5])       # NO F/M tenure difference
hist(data[data$SeniorCitizen=="Yes",5])
hist(data[data$SeniorCitizen=="No",5])    # NO Senior tenure difference
hist(data[data$Dependents=="Yes",5],nclass=15,xlim=c(0,75),ylim=c(0,1200))
hist(data[data$Dependents=="No",5],nclass=15,xlim=c(0,75),ylim=c(0,1200))       # YES dependents tenure difference
hist(data[data$Partner=="Yes",5],nclass=15,xlim=c(0,75),ylim=c(0,1200))
hist(data[data$Partner=="No",5],nclass=15,xlim=c(0,75),ylim=c(0,1200))       # YES Partner tenure difference

hist(data[data$PhoneService=="Yes",5])
hist(data[data$PhoneService=="No",5])     # NO PhoneService tenure difference
par(mfcol = c(1, 3))
hist(data[data$InternetService=="DSL",5])
hist(data[data$InternetService=="Fiber optic",5])
hist(data[data$InternetService=="No",5])    # NO Internet Service tenure difference
par(mfcol = c(1, 3))
hist(data[data$Contract=="Month-to-month",5],main="Month-to-month",nclass=15,xlim=c(0,75),ylim=c(0,1500))
hist(data[data$Contract=="One year",5],main="One year",nclass=15,xlim=c(0,75),ylim=c(0,1500))
hist(data[data$Contract=="Two year",5],main="Two year",nclass=15,xlim=c(0,75),ylim=c(0,1500))       # YES Contract tenure difference
par(mfcol = c(1, 2))
hist(data[data$PaperlessBilling=="Yes",5],main="Paperless Billing = Yes",nclass=15,xlim=c(0,75),ylim=c(0,800))
hist(data[data$PaperlessBilling=="No",5],main="Paperless Billing = No",nclass=15,xlim=c(0,75),ylim=c(0,800))  # No Paper Billing tenure difference

# numerical value의 hist ####
# Yes difference Monthly Charge respect to Churn 
par(mfcol = c(1, 2))
hist(data[data$Churn=="Yes",11],main="Churn people Monthly Charges",nclass = 10,xlim=c(0,130),ylim=c(0,1000),col="#e8c848")
hist(data[data$Churn=="No",11],main="Not churn people Monthly Charges",nclass=10,xlim=c(0,130),ylim=c(0,1000),col="#e8c848")   
# No difference Total Charge respect to Churn 
hist(data[data$Churn=="Yes",12],main="Churn people Total Charges",nclass = 15,xlim=c(0,10030),ylim=c(0,1300),col="#4a3e0d")
hist(data[data$Churn=="No",12],main="Not churn people Total Charges",nclass = 15,xlim=c(0,10030),ylim=c(0,1300),col="#4a3e0d")
par(mfcol = c(1, 3))
hist(data[data$Contract=="Month-to-month" & data$Churn=="Yes",5],xlab="tenure",main="Month-to-month & Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))
hist(data[data$Contract=="One year"& data$Churn=="Yes",5],xlab="tenure",main="One year & Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))
hist(data[data$Contract=="Two year"& data$Churn=="Yes",5],xlab="tenure",main="Two year & Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference

hist(data[data$Contract=="Month-to-month" & data$Churn=="No",5],xlab="tenure",main="Month-to-month & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))
hist(data[data$Contract=="One year"& data$Churn=="No",5],xlab="tenure",main="One year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))
hist(data[data$Contract=="Two year"& data$Churn=="No",5],xlab="tenure",main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference
# hist(data[data$MonthlyCharges>70,5]) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference(table(data[data$MonthlyCharges>70,5])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference
# hist(data[data$MonthlyCharges>70&data$Churn=="Yes",5]) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference(table(data[data$MonthlyCharges>70,5])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference
# hist(data[data$MonthlyCharges>70&data$Churn=="No",5]) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference(table(data[data$MonthlyCharges>70,5])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference


# # Those who tenure is long
# # barplot(table(data[data$tenure>55 & data$Churn=="No","SeniorCitizen"])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference(table(data[data$MonthlyCharges>70,5])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference
# # barplot(table(data[data$tenure>55 & data$Churn=="Yes","SeniorCitizen"])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference(table(data[data$MonthlyCharges>70,5])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference
# # 오래있었지만 나간사람과 안나간 사람의 차이는?
# summary(data[data$tenure>55 & data$Churn=="Yes",])
# summary(data[data$tenure>55 & data$Churn=="No",])
# summary(data[data$censor!=0 & data$Churn=="No",])
# summary(data[data$censor!=0 & data$Churn=="Yes",])
# barplot(table(data[data$tenure>55 & data$Churn=="No","gender"])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference(table(data[data$MonthlyCharges>70,5])) #& data$Churn=="No",5],main="Two year & No Churn",nclass=15,xlim=c(0,72),ylim=c(0,800))       # YES Contract tenure difference


# numerical variables correlation plot ####
library(corrplot)
numericVariables <- cbind(tenure, TotalCharges, MonthlyCharges)
cor.NV <- cor(numericVariables)
cor.NV
corrplot(cor.NV, method="circle") 

hist(tenure, xlim = c(0.4, 1), ylim = c(0, 1000),
     main = "Employ Evaluation Distribution", xlab = "Evaluation", ylab = "Employ Count", nclass = 15,
     col = c(rep("coral", 2), rep("slategray", 5), rep("coral", 4)))
barplot(table(projectCount), ylim = c(0, 2000),
        main = "Employ ProjectCount Distribution", xlab = "ProjectCount", ylab = "Employ Count",
        col = c("coral", rep("slategray", 3), "coral", "coral"))
hist(avgMonthlyHrs, xlim = c(100, 350), ylim = c(0, 1000),
     main = "Employ avgMonthlyHrs Distribution", xlab = "avgMonthlyHrs", ylab = "Employ Count", nclass = 10,
     col = c(rep("coral", 2), rep("slategray", 4), rep("coral", 4)))


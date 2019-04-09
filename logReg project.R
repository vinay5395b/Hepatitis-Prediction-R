setwd("F:\\Complete Machine Learning & Data Science with R-2019\\Codes\\Resources\\Section 9")

hep.orig<- read.delim("hepatitis.data.txt", header=FALSE, sep=",")

View(hep.orig)


hep.orig<- read.delim("hepatitis.data.txt", header=FALSE, sep="," , na.strings = c("", " ","?", "NA"))

colnames(hep.orig)<- c(
  "Class",
  "Age",
  "Sex",
  "Steroid",
  "Antivirals",
  "Fatigue",
  "Malaise",
  "Anorexia",
  "Liver.big",
  "Liver.firm",
  "Spleen.palpable",
  "Spiders",
  "Ascites",
  "Varices",
  "Bilirubin",
  "Alk.phosphate",
  "SGOT",
  "Albumin",
  "Protime",
  "Histology"
)

str(hep.orig)

sum(is.na(hep.orig))

colnames(hep.orig)[colSums(is.na(hep.orig))>0]

library(mice)

hep.orig$Sex<- is.factor(hep.orig$Sex)
md.pattern(hep.orig, plot=TRUE)

library(plyr)

hep.orig$Sex<-mapvalues(hep.orig$Sex, from = c(1,2), to=c("M", "F"))
str(hep.orig)
hep.orig$Sex<- as.factor(hep.orig$Sex)

library(ggplot2)
library(tabplot)

#converting to factors

hep.orig$Class<-mapvalues(hep.orig$Class, from = c(1,2), to=c(0,1))
hep.orig$Class<-mapvalues(hep.orig$Class, from = c(0,1), to=c("0","1"))
hep.orig$Class<-factor(hep.orig$Class)
str(hep.orig)


library(forcats)

hep.orig$Steroid<-mapvalues(hep.orig$Steroid, from=c(1,2), to=c("no","yes"))
hep.orig$Steroid<-factor(hep.orig$Steroid)
Steroid.new<-fct_explicit_na(hep.orig$Steroid, na_level = "None")
hep.orig$Steroid.new<-Steroid.new

str(hep.orig)

hep.orig$Antivirals<-mapvalues(hep.orig$Antivirals, from=c(1,2), to=c("no","yes"))
hep.orig$Antivirals<-factor(hep.orig$Antivirals)
Antivirals.new<-fct_explicit_na(hep.orig$Antivirals, na_level = "None")
hep.orig$Antivirals.new<-Antivirals.new

hep.orig$Fatigue<-mapvalues(hep.orig$Fatigue, from=c(1,2), to=c("no","yes"))
hep.orig$Fatigue<-factor(hep.orig$Fatigue)
Fatigue.new<-fct_explicit_na(hep.orig$Fatigue, na_level = "None")
hep.orig$Fatigue.new<-Fatigue.new

hep.orig$Malaise<-mapvalues(hep.orig$Malaise, from=c(1,2), to=c("no","yes"))
hep.orig$Malaise<-factor(hep.orig$Malaise)
Malaise.new<-fct_explicit_na(hep.orig$Malaise, na_level = "None")
hep.orig$Malaise.new<-Malaise.new

hep.orig$Anorexia<-mapvalues(hep.orig$Anorexia, from=c(1,2), to=c("no","yes"))
hep.orig$Anorexia<-factor(hep.orig$Anorexia)
Anorexia.new<-fct_explicit_na(hep.orig$Anorexia, na_level = "None")
hep.orig$Anorexia.new<-Anorexia.new

hep.orig$Liver.big<-mapvalues(hep.orig$Liver.big, from=c(1,2), to=c("no","yes"))
hep.orig$Liver.big<-factor(hep.orig$Liver.big)
Liver.big.new<-fct_explicit_na(hep.orig$Liver.big, na_level = "None")
hep.orig$Liver.big.new<-Liver.big.new

hep.orig$Liver.firm<-mapvalues(hep.orig$Liver.firm, from=c(1,2), to=c("no","yes"))
hep.orig$Liver.firm<-factor(hep.orig$Liver.firm)
Liver.firm.new<-fct_explicit_na(hep.orig$Liver.firm, na_level = "None")
hep.orig$Liver.firm.new<-Liver.firm.new

hep.orig$Spleen.palpable<-mapvalues(hep.orig$Spleen.palpable, from=c(1,2), to=c("no","yes"))
hep.orig$Spleen.palpable<-factor(hep.orig$Spleen.palpable)
Spleen.palpable.new<-fct_explicit_na(hep.orig$Spleen.palpable, na_level = "None")
hep.orig$Spleen.palpable.new<-Spleen.palpable.new

hep.orig$Spiders<-mapvalues(hep.orig$Spiders, from=c(1,2), to=c("no","yes"))
hep.orig$Spiders<-factor(hep.orig$Spiders)
Spiders.new<-fct_explicit_na(hep.orig$Spiders, na_level = "None")
hep.orig$Spiders.new<-Spiders.new

hep.orig$Ascites<-mapvalues(hep.orig$Ascites, from=c(1,2), to=c("no","yes"))
hep.orig$Ascites<-factor(hep.orig$Ascites)
Ascites.new<-fct_explicit_na(hep.orig$Ascites, na_level = "None")
hep.orig$Ascites.new<-Ascites.new

hep.orig$Varices<-mapvalues(hep.orig$Varices, from=c(1,2), to=c("no","yes"))
hep.orig$Varices<-factor(hep.orig$Varices)
Varices.new<-fct_explicit_na(hep.orig$Varices, na_level = "None")
hep.orig$Varices.new<-Varices.new

hep.orig$Bilirubin<-mapvalues(hep.orig$Bilirubin, from=c(1,2), to=c("no","yes"))
hep.orig$Bilirubin<-factor(hep.orig$Bilirubin)
Bilirubin.new<-fct_explicit_na(hep.orig$Bilirubin, na_level = "None")
hep.orig$Bilirubin.new<-Bilirubin.new
hep.orig$Bilirubin.new<-mapvalues(hep.orig$Bilirubin.new, from = c("no"), to=c(0))
class(hep.orig$Bilirubin.new)

str(hep.orig)

library(arm)
library(caret)
set.seed(101)

hep.orig$random<- runif(nrow(hep.orig), 0, 1)
training.data<-hep.orig[which(hep.orig$random<=0.8),]
test.data<-hep.orig[which(hep.orig$random>0.8),]
View(training.data)
View(test.data)

hep.model<-bayesglm(Class~Age+Sex+Steroid.new+Antivirals.new+Fatigue.new+Malaise.new+Anorexia.new+Liver.big.new+Liver.firm.new+Spleen.palpable.new+Spiders.new+Ascites.new+Varices.new+Alk.phosphate+SGOT+Albumin+Protime, data=hep.woNA, family = binomial("logit"), maxit=100)  #not working
summary(hep.model)

#final model
hep.model1<-bayesglm(Class~Malaise.new+Spleen.palpable.new+Albumin, data=training.data, family = binomial("logit"), maxit=100, drop.unused.levels = FALSE)
summary(hep.model1)
display(hep.model1)

predict<-predict(hep.model1, type="response")
predict
train_data_dup<-training.data
train_data_dup$predict<-NA
train_data_dup$predict<-predict
View(training.data)

View(train_data_dup)
train_data_dup$predict<-train_data_dup$predict[is.na(train_data_dup$predict)]<-predict
train_data_dup.woNA<-na.omit(train_data_dup)

train_data_dup.woNA$predict.updated<-NA
train_data_dup.woNA$predict.updated<-train_data_dup.woNA$predict.updated[is.na(train_data_dup.woNA$predict.updated)]<-predict
train_data_dup.woNA$predict.updated<-ifelse(train_data_dup.woNA$predict.updated>0.5,1,0)

with(train_data_dup.woNA, table(Class, predict.updated))

#total obs
a<-3+9+2+48

#incorrect obs
b=9+2

#classification error in %
b/a*100

#testing model on test dataset
hep.test<-na.omit(test.data)

test.predict<-predict(hep.model1, hep.test, type="response")
hep.test$predicted<-test.predict
hep.test$prediction.updated<-ifelse(test.predict>0.5,1,0)

with(hep.test, table(Class, prediction.updated))

a<-1+0+3+14
b=0+3
c=b/a*100

##Final accuracy if 83.33% 

#Using ROCR
#ROC curve-Receiver operating characteristic
#used for evaluating performance
#tpr vs fpr
#tpr = sensitivity vs recall

library(ROCR)
ROCRpred<- prediction(test.predict, hep.test$Class)
ROCRperf<- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

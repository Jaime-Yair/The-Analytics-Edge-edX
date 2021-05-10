#Video 4: Logistic Regression in R

quality=read.csv("quality.csv")
str(quality)

table(quality$PoorCare)

install.packages("caTools")

library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

qualityTrain = subset(quality, split == TRUE)
qualityTest  = subset(quality, split == FALSE)

nrow(qualityTrain)
nrow(qualityTest)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family=binomial)
summary(QualityLog)

predictTrain = predict(QualityLog, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)


#Video 5

Model = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)

summary(Model)


table(qualityTrain$PoorCare, predictTrain>0.5)

10/25
70/74

table(qualityTrain$PoorCare, predictTrain>0.7)

8/25
73/74

table(qualityTrain$PoorCare, predictTrain>0.2)
16/25
54/74


#Video 5: Thresholding

install.packages("ROCR")

library(ROCR)

ROCpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCperf = performance(ROCpred, "tpr", "fpr")
plot(ROCperf)
plot(ROCperf, colorize= TRUE)
plot(ROCperf, colorize= TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2,1.7))

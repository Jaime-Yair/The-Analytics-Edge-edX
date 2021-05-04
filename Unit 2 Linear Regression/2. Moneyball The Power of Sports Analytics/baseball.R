# Video 2: Making it to the Playoffs

baseball=read.csv("baseball.csv")

str(baseball)

moneyball=subset(baseball, Year<2002)
str(moneyball)

moneyball$RD=moneyball$RS-moneyball$RA
str(moneyball)

plot(moneyball$RD, moneyball$W)

WinsReg=lm(W~RD, data = moneyball)
summary(WinsReg)

# Video 3: Predicting Runs
str(moneyball)
RunsReg=lm(RS~OBP+SLG+BA,data = moneyball)
summary(RunsReg)

RunsReg=lm(RS~OBP+SLG,data = moneyball)
summary(RunsReg)

RunsReg=lm(RA~OOBP+OSLG,data = moneyball)
summary(RunsReg)


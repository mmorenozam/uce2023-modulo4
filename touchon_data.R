library(readr)
library(dplyr)
url <- "https://raw.githubusercontent.com/jtouchon/Applied-Statistics-with-R/master/RxP.csv"

touchon <- read_csv(url)

temp<-filter(touchon, Ind!=734 &
               Ind!=1024 &
               Ind!=1078 &
               Ind!=1127 &
               Ind!=1284)
temp<-filter(temp, SVL.initial>12)
temp<-filter(temp, Tail.initial<15)
RxP.clean<-temp
rm(temp)
RxP.clean<-RxP.clean %>%
  mutate(Pred = factor(Pred, levels=c('C','NL','L')))

RxP.byTank<-RxP.clean %>%
  group_by(Block,Tank.Unique,Pred,Hatch,Res) %>%
  summarize(Age.DPO = mean(Age.DPO),
            Age.FromEmergence = mean(Age.FromEmergence),
            SVL.initial = mean(SVL.initial),
            Tail.initial = mean(Tail.initial),
            SVL.final = mean(SVL.final),
            Mass.final = mean(Mass.final),
            Resorb.days = mean(Resorb.days))
RxP.byTank<-as.data.frame(RxP.byTank)
write.csv(RxP.byTank, "touchon.csv", row.names = F)

lm1 <- lm(Age.FromEmergence ~ Pred, data = RxP.byTank)
summary(lm1)


#### los modelos del modulo

library(olsrr)
library(car)
library(emmeans)
library(multcomp)

ranas <- read.csv("touchon.csv")
lm2 <- lm(log(Age.FromEmergence) ~ Pred, data = ranas)
ols_test_normality(lm2)
leveneTest(lm2, center = "mean")

lm3 <- lm(log(Age.DPO) ~ Pred, data = ranas)
ols_test_normality(lm3)
leveneTest(lm3, center = "mean")

lm4 <- lm(log(Age.DPO) ~ Pred*Res, data = ranas)
Anova(lm4)
Anova(lm4, white.adjust = T)

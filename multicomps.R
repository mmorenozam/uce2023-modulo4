library(rstatix)
library(ggplot2)
library(ggpubr)
library(car)
library(multcomp)
library(stringr)

ranas <- read.csv("touchon.csv")
lm2 <- lm(log(Age.FromEmergence) ~ Pred, data = ranas)

ph1 <- emmeans(lm2, specs = "Pred", type = "response")
contrast(ph1, specs = "Pred", method = "tukey", adjust = "none")

ranas$Pred <- factor(ranas$Pred, levels = c("C", "NL", "L"))
bxplot <- ggboxplot(ranas, x = "Pred", y = "Age.FromEmergence", color = "Pred")
hsdvals <- emmeans_test(log.Age.FromEmergence ~ Pred, data = ranas, p.adjust.method = "mvt")
hsdvals <- add_significance(hsdvals, 
                                p.col = "p.adj", 
                                output.col = "p.adj.signif",
                                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                symbols = c("***", "**", "*", ".", "ns"))
hsdvals <- hsdvals %>% add_xy_position(x = "Pred")

bxplot + stat_pvalue_manual(hsdvals, y.position = c(120, 130, 140))

gruposvals <- as.data.frame(cld(ph1))
gruposvals$Pred <- factor(gruposvals$Pred, levels = c("C", "NL", "L"))
ggplot(gruposvals,
       aes(x = Pred, y = response, fill = Pred))+
  geom_bar(stat = "identity", show.legend = F, color = "black")+
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), width=0.2)+
  geom_text(aes(label=str_trim(.group), y = response+SE, vjust=-0.5))


ranas$log.Age.DPO <- log(ranas$Age.DPO)
lm4 <- lm(log(Age.DPO) ~ Pred*Res, data = ranas)
ph3 <- emmeans(lm4, specs = "Pred", by = "Res", type = "response")
contrast(ph3, specs = "Pred", by = "Res", adjust = "tukey", method = "tukey")

ranas %>% games_howell_test(log.Age.DPO~Pred*Res)
emmeans_test(log.Age.DPO~Pred, data = ranas, p.adjust.method = "tukey")

ranas %>% welch_anova_test()
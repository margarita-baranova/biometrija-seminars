library(readxl)
cukur<-read_excel("cukurniedres_dati.xlsx")
head(cukur)
summary(cukur)
str(cukur)

#korelācija (nesanāk)
library(qqplotr)
library(cowplot)
p1 <- ggplot(cukur, aes(sample = HarvestMonth)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "Ievākšanas mēnesis")
p2 <- ggplot(cukur, aes(sample = Age)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "vecums")
p3 <- ggplot(cukur, aes(sample = Area)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "platiba")
p4 <- ggplot(cukur, aes(sample = Tonn_Hect)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "tonnas")
plot_grid(p1, p2, p3, p4)

p5 <- ggplot(cukur, aes(sample = HarvestDuration)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "Ievākšanas ilgums")
p6 <- ggplot(cukur, aes(sample = Fibre)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "Šķiedras")
p7 <- ggplot(cukur, aes(sample = Sugar)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "Cukurs")

plot_grid(p5, p6, p7)

library(ltm)
rcor.test(cukur[,8:10])

cor.test(cukur$Tonn_Hect,cukur$Sugar)

#regresijas analīze
modelis <- lm(formula = Tonn_Hect ~  Age + Fibre + Sugar + HarvestMonth, data = cukur)
summary(modelis)

modelis1 <- lm(formula = Tonn_Hect ~  jul_96, data = cukur)
summary(modelis1)







#korelācijas analīze nenormāli dati
library(ltm)
cor.test(cukur$Tonn_Hect,cukur$Fibre, method = "spearman")
cor.test(cukur$Age, cukur$Sugar,method="spearman")
cor.test(cukur$HarvestDuration,cukur$Tonn_Hect, method="spearman")
cor.test(cukur$HarvestDuration,cukur$Age, method="spearman")






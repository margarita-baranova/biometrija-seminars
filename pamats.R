#eksperimenta pēc kaut ko izmainu

# ---- libs ----

library(readxl)
library(qqplotr)
library(cowplot)
library(ltm)
library(tidyverse)
library(tidyverse)
library(qqplotr)
library(readxl)
library(Hmisc)
library(EnvStats)
library(rstatix)
library(pwr)
library(rcompanion)
library(car)
library(agricolae)

# ---- plāns, ko gribam izdarīt ----

#1 - atšķirības starp ziemeļiem un dienvidiem: raža, tonnas, cukura saturs, kā mainās cukura saturs atkarībā no nokrišņiem ziemeļos un dienvidos
#2 - kā augsnes tips ietekmē ražu, tonnas un cukura saturu, šķiedras
#3 - kā augsnes tips kopā ar varietāti ietekmē ražu, tonnas un cukura saturu, šķiedras
#4 - kā raža atšķiras varietātēm, vecumam, 
#5 - kā raža atšķiras no ievākšanas mēneša
# augsne, vecums
# augsne , varietāte

# ---- dati ----

#Saja faila ir informacija par 1005 cukurniedru laukiem.
#Faila kolonnas:
#1: DistrictPosition - lauka novietojums valsts dienvidos vai ziemeļos (Autralija)
#2: SoilName - augsnes tips
#3: Area - lauka lielums hektaros
#4: Variety - cukurniedru variatate
#5: Age - gadu skaits cik niedres ataudzetas pirms novaksanas
#6: HarvestMonth - razas novaksanas menesis
#7: HarvestDuration - razas novaksanas ilgums (dienas)
#8: Tonn_Hect - razas apjoms tonnas uz hektaru
#9: Fibre - skiedru daudzums (procentos)
#10: Sugar - cukuru daudzums (procentos)
#11:- jul_96-dec_97 - nokrisnu daudzums atbilstosaja menesi/gada

cukur<-read_excel("cukurniedres_dati.xlsx")
head(cukur)
summary(cukur)
str(cukur)

#---- datu tabulas pārveidošana ----

cukurs_atlasitie = cukur %>%
  mutate(MonthName = month.name[HarvestMonth]) %>% #pārveidojam mēnešus no skaitļiem uz nosaukumiem
  select(DistrictPosition, SoilName, Area, Variety, Tonn_Hect, Fibre, Sugar, MonthName) #atlasām noteiktas kolonnas, jo ir liekas


#---- normalitātes novērtēšana un vilkoksona testi ----


####cukura saturs un ziemeļi vai dienvidi:

ggplot(cukur, aes(sample = Sugar)) + facet_wrap(~DistrictPosition) +
  geom_qq_band() + stat_qq_line() + stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles") #cukurs izskatās normāli sadalīts, bet tikai N, ne S

ggplot(cukur,aes(DistrictPosition,Sugar))+geom_boxplot() #abām paraugkopām ir ietekmīgās vērtības

#Dati neatbilst normālajam sadalījumam, tādēļ jāizmanto neparametriskās metodes

#Vilkoksona tests divām neatkarīgām paraugkopām:

wilcox.test(cukur$Sugar ~ cukur$DistrictPosition)
#W(1005) = 195053; p<0.0001 #ir statistiski būtiska atšķirība starp paraugkopām

-1/(exp(1)*2.2e-16*log(2.2e-16)) #4.638126e+13 reizes atšķirība

wilcox_effsize(data=cukur,Sugar~DistrictPosition,paired=FALSE,ci=TRUE) #efekta apjoms ir 0.483, kas ir vidējs efekts (gandrīz spēcīgs)


#### šķiedras un ziemeļi/dienvidi:

ggplot(cukur, aes(sample = Fibre)) + facet_wrap(~DistrictPosition) +
  geom_qq_band() + stat_qq_line() + stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles") #nav normāls sadalījums

ggplot(cukur,aes(DistrictPosition,Fibre))+geom_boxplot() #abām paraugkopām ir ietekmīgās vērtības

#Dati neatbilst normālajam sadalījumam, tādēļ jāizmanto neparametriskās metodes

#Vilkoksona tests divām neatkarīgām paraugkopām:

wilcox.test(cukur$Fibre ~ cukur$DistrictPosition)
#W(1005) = 113739; p=0.01405 #ir statistiski būtiska atšķirība starp paraugkopām, bet ne tik būtiska kā cukuram

-1/(exp(1)*0.01405*log(0.01405)) #6.14 reizes atšķirība

wilcox_effsize(data=cukur,Fibre~DistrictPosition,paired=FALSE,ci=TRUE) #efekta apjoms ir 0.076, kas ir ļoti vājš efekts

#### raža (t/ha) un ziemeļi/dienvidi:

ggplot(cukur, aes(sample = Tonn_Hect)) + facet_wrap(~DistrictPosition) +
  geom_qq_band() + stat_qq_line() + stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles") #nav normāls sadalījums

ggplot(cukur,aes(DistrictPosition,Tonn_Hect))+geom_boxplot() #ļoooooti ietekmīgas vērtības augšējā galā

#Dati neatbilst normālajam sadalījumam, tādēļ jāizmanto neparametriskās metodes

#Vilkoksona tests divām neatkarīgām paraugkopām:

wilcox.test(cukur$Tonn_Hect ~ cukur$DistrictPosition)
#W(1005) = 145532; p<0.0001 #ir statistiski būtiska atšķirība starp paraugkopām

-1/(exp(1)*7.126e-06*log(7.126e-06)) #4356 reizes atšķirība

wilcox_effsize(data=cukur,Tonn_Hect~DistrictPosition,paired=FALSE,ci=TRUE) #efekta apjoms ir 0.142, kas ir vājš efekts


#---- Korelācijas ----
#pirms tam paskatīties datu normalitāti:

p1 <- ggplot(cukur, aes(sample = Tonn_Hect)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "tonnas")
p2 <- ggplot(cukur, aes(sample = Fibre)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "Šķiedras")
p3 <- ggplot(cukur, aes(sample = Sugar)) + geom_qq_band() +
  stat_qq_point() + stat_qq_line() + labs(title = "Cukurs")
plot_grid(p1, p2, p3, ncol=3)

#pieturamies pie neparametrikas :)

#neparametriskās korelācijas metodes ir Spīrmana un Kendala, mēs ņemam Spīrmana (jo tad abām pazīmēm jābūt kvantitatīvām)

#Spīrmena korelācija starp vecumu un šķiedru daudzumu:

cor.test(cukur$Age, cukur$Fibre, method="spearman") 
#Nav statistiski būtiskas korelācijas starp cukurniedru augu vecumu un šķiedru daudzumu,
#jo p vertıba ir lielāka par butiskuma lımeni
#(S(1003) = 159843313; p = 0.0804).


#Spīrmena korelācija starp vecumu un šķiedru daudzumu:

cor.test(cukur$Age, cukur$Sugar, method="spearman") 
#Ir statistiski būtiska korelācija starp cukurniedru augu vecumu un cukura daudzumu,
#jo p vertıba ir mazaka par butiskuma lımeni
#(S(1003) = 212937814; p < 0.0001).


#Spīrmena korelācija starp vecumu un šķiedru daudzumu:

cor.test(cukur$Age, cukur$Sugar, method="spearman") 
#Ir statistiski būtiska korelācija starp cukurniedru augu vecumu un cukura daudzumu,
#jo p vertıba ir mazaka par butiskuma lımeni
#(S(1003) = 212937814; p < 0.0001).


#Spīrmena korelācija starp vecumu un ražu:

cor.test(cukur$Age, cukur$Tonn_Hect, method="spearman") 
#Nav statistiski būtiskas korelācijas starp cukurniedru augu vecumu un ražu,
#jo p vertıba ir lielāka par butiskuma lımeni
#(S(1003) = 1.71e+08; p = 0.7337).









# tā kā dati nav normāli sadalīti





#---- Regresijas analīze ----







#---- Modeļu veidošana dažādām mijiedarbībām ----

# lm modeļi




# glm modeļi


# glmm modeļi


# gls modeļi


# daudzfaktoru metodes (PCA, CA, RDA, CCA, NMDS????????)










#korelācija (nesanāk)

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


rcor.test(cukur[,8:10])

cor.test(cukur$Tonn_Hect,cukur$Sugar)

#regresijas analīze
modelis <- lm(formula = Tonn_Hect ~  Age + Fibre + Sugar + HarvestMonth, data = cukur)
summary(modelis)

modelis1 <- lm(formula = Tonn_Hect ~  jul_96, data = cukur)
summary(modelis1)

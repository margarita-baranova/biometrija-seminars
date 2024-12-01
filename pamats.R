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
  select(DistrictPosition, SoilName, Age, Area, Variety, Tonn_Hect, Fibre, Sugar, MonthName) #atlasām noteiktas kolonnas, jo ir liekas


#---- normalitātes novērtēšana un vilkoksona testi ----


####cukura saturs un ziemeļi vai dienvidi:

ggplot(cukur, aes(sample = Sugar)) + facet_wrap(~DistrictPosition) +
  geom_qq_band() + stat_qq_line() + stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles") #cukurs izskatās normāli sadalīts, bet tikai N, ne S

ggplot(cukur,aes(DistrictPosition,Sugar))+geom_boxplot() #abām paraugkopām ir ietekmīgās vērtības

#Dati neatbilst normālajam sadalījumam, tādēļ jāizmanto neparametriskās metodes

#Vilkoksona tests divām neatkarīgām paraugkopām:

cukur_district<-wilcox.test(cukur$Sugar ~ cukur$DistrictPosition)
#W(1005) = 195053; p<0.0001 #ir statistiski būtiska atšķirība starp paraugkopām

-1/(exp(1)*2.2e-16*log(2.2e-16)) #4.638126e+13 reizes atšķirība

wilcox_effsize(data=cukur,Sugar~DistrictPosition,paired=FALSE,ci=TRUE) #efekta apjoms ir 0.483, kas ir vidējs efekts (gandrīz spēcīgs)

sjPlot::tab_model(cukur_district,show.stat=TRUE) # šo mēs neizmantojam, lai atspoguļotu wilcoxon testu, tos mēs aprakstam vnk.

#### šķiedras un ziemeļi/dienvidi:

ggplot(cukur, aes(sample = Fibre)) + facet_wrap(~DistrictPosition) +
  geom_qq_band() + stat_qq_line() + stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles") #nav normāls sadalījums

ggplot(cukur,aes(DistrictPosition,Fibre))+geom_boxplot() #abām paraugkopām ir ietekmīgās vērtības

#Dati neatbilst normālajam sadalījumam, tādēļ jāizmanto neparametriskās metodes

#Vilkoksona tests divām neatkarīgām paraugkopām:

fibre_District<-wilcox.test(cukur$Fibre ~ cukur$DistrictPosition)
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

sjPlot::tab_model(cukur_district, show.stat=TRUE)
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

########################################################
#Spīrmena korelācija starp vecumu un šķiedru daudzumu: #
########################################################


cor.test(cukur$Age, cukur$Fibre, method="spearman") 
#Nav statistiski būtiskas korelācijas starp cukurniedru augu vecumu un šķiedru daudzumu,
#jo p vertıba ir lielāka par butiskuma lımeni
#(S(1003) = 159843313; p = 0.0804).

########################################################
#Spīrmena korelācija starp vecumu un šķiedru daudzumu: #
########################################################


cor.test(cukur$Age, cukur$Sugar, method="spearman") 
#Ir statistiski būtiska korelācija starp cukurniedru augu vecumu un cukura daudzumu,
#jo p vertıba ir mazaka par butiskuma lımeni
#(S(1003) = 212937814; p < 0.0001).

########################################################
#Spīrmena korelācija starp vecumu un šķiedru daudzumu: #
########################################################


cor.test(cukur$Age, cukur$Sugar, method="spearman") 
#Ir statistiski būtiska korelācija starp cukurniedru augu vecumu un cukura daudzumu,
#jo p vertıba ir mazaka par butiskuma lımeni
#(S(1003) = 212937814; p < 0.0001).

############################################
#Spīrmena korelācija starp vecumu un ražu: #
############################################

cor.test(cukur$Age, cukur$Tonn_Hect, method="spearman") 
#Nav statistiski būtiskas korelācijas starp cukurniedru augu vecumu un ražu,
#jo p vertıba ir lielāka par butiskuma lımeni
#(S(1003) = 1.71e+08; p = 0.7337)

#Padomāt vēl par korelācijām....................

#Korelācijas vizualizācija

cukurs_kor = cukur %>%
  select(Age,Tonn_Hect, Fibre, Sugar)



#---- Regresijas analīze ----

modelis_raza1 <- lm(formula = Tonn_Hect ~  Age + MonthName, data = cukurs_atlasitie)
summary(modelis_raza1)

library(lindia)
gg_diagnose(modelis_raza1, ncol = 3)

#slikts modelis, fuuu

modelis_raza2 <- lm(formula = Tonn_Hect ~  Age + HarvestMonth, data = cukur)
summary(modelis)




modelis1 <- lm(formula = Tonn_Hect ~  jul_96, data = cukur)
summary(modelis1)

###############
# GLMM modeļi #
###############

library(ggplot2)
ggplot(cukur, aes(Variety, Tonn_Hect)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90)) #lielai daļai varietāšu mediānas ir OK

ggplot(cukur, aes(SoilName, Tonn_Hect)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90)) #Virgil un Jarra ir visvairāk tonnu, gan jau visvairāk platības

# GLM modelis - binomiālā regresija? dafak? NEVAR IZMANTOT, JO NAV TĀDU KATEGORIJU!!!!!!!!!! (kā fibrinogēns un globulīns, 20 vairāk vai mazāk bla)

library(ggplot2)
library(cowplot)
p1 <- ggplot(cukur, aes(fibrinogen, after_stat(count), fill = ESR)) +
  geom_density(position = "fill") +
  theme(legend.position = "top")
p2 <- ggplot(plasma, aes(globulin, after_stat(count), fill = ESR)) +
  geom_density(position = "fill") +
  theme(legend.position = "top")
plot_grid(p1, p2)

modelis_raza_var <- glm(Tonn_Hect ~ SoilName + Variety, data = cukur, family = poisson)
summary(modelis_raza_var) 

# GLMER vai kas tas tāds

library(lme4)
library(lmerTest)

cukur$fMONTH <- factor(cukur$HarvestMonth)

M1 <- lm(Tonn_Hect ~ SoilName * fMONTH, data = cukur) #ŠITO METAM ĀRĀ, JO NU KAS TAS TĀDS?
summary(M1)


Mlmer1 <- glmer(Sugar ~ Tonn_Hect + (1 | SoilName),data=cukur,family=poisson()) #karoč 
summary(Mlmer1)
Mlmer2 <- glmer(Richness ~ NAP + (1+NAP | fBeach),data=RIKZ,family=poisson()) #jauktos efektus neatdala ar komatu, uzreiz pierakstām formulā, bet liekam iekavās
Mlmer3 <- glmer(Richness ~ NAP + (NAP-1 | fBeach),data=RIKZ,family=poisson())


M.lm = lm(Tonn_Hect ~ SoilName * Variety, data=cukur)
cukur$prognoze <- predict(M.lm)
cukur$atlikums <- resid(M.lm)
ggplot(cukur, aes(prognoze, atlikums)) + geom_point() #izskatās drausmīgi, ir piltuve, krč, drausmas

#---- Modeļu veidošana dažādām mijiedarbībām ----

# lm modeļi

#nevar izmanot, jo nav linearitātes


# glm modeļi
#nevar izmantot, jo nav binomiāla

# glmm modeļi
#var izmantot, bet mēs neprotam :(

# gls modeļi
#laikam nemaz nevaram uztaisīt, nu i dirst!!!!!!!!!!!!!!!!!!!!
#kas tas tāds vispār ir????


# daudzfaktoru metodes (PCA, CA, RDA, CCA, NMDS????????)

cukur_atlasitie_PCA = cukur %>%
  select(Tonn_Hect, Sugar, Fibre)

CUKUR.pca <- prcomp(cukur_atlasitie_PCA, scale. = TRUE)
summary(CUKUR.pca)

library(ggfortify)
autoplot(CUKUR.pca) #TRASH


cukur_atlasitie_PCA2 = cukur %>%
  select(Sugar, Fibre)

CUKUR.pca2 <- prcomp(cukur_atlasitie_PCA2, scale. = TRUE)
summary(CUKUR.pca2)

library(ggfortify)
autoplot(CUKUR.pca2) #IZSKATĀS JAU CERĪGĀK

autoplot(CUKUR.pca2, loadings = TRUE, loadings.label = TRUE, label = TRUE, label.size = 6) #JUST LOOK AT THIS CHONKER!!!!!!!!!! OR DEVIL IDK, YOU CHOOSE

#### RDA vai nez kas tas ir

spe.hel <- decostand(spe, "hellinger")
spe.rda <- rda(spe.hel~., env)
summary(spe.rda)








#######################################
# Ko darīt ar tiem nokrišņu mēnešiem? #
#######################################

cukur_menesi = cukur %>%
  select(Sugar, Fibre, Tonn_Hect, jul_96, aug_96, sep_96, oct_96, nov_96, dec_96, jan_97, feb_97, mar_97, apr_97, mai_97, jun_97, jul_97, aug_97, sep_97, oct_97, nov_97, dec_97)

CUKUR.pca3 <- prcomp(cukur_menesi, scale. = TRUE)
summary(CUKUR.pca3)

autoplot(CUKUR.pca3) #diezgan pašsaprotami, jo ir divi Z un D, tiem atbilstoši nokrišņi

autoplot(CUKUR.pca3, loadings = TRUE, loadings.label = TRUE, label = TRUE, label.size = 6)

library(vegan)
menesi.ca <- cca(cukur_menesi)
menesi.ca

summary(menesi.ca)
library(ggfortify)
autoplot(menesi.ca, geom = "text")

#

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
#eksperimenta pēc kaut ko izmainu

# ---- libs ----

library(readxl)
library(qqplotr)
library(cowplot)
library(ltm)
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

wilcox.test(cukur$Sugar ~ cukur$DistrictPosition)
#W(1005) = 195053; p<0.0001 #ir statistiski būtiska atšķirība starp paraugkopām

wilcox_test_result <- wilcox.test(Sugar ~ DistrictPosition, data = cukur)
print(wilcox_test_result)

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

wilcox_test_result <- wilcox.test(Fibre ~ DistrictPosition, data = cukur)
print(wilcox_test_result)


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

wilcox_test_result <- wilcox.test(Tonn_Hect ~ DistrictPosition, data = cukur)
print(wilcox_test_result)

-1/(exp(1)*7.126e-06*log(7.126e-06)) #4356 reizes atšķirība

wilcox_effsize(data=cukur,Tonn_Hect~DistrictPosition,paired=FALSE,ci=TRUE) #efekta apjoms ir 0.142, kas ir vājš efekts


#Vizualizācija:
ggplot(cukur, aes(x = DistrictPosition, y = Tonn_Hect, fill = DistrictPosition)) +
  geom_boxplot() +
  labs(
    title = "Ražas salīdzinājums starp ziemeļu un dienvidu reģioniem",
    x = "Reģions (N = Ziemeļi, S = Dienvidi)",
    y = "Raža (tonnas/hektārs)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("N" = "lightblue", "S" = "lightgreen"))

# Vizualizācija (Boxplot ar Wilcoxon rezultātiem)
ggplot(cukur, aes(x = DistrictPosition, y = Tonn_Hect)) +
  theme_classic() +
  geom_boxplot(fill = NA) +
  geom_jitter(col = "grey", width = 0.25) +
  stat_n_text(y.pos = max(cukur$Tonn_Hect) * 0.1) + # Grupas N virs diagrammas
  annotate("text", 
           y = max(cukur$Tonn_Hect) * 0.95, 
           x = 1.5, 
           label = expression("W == 145532"), 
           parse = TRUE) + # W vērtība no Wilcoxon testa
  annotate("text", 
           y = max(cukur$Tonn_Hect) * 0.9, 
           x = 1.5, 
           label = "p = 7.126e-06") + # P-vērtība no Wilcoxon testa
  labs(title = "Ražas salīdzinājums starp ziemeļu un dienvidu reģioniem",
       x = "Reģions (N = Ziemeļi, S = Dienvidi)",
       y = "Raža (tonnas/hektārs)")

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

#Jauna tabuliņa:

cukur_atlasitie_kor = cukur %>%
  select(Age, Fibre, Sugar, Tonn_Hect)

library(corrplot)

source("https://www.sthda.com/upload/rquery_cormat.r")
mydata <- mtcars[, c(1,3,4,5,6,7)]
require("corrplot")
rquery.cormat(mydata, method = "spearman")
warnings()
korelacijas <- cukur_atlasitie_kor
rquery.cormat(korelacijas)

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
#(S(1003) = 212937814; p < 0.0001). Korelācijas koeficients rho -0.259 (vāja negatīva korelācija)



########################################################
#Spīrmena korelācija starp vecumu un šķiedru daudzumu: #
########################################################


cor.test(cukur$Age, cukur$Sugar, method="spearman") 
#Ir statistiski būtiska korelācija starp cukurniedru augu vecumu un cukura daudzumu,
#jo p vertıba ir mazaka par butiskuma lımeni
#(S(1003) = 212937814; p < 0.0001). rho = - 0.259 (vāja negatīva korelācija)

############################################
#Spīrmena korelācija starp vecumu un ražu: #
############################################

cor.test(cukur$Age, cukur$Tonn_Hect, method="spearman") 
#Nav statistiski būtiskas korelācijas starp cukurniedru augu vecumu un ražu,
#jo p vertıba ir lielāka par butiskuma lımeni
#(S(1003) = 1.71e+08; p = 0.7337)

#Padomāt vēl par korelācijām....................

#Raža ar fibre:
cor.test(cukur$Tonn_Hect, cukur$Fibre, method = "spearman")
#Ir statistiski būtiska korelācija starp cukurniedru ražu un šķiedru daudzumu,
#jo p vertıba ir mazāka par butiskuma lımeni
#(S(1003) = 189086479; p = 0.000185), rho = - 0.118 (ļoti vāja negatīva korelācija)


#Raža ar cukuru
cor.test(cukur$Tonn_Hect, cukur$Sugar, method = "spearman")
#Ir statistiski būtiska korelācija starp cukurniedru ražu un šķiedru daudzumu,
#jo p vertıba ir mazāka par butiskuma lımeni
#(S(1003) = 147440361; p < 0.0001), rho = 0.128 (ļoti vāja pozitīva korelācija)

#Cukurs ar fibre
cor.test(cukur$Sugar, cukur$Fibre, method = "spearman")
#Ir statistiski būtiska korelācija starp cukurniedru ražu un šķiedru daudzumu,
#jo p vertıba ir mazāka par butiskuma lımeni
#(S(1003) = 191662599; p = 2.372e-05), rho = - 0.133 (ļoti vāja negatīva korelācija)

#Kaut kāda random vizualizācija šim murgam:

korelacijas <- cukur_atlasitie_kor
rquery.cormat(korelacijas)




#---- Regresijas analīze ----

modelis_raza1 <- lm(formula = Tonn_Hect ~  Age + MonthName, data = cukurs_atlasitie)
summary(modelis_raza1)

library(lindia)
gg_diagnose(modelis_raza1, ncol = 3)

#slikts modelis, fuuu

modelis_raza2 <- lm(formula = Tonn_Hect ~  Age + HarvestMonth, data = cukur)
summary(modelis_raza2) #vēl sliktāks modelis, bleurgh




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

ggplot(cukur, aes(SoilName, Sugar)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90)) #Mission visvairāk

ggplot(cukur, aes(SoilName, Fibre)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90)) #visiem ļoti līdzīgi

ggplot(cukur, aes(Variety, Sugar)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90))

ggplot(cukur, aes(Variety, Fibre)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90))

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

#GEMINI uzteiktais modelis:

model <- lm(Tonn_Hect ~ Area + Age + HarvestMonth, data = cukur)
summary(model) #šitais ir modelis ir vislabākais no visiem, ko esam taisījušas

plot(model) #WELL WELL WELL THE RESIDUALS ARE *NOT* NORMALLY DISTRIBUTED


model1 <- lm(Sugar ~ Area + Age + HarvestMonth, data = cukur)
summary(model1) #Izskaidro 7 % no mainības, HarvestMonth nav būtisks


model2 <- lm(Tonn_Hect ~ SoilName + Age, data = cukur)
summary(model2) #SUUUUUDS


model3 <- lm(Tonn_Hect ~ SoilName*Age, data = cukur)
summary(model3) #vēl joprojām sūds


model4 <- lm(Sugar ~ SoilName + DistrictPosition, data = cukur)
summary(model4) #šitais jau ir labs, izskaidro 31 %, mediāna ok, augsnes tipi gandrīz visi ir būtiski

gg_diagnose(model4, ncol = 2) #šitais principā USABLE (c) Annija
plot(model4)

qqPlot(residuals(model4)) #piltuve

model6 <- lm(Sugar ~ SoilName * DistrictPosition, data = cukur)
summary(model6)


model5 <- lm(Fibre ~ SoilName + DistrictPosition, data = cukur)
summary(model5) #nav labs, tikai 2%

model7 <- lm(Sugar ~ SoilName + Variety, data = cukur)
summary(model7)

gg_diagnose(model7, ncol = 2) #ir nedaudz sliktāks par to labo

qqPlot(residuals(model7)) #ir labāk 


model8 <- lm(Fibre ~ SoilName + DistrictPosition, data = cukur)
summary(model8) #būtu ok, ja nebūtu 2 procenti


model9 <- lm(Fibre ~ SoilName + Variety, data = cukur)
summary(model9) #Not the best one

gg_diagnose(model9, ncol = 2) #ai figņa

# mums pagaidām vislabāk sanāk ar tiešajiem efektiem, nevis jauktajiem.

model10<- lm(Tonn_Hect ~ Variety + Age, data=cukur)
summary(model10) #nononononnon

gg_diagnose(model10, ncol = 2) #mēsls

model11<- lm(Sugar ~ Variety + Age, data=cukur)
summary(model11) # nav praktiski nekas būtisks, 21% normāla mediāna

model12<- lm(Sugar ~ Variety, data=cukur)
summary(model12) # nekas butisks tikai intercept

model13<- lm(Fibre ~ Variety + Age, data=cukur)
summary(model13) # nekas labs nav

model14<- lm(Tonn_Hect~ Variety + Age, data=cukur)
summary(model14) #fuflo

model15 <- lm(Tonn_Hect ~ SoilName + Age + Variety, data = cukur)
summary(model15) #nononononono



# GLMER vai kas tas tāds

library(lme4)
library(lmerTest)

cukur$fMONTH <- factor(cukur$HarvestMonth)

M1 <- lm(Tonn_Hect ~ SoilName * fMONTH, data = cukur) #ŠITO METAM ĀRĀ, JO NU KAS TAS TĀDS?
summary(M1)

library(nlme)
mod <- lmList(Sugar ~ Tonn_Hect|DistrictPosition, data = cukur)
summary(mod) #laikam nebija jēgas taisīt !!!!!

cukur$fHarvestMonth <- factor(cukur$HarvestMonth)
Mlme1 <- lme(Sugar ~ Age, random = ~1 | fHarvestMonth, data=cukur)
summary(Mlme1) #es nezinu, ko ar šo darīt, vai tas vispār ir jēdzīgi/////:





Mlmer1 <- glmer(Sugar ~  + (1 | fBeach),data=RIKZ,family=poisson())
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

autoplot(CUKUR.pca2, loadings = TRUE, loadings.label = TRUE, label = TRUE, label.size = 3) #JUST LOOK AT THIS CHONKER!!!!!!!!!! OR DEVIL IDK, YOU CHOOSE


cukur_atlasitie_PCA3 = cukur %>%
  select(Tonn_Hect, Fibre)

CUKUR.pca3 <- prcomp(cukur_atlasitie_PCA3, scale. = TRUE)
summary(CUKUR.pca3)


autoplot(CUKUR.pca3) #nekam neder galīgi

cukur_atlasitie_PCA4 = cukur %>%
  select(Tonn_Hect, Sugar)

CUKUR.pca4 <- prcomp(cukur_atlasitie_PCA4, scale. = TRUE)
summary(CUKUR.pca4)


autoplot(CUKUR.pca4) #nononoonononooooo


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


#################
#Krustkal tests #
#################

library(tidyverse)
 
kruskal.test(Tonn_Hect ~ SoilName, data = cukur)

install.packages("FSA")
library(FSA)
dunn_test(Tonn_Hect ~ SoilName, data = cukur)

#nezinu, kas te notiek un vai kaut kas ir pareizi. Es pareizi saliku mainīgos?
#Šo laikam nemaz nevar izmantot




#Katrinas dok
#es aptuveni mēģināju takā atbildēt uz 
#Vai pastāv statistiski būtiska atšķirība cukurniedru ražā (tonnas uz hektāru) starp ziemeļu un dienvidu reģioniem?

# Nepieciešamās bibliotēkas
library(tidyverse) 
library(readxl)    
library(qqplotr)   
library(rstatix)   

# Datu ielāde
data <- read_excel('cukurniedres_dati.xlsx')
#Normalitātes pārbaude
#Kvantiļu diagrammma
ggplot(data, aes(sample = Tonn_Hect)) +
  facet_wrap(~DistrictPosition) +
  geom_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(
    x = "Teorētiskās kvantiles",
    y = "Paraugkopas kvantiles",
    title = "Kvantiļu diagramma pēc reģioniem"
  ) +
  theme_minimal()
  
# Šapiro-Vilka tests
shapiro_results <- tapply(data$Tonn_Hect, data$DistrictPosition, shapiro.test)
print(shapiro_results)

# Secinājums par Šapiro-Vilka testu:
# Pieņemot, ka p-vērtības abām grupām ir lielākas par 0.05, ražas sadalījums katrā reģionā atbilst normālajam sadalījumam.
#Pēc kvantiļu dieagrammas abos reģionos novērojamas novirzes no normālā sadalījuma, īpaši galējo vērtību (astes) reģionos.
# Vertikālo amplitūdu diagramma (Boxplot)


wilcox_test_result <- wilcox.test(Tonn_Hect ~ DistrictPosition, data = data)
print(wilcox_test_result)
#Secinajums: Pie butiskuma lımeņa α = 0.05 starp Ziemeļu un Dienvidu
#laukumiem cukurniedru ražas apjoma vertıbu sadalıjumiem pastav statistiski
#butiska atšķirıba (W(1003) = 145532, p-vērtība = 7.126e-06).

wilcox_effect <- data %>%
  wilcox_effsize(Tonn_Hect ~ DistrictPosition, paired = FALSE, ci = TRUE)
# Secinājums :
# Efekta lielums ir mazs (0.142). 95% ticamības intervāls efektam ir no 0.07 līdz 0.20, apstiprinot,
#ka atšķirība ir neliela, bet statistiski nozīmīga.

#Secinājums:
#  Kopumā Ziemeļu reģionā vidējā cukurniedru raža ir statistiski būtiski lielāka nekā 
# Dienvidu reģionā. Lai gan atšķirība ir maza (efekta lieluma ziņā), tā ir nozīmīga un varētu būt praktiski svarīga.

# Rezultātu izdruka
print(wilcox_effect)
# Vizualizācija (Boxplot )
ggplot(data, aes(x = DistrictPosition, y = Tonn_Hect, fill = DistrictPosition)) +
  geom_boxplot() +
  labs(
    title = "Ražas salīdzinājums starp ziemeļu un dienvidu reģioniem",
    x = "Reģions (N = Ziemeļi, S = Dienvidi)",
    y = "Raža (tonnas/hektārs)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("N" = "lightblue", "S" = "lightgreen"))

# Vizualizācija (Boxplot ar Wilcoxon rezultātiem)
ggplot(data, aes(x = DistrictPosition, y = Tonn_Hect)) +
  theme_classic() +
  geom_boxplot(fill = NA) +
  geom_jitter(col = "grey", width = 0.25) +
  stat_n_text(y.pos = max(data$Tonn_Hect) * 0.1) + # Grupas N virs diagrammas
  annotate("text", 
           y = max(data$Tonn_Hect) * 0.95, 
           x = 1.5, 
           label = expression("W == 145532"), 
           parse = TRUE) + # W vērtība no Wilcoxon testa
  annotate("text", 
           y = max(data$Tonn_Hect) * 0.9, 
           x = 1.5, 
           label = "p = 7.126e-06") + # P-vērtība no Wilcoxon testa
  labs(
    title = "Ražas salīdzinājums starp ziemeļu un dienvidu reģioniem",
    x = "Reģions (N = Ziemeļi, S = Dienvidi)",
    y = "Raža (tonnas/hektārs)"
  )




library(haven)
library(readxl)
library(dplyr)
library(labelled)
library(gt)
library(expss) #apply_labels
Sys.setlocale("LC_ALL", "en_US.UTF-8")
getwd()
setwd("C:\\Users\\HP\\Documents\\Semetre 6\\Stage\\data_malnutrition\\Donnees_P1_2022\\Stata")

################### Chargement des bases  #############################
#######################################################################
meteo <- read_excel("meteo.xlsx")
menage<- read_dta("HDSS_SURVEYS - Copy.dta")
membre1 <- read_dta("membre.dta")

#library(openxlsx)
#write.xlsx(HD, "C:\\Users\\HP\\Documents\\Semetre 6\\Stage\\data_malnutrition\\Donnees_P1_2022\\Stata\\HD.xlsx")
#write.xlsx(membre1, "C:\\Users\\HP\\Documents\\Semetre 6\\Stage\\data_malnutrition\\Donnees_P1_2022\\Stata\\membre1.xlsx")





################# Nettoyage des donnee################
#######################################################

## 1) Variables de naissance(Mois-Annee)

library(forcats)

membre <- membre1 %>%
  
  mutate(
    
    age=ifelse(age == -9998, 2022, age),
    anneN = ifelse(anneeN == 9999, NA, anneeN),
    age_annee = 2022 - anneN,
    age_annee = ifelse(is.na(age_annee) & age >= 1000 & is.na(anneN),
                            2022 - age, age_annee),
    age_annee = ifelse(is.na(age_annee) & !is.na(age), age, age_annee),
    anneN = 2022-age_annee,
    date_naissance = as.Date(paste(anneN, moisN, "01", sep="-")),
    ageInMonths1 = ifelse(ageInMonths1<0, 0, ageInMonths1),
    age_mois = as.numeric(as.Date("2022-09-01") - date_naissance) / 30.4375,
    age_mois = round(age_mois, 0),
    age_mois = ifelse(age_mois<0, 0, age_mois),
    age_mois = ifelse(is.na(age_mois) & !is.na(ageInMonths1), ageInMonths1, age_mois),
    
    sexe_child = case_when(
      sexe == 1 ~ 0,
      sexe == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    
    sexe_child= factor(sexe_child, c(0,1), labels= c("masculin", "féminin")),
    qn37__1 = factor(qn37__1, c(0,1), labels = c("non","oui")),
    qn37__2 = factor(qn37__2, c(0,1), labels = c("non","oui")),
    qn37__3 = factor(qn37__3, c(0,1), labels = c("non","oui")),
    qn37__4 = factor(qn37__4, c(0,1), labels = c("non","oui")),
    qn37__5 = factor(qn37__5, c(0,1), labels = c("non","oui")),
    qn37__6 = factor(qn37__6, c(0,1), labels = c("non","oui")),
    
    qn31 = case_when(
      qn31 == 1 ~ 1,
      qn31 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    qn32 = case_when(
      qn32 == 1 ~ 1,
      qn32 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    an06 = case_when(
      an06 == 1 ~ 1,
      an06 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    an07 = case_when(
      an07 == 1 ~ 1,
      an07 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    an08 = case_when(
      an08 == 1 ~ 1,
      an08 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    qn29 = case_when(
      qn29 == 1 ~ 1,
      qn29 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    qn30 = case_when(
      qn30 == 1 ~ 1,
      qn30 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    qn31 = factor(qn31, c(0,1), labels = c("non","oui")),
    qn32 = factor(qn32, c(0,1), labels = c("non","oui")),
    an06 = factor(an06, c(0,1), labels = c("non","oui")),
    an07 = factor(an07, c(0,1), labels = c("non","oui")),
    an08 = factor(an08, c(0,1), labels = c("non","oui")),
    qn29 = factor(qn29, c(0,1), labels = c("non","oui")),
    qn30 = factor(qn30, c(0,1), labels = c("non","oui"))
  )%>%
  mutate(
      
    id_menageChild = substr(id_membre, 1, 10),
    qn_height = (qn_height1 + qn_height2)/2,
    qn_weight = (qn_weight1 + qn_weight2)/2,
    qn_heightm = (qn_heightm1 + qn_heightm2)/2,
    qn_weightm = (qn_weightm1 + qn_weightm2)/2
    
  )%>%
   
  mutate(
   
    qnhandicap = replace(qnhandicap, qnhandicap == 99, NA),
    qnhandicap = recode_factor(qnhandicap, `1` = 0, `2` = 1, `3` = 2),
    qnhandicap = factor(qnhandicap, 
                        levels = c(0, 1, 2), 
                        labels = c("physical_handicap", 
                                   "mental_handicap", 
                                   "no_handicap"))
  )%>%
  
  apply_labels(
    sexe_child = "Sexe de l'enfant",
    qn37__1 = "Fievre durant 4sem precedentes",
    qn37__2 = "Maux de tete durant 4sem precedentes",
     qn37__3 = "Douleur musculaire durant 4sem precedentes", 
    qn37__4 = "Toux durant 4sem precedentes",
    
    qn37__5 = "Essouflement durant 4sem precedentes",
    qn37__6 = "Ecoulement durant 4sem precedentes",
    qnhandicap = "Types d'handicap",
    qn31 = "Intervention nutritionelle",
    qn32 = "sous-alimentation",
    an06 = "Fievre durant 4sem precedentes",
    an07 = "Malaria durant 4sem precedentes",
    an08 = "Diarhée durant 4sem précédentes",
    qn29 = "Maladie chronique durant 4sem précédentes",#(4m)
    qn30 = "Maladie aigue durant 4sem précédentes"
  )

 ######### Extraction des caracteristiques des HEAD_MENAGES #############
########################################################################

head_menage <- menage %>%
  mutate(
    Headname = showFirstCHM,
    QS04 = factor(QS04, labels = c("robinet", "pompe/forage", "puits", "eau courante", "pluie", "bouteilles/sacs")),
    water = if_else(QS04 %in% c("robinet", "pompe/forage", "bouteilles/sacs"), 1, 0),
    QS06 = factor(QS06, labels = c("maison", "cours(ferme)", "cours(ciel ouvert)", "public(ferme)", "public(ciel ouvert)", "nature", "autre")),
    toilletes = if_else(QS06 %in% c("maison", "cours(ferme)", "public(ferme)"), 1, 0),
    QS07 = factor(QS07, labels = c("cable electrique", "lampe solaire", "batteries", "torches", "autres")),
    electricity = if_else(QS07 %in% c("cable electrique", "lampe solaire"), 1, 0),
    QS08 = factor(QS08, labels = c("bois de chauffage", "charbon", "gaz", "residus agricoles")),
    cooking = if_else(QS08 == "gaz", 1, 0),
    QM13 = ifelse(QM13 <= 5, 1, 0),
    crowding = factor(QM13, levels = c(1, 0), labels = c("moins de 5 personnes par chambre", "Plus 6 personnes par chambre")),
    QS01 = factor(QS01, labels = c("Tole ou similaire", "Paille/chaume", "Argile", "Bois")),
    QS02 = factor(QS02, labels = c("Beton", "Pierre taillee", "Argile", "Bois", "Autre")),
    toit = if_else(QS01 %in% c("Tole ou similaire"), 1, 0),
    mur = if_else(QS02 %in% c("Beton", "Pierre taillee"), 1, 0),
    milieu = case_when(
      milieu == 1 ~ 1,
      milieu == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    commune = case_when(
      commune == 1 ~ 1,
      commune == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    milieu = factor(milieu, levels = c(0, 1), labels = c("rural", "urbain")),
    commune = factor(commune, levels = c(0, 1), labels = c("Bourasso", "Nouna")),
    animaux = ifelse(menage$animaux == 1, 1, 0),
    animaux = factor(animaux, levels = c(0, 1), labels = c("non", "oui")),
    station = case_when(
      vill %in% c(1, 3, 4, 6, 7, 13, 20, 22, 24, 26, 35, 45, 55) ~ "Biron Marka",
      vill %in% c(2, 5, 10, 11, 12, 16, 17, 18, 19, 28, 33, 40, 41) ~ "Toni",
      vill %in% c(8, 14, 15, 39, 48, 56, 57, 58) ~ "Dionkongo",
      vill %in% c(9, 21, 27, 29, 30, 23, 31, 34) ~ "Koro",
      vill %in% c(42, 43, 49, 51, 53, 54, 36, 37, 44, 46, 47, 52, 59) ~ "Nouna",
      TRUE ~ NA_character_
    ),
    #station = as.factor(station),
    vill = as_factor(vill)
  ) %>%
  apply_labels(
    QM14 = "Nombre d'enfants <5ans",
    QS06 = "Types toilletes",
    QS04 = "Source d'eau",
    QM12 = "Taille du menage",
    QM11 = "Taille de la famille",
    QM13 = "Taille d'une chambre(indiv)",
    QM14 = "Nombre d'enfant <5ans",
    QM15 = "Nombre d'enfants scolarises",
    QS07 = "Source d'electricite",
    QS11__1 = "velo",
    QS11__2 = "moto",
    QS11__3 = "voiture",
    QS11__4 = "machine a  semer",
    QS11__5 = "lecteur CD",
    QS11__6 = "telephone",
    QS11__7 = "lecteur DVD",
    QS11__8 = "refrigerateur",
    QS11__9 = "cuisiniere",
    QS11__10 = "radio",
    QS11__11 = "tv",
    QS11__12 = "generateur",
    QS11__13 = "ordinateur",
    QS11__14 = "internet_wifi",
    milieu = "Milieu de résidence",
    commune = "Commune de résidence",
    station = "Station météorologique",
    vill = "Village"
  )%>%
  dplyr::select(
    idMenage, idConcession,location__Latitude,location__Longitude,location__Altitude,
    gps__Latitude, gps__Longitude, gps__Accuracy, gps__Altitude,
    milieu, vill,station, commune, QM11, QM12, QM13, crowding, QM14, QM15,
    QS01, toit, QS02, mur, qr01, QS04, water, QS06, toilletes, QS07, electricity, QS08, cooking,
    QS11__1, QS11__2, QS11__3, QS11__4, QS11__5, QS11__6, QS11__7, QS11__8, QS11__9, QS11__10,
    QS11__11, QS11__12, QS11__13, QS11__14, QA05, animaux, QS25, QS26, QS27, QS28,
    QS29, QS30, QS31, QS32, QS33
  )


####### Data managment- HEAD_MENAGE ####################

## Variables binaires

 variables_factor <- c(
   "QS11__1", "QS11__2", 
   "QS11__3", "QS11__4", "QS11__5", "QS11__6",
   "QS11__7", "QS11__8", "QS11__9", "QS11__10", 
   "QS11__11", "QS11__12", "QS11__13", "QS11__14"
 )

 #Boucle pour convertir chaque variable en facteur
 for (var in variables_factor) {
   head_menage[[var]] <- to_factor(head_menage[[var]])
 }
# Les variables continues

str(head_menage$QS11__1)
variables_to_numerize <- c("QS25", "QS26", "QS27", "QS28", "QS29", 
                           "QS30", "QS31", "QS32", "QS33", "QA05")

# Boucle pour traiter chaque variable
for (var in variables_to_numerize) {
  head_menage[[var]][head_menage[[var]] == 99] <- NA  # Remplacer 99 par NA
  head_menage[[var]] <- as.numeric(head_menage[[var]])  # Convertir en numC)rique
}


############### Creation de wealth index for menage ##############
#####################################################################
library(psych)
library(FactoMineR)
library(factoextra)


# Selection des variables pour l'ACP

acp_data <- head_menage %>%
  dplyr::select(water, toit, mur, electricity, cooking, animaux, crowding,toilletes,
         QS11__1, QS11__2, QS11__3, QS11__4, QS11__5, QS11__6, QS11__7, QS11__8, QS11__9, QS11__10, 
         QS11__11, QS11__12, QS11__13, QS11__14)%>%
   mutate(across(everything(), as.numeric))

acm_data <- head_menage%>%
  dplyr::select(water, toit, mur, electricity, cooking, animaux, crowding,toilletes,
                QS11__1, QS11__2, QS11__3, QS11__4, QS11__5, QS11__6, QS11__7, QS11__8, QS11__9, QS11__10, 
                QS11__11, QS11__12, QS11__13, QS11__14
    
  )

library(missMDA)
# Imputation des valeurs manquantes
imputed_data <- imputePCA(acp_data)$completeObs

# Réaliser l'ACP sur les données imputées
pca_result1 <- PCA(imputed_data, graph = TRUE, scale.unit = TRUE)


pca_result1 <- PCA(acp_data, graph = TRUE, scale.unit = TRUE)
# Calculer la matrice de corrélation
correlation_matrix <- cor(meteo23)
variance <- fviz_eig(pca_result1, 
                     addlabels = TRUE,  # Ajouter les étiquettes
                     label = "percent",  # Afficher le pourcentage
                     ylim = c(0, 100)) +  # Limiter l'axe y entre 0 et 100
  labs(title = "Pourcentage de Variance Expliquée par Chaque Axe",
       x = "Axes",
       y = "Pourcentage de Variance (%)") +
  theme_minimal()
pca_result <- principal(acp_data, nfactors = 5, rotate = "varimax",
                        covar=T, scores=TRUE, missing = TRUE)
scree(pca_result)
acm_result <- MCA(acm_data, ncp=5)

# Extraire les scores de la 1ere composante principale
wealth_index_acp <- (pca_result$scores[,1])
wealth_index_acp1 <- (pca_result$scores[,1] + pca_result$scores[,2])/2

#wealth_index <- pmax(pca_result$scores[,1], pca_result$scores[,2], pca_result$scores[,3],na.rm = TRUE)

#wealth_index <- pmax(acm_result$ind$coord[,1], acm_result$ind$coord[,2], acm_result$ind$coord[,3],na.rm = TRUE)

# pmax(acm_result$var$coord[,1], acm_result$var$coord[,2], acm_result$var$coord[,3],na.rm = TRUE)

wealth_index_acm <- acm_result$ind$coord[,1]


#table((cut(wealth_index1, breaks = 5)))
library(tidyverse)
# Ajouter l'indice de richesse au jeu de donn??es

nlab <- c("plus pauvre", "pauvre", "moyen", "riche", "plus riche")

head_menage <- head_menage %>%
  mutate(
    wealth_index_acp = factor(cut(as.numeric(wealth_index_acp), breaks = 5, labels = nlab)),
    wealth_index_acp1 = factor(cut(as.numeric(wealth_index_acp1), breaks = 5, labels = nlab)),
    wealth_index_acm = factor(cut(as.numeric(wealth_index_acm), breaks = 5, labels = nlab))
  ) %>%
  apply_labels(
    wealth_index_acm = "Indice de richesse ACM",
    wealth_index_acp = "Indice de richesse"
  ) %>%
  dplyr::select(
    idMenage, idConcession,location__Latitude,location__Longitude,location__Altitude,
    gps__Latitude, gps__Longitude, gps__Accuracy, gps__Altitude,
    milieu, vill, station, commune, wealth_index_acp,wealth_index_acp1, wealth_index_acm
  )



#table(head_menage$wealth_index_acm)
#table(head_menage$wealth_index_acp1)






######### Extraction des caractC)ristiques-MERES #############
#############################################################



meres <- membre %>%
  filter(sexe == 2 ) %>%

  mutate(id_menageMere = substr(id_membre, 1, 10),
         
         niveauEducation = case_when(
           niveauEducation == 1 ~ 0,
           niveauEducation == 2 ~ 1,
           niveauEducation == 3 ~ 2,
           niveauEducation == 4 ~ 3,
           TRUE ~ NA_real_
         ),
          
          niveauEducation = factor(niveauEducation, c(0,1,2,3),
                                   labels =c("sans niveau","alphabétisé",
                                             "primaire","secondaire")),
         
         occupation = case_when(
           occupation == 1 ~ 0,
           occupation == 2 ~ 1,
           occupation == 3 ~ 1,
           occupation == 4 ~ 2,
           occupation == 5 ~ 3,
           TRUE ~ NA_real_
         ),
         
          occupation = factor(occupation, c(0,1,2,3),
                              labels=c("ménagère","agriculteur/éleveur",
                                       "commerçante","autre"))
    )%>%
  dplyr::select(id_menageMere,membre__id, id_membre, age,nom, 
         membres, niveauEducation, occupation )%>%
  rename(
    ageM = age,
    nomM = nom, 
    prenomM = membres
  )%>%
  
  apply_labels(
    niveauEducation = "Niveau d'études",
    occupation ="Profession"
  )


######### Extraction des caracteristiques-CHILDS #############
##############################################################


enfants5 <- membre %>% filter(age_annee <= 5 & age_mois < 60) %>%
  mutate(
    
    qn_blength = ifelse(qn_blength>150, NA, qn_blength),
    qn_bweight = ifelse(qn_bweight==9999, NA, qn_bweight),
    qn_bweight = qn_bweight/1000,
    qn_heightm = qn_heightm/100
  )%>%
  
  dplyr::select(id_membre, membre__id,mere,id_menageChild, nom, membres, 
         age_annee, age_mois,sexe, sexe_child,
         
         an06, an07,an08,qn29,
         qn37__2,qn37__3, qn37__4, qn37__5, qn37__6,
         qnhandicap,qn30,qn31, qn32,
         
         qn_blength, qn_bweight, qn_height,qn_weight,#Mes anthro
         qn_heightm, qn_weightm
         )


dim(enfants5) # 2284 enfants de moins de 5 ans



#####################################################################
######### JOINTURE DES DONNEES: CHILDS-MERES-HEADMENAGE #############
#####################################################################


###            Joindre CHILD_-MERE                       ###
############################################################

## Conditions : 

#1)Verifier que CHILD et MERE sont du meme menage("id_menageChild" = "id_menageMere")
#2)Verifier que id_mere de CHILD correspond au id_membre de sa mere apres 1).


mere_enfants <- enfants5 %>%
  inner_join(meres, by = c("mere" = "membre__id", "id_menageChild" = "id_menageMere"))%>%
  rename(
    id_mere = mere,        # Renommer mere en id_mere
    id_menage_mere = id_membre.y,   # Renommer id_membre.y en id_mere_mere
    id_menage = id_menageChild,     # Renommer id_menageChild en id_menage
    id_menage_child = id_membre.x,
    id_membre_enfant = membre__id
  )


dim(mere_enfants) #2197 meres-enfants







###            Joindre CHILD-MERE-HEAD                   ###
############################################################


## Conditions : 

#1)Verifier que CHILD est gere par HEADMENAGE("id_menage" = "idMenage")


HDSS_final_0 <- mere_enfants %>%
  inner_join(head_menage, by = c("id_menage" = "idMenage")) %>%
  mutate(IMC = qn_weightm / qn_heightm^2,
         IMC_Categorie = case_when(
           IMC < 18.5 ~ 0,  
           IMC >= 18.5 & IMC < 25 ~ 1,  
           IMC >= 25 & IMC < 30 ~ 2, 
           IMC >= 30 ~ 3,  
           TRUE ~ NA_real_  
         )) %>%
  mutate(IMC_Categorie = factor(IMC_Categorie, 
                                levels = c(0, 1, 2, 3), 
                                labels = c("<18,5 (insuffisance pondérale)", 
                                           "[18,5 ; 25[ (poids normal)", 
                                           "[25 ; 30[ (surpoids)", 
                                           ">=30 (obésité)"))) %>%
  apply_labels(
    IMC_Categorie = "Indice de masse corporelle"
  )

dim(HDSS_final_0) # 1999




#####    Joindre CHILD-MERE-HEAD-meteo           #########
##########################################################



meteo1 <- meteo %>%
  rename(
    
    TempMoyenne = Temperature_Avg,
    TempMinimale = Temperature_Min,
    TempMaximale = Temperature_Max,
    PrecTotale = Precipitation_Sum,
    RadGlobaleMax = Global_Radiation,
    HumRelMoyenne = Relative_Humidity_avg,
    HumRelMinimale = Relative_Humidity_Min,
    HumRelMaximale = Relative_Humidity_Max,
    VitVentMoyenne = Wind_Speed_Avg,
    VitVentMaximale = Wind_Speed_Max
  ) %>%
  
  apply_labels(
    
    TempMoyenne = "Température moyenne",
    TempMinimale = "Température minimale ",
    TempMaximale = "Temp Maximale",
    PrecTotale = "Precipitations totales (mm)",
    RadGlobaleMax = "Radiation Globale Maximale (W/mB2)",
    HumRelMoyenne = "Humidité moyenne",
    HumRelMinimale = "Humidité minimale",
    HumRelMaximale = "Humidité maximale",
    VitVentMoyenne = "Vitesse du Vent Moyenne (km/h)",
    VitVentMaximale = "Vitesse du Vent Maximale (km/h)"
  )
HDSS_final_0$station <- as.character((HDSS_final_0$station))
meteo1$station_meteo <- as.character(meteo1$station_meteo) 

# Joindre les donnC)es climatiques a la base

HDSS_final <- HDSS_final_0%>%
  inner_join(meteo1, by=c("station" = "station_meteo"))%>%
  mutate(station = as.factor(station)
         )


###           Calcul des indicateurs anthropopmetriques           ###
#####################################################################



############  Calcul des zscore ###################
###################################################
library(anthro)

zscore <- anthro_zscores(
  HDSS_final$sexe,
  HDSS_final$age_mois,
  is_age_in_month= TRUE,
  HDSS_final$qn_weight,
  HDSS_final$qn_height
)


zscore <- zscore %>%
  mutate(
    stunting = ifelse(zlen < (-2), 1, 0),
    wasting = ifelse(zwei < (-2), 1, 0),
    underweight = ifelse(zwfl < (-2), 1, 0),
    stunting = factor(stunting, c(0,1), labels =c("non","oui")),
    wasting = factor(wasting, c(0,1), labels =c("non","oui")),
    underweight = factor(underweight, c(0,1), labels =c("non","oui")),
    var_malnu = case_when (
      is.na(stunting)~ NA_real_,
      wasting == "non" & underweight == "non" & stunting == "non" ~ 0,
      (wasting == "oui" & underweight == "non" & stunting == "non") |
        (wasting == "non" & underweight == "oui" & stunting == "non") |
        (wasting == "non" & underweight == "non" & stunting == "oui") ~ 1,
      TRUE ~ 2
    ),
  var_malnu = factor(var_malnu, levels=c(0,1,2),labels =c("Normal","Moyen", "Sévère") ,ordered = T)
  )%>%
  apply_labels(
    stunting ="Rétard de croissance",
    wasting = "Emaciation",
    underweight = "Insuffisance pondérale",
    var_malnu = "Etat nutritionnel"
  )%>%
  dplyr::select(zlen,stunting, zwei, wasting, zwfl,underweight,var_malnu)


HDSS_data <- cbind(HDSS_final, zscore)


library(FactoMineR)
library(factoextra)

library(dplyr)

# Sélectionner les variables climatiques
meteo23 <- HDSS_data %>%
  select(TempMoyenne,TempMaximale,TempMinimale, PrecTotale,
         HumRelMoyenne,HumRelMinimale,HumRelMaximale, VitVentMoyenne,
         VitVentMaximale)
pca_result <- PCA(meteo23, graph = TRUE, scale.unit = TRUE)
# Calculer la matrice de corrélation
correlation_matrix <- cor(meteo23)
variance <- fviz_eig(pca_result, 
         addlabels = TRUE,  # Ajouter les étiquettes
         label = "percent",  # Afficher le pourcentage
         ylim = c(0, 100)) +  # Limiter l'axe y entre 0 et 100
  labs(title = "Pourcentage de Variance Expliquée par Chaque Axe",
       x = "Axes",
       y = "Pourcentage de Variance (%)") +
  theme_minimal()

ggsave("varriance_index.png", plot = variance, width = 8, height = 6)

variance <- fviz_eig(pca_result, 
         addlabels = TRUE,  # Ajouter les étiquettes
         label = "percent",  # Afficher le pourcentage
         ylim = c(0, 100)) +  # Limiter l'axe y entre 0 et 100
  labs(title = "Pourcentage de Variance Expliquée par Chaque Axe",
       x = "Axes",
       y = "Pourcentage de Variance (%)") +
  theme_minimal()

val_propres <- fviz_pca_var(pca_result, col.var = "contrib", axes = c(1, 2), 
             gradient.cols = c("blue", "green", "red"), 
             repel = TRUE)  # repel = TRUE pour éviter le chevauchement des étiquettes

fviz_contrib(pca_result, choice = "var", axes = 1, top = 10) +
  ggtitle("Contribution des Variables à l'Axe 1") +
  theme_minimal()
#ggsave("repart_malnu.png", plot = repart_malnu, width = 12, height = 12, dpi = 500, limitsize = FALSE)


corr <- fviz_pca_var(
  pca_result,
  col.var = "contrib", # couleur selon contribution
  gradient.cols = c("blue", "yellow", "red"),
  repel = TRUE
) + ggtitle("Cercle des corrélations")

ggsave("correlation.png", plot = corr, width = 12, height = 12, dpi = 500, limitsize = FALSE)


kmeans_result <- kmeans(coordr, centers = k_optimal, nstart = 25)
Cluster_indi <-fviz_cluster(
  kmeans_result, data = coordr,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal()
) + ggtitle("Projetion des individus dans l’espace des axes principaux selon les clusters")

ggsave("Cluster_indi.png", plot = Cluster_indi, width = 12, height = 12, dpi = 500, limitsize = FALSE)

# Extraire les contributions et les corrélations pour l'axe 1
contributions <- get_pca_var(pca_result)
contributions_df <- data.frame(
  Variable = rownames(contributions$coord),
  Contribution = contributions$contrib[, 1],  # Contribution à l'axe 1
  Correlation = contributions$coord[, 1]        # Corrélation avec l'axe 1
)

# Calculer la moyenne des contributions
contribution_mean <- mean(contributions_df$Contribution)

# Créer le graphique à barres horizontal en ordre ascendant
contrib1 <-ggplot(contributions_df, aes(x = reorder(Variable, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Contribution, 2), "%")), hjust = -0.1, nudge_x = 0.1) +  # Contribution avec %
  geom_text(aes(label = paste0("r = ", round(Correlation, 2))), hjust = -0.3, nudge_x = 0.3) +  # Corrélation formatée
  coord_flip() +
  labs(title = "Contribution des Variables à l'Axe 1",
       x = "Variables",
       y = "Contribution") +
  geom_hline(yintercept = contribution_mean, linetype = "dashed", color = "red", size = 1) +  # Ligne pour la moyenne
  annotate("text", x = 0.5, y = contribution_mean + 0.05, 
           label = "Contribution moyenne", color = "red", hjust = 0) +  # Annotation de la moyenne
  theme_minimal()

ggsave("Contrib_axe1.png", plot = contrib1, width = 8, height = 6)


####### Axe 2 

# Extraire les contributions et les corrélations pour l'axe 2
contributions <- get_pca_var(pca_result)
contributions_df <- data.frame(
  Variable = rownames(contributions$coord),
  Contribution = contributions$contrib[, 2],  # Contribution à l'axe 2
  Correlation = contributions$coord[, 2]        # Corrélation avec l'axe 2
)

# Calculer la moyenne des contributions
contribution_mean <- mean(contributions_df$Contribution)

# Créer le graphique à barres horizontal en ordre ascendant
contrib2 <- ggplot(contributions_df, aes(x = reorder(Variable, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Réduire l'épaisseur des barres
  geom_text(aes(label = paste0(round(Contribution, 2), "%")), hjust = -0.1, nudge_x = 0.1) +  # Contribution avec %
  geom_text(aes(label = paste0("r = ", round(Correlation, 2))), hjust = -0.3, nudge_x = 0.3) +  # Corrélation formatée
  coord_flip() +
  labs(title = "Contribution des Variables à l'Axe 2",
       x = "Variables",
       y = "Contribution") +
  geom_hline(yintercept = contribution_mean, linetype = "dashed", color = "red", size = 1) +  # Ligne pour la moyenne
  annotate("text", x = 0.5, y = contribution_mean + 0.05, 
           label = "Contribution moyenne", color = "red", hjust = 0) + # Annotation de la moyenne
  theme_minimal()

ggsave("Contrib_axe2.png", plot = contrib2, width = 8, height = 6)

coordr <- pca_result$ind$coord
fviz_nbclust(var$coord, kmeans, method = "wss")


var <- get_pca_ind(pca_result)
groupe <- kmeans(var$coord, centers = 3, nstart = 25)
group <- as.factor(groupe$cluster)
cluster <- fviz_pca_var(pca_result, col.var = group,
             legend.title = "Cluster")
gp <-  as.factor(kmeans_result$cluster) 

fviz_pca_ind(pca_result, col.ind = HDSS_data$station)
ggsave("cluster.png", plot = cluster, width = 8, height = 6)

# Extraire les coordonnées des individus
coordr <- pca_result$ind$coord

# Déterminer le nombre optimal de clusters
fviz_nbclust(coordr, kmeans, method = "silhouette") +
  labs(title = "Nombre Optimal de Clusters via la Méthode de Silhouette")


# Effectuer le clustering K-means
set.seed(123)  # Pour la reproductibilité
k_optimal <- 3  # Remplacez par le nombre optimal trouvé
kmeans_result <- kmeans(coordr, centers = k_optimal, nstart = 25)
Cluster <- as.factor(kmeans_result$cluster)
table(Cluster)
# Ajouter les clusters au jeu de données
meteo23$Cluster <- as.factor(kmeans_result$cluster)

correlation <- fviz_pca_var(pca_result, col.var = Cluster,
                        legend.title = "Cluster")

# Visualiser les clusters
fviz_cluster(kmeans_result, data = coordr, 
             geom = "point", 
             stand = FALSE, 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal())


# Visualiser les clusters
fviz_cluster(kmeans_result, data = coordr, 
             geom = "point", 
             stand = FALSE, 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal())

# Résumé des clusters
cluster_summary <- meteo23 %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean))  # Calculer la moyenne des variables pour chaque cluster

print(cluster_summary)
meteo23 <-cbind(meteo23, Cluster)
table(meteo23$Cluster, HDSS_data$var_malnu)
table(HDSS_data$station)
head(meteo23)
HDSS_data <- cbind(HDSS_data, Cluster)

levels(Cluster)
rr<- Cluster
table(rr)
levels(rr) <- c("3","1","2")

library(dplyr)



HDSS_data <- HDSS_data %>%
  mutate(
    Cluster_new = case_when(
      Cluster == 3 ~ 1,
      Cluster == 1 ~ 2,
      Cluster == 2 ~ 3,
      TRUE ~ NA_real_
    ),
    Cluster_new = factor(Cluster_new, levels = c(1, 2, 3))
  )

library(gtsummary)
vars_climatique <- c("TempMoyenne","TempMaximale","TempMinimale", "PrecTotale",
                      "HumRelMoyenne","HumRelMinimale","HumRelMaximale", "VitVentMoyenne",
                      "VitVentMaximale")
biv_env <- HDSS_data %>%
  tbl_summary(
    include = (all_of(vars_climatiques)),
    type = all_categorical() ~ "continuous",
    statistic = list(all_continuous() ~ "{mean}"),
    by = Cluster
  ) %>%
  add_p() %>%  # Utiliser le test chi-carrC)
  bold_labels() %>%
  modify_spanning_header(all_stat_cols() ~ "**Clusters climatiques**")%>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(label = "**Variables environnementales**")

cluster_clim_kbl <- biv_env
save(cluster_clim_kbl, file = "cluster_clim_kbl.RData")
 

######################## Individus ################################"

ind <- get_pca_ind(pca_result)

fviz_pca_ind(pca_result)

kmeans_result
library(cluster)
clusplot(meteo23, kmeans_result$cluster, color = T, shade = F,
         labels = 0, lines= 0)



# Passer les données au format long pour ggplot2
library(ggplot2)
library(reshape2)
install.packages("reshape2")
library(gtsummary)
library(dplyr)

################# Imputation multiple ##############
####################################################

library(MASS)  
library(naniar)
library(dplyr)
library(mice)
library(VIM)
library(modelsummary)

caract_child = c("sexe_child","an06","an07","an08","qn29","qn30","qn31", "qn32",
                 "qn37__2","qn37__3","qn37__4","qn37__5","qn37__6")

caract_mere =c("niveauEducation","occupation", "IMC_Categorie")

caract_menage = c("milieu","commune","wealth_index_acp","wealth_index_acm","station")

indice_meteo = c("indice_meteo1","indice_meteo2")

caract_env <- c("TempMoyenne","PrecTotale","HumRelMoyenne","TempMinimale",
                "TempMaximale", "HumRelMinimale","HumRelMaximale")

indic_anthro = c("stunting","wasting","underweight","var_malnu")

HD <- HDSS_data[, c(caract_child, indic_anthro, caract_env,
                    caract_menage, caract_mere)]

gg_miss_var(HD, show_pct = TRUE) 
vis_miss(HD[, -ncol(HDSS_data)])
pct_miss_case(HD)


require(mice)
pred_mat <- matrix(rep(1), nrow = ncol(HD), ncol = ncol(HD))
names <- colnames(HD)
rownames(pred_mat) <- names
colnames(pred_mat) <- names
diag(pred_mat) <- 0
pred_mat[c(caract_child, indic_anthro, caract_env, caract_menage, caract_mere),] <- 0 # selection des variables du modC(le

meth <- c(
  sexe_child = "logreg",          # Imputation par mode prC)dit
  an06 = "logreg",               # Imputation normale
  an07 = "logreg",
  an08 = "logreg",
  qn29 = "logreg",            # RC)gression logistique
  qn30 = "logreg",
  qn31 = "logreg",
  qn32 = "logreg",
  qn37__2 = "logreg",
  qn37__3 = "logreg",
  qn37__4 = "logreg",
  qn37__5 = "logreg",
  qn37__6 = "logreg",
  stunting = "logreg",        # RC)gression logistique
  wasting = "logreg",
  underweight = "logreg",
  var_malnu = "polyreg",
  TempMoyenne = "pmm",
  TempMinimale = "pmm",
  TempMaximale = "pmm",
  PrecTotale = "pmm",
  HumRelMoyenne = "pmm",
  HumRelMaximale = "pmm",
  HumRelMinimale = "pmm",
  milieu = "polyreg",             # Imputation par mode prC)dit
  commune = "polyreg",
  wealth_index_acp = "polyreg",  # Imputation normale
  wealth_index_acm = "polyreg",
  station = "polyreg",
  niveauEducation = "polyreg",  # Imputation par rC)gression logistique
  occupation = "polyreg",
  IMC_categorie = "polyreg"
  
)





library(mice)
library(gtsummary)

resultat_imputation <- mice(data = HD,m = 5, maxiter=20,maxit = 20,
                            predictorMatrix = pred_mat, seed = 1000,
                            method = meth)
complete_data <- complete(resultat_imputation, action = 5)








 ############################################################################
###########################################################################
#################### Analyse spatiale 1 - Unite spatiale : Villages ##########
###########################################################################

HDSS_data <- HDSS_data %>%
  mutate(
    malnutri = case_when(
      is.na(var_malnu) ~ NA_real_,
      var_malnu %in% c("Moyen", "Sévère") ~ 1,
      TRUE ~ 0
    )
  ) 



#################### Cas de malnutrition par menage/Village ###################
library(sf)
library(deldir)
library(ggspatial)

#### Agreger les donnC)es individuelles(enfants) au niveu menage
### et calculer le nombres d'enfants malnutris par menage


data_menage <- HDSS_data %>% 
  group_by(id_menage, gps__Latitude, gps__Longitude, station,vill) %>%
  summarise(
    total_enfants = n(),
    total_malnutris = sum(malnutri, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(
    pct_malnutri = (total_malnutris/total_enfants)*100
    )

### Exclure les menages qui n'ont pas coordonnC)es geo

data_menage_comp <- data_menage%>%
  filter(!is.na(gps__Longitude) & !is.na(gps__Latitude))



data_menage_sf <- st_as_sf(data_menage_comp, 
                           coords = c("gps__Longitude","gps__Latitude"),
                           crs = "EPSG:4326")



###################Creation des polygonnes des villages ##################################


# 1. Charger le shapefile des villages (points)
water  <- st_read("Shapefile_Nouna\\water_surface.shp")

roads  <- st_read("Shapefile_Nouna\\Roads.shp")
villages <- st_read("Shapefile_Nouna\\HDSS_Villages.shp")

area <- st_read("Shapefile_Nouna\\HDSS_Area.shp")


# 2. VC)rifier les CRS
print(st_crs(area))  # Doit afficher EPSG:32630
print(st_crs(villages))  # Probablement EPSG:4326
View(villages)

## Placer les Koro et Kamiankoro à leur position

index_Kamiankoro <- which(villages$Villages == "Kamiankoro")
index_Koro <- which(villages$Villages == "Koro")

# Échanger les géométries
temp_geom <- villages$geometry[index_Kamiankoro]
villages$geometry[index_Kamiankoro] <- villages$geometry[index_Koro]
villages$geometry[index_Koro] <- temp_geom
#View(villages)

# Reprojeter villages vers EPSG:32630 pour aligner avec area
#villages <- st_transform(villages, 32630)

# 3. Extraire les coordonnC)es des points
coords <- st_coordinates(villages)
coords <- coords[complete.cases(coords) & !is.nan(coords[, "X"]) & 
                   !is.nan(coords[, "Y"]) & !is.infinite(coords[, "X"]) & 
                   !is.infinite(coords[, "Y"]), ]

# VC)rification : nombre de points aprC(s nettoyage
cat("Nombre de points dans coords :", nrow(coords), "\n")

# 4. Calculer le diagramme de VoronoC/ avec deldir
vor <- deldir(coords[, "X"], coords[, "Y"])

# 5. Convertir les tuiles de VoronoC/ en polygones
tiles <- tile.list(vor)
cat("Nombre de tuiles generees :", length(tiles), "\n")

# VC)rifier les doublons dans coords
if (any(duplicated(coords))) {
  cat("Attention : CoordonnC)es dupliquC)es dC)tectC)es dans coords.\n")
  coords_unique <- coords[!duplicated(coords), ]
  cat("Nombre de points uniques :", nrow(coords_unique), "\n")
} else {
  coords_unique <- coords
}

# CrC)er une liste de polygones C  partir des tuiles
polygons <- lapply(tiles, function(tile) {
  coords_tile <- cbind(tile$x, tile$y)
  coords_tile <- rbind(coords_tile, coords_tile[1, ])  # Fermer le polygone
  st_polygon(list(coords_tile))
})
polygons <- st_sfc(polygons, crs = "EPSG:4326")  # CRS dC)fini comme celui de area

# 6. Ajuster villages pour correspondre aux tuiles
used_indices <- unlist(lapply(tiles, function(tile) tile$ptNum))
villages_subset <- villages[used_indices, ]

# CrC)er l'objet sf initial avec les polygones et les attributs
villages_poly_1 <- st_sf(
  ID = villages_subset$fid,
  nom = villages_subset$Villages,
  geometry = polygons
)



villages_poly <- st_transform(villages_poly_1, 32630)

# 7. Intersecter les polygones de VoronoC/ avec le contour de area
villages_poly <- st_intersection(villages_poly, area)

# VC)rification du nombre de polygones aprC(s intersection
cat("Nombre de polygones aprC(s intersection :", nrow(villages_poly), "\n")


# Lissage les polygones intersectC)s
# Ajouter un lC)ger tampon pour arrondir les angles, puis simplifier
villages_poly_smoothed <- st_buffer(villages_poly, dist = 100)  # Ajuster la distance (en mC(tres)
villages_poly_smoothed <- st_simplify(villages_poly_smoothed, dTolerance = 50)  # Ajuster la tolC)rance

# VC)rification du nombre de polygones aprC(s lissage
cat("Nombre de polygones aprC(s lissage :", nrow(villages_poly_smoothed), "\n")

# 9. Visualisation avec ggplot2
ggplot() +
  geom_sf(data = area, fill = NA, color = "black") +  # Contour de la zone
  geom_sf(data = villages_poly, alpha = 0.5) +  # Polygones lissC)s
  geom_sf(data = villages, color = "red", size = 1) +  # Points des villages
  geom_sf_text(data = villages_poly, aes(label = nom), size = 2.8, 
               color = "black", alpha = 0.7) +
  
  theme_minimal() +
  labs(title = "Zone d etude et polygones de Voronoi lisses des villages")












#############################################################################

############# Joindre les donnC)es menages et leur villages respectifs

# Reprojeter data_menage_sf vers EPSG:32630 pour aligner avec villages_poly_smoothed
#data_menage_sf <- st_transform(data_menage_sf, 32630)
data_menage_with_village <- st_join(data_menage_sf, villages_poly_1, join = st_within)



# Filtrer les m??nages sans village assign??
data_menage_with_village <- data_menage_with_village %>%
  filter(!is.na(nom))

cat("Nombre de menages non assignes a un village :", sum(is.na(data_menage_with_village$nom)), "\n")


# Verifier la pr??sence de station et pct_malnutri
#if (!"station" %in% names(data_menage_with_village)) {
 # warning("La colonne 'station' n'existe pas d'agregation uniquement par 'nom'.")
 # group_vars <- "nom"
#} else {
#  group_vars <- c("nom", "station")
#}

# 2. Agreger les donnC)es menages par village
data_village <- data_menage_with_village %>%
  #group_by(across(all_of(group_vars))) %>%
  group_by(nom)%>%
  summarise(pct_malnutri_village = mean(pct_malnutri, na.rm = TRUE),
            total = sum(total_malnutris, na.rm = TRUE),
            n_menages = n(), .groups = "drop") %>%
  st_drop_geometry()

cat("Nombre de villages avec donnees agregees :", nrow(data_village), "\n")
### 36 villages sont pris en compte dans notre etude

# 3. Joindre au shapefile Vorono??
villages_data_na <- villages_poly_1 %>% 
  left_join(data_village, by = "nom")
cat("Villages sans donnees de malnutrition :", 
    sum(is.na(villages_data_na$total)), "\n")

# Filtrer les villages avec une valeur NA(cas de malnutr)
villages_data <- villages_data_na %>% 
  filter(!is.na(total))





################ Matrices de poids ####################################
######################################################################

library(spdep)

# Matrice de voisinage
# Determiner les voisins de chaque village
nb <- poly2nb(villages_data, queen = TRUE)
cat("Polygones sans voisins :", sum(card(nb) == 0), "\n")
#Poids

w_list <- nb2listw(nb, style = "B", zero.policy = TRUE) # Style  B pour 
#les matrices de contigitC)

######################## Autocorrelation spatiale  #############
#####################################################################

############ Indice de Moran global et test

moran_test_glob <- moran.test(villages_data$total, randomisation = T,
                         w_list, zero.policy = TRUE, alternative = "two.sided")
install.packages("stargazer")

library(stargazer)

# RC)sultats du test Moran
moran_results <- data.frame(
  Statistic = c(0.19634323),
  Expectation = c(-0.02857143),
  Variance = c(0.01125586),
  Std_Deviate = c(2.12),
  P_Value = c(0.017)
)

# Afficher les rC)sultats avec stargazer
stargazer(moran_results, type = "text", summary = FALSE,
          title = "Moran I Test Results",
          digits = 2)





print(moran_test_glob)
if (moran_test$p.value < 0.05) cat("Autocorrelation spatiale significative detectee.\n")

library(spdep)  # Pour le test de Moran
library(broom)  # Pour le formatage des rC)sultats
library(gt)     # Pour la crC)ation de tableaux

# Effectuer le test de Moran
moran_test_glob <- moran.test(villages_data$total, randomisation = FALSE, w_list, zero.policy = TRUE)

# Extraire les rC)sultats
stat_moran <- moran_test_glob$statistic
p_value <- moran_test_glob$p.value
esperannce <- moran_test_glob$estimate[1]
varianceProf() <- moran_test_glob$estimate[2]



############ Indice de Moran local et test #####################


moran_local <- localmoran_perm(villages_data$total, w_list, zero.policy = TRUE)

villages_data$moran_local <- moran_local[, 1]  # Indice local
villages_data$moran_local_p <- moran_local[, 5]  # P-valeur

# Calcul des valeurs centrees et retardees
villages_data$total_centered <- villages_data$total - 
  mean(villages_data$total, na.rm = TRUE)
lag_values <- lag.listw(w_list, villages_data$total_centered,zero.policy = TRUE)

# Classification des clusters
villages_data$cluster_type <- NA
villages_data$cluster_type[villages_data$total_centered  > 0 & lag_values > 0 & 
                             villages_data$moran_local_p < 0.05] <- "Haut-Haut"
villages_data$cluster_type[villages_data$total_centered < 0 & lag_values < 0 & 
                             villages_data$moran_local_p < 0.05] <- "Bas-Bas"
villages_data$cluster_type[villages_data$total_centered > 0 & lag_values < 0 & 
                             villages_data$moran_local_p < 0.05] <- "Haut-Bas"
villages_data$cluster_type[villages_data$total_centered < 0 & lag_values > 0 & 
                             villages_data$moran_local_p < 0.05] <- "Bas-Haut"
villages_data$cluster_type[is.na(villages_data$cluster_type)] <- "Non significatif"

villages_data_crs_transf <- st_transform(villages_data, 32630) 

villages_data_inters <- st_intersection(villages_data_crs_transf, area)

villages_data_smooth <- st_buffer(villages_data_inters, dist = 100)  # Ajuster la distance (en mC(tres)
villages_data_smooth <- st_simplify(villages_data_smooth, dTolerance = 50)  # Ajuster la tolC)rance


# V??rifier les valeurs de cluster_type
cat("Types de clusters pr??sents :", unique(villages_data$cluster_type), "\n")


################################################################################
################ Cartograhie des la repartition du phenomC(ne ####################


library(ggplot2)

library(gstat)
library(ggspatial)
bbox <- st_bbox(area)
View(villages_data)

library(viridis)

custom_palette <- c("#ffe5e5", "#ffcccc", "#ff9999", "#ff6666", "#cc0000")  # Du clair au foncC)


custom_palette <- c("#ffdddd", "#ffaaaa", "#ff7777", "#ff4444", "#ff0000")  # Dégradé allant vers le rouge

custom_palette <- c("white", "blue", "yellow", "green", "red")  # Chaud et évocateur

# Carte
ggplot() +
  #geom_sf(data = area, fill = "white", color = "black", alpha = 0.5) +
  #geom_sf(data = villages_all1, fill = "white", color = "black", alpha = 0.5) +
  #geom_sf(data = water,  color = "red", alpha = 0.5) 
  #geom_sf(data = roads,  color = "red", alpha = 0.5) 
  
  geom_sf(data = villages_all1, aes(fill = total), color = "black", size = 7, alpha = 7) +
  scale_fill_gradientn(name = "Malnutrition (effectifs)", colors = custom_palette, na.value = "white") +
  geom_sf_text(data = villages_all1, aes(label = nom), size = 4, color = "black", alpha = 3.2) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "br")+
  annotate("text", x = bbox["xmin"] +100, y = bbox["ymin"] +100,
           label ="Auteur : CRSN, Moussa SOULAMA", hjust =0, size=3, color= "black")+
  
  labs(,
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  #theme(legend.position = "bottom")+
  theme_void()




villages_all1 <- st_join(villages_poly_smoothed,
                         villages_data_smooth %>%
                           dplyr::select(ID, cluster_type,total),
                         join = st_equals,
                         left = TRUE)%>%
  mutate(
    cluster_status = case_when(
      !is.na(cluster_type)~ as.character(cluster_type),
      is.na(cluster_type) ~ "Non concerne"
    )
  )




# Carte avec palette viridis
library(viridis)
ggplot() +
  geom_sf(data = villages_all1, aes(fill = total), color = "black", size = 7, alpha = 7) +
  scale_fill_viridis(name = "Malnutrition (effectifs)", option = "B", na.value = "lightgrey") +  # Option D pour une palette dégradée
  geom_sf_text(data = villages_all1, aes(label = nom), size = 4, color = "black", alpha = 3.2) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "br") +
  annotate("text", x = bbox["xmin"] + 100, y = bbox["ymin"] + 100,
           label = "Auteur : CRSN, Moussa SOULAMA", hjust = 0, size = 3, color = "black") +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme_void()





library(ggplot2)
library(RColorBrewer)

# Palette séquentielle "Reds"
custom_palette <- brewer.pal(n = 5, name = "Oranges")

# Carte avec palette RColorBrewer
repart_malnu <- ggplot() +
  geom_sf(data = villages_all1, aes(fill = total), color = "black", size = 7, alpha = 7) +
  scale_fill_gradientn(name = "Malnutrition (effectifs)", colors = custom_palette, na.value = "white") +  geom_sf_text(data = villages_all1, aes(label = nom), size = 4, color = "black", alpha = 3.2) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  geom_sf_text(data = villages_all, aes(label = nom),
               size = 4, color = "black", alpha = 3.2)+
  annotation_scale(location = "br") +
  annotate("text", x = bbox["xmin"] + 100, y = bbox["ymin"] + 100,
           label = "Auteur : CRSN, Moussa SOULAMA", hjust = 0, size = 3.5, color = "black") +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme_void()+
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1.5, "cm"),  # Taille des clés de la légende
    legend.text = element_text(size = 13),  # Taille du texte dans la légende
    legend.title = element_text(size = 14)  # Taille du titre de la légende
  )


#ggsave("clusters_malnu.pdf", plot = clusters_malnu, width = 12, height = 12)
ggsave("repart_malnu.png", plot = repart_malnu, width = 12, height = 12, dpi = 500, limitsize = FALSE)











str(villages_poly_smoothed)
str(villages_data_smooth)
############# Identifications des clusters dans les villages################
############################################################################
library(dplyr)
villages_all <- st_join(villages_poly_smoothed,
                        villages_data_smooth %>%
                          dplyr::select(ID, cluster_type),
              join = st_equals,
              left = TRUE)%>%
  mutate(
    cluster_status = case_when(
      !is.na(cluster_type)~ as.character(cluster_type),
      is.na(cluster_type) ~ "Non concerne"
    )
  )




View(villages_all)
cluster_colors <- c("Haut-Haut" = "red", "Bas-Bas" = "blue", "Haut-Bas" = "purple", 
                    "Bas-Haut" = "green", "Non significatif" = "grey", 
                    "Non concerne" ="white")

library(gstat)
library(ggspatial)
bbox <- st_bbox(area)

 View(villages_all)
# Carte
clusters_malnu <- ggplot() +
  #geom_sf(data = villages_poly_smoothed, fill = "white", color = "black", alpha = 2) +
  geom_sf(data = villages_all, aes(fill = cluster_status), color = "black", size = 7, alpha = 7) +
  scale_fill_manual(values = c("Haut-Haut" = "red", "Bas-Bas" = "blue", "Haut-Bas" = "purple", 
                               "Bas-Haut" = "green", "Non significatif" = "lightgrey"),
                    name = "Types de Clusters", na.value = "white") +
  #scale_fill_manual(name ="Type de clusters", values = cluster_colors)+
  
  #facet_wrap(~ station, ncol = 2) +  # Ajout de ncol pour organiser les facettes
  geom_sf_text(data = villages_all, aes(label = nom),
               size = 4, color = "black", alpha = 3.2)+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "br")+
  annotate("text", x = bbox["xmin"] +100, y = bbox["ymin"] +100,
           label ="Auteur : CRSN, Moussa SOULAMA", hjust =0, size=4, color= "black")+

 # labs(title = "Clusters de malnutrition du SDSS-N",
#       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme_void()+
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1.5, "cm"),  # Taille des clés de la légende
    legend.text = element_text(size = 13),  # Taille du texte dans la légende
    legend.title = element_text(size = 14)  # Taille du titre de la légende
  )

save(clusters_malnu, file = "clusters_malnu.RData")

ggsave("clusters_malnu.pdf", plot = clusters_malnu, width = 12, height = 12)
ggsave("clusters_malnu.png", plot = clusters_malnu, width = 12, height = 12, dpi = 500, limitsize = FALSE)
############################################################################
###### Identifications des clusters dans le 5 stations meteo ###############

ggplot() +
  geom_sf(data = villages_poly_smoothed, fill = "white", color = "black", alpha = 0.5) +
  
  geom_sf(data = villages_data, aes(fill = cluster_type), color = "black", size = 2, alpha = 1) +
  scale_fill_manual(values = c("Haut-Haut" = "red", "Bas-Bas" = "blue", "Haut-Bas" = "purple", 
                               "Bas-Haut" = "green", "Non significatif" = "grey"),
                    name = "Type de Cluster (p < 0.05)", na.value = "lightblue") +
  
  
  #facet_wrap(~ station, ncol = 2) +  # Ajout de ncol pour organiser les facettes
  #annotation_north_arrow(location = "tr", which_north = "true",
   #                      style = north_arrow_fancy_orienteering()) +
  #annotation_scale(location = "br")+
  labs(title = "Clusters spatiaux de malnutrition par village et station",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom")



############################################################################
###########################################################################
#################### Analyse spatiale 2 - Unite spatiale : Menages ##########
###########################################################################

library(ggspatial)

data_menage_sf_na <- data_menage_sf %>%
  filter(!is.na(data_menage_sf$total_malnutris))

coords_menage <- st_coordinates(data_menage_sf_na)

w <- 1 / as.matrix(dist(coords_menage))
diag(w) <- 0

w_list <- mat2listw(w, style = "W", zero.policy = TRUE)

library(spdep)

moran_test <- moran.test(data_menage_sf_na$total_malnutris, w_list, zero.policy = TRUE)


# Moran local (LISA)
moran_local <- localmoran(data_menage_sf_na$total_malnutris, w_list, zero.policy = TRUE)
data_menage_sf_na$moran_local <- moran_local[, 1]
data_menage_sf_na$moran_local_p <- moran_local[, 5]
data_menage_sf_na$total_malnutris_centered <- data_menage_sf_na$total_malnutris - mean(data_menage_sf$total_malnutris, na.rm = TRUE)
lag_values <- lag.listw(w_list, data_menage_sf_na$total_malnutris_centered, zero.policy = TRUE)
data_menage_sf_na$cluster_type <- NA
data_menage_sf_na$cluster_type[data_menage_sf_na$total_malnutris_centered > 0 & lag_values > 0 & data_menage_sf$moran_local_p < 0.05] <- "Haut-Haut"
data_menage_sf_na$cluster_type[data_menage_sf_na$total_malnutris_centered < 0 & lag_values < 0 & data_menage_sf$moran_local_p < 0.05] <- "Bas-Bas"
data_menage_sf_na$cluster_type[data_menage_sf_na$total_malnutris_centered > 0 & lag_values < 0 & data_menage_sf$moran_local_p < 0.05] <- "Haut-Bas"
data_menage_sf_na$cluster_type[data_menage_sf_na$total_malnutris_centered < 0 & lag_values > 0 & data_menage_sf$moran_local_p < 0.05] <- "Bas-Haut"

data_menage_sf_na <- st_transform(data_menage_sf_na, 32630)
data_menage_sf_filt <- st_intersection(data_menage_sf_na, area)
st_crs(data_menage_sf_na)

############################### Cartographie ################################


ggplot() +
  geom_sf(data = villages_poly_smoothed, fill = "white", color = "black", size = 1.2, alpha = 1) +
  
  geom_sf(data = data_menage_sf_filt, aes(size = total_malnutris), shape = 16, color = "blue",
          alpha = 0.7) +
  scale_size_continuous(range = c(0,7), name = "Nombre de cas de malnutris") +
  geom_sf_text(data = villages_poly, aes(label = nom),
               size = 2.5, color = "black", alpha = 1)+
  labs(title = "Repartitioon du nombre de cas de malnutrition par menage",
       x = "Longitude", y = "Latitude") +
  theme_minimal()


library(gstat)
library(ggspatial)
bbox <- st_bbox(area)
ggplot() +
  geom_sf(data = villages_poly_smoothed, fill = "white", color = "black", size = 1.2, alpha = 1) +
  geom_sf(data = data_menage_sf_filt, aes(color = cluster_type),
          size = 2, alpha = 0.7)+
  scale_color_manual(values = c("Haut-Haut" = "red", "Bas-Bas" = "blue", "Haut-Bas" = "purple", 
                                "Bas-Haut" = "green", "Non significatif" = "grey"),
                     name = "Type de Cluster (p < 0.05)", na.value = "grey") +
  geom_sf_text(data = villages_poly_smoothed, aes(label = nom),
               size = 3, color = "black", alpha = 2)+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "br")+
  annotate("text", x = bbox["xmin"] +100, y = bbox["ymin"] +100,
           label ="Auteur : SDSS Nouna, Moussa SOULAMA", hjust =0, size=3, color= "black")+
  labs(title = "Clusters spatiaux de menages dans les village",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme_void()








        ###           Analyse descriptive                  ###
        ######################################################


library(gt)
library(tidyverse)
library(gtsummary)

caract_child = c("sexe_child","an06","an07","an08","qn29","qn30","qn31", "qn32",
                 "qn37__2","qn37__3","qn37__4","qn37__5","qn37__6")

caract_mere =c("niveauEducation","occupation", "IMC_Categorie")

caract_menage = c("milieu","commune","wealth_index_acp","wealth_index_acm","station")

indice_meteo = c("indice_meteo1","indice_meteo2")

caract_env <- c("TempMoyenne","PrecTotale","HumRelMoyenne","TempMinimale",
                "TempMaximale", "HumRelMinimale","HumRelMaximale")

indic_anthro = c("var_malnu")


caract_child = c("sexe_child","an06","an07","an08","qn29","qn30","qn31", "qn32",
                 "qn37__2","qn37__3","qn37__4","qn37__5","qn37__6")

caract_child1 = c("sexe_child","an06","an07","an08","qn37__4","qn37__6",
                  "qn37__2","qn37__3","qn37__5","qn31", "qn32")

caract_mere =c("niveauEducation","occupation", "IMC_Categorie")

caract_menage1 = c("milieu","commune","wealth_index_acp","wealth_index_acm","station")
caract_menage = c("milieu","commune","wealth_index_acp")


caract_env <- c("Cluster")

caract_env3 <- c("TempMoyenne","PrecTotale","HumRelMoyenne")
########### Caract - Menage #####
caract_menage = c("milieu","commune","wealth_index_acp")

des_menage <- HDSS_data %>%
  mutate(across(all_of(caract_menage), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = all_of(caract_menage),
    missing = "ifany",
    digits = all_categorical() ~ c(0,2) 
  ) %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "NA : valeur manquante")%>%
  italicize_levels() %>%
  modify_header(
    list(
      label = "**Caractéristiques du ménage**",
      stat_0 = "**Effectif (pourcentage)**"
    )
  )


des_menage_kbl <- des_menage
save(des_menage_kbl, file = "des_menage_kbl.RData")

des_child <- HDSS_data %>%
  apply_labels(
    sexe_child = "Sexe",
    an06 = "Fièvre 4sem précéd",      
    an07 = "Malaria 4sem précéd",  
    an08 = "Diarhée 4sem précéd",    
    qn37__2 = "Maux de tête 4sem précéd",
    qn37__3 =  "Douleur musculaire 4sem précéd",
    qn37__4 =  "Toux 4sem précéd" ,
    qn37__5 = "Essouflement 4sem précéd",
    qn37__6 = "Ecoulement nasal",
    
    # qn30 = "Maladie aiguë durant 4sem précédentes",
    
    qn32 = "Sous-alimentation"
    #qn29 = "Maladie chronique durant 4sem précédentes"
    
  )%>%
  mutate(across(all_of(caract_child), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = all_of(caract_child),
    missing = "ifany",
    digits = all_categorical() ~ c(0,2) 
  ) %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "NA : valeur manquante")%>%
  italicize_levels() %>%
  modify_header(
    list(
      label = "**Caractéristiques de l'enfant**",
      stat_0 = "**Effectif (pourcentage)**"
    )
  )
des_child_kbl <- des_child
save(des_child_kbl, file = "des_child_kbl.RData")

library(cardx)

des_mere <- HDSS_data%>%
    mutate(across(all_of(caract_mere), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = c(caract_mere),
    missing = "ifany",
    digits = all_categorical() ~ c(0,2),
    missing_stat = "{N_miss} ({p_miss})"
  )%>%
  bold_labels()%>%
  modify_footnote(all_stat_cols() ~ "NA : valeur manquante")%>%
  italicize_levels() %>%
  modify_header(
    list(
      label = "**Caractéristiques de la mère**",
      stat_0 = "**Effectif (poourcentage)**"
    )
  )
des_mere_kbl <- des_mere
save(des_mere_kbl, file = "des_mere_kbl.RData")


library(cardx)
des_indic <-  HDSS_data%>%
  mutate(across(all_of(indic_anthro), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))
  ) %>%
  tbl_summary(
    include = all_of(indic_anthro)
  )%>% 
  bold_labels()%>%
  modify_footnote(all_stat_cols() ~ "NA : valeur manquante")%>%
  italicize_levels() %>%
  modify_header(
    list(
      label = "**Variable dépendante**",
      stat_0 = "**Effectif (pourcentage)**"
    )
  )

des_indic_kbl <- des_indic
save(des_indic_kbl, file = "des_indic_kbl.RData")

theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")
levels(HDSS_data$Cluster)<- c("Vitesse moyenne du vent élevée", "Températures moyennes élevées", 
                            "Hudimité et précipitations moyennes élevées")

des_env <-  HDSS_data%>%
  mutate(across(all_of(caract_env), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))
  ) %>%
  tbl_summary(
    include = all_of(caract_env),
    label = list(
      Cluster = "Clusters climatiques"
    )
  )%>% 
  bold_labels()%>%
  italicize_levels() %>%
  modify_header(
    list(
      label = "**Caractéristique environnementale**",
      stat_0 = "**Effectif (pourcentage)**"
    )
  )

des_env <- HDSS_data %>%
  apply_labels(
    PrecTotale = "Précipitations totales (mm)",
    TempMoyenne = "Température moyenne (°C)",
    TempMinimale = "Température minimale (°C)",
    TempMaximale = "Température maximale (°C)",
    HumRelMinimale = "Humidité relative minimale (%)",
    HumRelMoyenne = "Humidité relative moyenne (%)"
  )%>%
  tbl_summary(
      include = all_of(caract_env),
    type = all_categorical() ~ "continuous",
    statistic = all_continuous() ~ "{mean} ({sd}) [{min} - {max}]"
  ) %>%
  bold_labels() %>%
  italicize_levels()%>%
  modify_header(
    list(
      label = "**Caractéristiques environnementales**",
      stat_0 = "**Description**"
    )
  )%>%
  modify_footnote(
    all_stat_cols() ~  "Moyenne (écart-type) [min - max]"
  )

des_env_kbl <- des_env
save(des_env_kbl, file = "des_env_kbl.RData")










#set_gtsummary_theme(theme_gtsummary_journal("jama"))

#set_gtsummary_theme(theme_gtsummary_compact(set_theme = TRUE, font_size = 15))

des_recap <- tbl_stack(tbls = list(des_indic,des_env,des_child,des_mere,des_menage),
                 group_header = c("Indicateurs anthropometriques",
                                  "Caracteristiques environnementales",
                                "Caracteristiques de l'enfant",
                                "Caracteristiques de la mere",
                                  "Caracteristiques du menage") 
                 ,quiet = TRUE)%>%
  bold_labels()
library(gt)
des_recap_gt <- as_gt(des_recap)
gtsave(des_recap_gt, "des_recap.html")

##############################################################
####################### Analyse bivariC)e #################
##############################################################


################# Menage-bivariC)e ###############

caract_child = c("sexe_child","an06","an07","an08","qn29","qn30","qn31", "qn32",
                 "qn37__2","qn37__3","qn37__4","qn37__5","qn37__6")

caract_child1 = c("sexe_child","an06","an07","an08","qn37__4","qn37__6",
                 "qn37__2","qn37__3","qn37__5","qn31", "qn32")

caract_mere =c("niveauEducation","occupation", "IMC_Categorie")

caract_menage1 = c("milieu","commune","wealth_index_acp","wealth_index_acm","station")
caract_menage = c("milieu","commune","wealth_index_acp")


caract_env <- c("TempMoyenne","TempMinimale","TempMaximale","PrecTotale","HumRelMoyenne", 
                "HumRelMinimale","HumRelMaximale", "VitVentMoyenne", "VitVentMaximale")

caract_env3 <- c("Cluster")


indic_anthro = c("stunting","wasting","underweight","var_malnu")

HDSS_data%>%
  tbl_summary(
    include = all_of(caract_env),
    by = Cluster,
    missing = "no",
    type = all_categorical() ~ "continuous"
) %>%
  add_p() %>%
  bold_labels() %>%
  italicize_levels() 

biv_menage <- HDSS_data %>%
  
  tbl_summary(
    include = caract_menage,
    by = var_malnu,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}",
      all_categorical()~ "{p}"
    ),
    
    digits = list(
      all_continuous() ~ 2,  # Formatage des nombres continus avec deux dC)cimales
      all_categorical() ~ 2   # Formatage des pourcentages sans dC)cimales
    )
    
  ) %>%
  add_p() %>%
  bold_labels() %>%
  italicize_levels() 

## Les variables niveau menage retenues : Milieu de res, Commune, Indice ACP


biv_menage <- HDSS_data %>%
  
  tbl_summary(
    include = caract_menage,
    by = var_malnu,
    missing = "no",
    statistic = list(
      all_categorical()~ "{n} ({p}%)"
    ),
    digits = list(
      all_categorical() ~ c(0,2)
    )
    
  ) %>%
  add_p() %>%  # Utiliser le test chi-carrC)
  bold_labels() %>%
  separate_p_footnotes()%>%
  italicize_levels()%>%
  modify_header(label = "**Caractéristiques du ménage**")%>%
  modify_spanning_header(all_stat_cols() ~ "**Etat nutritionnel**")

biv_menage_kbl <- biv_menage
save(biv_menage_kbl, file = "biv_menage_kbl.RData")


################# Enfant-bivariC)e ###############

### variables omis expres :


biv_child <- HDSS_data %>%
  apply_labels(
    sexe_child = "Sexe",
    an06 = "Fièvre 4sem précéd",      
    an07 = "Malaria 4sem précéd",  
    an08 = "Diarhée 4sem précéd",    
    qn37__2 = "Maux de tête 4sem précéd",
    qn37__3 =  "Douleur musculaire 4sem précéd",
    qn37__4 =  "Toux 4sem précéd" ,
    qn37__5 = "Essouflement 4sem précéd",
    qn37__6 = "Ecoulement nasal",
    
   # qn30 = "Maladie aiguë durant 4sem précédentes",
    
    qn32 = "Sous-alimentation"
    #qn29 = "Maladie chronique durant 4sem précédentes"
    
  )%>%
  tbl_summary(
    include = all_of(caract_child1),
    by = var_malnu,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}",
      all_categorical()~ "{n}({p}%)"
    ),
    digits = list(
      all_categorical() ~ c(0,2)
    )
    
  ) %>%
  add_p() %>%  # Utiliser le test chi-carrC)
  bold_labels() %>%
  separate_p_footnotes()%>%
  italicize_levels()%>%
  modify_header(label = "**Caractéristiques de l'enfant**")%>%
  modify_spanning_header(all_stat_cols() ~ "**Etat nutritionnel**")

biv_child_kbl <- biv_child
save(biv_child_kbl, file = "biv_child_kbl.RData")

#caract_child = c("sexe_child","an07","qn30", "qn32","qn37__4", "qn37__6")
biv_child%>%as_kable_extra(booktabs = TRUE,
                        longtable = TRUE,
                        linesep = "")%>%
  kableExtra::kable_styling(
    position = "center",
    latex_options = c("striped", "repeat_header","hover","hold-position"),
  )

biv_child%>%
  as_flex_table()%>%
  flextable::theme_zebra()%>%
  flextable::theme_booktabs()

################# Mere-bivariee ###############
reset_gtsummary_theme()
theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")
theme_gtsummary_printer(
  print_engine = c("flextable"),
  set_theme = TRUE
)
theme_gtsummary_compact(set_theme = TRUE, font_size = NULL)


theme_gtsummary_journal(
  journal = c("qjecon"),
  set_theme = TRUE
)

biv_mere <- HDSS_data %>%
  mutate(across(all_of(caract_mere), ~ as.factor(.))) %>%
  tbl_summary(
    include = c(caract_mere),
    by = var_malnu,
    missing = "no",
    percent ="column",
    statistic = list(
      all_continuous() ~ "{mean}",
      all_categorical()~ "{n} ({p}%)"
    ),
    digits = list(
      all_categorical() ~ c(0,2)
    )
    
  ) %>%
  add_p() %>%  # Utiliser le test chi-carrC)
  bold_labels() %>%
  separate_p_footnotes()%>%
  italicize_levels()%>%
  modify_header(label = "**Caractéristiques de la mère**")%>%
  modify_spanning_header(all_stat_cols() ~ "**Etat nutritionnel**")

biv_mere_kbl <- biv_mere
save(biv_mere_kbl, file = "biv_mere_kbl.RData")

biv_mere_kbl1 <- biv_mere%>%as_kable_extra(booktabs = TRUE,
                           longtable = TRUE,
                           linesep = "")%>%
  kableExtra::kable_styling(
    position = "center",
    latex_options = c("striped", "repeat_header","hover","hold-position")
  )

biv_mere%>%
  as_flex_table()%>%
  flextable::theme_zebra()%>%
  flextable::theme_booktabs()

## Les variables retenues : Education, profession

#library(webshot2)

#gt_table <- biv_mere %>%
#  as_gt()

# Enregistrer le tableau au format HTML
#gtsave(gt_table, "biv_mere_table.html")
#options(chromote.timeout = 50)  # ou une valeur plus C)levC)e si nC)cessaire
# Convertir le tableau HTML en image
#webshot("biv_mere_table.html", file = "biv_mere_table.png", vwidth = 1000, vheight = 800)





################# Mere-bivariC)e ###############
#reset_gtsummary_theme()
#set_gtsummary_theme(theme_gtsummary_journal("jama"), set_theme = F)

#set_gtsummary_theme(theme_gtsummary_compact(set_theme = TRUE, font_size = 15))
#gtsummary::check_gtsummary_theme()

theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")

biv_env <- HDSS_data %>%
  tbl_summary(
    include = c(caract_env3),
    by = var_malnu,
    label = list(
      Cluster = "Clusters climatiques"
    ),
    missing = "no",
    percent ="column",
    statistic = list(
      all_categorical()~ "{n} ({p}%)"
    ),
    digits = list(
      all_categorical() ~ c(0,2)
    )
    
  ) %>%
  add_p() %>%  # Utiliser le test chi-carrC)
  bold_labels() %>%
  separate_p_footnotes()%>%
  italicize_levels()%>%
  modify_header(label = "**Caractéristique environnementale**",p.valeur = "**p.value**")%>%
  modify_spanning_header(all_stat_cols() ~ "**Etat nutritionnel**")





biv_env <- HDSS_data %>%
  #mutate(across(all_of(caract_mere), ~ as.factor(.))) %>%
  tbl_summary(
    include = c(TempMoyenne),
    type = all_categorical() ~ "continuous",
    by = Cluster,
    missing_text = "(NA)"
  ) %>%
  add_p() %>%  # Utiliser le test chi-carrC)
  bold_labels() %>%
  italicize_levels()



biv_env2 <- HDSS_data %>%
  apply_labels(
    PrecTotale = "Précipitations totales (mm)",
    TempMoyenne = "Température moyenne (°C)",
    TempMinimale = "Température minimale (°C)",
    TempMaximale = "Température maximale (°C)",
    HumRelMinimale = "Humidité relative minimale (%)",
    HumRelMoyenne = "Humidité relative moyenne (%)"
  )%>%
  tbl_summary(
    include = (caract_env),
    type = all_categorical() ~ "continuous",
    by = var_malnu,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}"
    ),
    
    digits = list(
      all_continuous() ~ 2 # Formatage des nombres continus avec deux dC)cimales
    )
  ) %>%
  add_p() %>%  
  separate_p_footnotes()%>%
  bold_labels() %>%
  italicize_levels()%>%
  modify_header(label = "**Variables environnementales**")%>%
  modify_spanning_header(all_stat_cols() ~ "**Etat nutritionnel**")

biv_env_kbl <- biv_env
save(biv_env_kbl, file = "biv_env_kbl.RData")

biv_env2%>%as_kable_extra(booktabs = TRUE,
                          longtable = TRUE,
                          linesep = "")%>%
  kableExtra::kable_styling(
    position = "left",
    latex_options = c("striped", "repeat_header","hover","hold-position"),
  )
 
biv_env2%>%
  as_flex_table()%>%
  flextable::theme_booktabs()%>%
  flextable::theme_zebra()

#les variables retenues : Temp mini, Temp max, Precip, HumiditC) moy, Humidite mini

################# Tableau recap-bivariee ###############


biv <- tbl_stack(list(biv_env2,biv_child,biv_menage,biv_mere),
                 group_header = c("**facteurs environnementaux**","**Enfant**","**Menage**",
                                  "**Mere**"))
biv_gt<- as_gt(biv)
gtsave(biv_gt, "Tab_analyse bivariee.html")

##############################################################
####################### Analyse multivariee  #################
##############################################################

library(car)

caract_child_biv = c("sexe_child","an07","qn30", "qn32","qn37__4", "qn37__6")

caract_mere_biv =c("niveauEducation","occupation")

caract_menage_biv = c("milieu","commune","wealth_index_acp","station","vill")

caract_env_biv <-  c("Cluster")

caract_env_biv2 <- c("TempMinimale", "TempMaximale", "PrecTotale",    
                     "HumRelMoyenne", "HumRelMinimale")
caracteristiques_biv <- c(caract_child_biv, caract_mere_biv, caract_menage_biv, caract_env_biv)

# SC)lectionner les colonnes dans le dataframe HD
HD_select <- HDSS_data %>%
  select(all_of(caracteristiques_biv),var_malnu)

HD_omit <- na.omit(HD_select)
library(MASS)
library(ordinal)
library(performance)

modele_vide <- clmm(var_malnu ~ 1 + (1 | station), data = HD_omit)
summary(modele_vide)
performance::icc(modele_vide)

library(dplyr)

# Calcul taille moyenne des enfants par village
moyenne_taille_groupe <- HD_omit %>%
  group_by(station) %>%
  summarise(taille = n()) %>%
  summarise(moyenne = mean(taille)) %>%
  pull(moyenne)

# Supposons icc_village = 0.15 (exemple)
icc_village <- 0.049

# Calcul design effect
design_effect <- 1 + (moyenne_taille_groupe - 1) * icc_village

# Environ 5% de la variablilte de l'état nutritionnel est expliqué par le niveau station

library(ordinal)

# Formule du modèle avec effets fixes et effet aléatoire station

formule1 <- var_malnu ~ sexe_child+ qn32  + qn37__6 + niveauEducation+(1 | station)

formule2 <- var_malnu ~ sexe_child+ qn32 + qn37__6+ niveauEducation +(1 + HumRelMinimale | station)


formule <- var_malnu ~ sexe_child + an07 + qn32 + qn37__4 + qn37__6  +
  niveauEducation + occupation +
  milieu + commune + wealth_index_acp +  TempMinimale   +
  HumRelMoyenne +HumRelMinimale +
  (1 | station)

# Ajustement du modèle
model1 <- clmm(formule1, data = HD_omit)
model2 <- clmm(formule2, data = HD_omit)
tbl_regression(model2,
               exponentiate = TRUE,
               )%>%
  add_global_p(keep =TRUE)
anova(model1, modele)
# Résumé du modèle
summary(model1)
3639.30
library(rcompanion)

car::vif(model2)


nagelkerke(model2)





# Ajuster le modC(le sur chaque jeu de donnC)es imputC)
regr_all <- clm(var_malnu ~ sexe_child + an07  + qn32 +
                   qn37__4 + qn37__6 +
                   niveauEducation + occupation +
                   milieu + commune + wealth_index_acp +
                   Cluster, 
                   data = HD_omit,
                 link = "probit")
library(MASS)
library(ordinal)
library(gtsummary)
regr_all1 <- polr(var_malnu ~ sexe_child + an07 + qn30 + qn32 +
                  qn37__4 + qn37__6 +
                  niveauEducation + occupation +
                  milieu + commune + wealth_index_acp + station+
                  Cluster, 
                data = HD_omit, Hess = T,
                method ="probit")


reggg <- clm(var_malnu ~ sexe_child + an07 + qn32 + qn37__6 + niveauEducation + occupation + 
               commune + wealth_index_acp + Cluster,
             data = HD_omit, link = "probit")


HD_omit <- HD_omit%>%
  apply_labels(
    sexe_child = "Sexe",
    qn32 = "Sous alimentation",
    qn37__6 = "Ecoulement nasal 4 sem précéd",
    Cluster = "Clusters climatiques"
    
  )


levels(HD_omit$Cluster)<- c("Vitesse moyenne du vent élevée", "Températures moyennes élevées", 
               "Hudimité et précipitations moyennes élevées")

regr_all <- polr(var_malnu ~ sexe_child  + qn32 +
                   qn37__6 +
                  niveauEducation +
                   commune + wealth_index_acp + Cluster, 
                data = HD_omit)
anova(regr_all)

hoslem_test <- logitgof(HD_omit$var_malnu, fitted(regr_all), g = 2, ord = TRUE)


brant(regr_all)

PR_chisq <-pulkrob.chisq(regr_all,
          catvars = c("sexe_child", "qn32","qn37__6",
                      "niveauEducation","commune"))            # Pulkstenis-Robinson

PR_deviance <-pulkrob.deviance(regr_all,
                   catvars = c("sexe_child", "qn32","qn37__6",
                       "niveauEducation","commune", "wealth_index_acp","Cluster")) 



######## Verification de multicolinearit?? ######
library(car)
vif(regr_all)
vif(regr_selected)
############# Hypothese de proportionalit?? #########

library(brant)
brant(reg15) # Verifier avec pvalue 0.13
brant(reg15_1_step)

############ Significacit?? ind(Wald) et globale(LRT) ######

library(MASS)
modele_nul <- polr(var_malnu ~ 1, data = HD_omit)

anova(modele_nul, regr_all, test = "Chisq")



lipsitz.test(regr_all, g = 5)

library(gtsummary)
tb_reg <-tbl_regression(regr_all, 
               label = list(
                 sexe_child = "Sexe",
                 qn32 = "Sous alimentation",
                 qn37__6 = "Ecoulement nasal 4 sem précéd",
                 Cluster = "Clusters climatiques"
                 ),
               exponentiate = TRUE
               )%>%
  modify_header(
    label = list(
      p.valeur = "p.value"
    )
  )%>%
  modify_header(label = "**Variables**", p.valeur = "**p.value**")%>%
  add_global_p()%>%
  add_significance_stars(
    hide_p = FALSE,
    hide_ci = FALSE,
    pattern = "{p.value}{stars}"
  )%>%
  add_glance_table()%>%
  modify_spanning_header(all_stat_cols() ~ "**Etat nutritionnel**")%>%
  bold_labels() %>%
  italicize_levels()

regress_kbl <- tb_reg
save(regress_kbl, file = "regress_kbl.RData")


rg <- step(regr_all)
anova(rg)
car::vif(regr_all)
library(brant)
brant(regr_all)

regr_all1 <- polr(var_malnu ~ sexe_child +  qn32 +
                     qn37__6 +
                    niveauEducation + occupation +
                    commune + wealth_index_acp +
                    TempMinimale   +
                    HumRelMoyenne , 
                  data = HD_omit, Hess = T,
                  method ="probit")
library(car)
brant(regr_all1)

car::vif(regr_all)

tb <- tbl_regression(regr_all,
               exponentiate =T)%>%
  add_global_p()
summary(regr_all1)


library(generalhoslem)
hoslem_test <- logitgof(HD_omit$var_malnu, fitted(reg), g = 10, ord = TRUE)

regr_all2 <- polr(var_malnu ~ sexe_child + qn32 +
                     qn37__6 +
                    niveauEducation + 
                    milieu + commune +
                    TempMinimale   +
                    HumRelMoyenne +HumRelMinimale, 
                  data = HD_omit, Hess = T,
                  method ="probit")

modele_nul <- clm(var_malnu ~ 1, data = HD_omit,
                   link = "probit")

scope <- formula(regr_all)
scope1 <- formula(regr_all1)
library(MASS)
regr_selected <- step(modele_nul, scope = scope,
                      direction = "forward", trace = 1)


regr_selected1 <- step(modele_nul, scope = scope1,
                      direction = "forward", trace = 1)

library(brant)

brant(regr_all2)
library(car)
car::vif(regr_selected)
tbl_regression(regr_selected1,
               exponentiate =T)%>%
  add_global_p()
anova(regr_selected, test = "Chisq")

predprob <- data.frame(
  sexe_child = HD_omit$sexe_child,
  qn32 = HD_omit$qn32,
  qn37__6 = HD_omit$qn37__6,
  #qn30 = HD_omit$qn30,
  niveauEducation = HD_omit$niveauEducation,
  occupation = HD_omit$occupation,
  milieu = HD_omit$milieu,
  commune = HD_omit$commune,
  # wealth_index_acp = HD_omit$wealth_index_acp,
  TempMinimale = HD_omit$TempMinimale,
  HumRelMoyenne = HD_omit$HumRelMoyenne,
  HumRelMinimale = HD_omit$HumRelMinimale
  
)

fv <- predict(regr_selected, newdata = predprob, type = "prob")$fit
library(generalhoslem)
hoslem_test <- logitgof(HD_omit$var_malnu, fv, g = 10, ord = TRUE)





modele_nul <- polr(var_malnu ~ 1, data = HD_omit,
                   Hess = TRUE, method = "probit")

scope <- formula(regss15_1)
library(MASS)
regr_selected_1 <- step(modele_nul, scope = scope,
                      direction = "forward", trace = 1)

### Regression step by step

regss1 <- polr(var_malnu ~ sexe_child,
               data = HD_omit)

regss2 <- polr(var_malnu ~ sexe_child + an07,
               data = HD_omit)
drop1(regss2, test ="Chisq") 
anova(regss1, regss2) # an07 n'ameliore pas le modele
vif(regss2)



regss3 <- polr(var_malnu ~ sexe_child + qn30 ,
               data = HD_omit)
drop1(regss3, test ="Chisq") 
anova(regss3, regss1) # qn30 ameliore pas le modele
vif(regss2) # pas de colinearite
#Nous poursuivins avec regss3


regss4 <- polr(var_malnu ~ sexe_child + qn30 + qn32 ,
               data = HD_omit)
drop1(regss4, test ="Chisq") 
anova(regss4, regss3) # qn32 ameliore pas le modele regss3
vif(regss4) # pas de colinearite
#Nous poursuivins avec regss4


regss5 <- polr(var_malnu ~ sexe_child + qn30 + qn32 +
                 qn37__4,
               data = HD_omit)
drop1(regss5, test ="Chisq") 
anova(regss5, regss3) # qn37__4 n'ameliore pas le modele regss4
vif(regss5) # pas de colinearite
#Nous gardons  regss4 car l'intro de qn37__4 entraine 
# la non signif de qn30 et lui n'etant pas signif


regss6 <- polr(var_malnu ~ sexe_child + qn30 + qn32+
                 qn37__6,
               data = HD_omit)
drop1(regss6, test ="Chisq") 
anova(regss6, regss4) # qn37__6 n'ameliore pas le modele regss4
vif(regss6) # pas de colinearite
#Nous gradons regss4, car qn37__6 n'est pas sign



regss7 <- polr(var_malnu ~ sexe_child + qn30 + qn32+
                 niveauEducation,
               data = HD_omit)
drop1(regss7, test ="Chisq") 
anova(regss7, regss4) # niveauEducation ameliore le modele regss4
vif(regss7) # pas de colinearite
#Nous gardons regss7, meme si qn30 n'est plus signif


regss8 <- polr(var_malnu ~ sexe_child + qn30 + qn32+
                 niveauEducation + occupation,
               data = HD_omit)
drop1(regss8, test ="Chisq") 
anova(regss8, regss7) # occupation n'ameliore pas modele regss7
vif(regss8) # pas de colinearite
#Nous gradons regss7

regss9 <- polr(var_malnu ~ sexe_child  + qn32+
                 niveauEducation + milieu,
               data = HD_omit)
drop1(regss9, test ="Chisq") 
anova(regss9, regss7) # milieu ameliore  modele regss7
vif(regss9) # pas de colinearite
#Nous gradons regss9 cette fois ci nous avons exclu qn30

regss10 <- polr(var_malnu ~ sexe_child  + qn32+
                 niveauEducation + milieu + commune,
               data = HD_omit)
drop1(regss10, test ="Chisq") 
anova(regss10, regss9) # commune n'ameliore modele pas regss9
vif(regss10) # pas de colinearite
#Nous gradons regss9 


regss11 <- polr(var_malnu ~ sexe_child  + qn32+
                  niveauEducation + milieu + wealth_index_acp,
                data = HD_omit)
drop1(regss11, test ="Chisq") 
anova(regss11, regss9) # wealth_index_acp ameliore regss9
vif(regss11) # pas de colinearite
#Nous gradons regss11


regss12 <- polr(var_malnu ~ sexe_child  + qn32+
                  niveauEducation + milieu + wealth_index_acp
                +station,
                data = HD_omit)
drop1(regss12, test ="Chisq") 
anova(regss12, regss11) 
vif(regss12) 

#L'ajout de station entraine la non signif des var: milieu,
# wealth_index(meme si station est signif). Aussi,
# il y a les variables milieu et station depassent VIF>2
# Nous decidons d'exclure station meme si son ajout ameliore
# le modele en comprant les AIC de regr12 et regr11


regss13 <- polr(var_malnu ~ sexe_child  + qn32+
                  niveauEducation + milieu + wealth_index_acp
                +TempMinimale,
                data = HD_omit)
drop1(regss13, test ="Chisq") 
anova(regss13, regss11) 
vif(regss13)
# TempMinimale ameliore le modele regss11
#L'ajout de TempMinimale rend wealth_index_acp non signif
# Nous poursuivons avec regss13


regss14 <- polr(var_malnu ~ sexe_child  + qn32+
                  niveauEducation + milieu + wealth_index_acp
                +TempMinimale + TempMaximale,
                data = HD_omit)
drop1(regss14, test ="Chisq") 
anova(regss13, regss14) 
vif(regss14)

#L'ajout de TempMaximale rend (milieu non signif)
# et son ajout ameliore regss13 
# Cependant TempMaxi a un VIF = 17>5 
# donc nous decidons de l'exclure et de garder regss13

regss15 <- polr(var_malnu ~ sexe_child  + qn32+
                  niveauEducation + milieu + wealth_index_acp
                +TempMinimale + PrecTotale,
                data = HD_omit)
drop1(regss15, test ="Chisq") 
anova(regss15, regss13) 
vif(regss15)

## L'ajout de PrecTotale n'ameliore pas le modele regss13
## avec un VIF=15>5 nous decidons de grader reg13
library(ordinal)
library(MASS)
regss15 <- polr(var_malnu ~ sexe_child  + qn32+ qn30+
                  niveauEducation +occupation+ milieu + wealth_index_acp
                +TempMinimale + HumRelMoyenne,
                data = HD_omit,
                Hess =TRUE, method = "probit")


regss15_1 <- clm(var_malnu ~ sexe_child  + qn32+ qn30+
                  niveauEducation +occupation+ milieu + wealth_index_acp
                +TempMinimale + HumRelMoyenne,
                data = HD_omit,
                Hess =TRUE, link = "probit")
reg15_1_step <- step(regss15_1)
# Cr??er un data.frame avec toutes les variables du mod??le
predprob <- data.frame(
  sexe_child = HD_omit$sexe_child,
  qn32 = HD_omit$qn32,
  qn37__6 = HD_omit$qn37__6,
  #qn30 = HD_omit$qn30,
  niveauEducation = HD_omit$niveauEducation,
  occupation = HD_omit$occupation,
  milieu = HD_omit$milieu,
  commune = HD_omit$commune,
 # wealth_index_acp = HD_omit$wealth_index_acp,
  TempMinimale = HD_omit$TempMinimale,
  HumRelMoyenne = HD_omit$HumRelMoyenne,
  HumRelMinimale = HD_omit$HumRelMinimale
 
)

fv <- predict(regr_selected, newdata = predprob, type = "prob")$fit
library(generalhoslem)
hoslem_test <- logitgof(HD_omit$var_malnu, fv, g = 10, ord = TRUE)
hoslem_test <- logitgof(HD_omit$var_malnu, predicted_probs, g = 10, ord =TRUE)

# Obtenir les probabilit??s pr??dites
predicted_probs <- predict(reg15_1_step, type = "prob")$fit
# V??rifier la somme des probabilit??s par ligne
rowSums(predicted_probs)
if (is.matrix(predicted_probs)) {
  predicted_classes <- apply(predicted_probs, 1, which.max)
} else {
  predicted_classes <- predicted_probs  # Si c'est un vecteur de classes
}


# Convertir en classes pr??dites
predicted_classes <- apply(predicted_probs, 1, which.max)

# Cr??er un tableau de contingence
table_data <- table(True = HD_omit$var_malnu, Predicted = predicted_classes)


regss151 <- polr(var_malnu ~ sexe_child  + qn32+ qn30+
                   niveauEducation +occupation+ milieu + wealth_index_acp
                 +TempMinimale + HumRelMoyenne,
                 data = complete_data,
                 Hess =TRUE, method = "probit"
                 )
tbreg <- tbl_regression(reg15_1_step,
               exponentiate =TRUE)%>%
     add_global_p(keep =TRUE)%>%
     add_glance_table()
tbreg

drop1(regss15, test ="Chisq") 

######## Verification de multicolinearit?? ######
library(car)
vif(regss15)
vif(regr_selected)
############# Hypothese de proportionalit?? #########

library(brant)
brant(reg15) # Verifier avec pvalue 0.13
brant(reg15_1_step)

############ Significacit?? ind(Wald) et globale(LRT) ######

library(MASS)
modele_nul <- clm(var_malnu ~ 1, data = HD_omit,
                   Hess = TRUE, link = "probit")

anova(modele_nul, reg15_1_step, test = "Chisq")

#### Goodnes of fit test ##########

library(generalhoslem)
library(DescTools)
library(sure)
install.packages("ResourceSelection")
library(ResourceSelection)
ResourceSelection::hoslem.test(HD_omit$var_malnu,regss15$fitted.values)

##### Test de  Hosmer-Lemeshow #######

hoslem.test.polr(regss15, g=10)          #
logitgof(HD_omit$var_malnu, reg15_1_step$fitted.values, g = 10, ord = FALSE)
logitgof(complete_data$var_malnu, predict(regss151, type = "probs"), g = 10, ord = TRUE)
##### Test de Pulkstenis-Robinson(PR) Chi2 pearson #######

brant(regr_all)

PR_chisq <-pulkrob.chisq(regr_all,
          catvars = c("sexe_child", "qn32","qn37__6",
                      "niveauEducation","commune", "wealth_index_acp","Cluster"))            # Pulkstenis-Robinson

PR_deviance <-pulkrob.deviance(regr_all,
                   catvars = c("sexe_child", "qn32","qn37__6",
                       "niveauEducation","commune", "wealth_index_acp","Cluster")) 

library(stargazer)

stargazer(reg15_1_step)

library(DescTools)

null_model <- clm(var_malnu ~ 1, data = HD_omit, link = "probit")
ll_model <- logLik(regr_selected)[1]
ll_null <- logLik(null_model)[1]

# Calculer McFadden R2
mcfadden_r2 <- 1 - (ll_model / ll_null)

# Calculer Nagelkerke R2
n <- nrow(HD_omit)
cox_snell_r2 <- 1 - exp((2/n) * (ll_null - ll_model))
nagelkerke_r2 <- cox_snell_r2 / (1 - exp(-(2/n) * ll_null))
library(rcompanion)
nagelkerke(reg15_1_step)

############ Tetst de Lipsitz ###################
lipsitz.test(regss15)                    # Lipsitz


summary(modele)                         # Significativit?? des coefficients
PseudoR2(modele, which="all")           # Pseudo-R??
predictions <- predict(modele, type="class")
table(observed=HD_omit$var_malnu, predicted=predictions)  # Matrice de confusion
mean(predictions == HD_omit$var_malnu)  # Taux de classification
resids <- resid(modele)                 # R??sidus
plot(resids)






deviance_full <- deviance(regss151)

# Ajuster un modC(le nul (sans prC)dicteurs)
reg_null <- polr(var_malnu ~ 1, data = HD_omit)

# Calculer la deviance du modC(le nul
deviance_null <- deviance(reg_null)

# Calculer le pseudo RB2 de McFadden
pseudo_r2 <- 1 - (deviance_full / deviance_null)

install.packages("rcompanion")
library(rcompanion)
nagelkerke(regr_all)


install.packages("pscl")
library(pscl)


regss151 <- polr(var_malnu ~ sexe_child  + qn32+
                  niveauEducation + milieu + 
                +TempMinimale + HumRelMoyenne,
                data = HD_omit)
drop1(regss151, test ="Chisq") 

summary_table<- tbl_regression(regss15,
               exponentiate =TRUE)%>%
  add_global_p(keep =TRUE)%>%
  add_rows()
pseudo_r2 <- pR2(regr_all)  # Calculer le pseudo RB2

# Ajouter le pseudo RB2 C  la note de source
summary_table <- summary_table %>%
  add_source_note(
    "Pseudo RB2 : McFadden = {round(pseudo_r2[1], 4)}, Cox & Snell = {round(pseudo_r2[2], 4)}, Nagelkerke = {round(pseudo_r2[3], 4)}"
  )



summary_table <- tbl_regression(regss15, exponentiate = TRUE) %>%
  add_global_p(keep = TRUE)
gtt <- as_gt(summary_table)
names(gtt)
gtt%>%
  rows_add(
    Characteristic = "Statistiques supplC)mentaires",
    OR = NA,
    IC = "NA",
    pvalue = NA
  )

# Calculer le pseudo RB2
pseudo_r2 <- pR2(regss15)

# Obtenir les statistiques de glisse
glance_stats <- broom::glance(regss15)

# Ajouter les statistiques d'ajustement manuellement
logLik_value <- logLik(regss15)
n <- length(regss15$fitted.values)  # Nombre d'observations
k <- length(coef(regss15))  # Nombre de paramC(tres

aic <- -2 * logLik_value + 2 * k
bic <- -2 * logLik_value + log(n) * k

# CrC)er un tableau avec les statistiques d'ajustement
adjustment_stats <- tibble::tibble(
  label = c("Pseudo RB2 (McFadden)", 
            "Pseudo RB2 (Cox & Snell)", 
            "Pseudo RB2 (Nagelkerke)", 
            "AIC", 
            "BIC"),
  OR = c(round(pseudo_r2[1], 4), 
               round(pseudo_r2[2], 4), 
               round(pseudo_r2[3], 4), 
               round(aic, 2), 
               round(bic, 2)),
  CI = NA,
  p.value = NA  # Valeurs p non applicables ici
)

# Ajouter les statistiques d'ajustement au tableau
summary_table <- summary_table %>%
  add_rows(adjustment_stats, before )

# Afficher le tableau
summary_tale$

str(summary_table)














# Nous gardons regss15


regss16 <- polr(var_malnu ~ sexe_child  + qn32+ qn30+
                  niveauEducation + milieu + wealth_index_acp
                +TempMinimale + HumRelMoyenne+ HumRelMinimale,
                data = HD_omit)
drop1(regss15, test ="Chisq") 
anova(regss16, regss15) 
vif(regss16)
## Nous gardons reg15 meme si HumRelMini ameliore le model
# le vif= 24 > 5 

regr_all <- polr(var_malnu ~ sexe_child + an07 + qn30 + qn32 +
                   qn37__4 + qn37__6 +
                   niveauEducation + occupation +
                   milieu + commune + wealth_index_acp + station +
                   TempMinimale + TempMaximale + PrecTotale +
                   HumRelMoyenne +HumRelMinimale, 
                 data = HD_omit)

library(MASS)
regr_all1 <- polr(var_malnu ~ sexe_child + an07 + qn30 + qn32 +
                  qn37__4 + qn37__6 +
                  niveauEducation + occupation +
                  milieu + commune + wealth_index_acp + station +
                  TempMinimale + TempMaximale + PrecTotale +
                  HumRelMoyenne +HumRelMinimale, 
                data = complete_data)

reg1 <-polr(formula = var_malnu ~ sexe_child + qn32 + qn37__6 +
              niveauEducation + 
              HumRelMoyenne + TempMoyenne + PrecTotale, 
            data = complete_data)

drop1(reg1, test = "Chisq")

# Ajout de occupation

reg2 <-polr(formula = var_malnu ~ sexe_child + qn32 + qn37__6+
              niveauEducation + occupation + 
              HumRelMoyenne + TempMoyenne+ PrecTotale, 
            data = complete_data)

anova(reg1, reg2) #

##  La valeur p de 0.1429457 est supC)rieure au seuil classique de 0.05. 
##Cela signifie que l'ajout de la variable occupation au 
#n'amC)liore pas significativement le modC(le par rapport au modC(le 1


reg3 <-polr(formula = var_malnu ~ sexe_child + qn32 + 
              qn37__6 + niveauEducation + 
              commune + 
              HumRelMoyenne + TempMoyenne+ PrecTotale, 
            data = complete_data)
reg4 <- polr(var_malnu ~ sexe_child+  qn30+qn32  , data = complete_data)

anova(reg1, reg3) # L'ajout du milieu de resid au modele n'ameliore pas
                  # signif le modele par rapport au modele 1.

reg4 <-polr(formula = var_malnu ~ sexe_child + qn32 + 
              qn37__6 + niveauEducation + 
              occupation + IMC_Categorie + commune +
              HumRelMoyenne + TempMoyenne, 
            data = complete_data)

anova(reg1, reg4) # L'ajout du commune de resid au modele n'ameliore pas
                  # signif le modele par rapport au modele 1.


reg5 <-polr(formula = var_malnu ~ sexe_child + qn32 + 
              qn37__6 + niveauEducation + 
              occupation + IMC_Categorie + 
              HumRelMoyenne + TempMoyenne + PrecTotale, 
            data = complete_data)
anova(reg1, reg5) # L'ajout du PrecTotale au modele ameliore pas
                  # signif le modele par rapport au modele 1.


+ an07 + qn30 + qn32 +
  qn37__4 + qn37__6 +

reg6 <-polr(formula = var_malnu ~ sexe_child + qn30 +qn32 + 
              qn37__6 + niveauEducation + 
              occupation + IMC_Categorie + 
              HumRelMoyenne + TempMoyenne + PrecTotale, 
            data = complete_data)

anova(reg5, reg6) # L'ajout du qn30 au modele n'ameliore pas
                  # signif le modele par rapport au modele 5.


reg7 <-polr(formula = var_malnu ~ sexe_child + an07 +qn32 + 
              qn37__6 + niveauEducation + 
              occupation + IMC_Categorie + 
              HumRelMoyenne + TempMoyenne + PrecTotale, 
            data = complete_data)
anova(reg5, reg7)




drop1(regr_all1,test = "Chisq")

step(regr_all1)

model_final <- step(regr_all1, direction = "both")


anova(model_final)
                  
drop1(regr_all, test = "Chisq")
reg <-glm(stunting ~ sexe_child + an07 + qn32 + qn37__6 + niveauEducation + 
             occupation+
       commune + wealth_index_acp + HumRelMoyenne + TempMoyenne + 
       PrecTotale, data = complete_data,
       family = binomial)

tbl_regression(reg1, exponentiate = TRUE)%>%
  add_global_p()
library(modelsummary)
modelsummary(reg1)
######################################################################

## Regression logistique odinale ##################""""

                                

formule1 <- as.formula(paste("var_malnu ~", 
                            paste(c( caract_child_biv,
                                    caract_menage_biv, 
                                    caract_mere_biv), collapse = " + "))) 


 



logit_ord2 <- MASS::polr(formule1, data = HDSS_data, method = "logistic")





# Liste des variables indC)pendantes
variables_independantes <- c("sexe_child", "an07", "qn32", "qn37__6", 
                             "niveauEducation", "occupation", 
                             "commune", "wealth_index_acp", 
                             "HumRelMoyenne", "TempMoyenne", 
                             "PrecTotale")



# Initialisation d'une liste pour stocker les rC)sultats
resultats_aic <- list()

# Boucle sur les tailles de combinaison
for (i in 1:length(variables_independantes)) {
  combinaisons <- combn(variables_independantes, i, simplify = FALSE)
  
  for (combinaison in combinaisons) {
    formule <- as.formula(paste("stunting ~", paste(combinaison, collapse = " + ")))
    modele <- glm(formule, data = complete_data, family = binomial)
    aic <- AIC(modele)
    
    # Stocker les rC)sultats
    resultats_aic[[paste(combinaison, collapse = "_")]] <- aic
  }
}

# Convertir les rC)sultats en dataframe
resultats_aic_df <- data.frame(CatC)gorie = names(resultats_aic), AIC = unlist(resultats_aic))

# Trier par AIC
resultats_aic_df <- resultats_aic_df[order(resultats_aic_df$AIC), ]










#########################################################################"
##### Tableau de la regression ordinale



logit_res2 <- tbl_regression(logit_ord2, exponentiate = TRUE) %>% 
  bold_labels() %>% 
  italicize_labels()%>%
  add_global_p()%>%
  modify_caption(caption = "child-mere-menage") 

############ Interpretation


#Comm:  signif : sexe, Ecoul, sous-al,Educ,mil,acp,educa(bien) 






#tbl_combined <- tbl_merge( tbls = list(logit_res4,logit_res6,logit_res5,
#                                       logit_res8, logit_res2),
#                 tab_spanner = c("**Model 1**", "**Model 2**", "**Model 3**",
#                                 "**Modele 4**","**Model 5") )

#tbl_combined1 <- tbl_merge( tbls = list(logit_res8, logit_res2),
#                           tab_spanner = c("**Model 1**", "**Model 2**") )

tbl_comb <- as_gt(logit_res2)
gtsave(tbl_comb, "Regression Logistique ordinale.html")



#install.packages("ggplot2")
#install.packages("broom")
#install.packages("GGally")
library(GGally)


library(broom)
#library(ggplot2)
#install.packages("ggstats")
library(ggstats)
ggcoef_model(regr_all, exponentiate =TRUE,
             tidy_fun = broom.helpers::tidy_parameters
)

  
tab_rlo <- ggstats::ggcoef_table(
    regr_all ,
    exponentiate = TRUE,
    tidy_fun = broom.helpers::tidy_parameters
  )
ggsave("tab_rlo.png", plot = tab_rlo, width = 8, height = 6)


summary(logit_ord2)





##############################################"


######################## stunting- Bivariee - menage ##################
########################################################################




tb_1_stunting <- HDSS_data %>%
 
  mutate(across(all_of(caract_menage), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = c(caract_menage),
     by = stunting,
    missing_text = "(NA)"
  ) %>%
  add_p() %>%
  bold_labels() %>%
  italicize_levels() 



######################## wasting- Bivariee - menage ##################
########################################################################


tb_1_wasting <- HDSS_data %>%
  
  mutate(across(all_of(caract_menage), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = c(caract_menage),
    by = wasting,
    missing_text = "(NA)"
  ) %>%
  add_p() %>%
  bold_labels() %>%
  italicize_levels() 



######################## underweight- Bivariee - menage ##################
########################################################################


tb_1_underweight <- HDSS_data %>%
 
  mutate(across(all_of(caract_menage), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = c(caract_menage),
    by = underweight,
   missing_text = "(NA)"
  ) %>%
  add_p(test.args = list(workspace = 2e9, simulate.p.value = TRUE)) %>%
  bold_labels() %>%
  italicize_levels()




######################## Stunting- Bivariee - mere ##################
#####################################################################

tb_2_stunting <- HDSS_data%>%
  mutate(across(all_of(caract_mere), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  
  tbl_summary(
    include = c(caract_mere),
    missing_text =  "(NA)",
    by = stunting
    
  )%>%
  add_p() %>%
  bold_labels()%>%
  #modify_footnote(everything() ~ NA)%>%
  italicize_levels() 
















######################## Wasting- Bivariee - mere ##################
#####################################################################

tb_2_wasting <- HDSS_data %>%
  mutate(across(all_of(caract_mere), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  
  tbl_summary(
    include = c(caract_mere),
    by = wasting,
    missing_text = "(NA)"
    
  )%>%
  add_p() %>%
  bold_labels()%>%
  #modify_footnote(everything() ~ NA)%>%
  italicize_levels()

######################## underweight- Bivariee - mere ##################
########################################################################


tb_2_underweight <- HDSS_data %>%
  mutate(across(all_of(caract_mere), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = c(caract_mere),
    by = underweight,
    missing_text = "(NA)"
  ) %>%
  add_p() %>%
  bold_labels() %>%
  italicize_levels() 





######################## Stunting- Bivariee - Child##################
#########################################################################


tb_3_stunting <- HDSS_data %>%
  mutate(across(all_of(caract_child), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = all_of(caract_child),
    missing_text = "(NA)",
    by = stunting
  ) %>%
  add_p() %>%
  bold_labels() %>%
  modify_footnote(everything() ~ NA) %>%
  italicize_levels() 

######################## Wasting- Bivariee - Child##################
#########################################################################

tb_3_wasting <- HDSS_data %>%
  mutate(across(all_of(caract_child), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = all_of(caract_child),
    missing_text = "(NA)",
    by = wasting
  ) %>%
  #add_p(test.args = list(workspace = 2e9, simulate.p.value = TRUE)) %>%
  add_p()%>%
  bold_labels() %>%
  modify_footnote(everything() ~ NA) %>%
  italicize_levels() 

######################## Underweight- Bivariee - Child##################
#########################################################################

tb_3_underweight <- HDSS_data %>%
  mutate(across(all_of(caract_child), ~ as.factor(.) %>% forcats::fct_explicit_na(na_level = "NA"))) %>%
  tbl_summary(
    include = all_of(caract_child),
    missing_text = "(NA)",
    by = underweight
  ) %>%
  add_p() %>%
  bold_labels() %>%
  modify_footnote(everything() ~ NA) %>%
  italicize_levels()

####################################################################"""

biv_stunting 

biv_stunting <- tbl_stack(tbls = list(tb_3_stunting,tb_2_stunting,tb_1_stunting),
                       #group_header = c("**Caract??ristiques de l'enfant**",
                        #                "**Caract??ristiques de la m??re**",
                         #               "**Caract??ristiques du menage**") 
                       ,quiet = TRUE)


biv3 <- tbl_merge(list(tb_3_stunting,tb_3_wasting,tb_3_underweight),
                  tab_spanner = c("**Retard de croissance**","**Emaciation**",
                                  "**Insuffisance pon??rale**"))
biv2 <- tbl_merge(list(tb_2_stunting,tb_2_wasting,tb_2_underweight))
biv1 <- tbl_merge(list(tb_1_stunting,tb_1_wasting,tb_1_underweight))

bivv <-tbl_stack(list(biv3,biv2,biv1),
                 group_header = c("**Caract??ristiques de l'enfant**",
                                   "**Caract??ristiques de la m??re**",
                                 "**Caract??ristiques du menage**") 
)

biv_wasting <- tbl_stack(tbls = list(tb_3_wasting,tb_2_wasting,tb_1_wasting),
                          #group_header = c("**Caract??ristiques de l'enfant**",
                           #                "**Caract??ristiques de la m??re**",
                            #               "**Caract??ristiques du menage**") 
                          ,quiet = TRUE)


biv_underweight <- tbl_stack(tbls = list(tb_3_underweight,tb_2_underweight,tb_1_underweight),
                         #group_header = c("**Caract??ristiques de l'enfant**",
                        #                  "**Caract??ristiques de la m??re**",
                         #                 "**Caract??ristiques du menage**") 
                         ,quiet = TRUE)

biv <- tbl_merge(
  list(biv_stunting, biv_wasting, biv_underweight),
  
  tab_spanner = c("**Retard de croissance**","**Emaciation**","**Insuffisance pon??rale**")
)%>%
  modify_header(
    label = "**Variables**"
  )

biv <- tbl_stack(list(biv,))

cbind(biv_stunting, biv_wasting, biv_underweight)

tb_2_stunting
tb_3_stunting
tb_1_stunting


#####################################################################
############# RC)gresion multiniveau #################################
#####################################################################

library(lmer4)
library(lme4)

# Ajuster le modC(le
regr_multin <- glmer(stunting ~ sexe_child + an06 + an07 + an08 + qn29 + qn30 + qn31 + qn32 +
                       qn37__2 + qn37__3+
                      qn37__4 + qn37__5 + qn37__6 + 
                      #qnhandicap +
                       niveauEducation + occupation + 
                       milieu + wealth_index_acp + 
                       (1 | station), data = HDSS_data, family = binomial(link = "cloglog"))
summary(regr_multin)
str(HDSS_data$qnhandicap)
# Obtenir la variance des effets alC)atoires
variance_station <- as.data.frame(VarCorr(regr_multin))$vcov[1]

# Calculer l'ICC
icc_station <- variance_station / (variance_station + (pi^2 / 3))

# Afficher les rC)sultats
cat("Variance - station: ", variance_station, "\n")
cat("ICC - station: ", icc_station, "\n")







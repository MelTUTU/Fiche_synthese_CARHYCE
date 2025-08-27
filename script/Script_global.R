# PREPARATION DU TRAVAIL

## Chargement des librairies
library(readr) # permet notamment d'importer les csv
library(tidyverse) # permet notamment d'utiliser les pipe, dyplyr ...
# install.packages("lubridate") # lancer ligne si jms installée sur PC
library("lubridate") # permet de bosser sur les dates
library(stringr) # permet de manipuler les chaînes de caractères

library(wesanderson) # copntient palettes de couleurs sympa
library(hrbrthemes)# contient des thèmes particuliers
library(ggthemes) # contient des thèmes particuliers
library(ggrepel) # utile pour graphiques, permet d'éviter chevauchement d'étiquettes notamment
library(gridExtra) # permet de présenter des graphes cote à cote



## définition du répertoire de travail
list.files("./data_raw/export_csv_data-20250827100437")

## chargement des principales tables de travail issues de l'export CARHYCE
### table OPERATION : import "délicat", en lien avec la nature même du fichier, donc obligé de préciser plein d'options d'import
operation <- read_delim("data_raw/export_csv_data-20250827100437/OPERATION.csv", 
                        delim = ";", escape_double = FALSE, col_types = cols(OPE_DEBIT_MESURE = col_number(), 
                                                                             OPE_DEBIT_STATION_HYDRO = col_number(), 
                                                                             OPE_LPBEV = col_number(), OPE_LMEV = col_number(), 
                                                                             OPE_LR = col_number(), OPE_LTH = col_number(), 
                                                                             OPE_DIP = col_number(), OPE_PENTE_LIGNE_EAU = col_number()), 
                        locale = locale(encoding = "WINDOWS-1252"), 
                        na = c("null",""),
                        trim_ws = TRUE)

#str(operation) # str renvoie la structure d'un objet et permet notamment de voir le type de chaque variable (nombre, charactère ...) et ainsi de vérifier si l'import s'est bien passé

### table STATION DE MESURE
sta_mesure <- read_delim("data_raw/export_csv_data-20250827100437/STATION_MESURE.csv", 
                             delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                             trim_ws = TRUE)
#str(sta_mesure)

### table POINT DE PRELEVEMENT
pvmt <- read_delim("data_raw/export_csv_data-20250827100437/POINT_PRELEVEMENT.csv",
                   na=c("null",""))
#str(pvmt)

### table TRANSECT
transect <- read_delim("data_raw/export_csv_data-20250827100437/TRANSECT.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(TRA_LPB = col_number(), 
                                                                            TRA_L_MOUILLEE = col_number(), TRA_HPB = col_number()), 
                       locale = locale(encoding = "WINDOWS-1252"), 
                       na = "empty", trim_ws = TRUE)
#str(transect)

### table POINT
point <- read_delim("data_raw/export_csv_data-20250827100437/POINT.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

#str(point)

point<-point %>%
  mutate (POI_EN_EAU= gsub(x=POI_EN_EAU,pattern='";"',replacement='')) # remplace un ";" qui traine dans une colonne

## Consolider le tableau opération à partir d'autres tables : rappatrier dans tableau operation le code sandre,date, département et info station
ope_croise <- operation %>% 
  select(PPR_ID,OPE_ID,OPE_DATE_REALISATION,OPE_LPBEV:OPE_PENTE_LIGNE_EAU) %>% 
  left_join(y=pvmt %>% 
              select(PPR_ID,SME_CD_STATION_MESURE_EAUX_SURFACE)) %>% 
  mutate(annee=dmy(OPE_DATE_REALISATION),
         annee=year(annee)) %>% 
  left_join(y=station_mesure %>% 
              select(SME_CD_STATION_MESURE_EAUX_SURFACE:SME_COORD_Y_STATION_MESURE_EAUX_SURFACE))

## Constituer un tableau contenant seulement les opérations les plus récentes (1opé/station)
ope_recent <-ope_croise %>% 
  dplyr::group_by(SME_CD_STATION_MESURE_EAUX_SURFACE) %>% 
  filter(annee==max(annee, na.rm=TRUE))


# STATISTIQUES CONCERNANT LES TRANSECTS

## Remplacer par NA, directement dans mon tableau, toutes valeurs qui sont inférieures strictement à 0
transect[transect<0]<-NA

#calculer la largeur à plein bord moyenne, médiane, min, max
lpb_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_lpb=mean(TRA_LPB,na.rm=TRUE),median_lpb=median(TRA_LPB, na.rm=TRUE),min_lpb=min(TRA_LPB, na.rm=TRUE),max_lpb=max(TRA_LPB,na.rm=TRUE))

#calculer la largeur mouillée moyenne, médiane, min, max
lm_analyse <-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_lm=mean(TRA_L_MOUILLEE,na.rm=TRUE),median_lm=median(TRA_L_MOUILLEE, na.rm=TRUE),min_lm=min(TRA_L_MOUILLEE, na.rm=TRUE),max_lm=max(TRA_L_MOUILLEE,na.rm=TRUE))

#calculer la hauteur pb moyenne, médiane, min, max
Hpb_analyse<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(moy_Hpb=mean(TRA_HPB,na.rm=TRUE),median_Hpb=median(TRA_HPB, na.rm=TRUE),min_Hpb=min(TRA_HPB, na.rm=TRUE),max_Hpb=max(TRA_HPB,na.rm=TRUE))

# Grouper tout ça dans un tableau
tot_analyse<-dplyr::left_join(lpb_analyse,lm_analyse,by='TRA_OPE_ID')
tot_analyse<-dplyr::left_join(tot_analyse,Hpb_analyse,by='TRA_OPE_ID')

#nombre de transect réalisés lors de l'opération
nb_trans<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(nb=n())
tot_analyse<-dplyr::left_join(nb_trans,tot_analyse,by='TRA_OPE_ID') # permet de rajouter l'info dans le tableau


# Lolipop représentant les lm vs les lpb (on crée d'abord les segments et ensuite les points, pour qu'ils soient bien cachés dessous)
g_largeur <-ggplot(tot_analyse)+
  geom_segment(aes(x=0,xend=moy_lpb,y=TRA_OPE_ID,yend=TRA_OPE_ID),col=colors()[128],linetype="dashed")+
  geom_segment(aes(x=0,xend=moy_lm,y=TRA_OPE_ID,yend=TRA_OPE_ID),col=colors()[128])+
  geom_point(aes(x=moy_lpb,y=TRA_OPE_ID),col=colors()[94],size=2)+
  geom_point(aes(x=moy_lm,y=TRA_OPE_ID),col=colors()[128],size=2)+
  theme_few() +
  labs(title="Largeur mouillée et largeur à plein bord moyennes par opération Carhyce",
       subtitle = "Occitanie",
       x="Largeur en mètres",
       y="Code opération",
       caption="Extraction du XX/XX/XX")
g_largeur
          # > graphique pas hyper lisible, ou alors il faudrait bien l'étaler dans la hauteur pour que les opé se superposent pas trop


# STATISTIQUES A l'ECHELLE D'UNE STATION

# sélectionner l'opé récente sur une station
mon_num=1698
ma_station <-ope_recent %>% 
  filter (OPE_ID==mon_num)

#afficher stat de largeurs mon opé
mes_stat<-tot_analyse %>% 
  filter(TRA_OPE_ID==mon_num)

#faire le lolipop largeur
couleur_eau<- colors()[128] #je définie mes couleurs au préalable
couleur_terre <- colors()[94]
couleur_sauge<-colors()[517]

# Lolipop des largeurs : Méthode AVEC légende à côté
g_largeur2_mes_stat <-ggplot(mes_stat)+
  geom_segment(aes(x=0,xend=moy_lpb,y=TRA_OPE_ID,yend=TRA_OPE_ID),col=couleur_eau,linetype="dashed")+
  geom_segment(aes(x=0,xend=moy_lm,y=TRA_OPE_ID,yend=TRA_OPE_ID),col=couleur_eau) +
  geom_point(aes(x=moy_lpb,y=TRA_OPE_ID, color = "Largeur à plein bord moyenne"),size=4)+
  geom_point(aes(x=moy_lm,y=TRA_OPE_ID, color = "Largeur mouillée moyenne"),size=4)+
  scale_color_manual(values = c("Largeur à plein bord moyenne" = couleur_terre, #permet d'ajouter la légende et de personnaliser ses couleurs
                                "Largeur mouillée moyenne" = couleur_eau)) +
  theme_few() +
  labs(title="Largeurs mouillée et à plein bord moyennes", #ajoute les titres
       subtitle = paste("Opération n°", mes_stat$TRA_OPE_ID),
       x="Largeur en mètres",
       y=NULL,
       caption="Extraction du XX/XX/XX",
       color = "Type de largeur")+
  theme(
    axis.ticks.y = element_blank(),  # Supprime les tick marks
    axis.text.y = element_blank())+    # Supprime les étiquettes
  geom_text_repel(aes(x = moy_lpb, y = TRA_OPE_ID, label = round(moy_lpb,1)), size = 3)+
  geom_text_repel(aes(x = moy_lm, y = TRA_OPE_ID, label = round(moy_lm, 1)), size = 3)#+
#annotate("text",x=0,y=mes_stat$TRA_OPE_ID,label="largeur à plein bord",hjust=0,size=3)

g_largeur2_mes_stat
save(g_largeur2_mes_stat, file="output/lolipop_largeur.Rdata") # sauvergarder le graphique pour l'importer ensuite dans R markdown

# Lolipop des largeurs : Méthode SANS légende à côté

couleur_eau<- colors()[128]
couleur_terre <- colors()[94]

g_largeur3_mes_stat <-ggplot(mes_stat)+
  geom_segment(aes(x=0,xend=moy_lpb,y=TRA_OPE_ID,yend=TRA_OPE_ID),col=couleur_eau,linetype="dashed")+
  geom_segment(aes(x=0,xend=moy_lm,y=TRA_OPE_ID,yend=TRA_OPE_ID),col=couleur_eau) +
  geom_point(aes(x=moy_lpb,y=TRA_OPE_ID), col = couleur_terre,size=4)+
  geom_point(aes(x=moy_lm,y=TRA_OPE_ID), col = couleur_eau,size=4)+
  
  theme_few() +
  
  labs(title="Largeurs mouillée et à plein bord moyennes", #ajoute les titres
       subtitle = paste("Opération n°", mes_stat$TRA_OPE_ID),
       x="Largeur en mètres",
       y=NULL,
       caption="Extraction du XX/XX/XX")+
  
  theme(
    axis.ticks.y = element_blank(),  # Supprime les tick marks
    axis.text.y = element_blank(), # Supprime les étiquettes
    legend.position="none") +  # Supprime la légende  
  
  geom_text_repel(aes(x = moy_lpb, y = TRA_OPE_ID, label = paste("Lpb :", round(moy_lpb,1))), size = 3)+
  geom_text_repel(aes(x = moy_lm, y = TRA_OPE_ID, label = paste ("Lm : ", round(moy_lm, 1))), size = 3)

g_largeur3_mes_stat

#Lolipop des hauteurs plein bord

g_hauteur_mes_stats<-ggplot(mes_stat)+
  geom_segment(aes(x=TRA_OPE_ID,xend=TRA_OPE_ID,y=0,yend=max_Hpb),col=couleur_sauge)+
  geom_point(aes(x=TRA_OPE_ID,y=max_Hpb),size=5.6,shape = 25,fill=couleur_sauge,color=couleur_sauge)+
  geom_point(aes(x=TRA_OPE_ID,y=0),size=5.6,shape = 3,fill=couleur_sauge,color=couleur_eau)+
  geom_point(aes(x=TRA_OPE_ID,y=median_Hpb),size=4.6,shape = 21,fill=couleur_sauge,color=couleur_sauge)+
  scale_color_manual(values = c("Hauteur à plein bord max" = couleur_sauge, #permet d'ajouter la légende et de personnaliser ses couleurs
                                "Hauteur à plein bord médiane" = couleur_sauge)) +
  theme_few() +
  labs(title="Hauteur à plein bord", #ajoute les titres
       subtitle = paste("Opération n°", mes_stat$TRA_OPE_ID),
       x=NULL,
       y="Hauteur en mètres",
       caption="Extraction du XX/XX/XX")+
  theme(
    axis.ticks.x = element_blank(),  # Supprime les tick marks
    axis.text.x = element_blank())+
  geom_text_repel(aes(x = TRA_OPE_ID, y = max_Hpb, label = paste("Max : ", round(max_Hpb, 1))), size = 3)+
  geom_text_repel(aes(x = TRA_OPE_ID, y = median_Hpb, label = paste ("Médiane : ", round(median_Hpb, 1))), size = 3)+
  geom_text_repel(aes(x=TRA_OPE_ID,y=0,label = "surface de l'eau"), size=3)

g_hauteur_mes_stats

# Afficher les graphiques côte à côte
figure_geom<-grid.arrange(g_largeur3_mes_stat, g_hauteur_mes_stats, ncol=2)


# STATISTIQUES CONCERNANT LES POINTS

## Récupération d'infos dans d'autres tableaux
colnames(point)[colnames(point) == "POI_TRA_ID"] <- "TRA_ID" # renommer la colonne qui va nous servir à faire la jointure pour qu'elle soit la même dans les deux tableaux
point_croise <- point %>% 
    left_join(
      y=transect %>% select(TRA_ID,TRA_OPE_ID),
      by = "TRA_ID")

## Combien de points par transects (min, max, médiane)
point_analyse <- point_croise %>%
  group_by(TRA_OPE_ID, TRA_ID) %>% # on groupe par opération puis par transect
  summarise(nb_points = n(), .groups = "drop") %>% #On compte le nombre de lignes avec n() pour chaque combinaison TRA_OPE_ID/TRA_ID et on force force dplyr à supprimer ce groupement et à retourner un data frame "plat" (non groupé). la fonction summarise créée un nv data frame
  group_by(TRA_OPE_ID) %>% # On regroupe uniquement par TRA_OPE_ID pour calculer les statistiques par opération
  summarise(
    min_points = min(nb_points),
    max_points = max(nb_points),
    median_points = median(nb_points),
    .groups = "drop" # force dplyr à supprimer ce groupement et à retourner un data frame "plat" (non groupé).
  )

## Combien de points en eau / hors d'eau
point_analyse_eau <- point_croise %>%
  group_by(TRA_OPE_ID, TRA_ID) %>%
  summarise(
    nb_points_eau = sum(POI_PROFONDEUR >= 0, na.rm = TRUE),
    nb_points_total = n(),
    .groups = "drop"
  ) %>%
  mutate( # pour ajouter une nouvelle variable
    pourcentage_eau = ifelse(nb_points_total > 0,
                             round(100 * nb_points_eau / nb_points_total, digits = 0), #permet d'arrondir 
                             NA)# permet d'écarter les cas où y'a pas de points et où donc on peut pas diviser par 0
  )






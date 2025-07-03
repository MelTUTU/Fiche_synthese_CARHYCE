# ANALYSE A LA STATION

# ATTENTION : faire tourner script import_donnees au préalable

#chargement librairie
library(ggplot2)
library(wesanderson)
library(hrbrthemes)# contient des thèmes particuliers
library(ggthemes) # contient des thèmes particuliers
library(ggrepel)

#Remplacer par NA, directement dans mon tableau, toutes valeurs qui sont inférieures strictement à 0
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

## Construction d'un graphique représentant les lm et lb moyennes 
#OPTION A
# Selection de ces seules variables
tab_lm_lpb_moy <-tot_analyse %>% 
  dplyr::select(TRA_OPE_ID,moy_lpb,moy_lm)

# Transformation du tableau pour répartir les colonnes en lignes
tab_transfo<-tab_lm_lpb_moy %>% 
  pivot_longer(cols=-'TRA_OPE_ID',
               names_to="type_largeur",
               values_to="valeur_largeur")

# Création d'un loolipop permettant de mettre sur un même plan la largeur à plein bord et la largeur mouillée
g_largeur<-ggplot(data=tab_transfo)+
  geom_point(aes(x=valeur_largeur,y=TRA_OPE_ID,color=type_largeur))+
  geom_segment(aes(x=0,xend=valeur_largeur,y=TRA_OPE_ID,yend=TRA_OPE_ID),color='blue')
g_largeur  


#OPTION B : sans créer de tableau plus court et avec infos dans colonnes
#on crée d'abord les segments et ensuite les points, pour qu'ils soient bien cachés dessous
g_largeur2 <-ggplot(tot_analyse)+
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
 g_largeur2


  
#graphique > marche pas car je n'arrive pas à installer la librairie qu'il faut
#library(streamgraph)
#graph_1<-streamgraph(tab_transfo,key='type',value='valeur',date='TRA_ID',
 #                    width="400px", height="300px")




#nombre de transect réalisés lors de l'opération
nb_trans<-transect %>% 
  dplyr::group_by (TRA_OPE_ID) %>%
  summarise(nb=n())


#combiner les tableaux
tot_analyse2<-dplyr::left_join(nb_trans,tot_analyse,by='TRA_OPE_ID')


# sélectionner une station
ma_station <-ope_recent2 %>% 
  filter (OPE_ID=="817")

#afficher stat sur une seule opé
mes_stat<-tot_analyse2 %>% 
  filter(TRA_OPE_ID=="817")

#faire le graphique sur une seule opé
#je définie mes couleurs au préalable
couleur_eau<- colors()[128]
couleur_terre <- colors()[94]
couleur_sauge<-colors()[517]

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

g_hauteur_mes_stats<-ggplot(mes_stat)+
  geom_segment(aes(x=TRA_OPE_ID,xend=TRA_OPE_ID,y=0,yend=max_Hpb),col=couleur_sauge)+
  geom_point(aes(x=TRA_OPE_ID,y=max_Hpb),size=5.6,shape = 25,fill=couleur_sauge,color=couleur_sauge)+
  geom_point(aes(x=TRA_OPE_ID,y=0),size=5.6,shape = 3,fill=couleur_sauge,color=couleur_sauge)+
  geom_point(aes(x=TRA_OPE_ID,y=median_Hpb),size=5.6,shape = 1,fill=couleur_sauge,color=couleur_sauge)
g_hauteur_mes_stats


geom_point(data = df_longdist, aes(x = max), size = 5.6, shape = 21, 
           fill = "grey65", color = "#FFFCFC", stroke = .5)




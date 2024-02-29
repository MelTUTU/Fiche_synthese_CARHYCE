# à lancer après car_gen
library(ggplot2)

# boxplot des largeur à plein bord en fontion des départements
graphique1<-
  ggplot(data=ope_recent)+
  aes(x=SME_CD_DEPARTEMENT,
      y=OPE_LPBEV)+
  # geom_boxplot(outlier.shape = NA)
  # geom_violin()

graphique1 <-
  graphique1+
  aes (fill=SME_CD_DEPARTEMENT)+
  geom_jitter(fill="grey80",
              width=0.1)+
  theme_grey()+
  labs (title = "Répartition des largeurs à plein bord évaluée par département occitans",
        subtitle = "Lpbev est approchée par la moyenne de trois mesures de largeur à plein bord",
        x = "Département",
        y = "Largeur à plein bord évaluée (en m)")+
  theme(legend.position = "none")

graphique1

ggsave(plot=graphique1,
       filename="output/largeur_pb_ev_dpt.png",
       width = 7,
       height = 5,
       dpi=180)
library(wesanderson)

# Histogramme en vignette par département
graphique2<- ggplot(data=ope_recent)+
  aes(x=OPE_LPBEV)+
  geom_density(fill="green")+
  facet_wrap(~SME_CD_DEPARTEMENT)
graphique2



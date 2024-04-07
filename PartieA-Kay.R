library(fpp3)
library(ggplot2)
load("dev5_tanomaly.RData") ## adapter le chemin
set.seed(1989922) # remplacer par votre matricule
ind.cell <- sample(ncol(tanomaly.train)-1, 1) ## selection de maille

## pour visualiser où se trouve la maille
ggplot(lonlat.coord[ind.cell,], aes(x = lon, y = lat)) + geom_point(size = 2)+
  borders(xlim = c(-140, -50), ylim = c(40, 89))

tanomaly.series <- tsibble(
  Date = tanomaly.train[,1],
  Observation = tanomaly.train[,ind.cell+1],
  index = Date
)


tanomaly.series.test <- tsibble(
  Date = tanomaly.test[,1],
  Observation = tanomaly.test[,ind.cell+1],
  index = Date
)


## graphique temporel
autoplot(tanomaly.series, Observation, linewidth = 1.2)+
  labs(y = "degres C", x = "mois") + theme(text = element_text(size=20))

#Autour de l'annee 1980, il y a un record sur la temperature la plus basse et aussi
#un peu apres 1989, on a un record pour la temperature la plus elevee de plus de 15
#degrees

## deux types de graphiques saisonniers
tanomaly.series |> gg_season(Observation, labels = "both", linewidth = 1.2) +
  labs(y = "degres C",x = "mois")+ theme(text = element_text(size=20))

#On voit que pour tous les annees, les temperatures entre juillet et decembre
#restent stables et que les plus grand peaks sont typiquement en debut d'annee.


tanomaly.series |> gg_subseries(Observation) + theme(text = element_text(size=20))
#Durant les mois entre Aout et Decembre, la moyenne de la temperature reste assez stable
#Fevrier a la plus grande moyenne qui est causé par un grand peak durant les premiers annees
#Mars a les temperatures les plus basses, mais une moyenne superieure à la majorité.


## graphique de lag: tester plusieurs lags en changeant la valeur de l’argument "lags"
tanomaly.series |> gg_lag(Observation, geom = "point", lags = 12) +
  theme(text = element_text(size=20),legend.title=element_blank())

#Pour de petites valeurs de lag (ex lag = 1 ou 2), les données de couleurs vert
#sont principalement centrés, toutefois en augmentant le lag, celle-ci se
#dirigent plus vers la droite. LMèinverse pour les données mauve ou celle-ci sont
#eparpilles pour les petits lag, mais se concentrent davatanage et pointent vers le
#haut en augmentant la valeur du lag.

## graphique d’auto-corrélation
tanomaly.series |> ACF(Observation, lag_anomaly = 24) |> autoplot()

#Chaque lag ici correspond à 1 mois ecoulee (1M) et on voit que la correlation
#durant les deux premiers mois d'une annee sont élevées puis commencent à diminuer
#jusqu'à tomber dans une corrélation négative pour 4 mois (mai-aout) et recommence
#à monter. Le dernier mois de l'année repréesente typiquement le mois avec la 
#plus grande corrélation. Structure assez récurrent mais pas parfait entre les 
#deux années des lags. Année 1 à 4 mois de corrélation négative tandis que l'année
#2 en a 6.

#On voit aussi que ces premiers et derniers mois sont supérieure à l'intervalle
#de confiance de 95% signifiant que ces corrélations sont fortes, mais aucune
#est inférieure à 95%. Il y a autocorrelation, donc pas de bruit blanc.


## Décomposition STL stockée dans l’objet tanomaly.stl
tanomaly.series |> model(STL(Observation ~ trend(window = 12) + season(window = "periodic"), robust = TRUE)) |> components() -> tanomaly.stl
## Graphique standard de la décomposition
autoplot(tanomaly.stl)
## Graphique de l’ACF de la composante résiduelle
tanomaly.stl |> ACF(remainder, lag_anomaly = 24) |> autoplot()


#Il existe une tendance saisonnier périodique (voir graphe season_year), on 
#garde donc la composante season().

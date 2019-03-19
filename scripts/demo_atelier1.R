#Démo avec jeu de données dt.cegep.clean

dt.cegep.clean[,2]
mean(dt.cegep.clean[, 2])
mean(dt.cegep.clean$MGS)
mean(dt.cegep.clean[1:10, 2])
boxplot(dt.cegep.clean[, 2])

mean(dt.cegep.clean$philo101)
mean(dt.cegep.clean$philo102)
mean(dt.cegep.clean$philo103)

t.test(dt.cegep.clean$philo101, dt.cegep.clean$philo102)


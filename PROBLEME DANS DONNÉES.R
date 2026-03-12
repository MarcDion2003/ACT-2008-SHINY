historiques <- read.csv2("resultats-complets.csv")
partiels <- read.csv2("resultats-partiels.csv")
normales <- read.csv2("normales.csv")
## le trou 14 a plusieurs 0
sum(is.na(rowMeans(historiques)))
sum(is.na((historiques)))
colSums(is.na(historiques))
which(!complete.cases(historiques))
historiques[1171, 14 + 1]
## 1 NA observé
## trou 1, 10, 11, 12, 14, 18; 30 ou 33 observé

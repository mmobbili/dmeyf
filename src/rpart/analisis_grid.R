salida_fi<-data.frame(E250_flavia)

max_cp <- aggregate(ganancia ~ cp, data=salida_fi, FUN=max)

max_minsplit <- aggregate(ganancia ~ minsplit, data=salida_fi, FUN=max)

max_minbucket <- aggregate(ganancia ~ minbucket, data=salida_fi, FUN=max)

max_maxdepth <- aggregate(ganancia ~ maxdepth, data=salida_fi, FUN=max)

plot(max_cp, xlab="cp")

plot(max_minsplit, xlab="minsplit")

plot(max_minbucket, xlab="minbucket")

plot(max_maxdepth, xlab="maxdepth")

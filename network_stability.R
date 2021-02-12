library('BoolNet')
# Codigo para analizar la estabilidad de la red
net <- loadNetwork("RedMacrofagos_120121.csv")
net
distanHamming <- perturbTrajectories(net, measure = "hamming", 
                                     numSamples = 1000, updateType = "synchronous")
distances <- distanHamming[[1]]
#Codigo para graficar distancias de Hamming
hist(distances, col = "gray", xlab= "Distancias de Hamming", main = "Frecuencias de las DH")
abline(v = distanHamming[[2]], col="red", lwd=3)

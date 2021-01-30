library('BoolNet')
# Codigo para obtener atractores
net <- loadNetwork("RedMacrofagos_120121.csv")
net

attr <- getAttractors(net, method = "sat.restricted", maxAttractorLength = 1,
                      returnTable = TRUE)
attr

numberAttr <- length(attr$attractors)
getAttractorSequence(attractorInfo = attr, 1952)
attractors <- data.frame(sapply(c(1:1952), getAttractorSequence, attractorInfo = attr))
attractors <- data.frame(t(attractors))
attractors <- apply(attractors,2,as.character)
write.csv(attractors, "atractores_290121.csv", row.names = F)

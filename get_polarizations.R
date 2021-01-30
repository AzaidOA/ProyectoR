# Obtencion de estados celulares con base en los atractores obtenidos
states <- read.csv("atractores_290121.csv", header = TRUE)
M0 <- data.frame()
M1 <- data.frame()
M2a <- data.frame()
M2b <- data.frame()
M2c <- data.frame()
M2d <- data.frame()
not_m1 <- c("IRF4", "STAT6", "ERK", "STAT3_IL10", "IL10_out")
not_m2a <- c("STAT1", "STAT5", "NFKB", "ERK", "STAT3_IL6", "IL12_out", 
             "VEGF_out", "IL6_out")
not_m2b <- c("STAT1", "STAT5", "IRF3", "STAT6", "IRF4", "STAT3_IL10", "VEGF_out")
not_m2c <- c("STAT1", "STAT5", "IRF3", "STAT6", "IRF4", "ERK", "NFKB", 
             "IL12_out", "IL6_out", "VEGF_out")
not_m2d <- c("STAT1", "STAT5", "STAT6", "IRF4", "ERK", "IL6_out")
flag <- FALSE
total <- 0
for(row in 1:nrow(states)){
  if(!any(states[row,])){
    pol <- cbind(polarization = "M0", states[row,])
    M0 <- rbind(M0, pol) 
    flag <- TRUE
  }
  if(states[row, "IL12_out"] &&  (states[row, "IL6_out"] || states[row, "STAT1"] || states[row, "STAT5"] 
                                  || states[row, "NFKB"] || states[row, "IRF3"])){
    if(!any(states[row, not_m1])){
      pol <- cbind(polarization = "M1", states[row,])
      M1 <- rbind(M1, pol)
      flag <- TRUE
    }
  }
  if(states[row, "PPARG"] && states[row, "STAT6"] && states[row, "IRF4"] 
     && states[row, "IL10_out"]){
    if(!any(states[row, not_m2a])){
      pol <- cbind(polarization = "M2a", states[row,])
      M2a <- rbind(M2a, pol)
      flag <- TRUE
    }
  }
  if(states[row, "IL10_out"] && (states[row, "IL6_out"] || states[row, "NFKB"] ||
                                 states[row, "ERK"])){
    if(!any(states[row, not_m2b])){
      pol <- cbind(polarization = "M2b", states[row,])
      M2b <- rbind(M2b, pol)
      flag <- TRUE
    }
  }
  if(states[row, "STAT3_IL10"] && states[row, "IL10_out"]){
    if(!any(states[row, not_m2c])){
      pol <- cbind(polarization = "M2c", states[row,])
      M2c <- rbind(M2c, pol)
      flag <- TRUE
    }
  }
  if(states[row, "VEGF_out"] && states[row, "IL10_out"] && 
     states[row, "HIF1a"]){
    if(!any(states[row, not_m2d])){
      pol <- cbind(polarization = "M2d", states[row,])
      M2d <- rbind(M2d, pol)
      flag <- TRUE
    }
  }
  if(flag){
    total <- total + 1
    flag <- FALSE
  }
}
final_pol <- rbind(M0, M1, M2a, M2b, M2c, M2d)

# Guardando los resultados en una grafica y en un csv
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")
barplot(height= sapply(list(M0, M1, M2a, M2b, M2c, M2d), nrow), 
        names= c("M0", "M1", "M2a", "M2b", "M2c", "M2d"), 
        col=coul, xlab = c("Tipos celulares"),ylab = c("Incidencia"), ylim = c(0,150))
write.csv(final_pol, "estados.csv", row.names = F)

stat_kgw[nrow(stat_kgw)+1,]<-NA
rownames(stat_kgw)[rownames(stat_kgw) == "5"] = "f-ratio"

for(i in 1:ncol(stat_kgw)){
  
  a <- statmw1a[5, i]/statcont[5, i]
  
  b <- statcont[5, i]/statmw1a[5, i]
  
  if(a > b) {
    statmw1a[6, i] <- a
  }else {
    statmw1a[6, i] <- b
  }
}

par(new = TRUE)
par(mfrow=c(5,5))

for(i in 1:ncol(kgw_chem)) {
  
  hist(unlist(kgw_chem[,i]), na.rm = TRUE, main = "Konza Groundwater", xlab=colnames(kgw_chem[i]))
  
}

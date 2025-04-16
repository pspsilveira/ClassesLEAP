# print(seed <- runif(1,1,1e6))
seed <- 392075
set.seed(seed)
# anemicos
anemicas <- rnorm(809, mean=12.972, sd=1.5)
saudaveis <- rnorm(1770, mean=18.246, sd=3)

data <- data.frame(matrix(data=c(
  rep("anêmica",length(anemicas)),
  rep("saudável",length(saudaveis)),
  anemicas,saudaveis
), ncol=2, nrow=length(anemicas)+length(saudaveis),
byrow=FALSE))
names(data) <- c("Grupo","Ferritina")
data$Ferritina <- as.numeric(data$Ferritina)
mincutoff <- min(data$Ferritina,na.rm=TRUE)
maxcutoff <- max(data$Ferritina,na.rm=TRUE)
cutoffs <- seq(mincutoff,maxcutoff,length.out=1000)
se <- c()
sp <- c()
for (cutoff in cutoffs)
{
  data$a <- data$b <- data$c <- data$d <- 0
  data$Pos <- FALSE
  data$Pos[data$Ferritina <  cutoff] <- TRUE
  data$a[data$Grupo=="anêmica" & data$Pos] <- 1
  data$b[data$Grupo=="saudável" & data$Pos] <- 1
  data$c[data$Grupo=="anêmica" & !data$Pos] <- 1
  data$d[data$Grupo=="saudável" & !data$Pos] <- 1
  tabela2x2 <- matrix(data=c(
    sum(data$a,na.rm=TRUE),
    sum(data$b,na.rm=TRUE),
    sum(data$c,na.rm=TRUE),
    sum(data$d,na.rm=TRUE)
  ),ncol=2,nrow=2,byrow=TRUE)
  colnames(tabela2x2) <- c("Anêmicas","Saudáveis")
  rownames(tabela2x2) <- c(paste0("Ferritina < ",round(cutoff,2)),
                           paste0("Ferritina >= ",round(cutoff,2)))
  if(sum(tabela2x2[,1],na.rm=TRUE)>0 & sum(tabela2x2[,2],na.rm=TRUE)>0)
  {
    se <- c(se,tabela2x2[1,1]/sum(tabela2x2[,1]))
    sp <- c(sp,tabela2x2[2,2]/sum(tabela2x2[,2]))
  } else
  {
    se <- c(se,NA)
    sp <- c(sp,NA)
  }
}
ROC <- data.frame(cutoffs,se,sp)
# escolhendo pelo índice de Youden
Y <- se+sp-1
ycut <- which(Y==max(Y,na.rm=TRUE))
ycutoff <- ROC$cutoffs[ycut]
# escolhendo pela minima distancia
ROC$dist <- (((1-sp)^2+(1-se)^2))^0.5
dcut <- which(ROC$dist==min(ROC$dist,na.rm=TRUE))
dcutoff <- ROC$cutoffs[dcut]
plot(1-sp,se,type="b",cex=0.5,
     pch=21,col="#00800020",bg="#00800020",
     xlim=c(0,1.08),ylim=c(0,1.08),bty="n",
     main=paste0("ROC (Receiver Operating Characteristic)\n",
                 "para Ferritina (cutoff from ",round(mincutoff,2),
                 " to ",round(maxcutoff,2),")"),
     xlab="1 - especificidade",
     ylab="sensibilidade")
lines(c(0,1),c(0,1),lwd=0.5,lty=5) # bissetriz
lines(c(0,1-sp[dcut]),c(1,se[dcut]),lty=2)
points(1-sp[dcut],se[dcut],pch=16)
lines(c(0,1-sp[ycut]),c(1,se[ycut]),lty=2)
points(1-sp[ycut],se[ycut],pch=16)
text(1-sp[dcut],se[dcut],paste0(round(mean(dcutoff),2)," (distância mínima)"),pos=4)
text(1-sp[ycut],se[ycut],paste0(round(mean(ycutoff),2)," (Youden máximo)"),pos=4)
text(0,0,round(mincutoff,2),pos=4)
text(1,1,round(maxcutoff,2),pos=1)

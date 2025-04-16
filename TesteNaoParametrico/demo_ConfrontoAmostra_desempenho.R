source("eiras.create.population.R")
source("eiras.rounder.R")

echo = TRUE

s <- 27863
set.seed(s)
col1 <- "#000000"
col2 <- eiras::FriendlyColor(8)
col1t <- paste0(col1,"30")
col2t <- paste0(col2,"30")
numamostras <- 20000
n1 <- 6
n2 <- 8
alpha <- 0.05

pop1 <- create.population(n=c(7000,2000,2000),
                          mean=c(135, 160, 210),
                          sd=c(16,17,18))
pop2 <- create.population(n=c(1500,4000,6500),
                          mean=c(111, 186, 236),
                          sd=c(19,20,23))

# amostras
rm(.Random.seed, envir=globalenv()) # reset seed
runif(1)

c_tstud <- c()
c_twelch <- c()
c_utrad <- c()
c_bm <- c()
seeds <- c()
a <- b <- c <- d <- c(0,0,0,0,0,0)
for (amostra in 1:numamostras)
{
  # cat(".")
  # if(amostra%%100 == 0)
  #   cat(amostra,"\n")
  amostra1 <- sample(pop1,size=n1)
  amostra2 <- sample(pop2,size=n2)
  
  # cat("\nTeste t de Student:\n")
  t <- t.test(amostra1,amostra2,var.equal=TRUE)
  p.tstud <- t$p.value
  c_tstud <- c(c_tstud, p.tstud)
  
  # cat("\nTeste t de Welch/Satterthwite:\n")
  t2 <- t.test(amostra1,amostra2)
  p.twelch <- t2$p.value
  c_twelch <- c(c_twelch, p.twelch)
  
  # cat("\nTeste U de Mann-Whitney Convencional:\n")
  u <- wilcox.test(amostra1,amostra2, exact = FALSE)
  p.utrad <- u$p.value 
  c_utrad <- c(c_utrad, p.utrad)
  
  # cat("\nTeste de Brunner-Munzel:\n")
  bm <- brunnermunzel::brunnermunzel.permutation.test(amostra1, amostra2, force=TRUE)
  p.bm <- bm$p.value
  c_bm <- c(c_bm, p.bm)
  
  # # quero achar testes em que np rejeitam H0 e p não rejeitam H0
  # # e, de preferencia, que ambas as amostras não normais
  # if(p.utrad<0.05 & p.tstud>=0.05 & p.twelch>=0.05) #  & p.bm<0.05
  # {
  #   swt1 <- shapiro.test(amostra1)
  #   swt2 <- shapiro.test(amostra2)
  #   if(swt1$p.value < 0.05 & swt2$p.value < 0.05)
  #   {
  #     cat("\n",amostra1)
  #     cat("\n",amostra2)
  #     plot(density(amostra1),ylim=c(0,0.03))
  #     lines(density(amostra2),lty=2)
  #     xxx
  #   }
  # }

  # t Student vs U MWW
  if (p.tstud<alpha & p.utrad<alpha) {a[1] <- a[1]+1}
  if (p.tstud<alpha & p.utrad>=alpha) {b[1] <- b[1]+1}
  if (p.tstud>=alpha & p.utrad<alpha) {c[1] <- c[1]+1}
  if (p.tstud>=alpha & p.utrad>=alpha) {d[1] <- d[1]+1}
  # t Welch vs U MWW
  if (p.twelch<alpha & p.utrad<alpha) {a[2] <- a[2]+1}
  if (p.twelch<alpha & p.utrad>=alpha) {b[2] <- b[2]+1}
  if (p.twelch>=alpha & p.utrad<alpha) {c[2] <- c[2]+1}
  if (p.twelch>=alpha & p.utrad>=alpha) {d[2] <- d[2]+1}
  # t Student vs BM
  if (p.tstud<alpha & p.bm<alpha) {a[3] <- a[3]+1}
  if (p.tstud<alpha & p.bm>=alpha) {b[3] <- b[3]+1}
  if (p.tstud>=alpha & p.bm<alpha) {c[3] <- c[3]+1}
  if (p.tstud>=alpha & p.bm>=alpha) {d[3] <- d[3]+1}
  # t Welch vs BM
  if (p.twelch<alpha & p.bm<alpha) {a[4] <- a[4]+1}
  if (p.twelch<alpha & p.bm>=alpha) {b[4] <- b[4]+1}
  if (p.twelch>=alpha & p.bm<alpha) {c[4] <- c[4]+1}
  if (p.twelch>=alpha & p.bm>=alpha) {d[4] <- d[4]+1}
  # U MWW vs BM
  if (p.utrad<alpha & p.bm<alpha) {a[5] <- a[5]+1}
  if (p.utrad<alpha & p.bm>=alpha) {b[5] <- b[5]+1}
  if (p.utrad>=alpha & p.bm<alpha) {c[5] <- c[5]+1}
  if (p.utrad>=alpha & p.bm>=alpha) {d[5] <- d[5]+1}
  # t Student vs t Welch
  if (p.tstud<alpha & p.twelch<alpha) {a[6] <- a[6]+1}
  if (p.tstud<alpha & p.twelch>=alpha) {b[6] <- b[6]+1}
  if (p.tstud>=alpha & p.twelch<alpha) {c[6] <- c[6]+1}
  if (p.tstud>=alpha & p.twelch>=alpha) {d[6] <- d[6]+1}
}

if(echo)
  cat("\nSimulação com total de ",numamostras," amostragens.\n")
pares <- c("U MWW","t Student",
           "U MWW","t Welch",
           "Brunner-Munzel","t Student",
           "Brunner-Munzel","t Welch",
           "Brunner-Munzel","U MWW",
           "t Welch","t Student"
           )
i2 <- 1
m <- list()
G <- list()
z <- list()
p <- list()
for (i in 1:6)
{
  if(echo)
    cat("\nTabela de contingencia (concordancia entre os testes):\n")
  m[[i]] <- matrix(c(a[i],b[i],c[i],d[i]),ncol=2,nrow=2,byrow=TRUE)
  colnames(m[[i]]) <- c(paste0(pares[i2],":H1"),paste0(pares[i2],":H0"))
  rownames(m[[i]]) <- c(paste0(pares[i2+1],":H1"),paste0(pares[i2+1],":H0"))
  if(echo)
    print (m[[i]])

  if(echo)
  {
    cat(paste0("\nTeste da concordância entre ",
               pares[i2]," e ",pares[i2+1],":\n"))
    cat("\n\tH0: G =  0")
    cat("\n\tH1: G <> 0")
  }
  G[[i]] <- ((a[i]+d[i])-(b[i]+c[i]))/numamostras
  z[[i]] <- (a[i]+d[i] - numamostras/2)/sqrt(numamostras/4)
  p[[i]] <- 2*(1-pnorm(abs(z[[i]])))
  if(echo)
  {
    cat("\n")
    cat("\n\tG = ",G[[i]],sep="")
    cat("\n\tz = ",z[[i]],", p = ",p[[i]],sep="")
    cat("\n\nConclusão:")
    if(p[[i]]<0.05)
    {
      cat(" há concordância")
    } else
    {
      cat(" não há associação")
    }
    cat(paste0(" entre os testes ",pares[i2]," e ",pares[i2+1],"."))
    cat("\n")
  }
  
  
  
  G[[i]] <- rounder(G[[i]]) # textual format
  z[[i]] <- rounder(z[[i]]) # textual format
  p[[i]] <- rounder(p[[i]],statistics=TRUE) # textual format
  i2 <- i2+2  
} # for i

r_utrad <- sum(c_utrad<alpha,na.rm=TRUE)/numamostras
r_bm <- sum(c_bm<alpha,na.rm=TRUE)/numamostras
r_tstud <- sum(c_tstud<alpha,na.rm=TRUE)/numamostras
r_twelch <- sum(c_twelch<alpha,na.rm=TRUE)/numamostras
if(echo)
{
  cat("\n")
  cat("\nProporcao de Rejeicoes corretas:")
  cat("\n\tt de Student: ",r_tstud,sep="")
  cat("\n\tt de Welch: ",r_twelch,sep="")
  cat("\n\tU de Mann-Whitney: ",r_utrad,sep="")
  cat("\n\tBrunner-Munzel: ",r_bm,sep="")
  cat("\n\nDiferencas:\n")
}
i2 <- 1
bint <- list()
c.txt <- list()
for (i in 1:6)
{
  if(echo)
  {
    cat("\nTeste da diferença de proporcao de sucessos:")
    cat(paste0("\n\tH0: p(",pares[i2+1],")-p(",pares[i2],") =  0"))
    cat(paste0("\n\tH1: p(",pares[i2+1],")-p(",pares[i2],") <> 0"))
    cat("\n")
  }
  binaux <- bintest <- DescTools::BinomDiffCI(a[i]+b[i],numamostras,a[i]+c[i],numamostras)
  binaux <- binaux*100
  binaux <- rounder(binaux,digits=2)
  bint[[i]] <- paste0(binaux[1]," [",binaux[2],",",binaux[3],"]")
  if(echo)
    print(bintest)

  c.txt[[i]] <- paste0("a proporção de rejeições corretas pelo ",
             pares[i2+1]," é ")
  if(bintest[2]>0 & bintest[3]>0 )
  {
    c.txt[[i]] <- paste0(c.txt[[i]],"superior")
  } 
  if(bintest[2]<0 & bintest[3]<0 )
  {
    c.txt[[i]] <- paste0(c.txt[[i]],"inferior")
  } 
  if(bintest[2]<0 & bintest[3]>0 )
  {
    c.txt[[i]] <- paste0(c.txt[[i]],"indistinguível")
  }
  c.txt[[i]] <- paste0(c.txt[[i]],paste0(" à do ",pares[i2],"."))

  if(echo)
  {
    cat(paste0("\nConclusão: ",c.txt[[i]],"\n"))
  }
  i2 <- i2+2
} # for i

d_utrad <- density(c_utrad,na.rm=TRUE)
d_bm <- density(c_bm,na.rm=TRUE)
d_tstud <- density(c_tstud,na.rm=TRUE)
d_twelch <- density(c_twelch,na.rm=TRUE)
ymax <- max(c(d_tstud$y,d_twelch$y,d_utrad$y,d_bm$y),na.rm=TRUE)
plot (NA,
      main="Distribuição dos valores p obtidos",
      xlab="valor p", ylab="Densidade",
      xlim=c(0,1), ylim=c(0,ymax), axes=FALSE)
axis(1)
axis(2)
abline(v=alpha,lty=2)
lines(d_twelch,lwd=2, lty=1, col="royalblue")
lines(d_tstud,lwd=2, lty=2, col="cyan4")
lines(d_utrad,lwd=2, lty=4, col="orangered2")
lines(d_bm,lwd=2, lty=5, col="chocolate3")
legend("topright",
       c(paste0("t Welch (rej.",round(r_twelch*100,1),"%)"), 
         paste0("t Student (rej.",round(r_tstud*100,1),"%)"), 
         paste0("U Mann-Whitney (rej.",round(r_utrad*100,1),"%)"), 
         paste0("Brunner-Munzel (rej.",round(r_bm*100,1),"%)")),
       lty=c(1,2,4,5),
       lwd=2,
       col=c("royalblue","cyan4","orangered2","chocolate3"),
       box.lwd=0, bg="transparent")

plot (NA,
      main="(detalhe, abaixo de p=0.05)",
      xlab="valor p", ylab="Densidade",
      xlim=c(0,0.055), ylim=c(2,ymax), axes=FALSE)
axis(1)
axis(2)
abline(v=alpha,lty=2)
lines(d_twelch,lwd=2, lty=1, col="royalblue")
lines(d_tstud,lwd=2, lty=2, col="cyan4")
lines(d_utrad,lwd=2, lty=4, col="orangered2")
lines(d_bm,lwd=2, lty=5, col="chocolate3")
legend("topright",
       c(paste0("t Welch (rej.",round(r_twelch*100,1),"%)"), 
         paste0("t Student (rej.",round(r_tstud*100,1),"%)"), 
         paste0("U Mann-Whitney (rej.",round(r_utrad*100,1),"%)"), 
         paste0("Brunner-Munzel (rej.",round(r_bm*100,1),"%)")),
       lty=c(1,2,4,5),
       lwd=2,
       col=c("royalblue","cyan4","orangered2","chocolate3"),
       box.lwd=0, bg="transparent")

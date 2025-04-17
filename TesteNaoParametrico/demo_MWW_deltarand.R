source("eiras.pseudomediana.R")

Grupo_1 <- round(runif(30,1,10),3)
Grupo_2 <- round(runif(20,8,20),3)

delta <- c()
for(m in Grupo_1)
{
  for(n in Grupo_2)
  {
    delta <- c(delta,m-n)
  }  
}
Delta <- median(delta)

cat(paste0("\nDadas duas amostras:"))
cat(paste0("\n Grupo 1: ",paste(Grupo_1,collapse=", ")))
cat(paste0("\n Grupo 2: ",paste(Grupo_2,collapse=", ")))
cat(paste0("\n\nObtém-se Delta = ",Delta, 
           " (",length(Grupo_1)*length(Grupo_2)," pares de valores)"))

cat("\n\nCalculando pseudomedianas com função própria:\n")
X <- pseudomediana(Grupo_1)
cat(paste0("\n Pseudomediana(Grupo 1) = ",
           X$statistics[1]," (",X$statistics[2]," pares de valores)"))
Y <- pseudomediana(Grupo_2)
cat(paste0("\n Pseudomediana(Grupo 2) = ",
           Y$statistics[1]," (",Y$statistics[2]," pares de valores)"))
cat(paste0("\nDif. das pseudomedianas = ",
           X$statistics[1]-Y$statistics[1]))
           
cat("\n\nCalculando pseudomedianas com o pacote DescTools:\n")
X2 <- DescTools::HodgesLehmann(Grupo_1)
cat(paste0("\n Pseudomediana(Grupo 1) = ",X2))
Y2 <- DescTools::HodgesLehmann(Grupo_2)
cat(paste0("\n Pseudomediana(Grupo 2) = ",Y2))
cat(paste0("\nDif. das pseudomedianas = ",X2-Y2))

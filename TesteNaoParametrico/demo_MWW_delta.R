source("eiras.pseudomediana.R")

ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)

delta <- c()
for(m in ComTreino)
{
  for(n in SemTreino)
  {
    delta <- c(delta,m-n)
  }  
}
Delta <- median(delta)

cat(paste0("\nDadas duas amostras:"))
cat(paste0("\n Com treino: ",paste(ComTreino,collapse=", ")))
cat(paste0("\n Sem treino: ",paste(SemTreino,collapse=", ")))
cat(paste0("\n Diferenças: ",paste(delta,collapse=", ")))
cat(paste0("\nObtém-se Delta = ",Delta, 
           " (",length(ComTreino)*length(SemTreino)," pares de valores)"))

X <- pseudomediana(ComTreino)
cat(paste0("\n Pseudomediana(Com Treino) = ",
           X$statistics[1]," (",X$statistics[2]," pares de valores)"))
Y <- pseudomediana(SemTreino)
cat(paste0("\n Pseudomediana(Sem Treino) = ",
           Y$statistics[1]," (",Y$statistics[2]," pares de valores)"))

cat(paste0("\nDif. das pseudomedianas = ",
           X$statistics[1]-Y$statistics[1]))

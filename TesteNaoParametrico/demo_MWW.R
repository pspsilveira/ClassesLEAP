alfa <- 0.05
ComTreino <- c(2,3,3,3,3,3,4,4,4,5)
SemTreino <- c(1,2,2,2,2,3,3,3,3,3,3,4)
dt_treino <- data.frame(
  c(rep("Com",length(ComTreino)),
    rep("Sem",length(SemTreino))),
  c(ComTreino,SemTreino)
)
names(dt_treino) <- c("Treino","Simpatia")
dt_treino$Treino <- factor(dt_treino$Treino)

cat("\nCom treino:",ComTreino)
cat("\n\tmediana =",median(ComTreino))
cat("\nSem treino:",SemTreino)
cat("\n\tmediana =",median(SemTreino))
cat("\n")
dif <- median(ComTreino) - median(SemTreino)
cat("\nDiferenca das medianas amostrais (Com treino - Sem treino) = ",dif,sep="")
cat("\n")

cat("\nTeste U de Mann-Whitney Convencional (homocedástico):\n")
print(wilcox.test(Simpatia~Treino,
                  data=dt_treino,
                  exact=FALSE,
                  correct=FALSE,
                  conf.int=TRUE,
                  conf.level=1-alfa))
cat("\nTeste U de Mann-Whitney Exato (homocedástico):\n")
print(exactRankTests::wilcox.exact(SemTreino, ComTreino,
                                   paired=FALSE,
                                   conf.int=TRUE,
                                   conf.level=1-alfa))
cat("\nTeste U de Mann-Whitney Bootstrapping (homocedástico):\n")
print(coin::wilcox_test(Simpatia~Treino,
                        data = dt_treino,
                        distribution=coin::approximate(nresample=1e6), 
                        conf.int=TRUE,
                        conf.level=1-alfa))
cat("\nTeste B de Brunner-Munzel (heterocedástico):\n")
print(lawstat::brunner.munzel.test(SemTreino, ComTreino))



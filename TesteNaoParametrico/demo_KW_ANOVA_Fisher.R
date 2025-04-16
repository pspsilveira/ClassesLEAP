source("eiras.shape.test.R")

# ANOVA1f_indep_Welch_sodio.R
# para ajustar este RScript para outros dados
# troque a planilha xlsx e substitua as palavras
# Grupo pela nova VI (fator)
# Sintoma2 pela nova VD (resposta)

source("eiras_plotIC.R")

# suppress warnings
options(warn=-1)

alfa <- 0.05
TH <- as.data.frame(readxl::read_excel("Enxaqueca.xlsx"))
TH$Grupo <- factor(TH$Grupo, levels=unique(TH$Grupo))

print(TH)
cat("\nTabulando Sintoma2 por Grupo:")
print(table(TH$Sintoma2,TH$Grupo))

print(with(TH, psych::describeBy(Sintoma2,Grupo,m=1,digits=2)))

# teste de assimetria e normalidade das diferencas
s <- list()
s[["terapia"]] <- TH$Sintoma2[TH$Grupo=="Terapeuta"]
s[["autoaj"]] <- TH$Sintoma2[TH$Grupo=="Autoajuda"]
s[["espera"]] <- TH$Sintoma2[TH$Grupo=="Lista de espera"]
shape <- shape.test(s, labels = c("Terapeuta",
                                  "Autoajuda",
                                  "Lista de espera"))
boxplot(Sintoma2~Grupo,data=TH,
        ylab=names(TH)[which(names(TH)=="Sintoma2")],
        xlab=names(TH)[which(names(TH)=="Grupo")]
)
with(TH, gplots::plotmeans(Sintoma2~Grupo,
                           error.bars="conf.int", 
                           level=1-alfa/length(unique(TH$Grupo)),
                           connect=FALSE,
                           ylab=names(TH)[which(names(TH)=="Sintoma2")],
                           xlab=names(TH)[which(names(TH)=="Grupo")],
                           main="IC95% Bonferroni",
                           barcol="black"))
car::densityPlot(Sintoma2~Grupo, data=TH, rug=TRUE, from=0, normalize=TRUE,
                 na.rm=TRUE, ylab="Densidade", col=c("black", "black", "black"))
cat("\n")

plot(ecdf(TH$Sintoma2[TH$Grupo=="Terapeuta"]), 
     main="Empirical Cumulative Distribution Function",
     xlim=c(0.5,5.5),
     xlab="Sintoma2", 
     ylab="Probabilidade",
     lwd=2, verticals = TRUE)
lines(ecdf(TH$Sintoma2[TH$Grupo=="Autoajuda"]), 
      lwd=2, col="blue", verticals = TRUE)
lines(ecdf(TH$Sintoma2[TH$Grupo=="Lista de espera"]), 
      lwd=2, col="brown", verticals = TRUE)
legend("topleft",
       c("Terapeuta", "Autoajuda", "Espera"),
       lty=1,
       lwd=2,
       col=c("black","blue","brown"),
       box.lwd=0, bg="transparent")

# nomes do fator encurtados
TH$Grupo <- as.character(TH$Grupo)
fatores <- unique(as.character(TH$Grupo))
letra <- "A"
legenda <- c()
cat ("\nLegenda:\n")
for( f in 1:length(fatores))
{
  cat("\t",letra," ... ",fatores[f],"\n",sep="")
  legenda <- c(legenda,paste(letra," ... ",fatores[f],"\n",sep=""))
  TH$Grupo[TH$Grupo==fatores[f]] <- letra
  ascii <- strtoi(charToRaw(letra),16L)
  letra <- rawToChar(as.raw(ascii+1))
}
TH$Grupo <- as.factor(TH$Grupo)


cat("\nANOVA unifatorial independente de Fisher\n\n")
alfa <- 0.05
VD <- names(TH)[which(names(TH)=="Sintoma2")]
VI <- names(TH)[which(names(TH)=="Grupo")]
cat("VD =", VD,"\n")
cat("Fator =", VI,"\n")
cat("\nAnalise de significancia estatistica: teste omnibus\n")
modelo <- lm(Sintoma2~Grupo, 
             data=TH)

# ANOVA da one-way ANOVA
cat("\nANOVA")
print(anv <- car::Anova(modelo))

EMM <- emmeans::emmeans(modelo, 
                        specs=trt.vs.ctrl~Grupo,
                        ref=1,
                        reverse=FALSE,
                        level=1-alfa,
                        adjust="dunnett")

print(summary(EMM, 
              infer=TRUE)$contrasts)

print(multcomp::cld(object=EMM$emmeans,
                    level=1-alfa,
                    adjust="bonf",
                    Letters=letters,
                    alpha=alfa))


cat("\nAnalise de significancia pratica: tamanho de efeito\n")
eta2 <- effectsize::eta_squared(anv,
                                partial=FALSE,
                                generalized=FALSE,
                                ci=1-alfa,
                                alternative="two.sided",
                                verbose=TRUE)
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2, digits=4)

# enable warnings
options(warn=0)

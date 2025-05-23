library(PMCMRplus)
## Sachs, 1997, p. 675
## Six persons (block) received six different diuretics
## (A to F, treatment).
## The responses are the Na-concentration (mval)
## in the urine measured 2 hours after each treatment.
## Assume A is the control.
Data <- matrix(c(
  3.88, 5.64, 5.76, 4.25, 5.91, 4.33, 30.58, 30.14, 16.92,
  23.19, 26.74, 10.91, 25.24, 33.52, 25.45, 18.85, 20.45,
  26.67, 4.44, 7.94, 4.04, 4.4, 4.23, 4.36, 29.41, 30.72,
  32.92, 28.23, 23.35, 12, 38.87, 33.12, 39.15, 28.06, 38.23,
  26.65),nrow=6, ncol=6,
  dimnames=list(1:6, LETTERS[1:6]))
print(Data)

## Global Friedman test
print(PMCMRplus::friedmanTest(Data))
## Exact many-one test
print(PMCMRplus::frdManyOneExactTest(y=Data, p.adjust = "bonferroni"))
## Eisinga et al. 2017
print(PMCMRplus::frdAllPairsExactTest(y=Data, p.adjust = "bonferroni"))

# Teste de Friedman nativo
print(friedman.test(Data))

# Reorganizar para long format
df <- as.data.frame(Data)
df$sujeito <- factor(1:nrow(df))
library(reshape2)
df_long <- reshape2::melt(df, id.vars = "sujeito",
                          variable.name = "tratamento",
                          value.name = "sodio")

# Post hoc: Wilcoxon pareado com correção de Bonferroni
print(pairwise.wilcox.test(df_long$sodio,
                           df_long$tratamento,
                           paired = TRUE,
                           p.adjust.method = "bonferroni"))


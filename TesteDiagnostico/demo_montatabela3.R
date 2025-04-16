nomelin<-"H"
nomecol<-"M"
a<-50
b<-150
c<-125
d<-175
tabela <- matrix(data=c(a,b,c,d), ncol=2, byrow = TRUE)
colnames(tabela) <- c(paste0(nomecol,"+"),paste0(nomecol,"-"))
rownames(tabela) <- c(paste0(nomelin,"+"),paste0(nomelin,"-"))
print(tabela)
print(ptable <- prop.table(tabela))


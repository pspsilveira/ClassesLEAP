
shape.test <- function(samples, alpha=0.05, B=0, labels=NA, 
                       echo=TRUE, 
                       echo.descriptive=FALSE, 
                       echo.assumptions=FALSE)
{
  if(!is.list(samples)) {samples <- list(samples)}
  if(is.na(labels[1]))
  {
    chr <- 64
    labels < rep("",length(samples))
    for (i in 1:length(samples))
    {
      chr <- chr+1
      labels[i] <- intToUtf8(chr)
    }
  }
  
  namesanatomy <- c("n","mean","median","mode",
                    "pseudomedian", "psdmdn.lower","psmddn.upper",
                    "Q1(min)","Q2","Q3(median)","Q4","Q5(max)",
                    "st.dev","IQR",
                    "Skewness","skwns.lower","skwns.upper",
                    "Kurtosis","krts.lower","krts.upper")

  list <- list()
  for (i in 1:length(samples))
  {
    anatomy <- matrix(nrow=1, ncol=length(namesanatomy))
    colnames(anatomy) <- namesanatomy
    
    if(echo){cat(eiras::BarTitle(labels[i]))}
    values <- samples[[i]]
    
    anatomy[1] <- length(values)
    anatomy[2] <- mean(values,na.rm=TRUE)
    anatomy[3] <- median(values,na.rm=TRUE)
    dens <- density(values, na.rm=TRUE)
    anatomy[4] <- mean(dens$x[which.max(dens$y)])
    psm <- DescTools::HodgesLehmann(values) #  eiras::HodgesLehmann(values,alpha=alpha,B=B)
    anatomy[5] <- psm # $hl
    anatomy[6] <- NA # psm$lwr
    anatomy[7] <- NA # psm$upr
    anatomy[8:12] <- quantile(values,na.rm=TRUE)
    anatomy[13] <- sd(values,na.rm=TRUE)
    anatomy[14] <- IQR(values,na.rm=TRUE)
    anatomy[15:17] <- DescTools::Skew(values,na.rm=TRUE,ci.type="perc",conf.level=1-alpha)
    anatomy[18:20] <- DescTools::Kurt(values,na.rm=TRUE,ci.type="perc",conf.level=1-alpha)
    
    if(echo | echo.descriptive)
    {
      cat(eiras::BarTitle("Distribution anatomy",level=4))
      cat(eiras::BarTitle("- central tendency measures:",level=8))
      # prmatrix(anatomy[1,1:7],collab="",quote=FALSE)
      prmatrix(anatomy[1,1:5],collab="",quote=FALSE)
      cat(eiras::BarTitle("- quartiles:",level=8))
      prmatrix(anatomy[1,8:12],collab="",quote=FALSE)
      cat(eiras::BarTitle("- dispersion measures:",level=8))
      prmatrix(anatomy[1,13:14],collab="",quote=FALSE)
      cat(eiras::BarTitle("- skewness:",level=8))
      prmatrix(anatomy[1,15:17],collab="",quote=FALSE)
      cat(eiras::BarTitle("- kurtosis excess:",level=8))
      prmatrix(anatomy[1,18:20],collab="",quote=FALSE)
    }
    list[[paste0("anatomy.",labels[i])]] <- anatomy
    rm(anatomy)
    gc()
    
    list[[paste0("simmetry.",labels[i])]] <- lawstat::symmetry.test(values,boot=FALSE)
    n <- min(c(5000,length(values)),na.rm=TRUE)
    list[[paste0("normality.",labels[i])]] <- shapiro.test(sample(values,size=n))
    if(echo | echo.assumptions)
    {
      cat(eiras::BarTitle("Symmetry test:",level=4))
      print(list[[paste0("simmetry.",labels[i])]])
      cat(eiras::BarTitle("Normality test:",level=4))
      n <- min(c(5000,length(values)),na.rm=TRUE)
      cat("\n\ttesting with n = ",n,"\n",sep="")
      print(list[[paste0("normality.",labels[i])]])
    }
  }    
  
  if(length(samples)>=2)
  {
    if(echo | echo.assumptions)
    {
      cat(eiras::BarTitle("Homoscedasticity test:",level=4))
      cat("\n\tassuming interval variables\n\n",sep="")
    }
    alldata <- c()
    alllabel <- c()
    for (i in 1:length(samples))
    {
      alldata <- c(alldata,samples[[i]])
      alllabel <- c(alllabel,rep(labels[i],length(samples[[i]])))
    }
    dtfrm <- data.frame(alldata,alllabel)
    names(dtfrm) <- c("value","label")
    dtfrm$label <- factor(dtfrm$label)
    list[["levene"]] <- car::leveneTest(value ~ label, data=dtfrm, center="median")
    if(echo | echo.assumptions)
    {
      print(list[["levene"]])
    }
  }
  
  return(list)
}
# 
# s <- 27863
# set.seed(s)
# pop1 <- eiras::create.population(n=c(7000,2000,2000),
#                                  mean=c(135, 160, 210),
#                                  sd=c(16,17,18))
# pop2 <- eiras::create.population(n=c(1500,4000,6500),
#                                  mean=c(111, 186, 236),
#                                  sd=c(19,20,23))
# pop3 <- rnorm(n=31000,mean=3000,sd=200)
# # shape.test(pop1, pop2, alab="População 1", blab="População 2")
# s <- list()
# s[["group1"]] <- pop1
# s[["group2"]] <- pop2
# s[["group3"]] <- pop3
# l <- shape.test(s)
#    
# para exibir valores p e similares
to_superscript <- function(x)
{
  sup_digits <- c("-" = "\u207B",
                  "0" = "\u2070", "1" = "\u00B9", "2" = "\u00B2", "3" = "\u00B3",
                  "4" = "\u2074", "5" = "\u2075", "6" = "\u2076", "7" = "\u2077",
                  "8" = "\u2078", "9" = "\u2079")
  paste0(sup_digits[strsplit(as.character(x), "")[[1]]], collapse = "")
}

locate_exponent <- function(input_string)
{
  # Encontra a posição do conteúdo dentro das chaves
  match_info <- gregexpr("\\{-?[0-9]+\\}", input_string)
  # Obtém a posição inicial do match
  start_pos <- match_info[[1]]
  # Obtém o tamanho do match
  match_length <- attr(start_pos, "match.length")
  # Calcula a posição final
  end_pos <- start_pos + match_length - 1
  # Exibir os resultados
  return(c(start_pos,end_pos))
}

# p=0.00012123123
# digits=5
# symbol=c("=","<<")
# x10=TRUE
# LaTeX=FALSE
# force.scientific=FALSE
# statistics=TRUE
# suffix=NA
# symbol=NA

rounder <- function(p,
                    digits=4,
                    x10=TRUE,LaTeX=FALSE,
                    force.scientific=FALSE,
                    statistics=FALSE,
                    suffix=NA,symbol=NA
                   )
{
  p.txt <- rep("",length(p))
  limit <- round(10^(-digits),digits)
  for(p.aux in seq_along(p))
  {
    pv <- as.numeric(p[p.aux])
    signal <- ifelse(pv<0,-1,1)
    ok <- FALSE
    if(is.na(suffix))
      suffix <- ifelse(statistics,"p","")
    if(is.na(symbol[1]))
    {
      if(statistics)
        symbol <- c("=","<<",">>")
      else
        symbol <- rep("",3)
    }
    if(statistics) # exception for p==0 or p==1
    {
      if(!ok & pv==0) {p2 <- paste0(suffix[1],symbol[2],sprintf(paste0("%.",digits,"f"),limit)); ok <- TRUE}
      if(!ok & pv==1) {p2 <- paste0(suffix[1],symbol[3],sprintf(paste0("%.",digits,"f"),1-limit)); ok <- TRUE}
    }
    if(!ok)
    {
      if(!ok & (pv==0 | abs(pv)>=limit)) 
      {
        p2 <- paste0(suffix[1],symbol[1],sprintf(paste0("%.",digits,"f"),pv)) 
        ok <- TRUE
      }
      if(!ok & pv<limit) 
      {
        digits2 <- round(digits/2,0) 
        if(digits2 < 1)
          digits2 <- 1
        p2 <- paste0(suffix[1],symbol[1], sprintf(paste0("%.",digits2,"e"),pv)) 
        ok <- TRUE
      }
      if(force.scientific) 
      {
        p2 <- paste0(suffix[1],symbol[1], sprintf(paste0("%.",digits,"e"),pv))
        ok <- TRUE
      }
    }
    if(x10)
    {
      check <- grep("e-",p2)
      if(length(check)>0)
      {
        p2 <- paste0(gsub("e-","x10^{-",p2),"}")
        p2 <- gsub("x","\u00b7",p2)
        # replace numbers between {} but superscript unicode
        if(!LaTeX)
        {
          p2 <- gsub("\\^","",p2)
          pos <- locate_exponent(p2)
          p3 <- rep(1,2)
          p3[1] <- substr(p2,1,pos[1]-1)
          p3[2] <- substr(p2,pos[1]+1,pos[2]-1)
          p3[2] <- to_superscript(as.character(as.numeric(p3[2])))
          p2 <- paste(p3,collapse="")
        }
      }
    }
    if(LaTeX)
    {
      p2 <- gsub("\u00b7"," \\\\cdot ",p2)
      p2 <- paste0("$",p2,"$")
    }
    p.txt[p.aux] <- p2
  }
  return(p.txt)
}

rounder(0,statistics = T)

# print(rounder(0,digits=2))
# print(rounder(0.12321521312,digits=2))
# print(rounder(0.12321521312,digits=3))
# print(rounder(0.12321321312))
# print(rounder(0.12321521312,digits=5))
# print(rounder(1.546e-45))
# print(rounder(0.0012123123))
# print(rounder("0.0012123123"))
# print(rounder(0.00012123123))
# print(rounder(0.00016123123))
# 
# print(rounder(0.00012723123,digits=6))
# print(rounder("0.000012123123"))
# print(rounder(0.000012123123,digits=7))
# print(rounder(0.000012123123,digits=7,statistics=TRUE))
# print(rounder(0.000012123123,digits=7,force.scientific = TRUE))
# print(rounder(0.000012123123,force.scientific = TRUE))
# print(rounder(1.369e-204))
# print(rounder(1.369e-204,x10=FALSE))
# print(rounder(1.369e-204,LaTeX=TRUE))
# print(rounder(1.369e-204,LaTeX=TRUE,x10=FALSE))
# 

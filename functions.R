# Local functions


#
# Fisher & chisq.tests with error catching
gym.test = function(x,y,...){
  a = chisq.test(x,y,simulate.p.value = T,...)
  b = "fake"
  class(b) = "try-error"
  j = 1
  while("try-error" %in% class(b)){
    workspace = 200000 * j
    b = try(fisher.test(x,y,workspace=workspace),silent = T)
    j = j + 1
    if("try-error" %in% class(b)){
      cat(paste0("Increasing workspace by ",j,"\n"))
    }
    if(j > 20){
      return(print("Workspace coefficient larger than 20! Terminatig"))
    }
  }
  out = c(ChiP = round(a$p.value,3),FisherP = round(b$p.value,3))
  # Return in class for easier bucketing
  print(c(ChiP = round(a$p.value,3),FisherP = round(b$p.value,3)))
  return(out)
}


# Get T/F for questions 9, 10, 11 & 12
get.binary = function(x,rev=F){
  if(!is.numeric(x)){
    return(cat("Please supply numeric vector\n"))
  }
  if(!rev){
    x[x<3] = F
    x[x>2] = T
    return(as.logical(x)) 
  }
  if(rev){
    x[x<3] = T
    x[x>2] = F
    return(as.logical(x)) 
  }
}

# Flip Sc/IG use cols
busted = function(x,a,b){
  tmp = x[,a]
  tmp2 = x[,b]
  x[,a] = tmp2
  x[,b] = tmp
  return(x)
}

brief.plot = function(what){
  par(mfrow=c(1,3),mar=c(4,2,4,1))
  barplot(rbind(unlist(fxb[what]),unlist(fxg[what])),beside=T,main=paste0("FB ",what),
          col = rainbow(2,start=.55,end=0,alpha = .3),
          names.arg = 1:length(unlist(fxb[what])))
  barplot(rbind(unlist(fyb[what]),unlist(fyg[what])),beside=T,add=T,
          col = rainbow(2,start=.55,end=0,alpha = .7),
          names.arg = 1:length(unlist(fxb[what])))
  
  barplot(rbind(unlist(ixb[what]),unlist(ixg[what])),beside=T,main=paste0("IG ",what),
          col = rainbow(2,start=.55,end=0,alpha = .3),
          names.arg = 1:length(unlist(fxb[what])))
  barplot(rbind(unlist(iyb[what]),unlist(iyg[what])),beside=T,add=T,
          col = rainbow(2,start=.55,end=0,alpha = .7),
          names.arg = 1:length(unlist(fxb[what])))
  
  barplot(rbind(unlist(sxb[what]),unlist(sxg[what])),beside=T,main=paste0("SC ",what),
          col = rainbow(2,start=.55,end=0,alpha = .3),
          names.arg = 1:length(unlist(fxb[what])))
  barplot(rbind(unlist(syb[what]),unlist(syg[what])),beside=T,add=T,
          col = rainbow(2,start=.55,end=0,alpha = .7),
          names.arg = 1:length(unlist(fxb[what])))
}

brief.table = function(what,digits=2,...){
  mn = c(mean(dfnum[[what]]),mean(dfnumb[[what]]),mean(dfnuma[[what]]),
         mean(dfnum[[what]][dfnum$q3.1_gender=="kvinde"]),
         mean(dfnum[[what]][dfnum$q3.1_gender=="mand"]),
         mean(dfnumb[[what]][dfnumb$q3.1_gender=="kvinde"]),
         mean(dfnumb[[what]][dfnumb$q3.1_gender=="mand"]),
         mean(dfnuma[[what]][dfnuma$q3.1_gender=="kvinde"]),
         mean(dfnuma[[what]][dfnuma$q3.1_gender=="mand"]))
  sd = c(sd(dfnum[[what]]),sd(dfnumb[[what]]),sd(dfnuma[[what]]),
         sd(dfnum[[what]][dfnum$q3.1_gender=="kvinde"]),
         sd(dfnum[[what]][dfnum$q3.1_gender=="mand"]),
         sd(dfnumb[[what]][dfnumb$q3.1_gender=="kvinde"]),
         sd(dfnumb[[what]][dfnumb$q3.1_gender=="mand"]),
         sd(dfnuma[[what]][dfnuma$q3.1_gender=="kvinde"]),
         sd(dfnuma[[what]][dfnuma$q3.1_gender=="mand"]))
  tbl = rbind(mn,sd)
  rownames(tbl) = c("Mean","St. dev.")
  colnames(tbl) = c("All","All NoEdu","All Edu","All F",
                    "All M","All F NoEdu","All M NoEdu","All F Edu","All M Edu")
  print(kable(tbl,digits=digits,...))
  cat("\n")
}

brief.table.platform = function(what,digits=2,...){
  # Fb
  mnfb = c(mean(dfnum[[what]][dfnum$platform == "fb"]),
           mean(dfnumb[[what]][dfnumb$platform == "fb"]),
           mean(dfnuma[[what]][dfnuma$platform == "fb"]),
         mean(dfnum[[what]][dfnum$q3.1_gender=="kvinde" & dfnum$platform == "fb"]),
         mean(dfnum[[what]][dfnum$q3.1_gender=="mand" & dfnum$platform == "fb"]),
         mean(dfnumb[[what]][dfnumb$q3.1_gender=="kvinde" & dfnumb$platform == "fb"]),
         mean(dfnumb[[what]][dfnumb$q3.1_gender=="mand" & dfnumb$platform == "fb"]),
         mean(dfnuma[[what]][dfnuma$q3.1_gender=="kvinde" & dfnuma$platform == "fb"]),
         mean(dfnuma[[what]][dfnuma$q3.1_gender=="mand" & dfnuma$platform == "fb"]))
  sdfb = c(sd(dfnum[[what]][dfnum$platform == "fb"]),
           sd(dfnumb[[what]][dfnumb$platform == "fb"]),
           sd(dfnuma[[what]][dfnuma$platform == "fb"]),
         sd(dfnum[[what]][dfnum$q3.1_gender=="kvinde" & dfnum$platform == "fb"]),
         sd(dfnum[[what]][dfnum$q3.1_gender=="mand" & dfnum$platform == "fb"]),
         sd(dfnumb[[what]][dfnumb$q3.1_gender=="kvinde" & dfnumb$platform == "fb"]),
         sd(dfnumb[[what]][dfnumb$q3.1_gender=="mand" & dfnumb$platform == "fb"]),
         sd(dfnuma[[what]][dfnuma$q3.1_gender=="kvinde" & dfnuma$platform == "fb"]),
         sd(dfnuma[[what]][dfnuma$q3.1_gender=="mand" & dfnuma$platform == "fb"]))
  #
  # ig
  mnig = c(mean(dfnum[[what]][dfnum$platform == "ig"]),
           mean(dfnumb[[what]][dfnumb$platform == "ig"]),
           mean(dfnuma[[what]][dfnuma$platform == "ig"]),
           mean(dfnum[[what]][dfnum$q3.1_gender=="kvinde" & dfnum$platform == "ig"]),
           mean(dfnum[[what]][dfnum$q3.1_gender=="mand" & dfnum$platform == "ig"]),
           mean(dfnumb[[what]][dfnumb$q3.1_gender=="kvinde" & dfnumb$platform == "ig"]),
           mean(dfnumb[[what]][dfnumb$q3.1_gender=="mand" & dfnumb$platform == "ig"]),
           mean(dfnuma[[what]][dfnuma$q3.1_gender=="kvinde" & dfnuma$platform == "ig"]),
           mean(dfnuma[[what]][dfnuma$q3.1_gender=="mand" & dfnuma$platform == "ig"]))
  sdig = c(sd(dfnum[[what]][dfnum$platform == "ig"]),
           sd(dfnumb[[what]][dfnumb$platform == "ig"]),
           sd(dfnuma[[what]][dfnuma$platform == "ig"]),
           sd(dfnum[[what]][dfnum$q3.1_gender=="kvinde" & dfnum$platform == "ig"]),
           sd(dfnum[[what]][dfnum$q3.1_gender=="mand" & dfnum$platform == "ig"]),
           sd(dfnumb[[what]][dfnumb$q3.1_gender=="kvinde" & dfnumb$platform == "ig"]),
           sd(dfnumb[[what]][dfnumb$q3.1_gender=="mand" & dfnumb$platform == "ig"]),
           sd(dfnuma[[what]][dfnuma$q3.1_gender=="kvinde" & dfnuma$platform == "ig"]),
           sd(dfnuma[[what]][dfnuma$q3.1_gender=="mand" & dfnuma$platform == "ig"]))
  # sc
  mnsc = c(mean(dfnum[[what]][dfnum$platform == "sc"]),
           mean(dfnumb[[what]][dfnumb$platform == "sc"]),
           mean(dfnuma[[what]][dfnuma$platform == "sc"]),
           mean(dfnum[[what]][dfnum$q3.1_gender=="kvinde" & dfnum$platform == "sc"]),
           mean(dfnum[[what]][dfnum$q3.1_gender=="mand" & dfnum$platform == "sc"]),
           mean(dfnumb[[what]][dfnumb$q3.1_gender=="kvinde" & dfnumb$platform == "sc"]),
           mean(dfnumb[[what]][dfnumb$q3.1_gender=="mand" & dfnumb$platform == "sc"]),
           mean(dfnuma[[what]][dfnuma$q3.1_gender=="kvinde" & dfnuma$platform == "sc"]),
           mean(dfnuma[[what]][dfnuma$q3.1_gender=="mand" & dfnuma$platform == "sc"]))
  sdsc = c(sd(dfnum[[what]][dfnum$platform == "sc"]),
           sd(dfnumb[[what]][dfnumb$platform == "sc"]),
           sd(dfnuma[[what]][dfnuma$platform == "sc"]),
           sd(dfnum[[what]][dfnum$q3.1_gender=="kvinde" & dfnum$platform == "sc"]),
           sd(dfnum[[what]][dfnum$q3.1_gender=="mand" & dfnum$platform == "sc"]),
           sd(dfnumb[[what]][dfnumb$q3.1_gender=="kvinde" & dfnumb$platform == "sc"]),
           sd(dfnumb[[what]][dfnumb$q3.1_gender=="mand" & dfnumb$platform == "sc"]),
           sd(dfnuma[[what]][dfnuma$q3.1_gender=="kvinde" & dfnuma$platform == "sc"]),
           sd(dfnuma[[what]][dfnuma$q3.1_gender=="mand" & dfnuma$platform == "sc"]))
  #
  tbl = rbind(mnfb,sdfb,mnig,sdig,mnsc,sdsc)
  nmzz = expand.grid(c("Mean ","St. dev. "),c("FB","IG","SC"))
  rownames(tbl) = paste0(nmzz[,1],nmzz[,2])
  colnames(tbl) = c("All","All NoEdu","All Edu","All F",
                    "All M","All F NoEdu","All M NoEdu","All F Edu","All M Edu")
  # return(tbl)
  print(kable(tbl,digits=digits,...))
  cat("\n")
}


brief.pca = function(what,factors=1,digits=2,caption="",...){
  x = prcomp(what)
  y = factanal(what,factors=factors)
  z = alpha.recursive(what,...)
  tbl = x$rotation
  tbl = cbind(tbl,uniq.=y$uniquenesses)
  tbl = cbind(tbl,unclass(y$loadings))
  colnames(tbl)[(dim(what)[2]+2):ncol(tbl)] = paste0("F",1:ncol(unclass(y$loadings)))
  tbl = data.frame(tbl)
  tbl = cbind(tbl,AlphaDropp = !names(what) %in% names(z))
  rownames(tbl) = sub("\\..*","",rownames(tbl))
  kable(tbl,digits=digits,caption=caption,...)
}



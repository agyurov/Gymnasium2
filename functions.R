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
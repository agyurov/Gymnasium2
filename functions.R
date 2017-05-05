# Local functions


# Fisher & chisq.tests with error catching
gym.test = function(x,y,...){
  a = chisq.test(x,y,simulate.p.value = T,...)
  b = "fake"
  class(b) = "try-error"
  j = 1
  while("try-error" %in% class(b)){
    workspace = 200000 * j
    b = try(fisher.test(x,y,workspace=workspace))
    j = j + 1
    if("try-error" %in% class(b)){
      cat(paste0("Increasing workspace by ",j,"\n"))
    }
    if(j > 20){
      return(print("Workspace coefficient larger than 20! Terminatig"))
    }
  }
  print(x)
  print(unlist(list(ChiP = round(a$p.value,3),FisherP = round(b$p.value,3))))
  return(list(ChiP = round(a$p.value,3),FisherP = round(b$p.value,3)))
}
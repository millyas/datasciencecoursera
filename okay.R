myfunction <- function(x) {
{  for(i in 1:length(x))
{  for(j in 1:(nrow(x[i])))
  if(is.na(x[j,i])==T)
  {
    x <- x[-j,]
    
  }
}
}
return(x)
}
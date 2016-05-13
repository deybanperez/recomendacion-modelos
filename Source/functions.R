install = function(pkg)
{
  # If is is installed does not install packages
  if (!require(pkg, character.only = TRUE))
  {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE))
      stop(paste("load failure:", pkg))
  }
}


subString = function(element)
{
  return (substring(element, 5))
}


changeType = function(element)
{
  return(as.numeric(element))
}

changeFormat = function(element)
{
  classes = c("deportes", "politica", "variedades", "internacional",
              "nacionales", "sucesos", "comunidad", "negocios", "opinion")
  
  article = element%%9; 
  class = element%/%9 + 1;
  
  if(article == 0)
  {
    article = 9;
    class = class-1;
  }
  
  return(paste(classes[class], "/", "articulo",article, sep = ""))
}

convertFormat = function(element)
{
  items = as.integer(element)
  newItems = changeFormat(items[1])
  
  if(length(items) == 1)
  {
    return(newItems)
    
  }else
  {
    for(i in 2:length(items))
    {
      newItems = paste(newItems,",", changeFormat(items[i]), sep = "")
    }
    
    return(newItems)
  }
}

generate_ROC = function(scores, real, target)
{
  scores = as.numeric(scores)
  newOrder = order(scores, decreasing = TRUE)
  scores = scores[newOrder]
  real = real[newOrder]
  returnTP = vector(mode = "numeric")
  returnFP = vector(mode = "numeric")
  scorePrev = Inf
  FP = 0
  TP = 0
  i = 1
  P = length(real[real == target])
  N = length(real) - P
  index = 1
  
  while (i <= length(scores))
  {
    if(scores[i] != scorePrev)
    {
      returnTP[index] = TP/P
      returnFP[index] = FP/N
      scorePrev = scores[i]
      index = index +1
    }
    
    if(real[i] == target)
    {
      TP = TP + 1
    }else
    {
      FP = FP  +1
    }
    i = i+1
  }
  
  returnTP[length(returnTP)+1] = TP/P
  returnFP[length(returnFP)+1] = FP/N
  
  plot(returnFP, returnTP, type = "b", main = "ROC Curve",
       xlab = "FP-Rate", ylab = "TP-Rate", col = "green")
  abline(0,1, col = "blue")
  lines(returnFP,returnTP, col = 1)
  points(returnFP, returnTP, col = 2, pch = 19)
}
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
  scores = order(scores, decreasing = TRUE)
  
  
}
#Changing the work space
setwd("C:/Users/deyban.perez/Documents/Repos/Recomendacion/Script")
#Reading datasets
df_ejemplo = read.csv("../ejemplo.csv", sep = ",")
df_periodico = read.csv("../periodico.csv", sep = ",")
#Here we will start with the logic to detect bots
#Transfor time into POSIX format
entry = as.POSIXct(df_periodico$entry, format = "%Y-%m-%d %H:%M:%S")
exit = as.POSIXct(df_periodico$exit, format = "%Y-%m-%d %H:%M:%S")
#Substract time exit minus entry
diff = difftime(exit, entry, units = "secs")
#Now we should find transactions which average will les < 20 secs
df_periodico$articles = as.character(df_periodico$articles)
strsplit(x = df_periodico$articles, split = "{", )
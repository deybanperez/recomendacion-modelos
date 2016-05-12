#Changing the work space
setwd("C:/Users/deyban.perez/Documents/Repos/Recomendacion")
source(file = "Source/functions.R")#Reading datasets
df_ejemplo = read.csv("../ejemplo.csv", sep = ",")
df_periodico = read.csv("../periodico.csv", sep = ",")
#Here we will start with the logic to detect bots
#Transfor time into POSIX format
entry = as.POSIXct(df_periodico$entry, format = "%Y-%m-%d %H:%M:%S")
exit = as.POSIXct(df_periodico$exit, format = "%Y-%m-%d %H:%M:%S")
#Substract time exit minus entry
diff = difftime(exit, entry, units = "secs")
#Now we split articles into journal and split items individually
df_periodico$articles = as.character(df_periodico$articles)
split = substring(df_periodico$articles, 2)
split = strsplit(x = split, split = ",|}")
#Now we recognize a bot if (time/# transactions < 20)
bots = df_periodico$X[(diff/lengths(split[1:nrow(df_periodico)])) <= 20]
length(bots)
#We will remove bots from dataset
df_periodico = df_periodico[-bots,];
#The new number of rows is the following:
nrow(df_periodico)
######################################################################
#Now we will start with the items from homework
#1) Modify order in the transactions
colnames(df_periodico)[5] = "items"
split = split[-bots]
articles = lapply(split, subString)
df_periodico$articles = lapply(articles, convertFormat)
#3)
articles = df_periodico$articles
class(articles[[1]])
list = lapply(articles,strsplit, split = ",")
list = lapply(list, unlist)
transactions = as(list, "transactions")
sort(itemFrequency(transactions, type = "absolute"), decreasing = TRUE)[1:10]
diff = diff[-bots]
shortest_visit = df_periodico[order(diff),c(2, 6)][1:10,2]
highest_visit = df_periodico[order(diff, decreasing = TRUE),c(2, 6)][1:10,2]
highest_visit
shortest_visit
##############################################################################
#ALgoritmo para las curvas ROC
##############################################################################

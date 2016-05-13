#Changing the work space
setwd("C:/Users/deyban.perez/Documents/Repos/Recomendacion")
source(file = "functions.R")#Reading datasets
install("arules")
install("arulesViz")

df_ejemplo = read.csv("ejemplo.csv", sep = ",")
df_periodico = read.csv("../Data/periodico.csv", sep = ",")
#Here we will start with the logic to detect bots
#Transfor time into POSIX format
nrow(df_periodico)



entry = as.POSIXct(df_periodico$entry, format = "%Y-%m-%d %H:%M:%S")
exit = as.POSIXct(df_periodico$exit, format = "%Y-%m-%d %H:%M:%S")
#Substract time exit minus entry
diff = difftime(exit, entry, units = "secs")
#Now we split articles into journal and split items individually
df_periodico$articles = as.character(df_periodico$articles)
split = substring(df_periodico$articles, 2)
split = strsplit(x = split, split = ",|}")
#Now we recognize a bot if (time/# transactions < 20)
bots = df_periodico$X[(diff/lengths(split[])) <= 20]
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
shortest_visit = as.character(df_periodico$ID[order(diff)][1:10])
highest_visit = df_periodico[order(diff, decreasing = TRUE),c(2, 6)][1:10,2]

##############################################################################
#ALgoritmo para las curvas ROC
##############################################################################
Class = c("p","p","n","p","p","p","n","n","p","n",
          "p","n","p","n","n","n","p","n","p","n")

Score = c("0.9","0.8","0.7","0.6","0.55","0.55","0.55","0.52","0.51","0.505",
          "0.4","0.39","0.38","0.37","0.36","0.35","0.34","0.33","0.30","0.1")
Score = as.numeric(Score)

Target = "p"

returnTP = vector(mode = "numeric", length = length(Score))
returnFP = vector(mode = "numeric", length = length(scores))

generate_ROC(Score, Class, "p")

df_1 = read.csv("roc_1.csv.txt", sep = ",")
generate_ROC(df_1$SCORE, df_1$CLASS, "p")


y = c(2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1)
scores = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.54, 0.53, 0.52, 0.5, 0.5, 0.5, 0.5, 0.38, 0.37, 0.36, 0.35, 0.34, 0.33, 0.30, 0.1)
target = 2

generate_ROC(scores, y, 2)







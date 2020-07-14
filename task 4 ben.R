#Call library 
library(arulesViz)
library(arules)

#RStudio does not inherently understand transactional data. If the dataset is uploaded
#using the read.csv() function, RStudio will try to create variables (our dataset doesn't
#have variables), and you will encounter problems. Consequently, you will need to upload 
#the data through read.transactions() function. 

#read.transactions() function - changes the dataset into a SPARse Matrix. It makes 
#each row represent a transaction and creates columns for each item that a customer 
#might purchase. So Electronidex sells 125 items, so the sparse matric creates 125 
#columns, and changes the data to binary (1 = puchased, 0 = no purchase).

####################
####Loading Data####
####################
transactions <- read.transactions("C:/Users/admin/Desktop/Task 4/ElectronidexTransactions2017.csv"
                                  ,format = "basket"
                                  ,sep = ","
                                  ,rm.duplicates = FALSE
                                  ,cols = NULL)
transactions

############################
####Getting to know Data####
############################
str(transactions) ##see Structure. As you can see, Formal class: 'Transactions'
summary(transactions) ##Most Frequent item is Imac, Apple Earpods, HP Laptop...
#<- the average purchase is 4 items or more.
#<- 2163 items were sold individually
#<- 2 items were purchased 1647 times 
#<- 3 items were purchased 1294 times
#<- 4 items were purchased 1021 times 

inspect (transactions) # You can view the transactions.9835 items <- Transaction ID, 
                       #9835 <- Transaction ID, the others is the basket (Dell Desktop,
                       #DOSS touch Wireless Bluetooth, HP Laptop, etc).
length (transactions) # Number of transactions.
size (transactions) # Number of items per transaction
LIST(transactions) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(transactions)# To see the item labels

##########################################
####Getting to know Data - Visualizing####
##########################################

##Top 10 most frequently bought items

itemFrequencyPlot(transactions
                  ,topN = 10
                  ,main = 'Item Frequency Plot'
                  ,type = "absolute"
                  ,ylab = "Item Frequency (absolute)")

#Data frame for transaction that occurred 1 time. 
Cat1 = transactions[which(size(transactions) == 1),]

#Top 10 most popular products that were purchased alone. 
barplot(sort(itemFrequency(Cat1, type = 'absolute')
             ,decreasing = T)[1:10]
             ,las = 2 
             ,cex.axis = .8)

###################################
####Apply the Apriori Algorithm####
###################################

#The package just installed (arules), contains this algorithm. This algorithm is helpful 
#when using large data sets to help uncover insights pertaining to transaction datasets.

##https://www.youtube.com/watch?v=2otyDYe_V0o##

RulesName<- apriori(transactions, parameter = list(supp = 0.01, conf = 0.4, minlen=2))
##supp: indicates looking for transactions that has at least happend % of the time. 
##  i.e looking at products that are together at least %
##conf: <- Confidence
## default is minlen = 1, but i want 2 (see below for URL):
              #https://www.rdocumentation.org/packages/arules/versions/1.6-4/topics/apriori

inspect(RulesName[1:10]) <#Error in slot(x, s)[i] : subscript out of bounds
  ##WHy did this happen? We have 0 rules (look at previous code output on console)
  #We need to further optimize the parameters.
inspect(head(RulesName))
summary(RulesName)
library(arulesViz)
install.packages("caTools")
library(caTools)
plot(RulesName)   
  
inspect(sort(RulesName[1:10], by = "lift"))

is.redundant(RulesName)



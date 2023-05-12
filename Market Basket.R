library(readxl)
library(plyr)
library(dplyr)
library(knitr)
library(arules)
library(arulesViz)
library(tidyverse)
library(ggplot2)
library(magrittr)

#load data
items <- read_excel('Assignment-1_Data.xlsx')

#removing rows with no items
complete_items <- data.frame(items[complete.cases(items),])
#nrow(complete_items)
#nrow(items)

#combining products with same bill no. and date
transac_data <- ddply(complete_items,c("BillNo","Date"),function(df1)paste(df1$Itemname,collapse= ","))
transac_data$BillNo <- NULL
transac_data$Date <- NULL
colnames(transac_data) <- c("items")

#to csv
write.csv(transac_data, "itemlist_data.csv", quote = FALSE, row.names = FALSE)

#Analysing transaction data
final_transac <- read.transactions("itemlist_data.csv", format = 'basket', sep =',')
summary(final_transac)

#Frequency plot
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
library(RColorBrewer)
itemFrequencyPlot(final_transac, topN=20, type ="absolute", col= brewer.pal(8,'Pastel2'), main ="Item Frequency Plot")

#Apriori Rules
generated.rules <- apriori(final_transac, parameter = list(supp= 0.001, conf= 0.8, maxlen = 10 ))
generated.rules <- sort(generated.rules, by = 'confidence', decreasing = TRUE)
summary(generated.rules)

#rules inspection
inspect(generated.rules[1:5])

#Visualization rules
Rules <- generated.rules[quality(generated.rules)$confidence>0.6]
plot(Rules)
top10Rules <- head(generated.rules, n=10, by="confidence", jitter=0)
plot(top10Rules)







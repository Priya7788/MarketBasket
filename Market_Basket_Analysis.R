install.packages("tidyverse")
install.packages("arules")
install.packages("arulesViz")
insatll.packages("knitr")
install.packages("gridExtra")
install.packages("lubridate")
# Load libraries
library(tidyverse) # data manipulation
library(arules) # mining association rules and frequent itemsets
library(arulesViz) # visualization techniques for association rules
library(knitr) # dynamic report generation
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics
library(lubridate) # work with dates and times


trans <- read.transactions("MBA_Dataset.csv", format = "basket",sep=",")
#view(trans)
trans
view(trans)

# Absolute Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type="absolute",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

# Relative Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type="relative", col="Red", xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot")

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)
rules_sup0.1 <-integer(length =9)


# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(trans, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)){
  
  rules_sup5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)){
  
  rules_sup1[i] <- length(apriori(trans, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)){
  
  rules_sup0.5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
}



# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") +
  theme_bw()


# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 

  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.5%") + 
  
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
plot2
# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
rules_sup1_conf50 <- apriori(trans, parameter=list(sup=supportLevels[3], 
                                                   conf=0.50, target="rules"))
inspect(rules_sup1_conf50)
Final<-as(rules_sup1_conf50,"data.frame")
write.csv(Final,file="Final_rules.csv",row.names = FALSE) 

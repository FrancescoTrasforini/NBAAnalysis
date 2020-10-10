#the following script aims at analyzing the correlation between winning
#a quarter and the final result of NBA playoff games in the 2020 season
my_data = NBA_Playoffs_Results_QbyQ
#load library
library("readxl")
#
count1st = 0
count2nd = 0
count3rd = 0
count4th = 0
ties = 0

for(i in 1:nrow(my_data)){
  if(isTRUE(my_data$Q1HT[i] > my_data$Q1AT[i]) 
     && isTRUE(my_data$TOTHT[i] > my_data$TOTAT[i]) || 
     isTRUE(my_data$Q1AT[i] > my_data$Q1HT[i]) &&
     isTRUE(my_data$TOTAT[i] > my_data$TOTHT[i])){
    count1st = count1st + 1
  }
  if(isTRUE(my_data$Q2HT[i] > my_data$Q2AT[i]) 
     && isTRUE(my_data$TOTHT[i] > my_data$TOTAT[i]) || 
     isTRUE(my_data$Q2AT[i] > my_data$Q2HT[i]) &&
     isTRUE(my_data$TOTAT[i] > my_data$TOTHT[i])){
    count2nd = count2nd + 1
  }
  if(isTRUE(my_data$Q3HT[i] > my_data$Q3AT[i]) 
     && isTRUE(my_data$TOTHT[i] > my_data$TOTAT[i]) || 
     isTRUE(my_data$Q3AT[i] > my_data$Q3HT[i]) &&
     isTRUE(my_data$TOTAT[i] > my_data$TOTHT[i])){
    count3rd = count3rd + 1
  }
  if(isTRUE(my_data$Q4HT[i] > my_data$Q4AT[i]) 
     && isTRUE(my_data$TOTHT[i] > my_data$TOTAT[i]) || 
     isTRUE(my_data$Q4AT[i] > my_data$Q4HT[i]) &&
     isTRUE(my_data$TOTAT[i] > my_data$TOTHT[i])){
    count4th = count4th + 1
  }
  if(!is.na(my_data$OVHT[i])){
    ties = ties + 1
  }
}

#P{"winning 1st quarter and winning game"}
P1 = count1st/i
#P{"winning 2nd quarter and winning game"}
P2 = count2nd/i
#P{"winning 3rd quarter and winning game"}
P3 = count3rd/i
#P{"winning 4th quarter and winning game"}
P4 = count4th/i

prob_vect <- c(P1,P2,P3,P4)
avg <- mean(prob_vect)
sigma2 <- var(prob_vect)
std_dev <- sd(prob_vect)

prob_df <- data.frame(
  event = c("W1->WG","W2->WG","W3-WG","W4->WG"),
  probability = prob_vect
  )

# Load ggplot2
library(ggplot2)

ggplot(prob_df, aes(x=event,y=probability),ylim = 1) + 
         geom_bar(stat="identity") + 
  geom_errorbar( aes(x=event, ymin=probability-std_dev, ymax=probability+std_dev),
                 width=0.4, colour="orange", alpha=0.9, size=1.3)

cat("\014")

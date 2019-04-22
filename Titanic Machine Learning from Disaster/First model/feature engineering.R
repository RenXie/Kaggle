library(tidyverse)
library(forcats)
library(stringr)
library(caTools)
library(dplyr)

library(DT)

library(data.table)

library(pander)

library(ggplot2)

library(scales)

library(grid)

library(gridExtra)

library(corrplot)

library(VIM) 

library(knitr)

library(vcd)

library(caret)

#Check for Missing values


na.cols <- which(colSums(is.na(full)) > 0)
sort(colSums(sapply(full[na.cols], is.na)), decreasing = TRUE)



#Feature engineer

full <- full %>%

  mutate(

    Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),

    `Age Group` = case_when(Age < 13 ~ "Age.0012",

                            Age >= 13 & Age < 18 ~ "Age.1317",

                            Age >= 18 & Age < 60 ~ "Age.1859",

                            Age >= 60 ~ "Age.60Ov"))




### Embarked


full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'S')



### Titles

# Extract an individual's title from the *Name* .


names <- full$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
full$title <- title
table(title)




###MISS, Mrs, Master and Mr are taking more numbers


full$title[full$title == 'Mlle']        <- 'Miss'

full$title[full$title == 'Ms']          <- 'Miss'

full$title[full$title == 'Mme']         <- 'Mrs'

full$title[full$title == 'Lady']          <- 'Miss'

full$title[full$title == 'Dona']          <- 'Miss'





full$title[full$title == 'Capt']        <- 'Officer'

full$title[full$title == 'Col']        <- 'Officer'

full$title[full$title == 'Major']   <- 'Officer'

full$title[full$title == 'Dr']   <- 'Officer'

full$title[full$title == 'Rev']   <- 'Officer'

full$title[full$title == 'Don']   <- 'Officer'

full$title[full$title == 'Sir']   <- 'Officer'

full$title[full$title == 'the Countess']   <- 'Officer'

full$title[full$title == 'Jonkheer']   <- 'Officer'



### Family Groups

full$FamilySize <-full$SibSp + full$Parch + 1

full$FamilySized[full$FamilySize == 1] <- 'Single'

full$FamilySized[full$FamilySize < 5 & full$FamilySize >= 2] <- 'Small'

full$FamilySized[full$FamilySize >= 5] <- 'Big'

full$FamilySized=as.factor(full$FamilySized)




###Tickets


##Engineer features based on all the passengers with the same ticket

ticket.unique <- rep(0, nrow(full))

tickets <- unique(full$Ticket)



for (i in 1:length(tickets)) {

current.ticket <- tickets[i]

party.indexes <- which(full$Ticket == current.ticket)





for (k in 1:length(party.indexes)) {

ticket.unique[party.indexes[k]] <- length(party.indexes)

}

}



full$ticket.unique <- ticket.unique





full$ticket.size[full$ticket.unique == 1]   <- 'Single'

full$ticket.size[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'

full$ticket.size[full$ticket.unique >= 5]   <- 'Big'


### Independent Variable/Target



### Survival



full <- full %>%

mutate(Survived = case_when(Survived==1 ~ "Yes",

Survived==0 ~ "No"))



crude_summary <- full %>%

filter(set=="train") %>%

select(PassengerId, Survived) %>%

group_by(Survived) %>%

summarise(n = n()) %>%

mutate(freq = n / sum(n))



crude_survrate <- crude_summary$freq[crude_summary$Survived=="Yes"]



kable(crude_summary, caption="2x2 Contingency Table on Survival.", format="markdown")


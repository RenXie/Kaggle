data$GroupId2 = paste( data$Surname, data$Pclass,sub('.$','X',data$Ticket),data$Fare,data$Embarked,sep='-')
for (i in 1:1309){
  if (data$Title[i]=='man' & data$GroupId2[i] %in% x)
    if (data$GroupSurvival[data$GroupId==data$GroupId2[i]][1]==0)
      data$GroupSurvival[i] <- 0
}
x = which(data$Passenger<=891 & data$GroupSurvival!=0 & data$GroupSurvival!=1 & data$Sex=='male')
data2 <- data[x,]
data2$Survived <- factor(data2$Survived)
g1 = ggplot(data=data2) +
  geom_bar(stat='count',aes(x=Pclass,fill=Survived))
g2 = ggplot(data=data2[!is.na(data2$Age),]) +
  geom_histogram(bins=20,aes(x=Age,fill=Survived))
g3 = ggplot(data=data2) +
  geom_bar(stat='count',aes(x=Embarked,fill=Survived))
g4 = ggplot(data=data2) +
  geom_bar(stat='count',aes(x=CabinLetter,fill=Survived))
grid.arrange(g1,g2,g3,g4,nrow=2,top='Analysis of training set\'s 524 MALE non-woman-child-group passengers')
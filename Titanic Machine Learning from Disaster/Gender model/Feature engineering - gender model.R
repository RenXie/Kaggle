train <- read.csv('train.csv',stringsAsFactors=F)
test <- read.csv('test.csv',stringsAsFactors=F)
test$Survived <- NA; data <- rbind(train,test)
data$Surname = substring( data$Name,0,regexpr(',',data$Name)-1)
data$GroupId = paste( data$Surname, data$Pclass, sub('.$','X',data$Ticket), data$Fare, data$Embarked, sep='-')
data[c(195,1067,59,473,1142),c('Name','GroupId')]

# engineer titles
data$Title <- 'man'
data$Title[data$Sex=='female'] <- 'woman'
data$Title[grep('Master',data$Name)] <- 'boy'
# color variable is used in plots below
data$Color <- data$Survived
# engineer "woman-child-groups"
data$GroupId[data$Title=='man'] <- 'noGroup'
data$GroupFreq <- ave(1:1309,data$GroupId,FUN=length)
data$GroupId[data$GroupFreq<=1] <- 'noGroup'
data$TicketId = paste( data$Pclass,sub('.$','X',data$Ticket),data$Fare,data$Embarked,sep='-')
count = 0
# add nannies and relatives to groups
for (i in which(data$Title!='man' & data$GroupId=='noGroup')){
  data$GroupId[i] = data$GroupId[data$TicketId==data$TicketId[i]][1]
  if (data$GroupId[i]!='noGroup') {
    # color variable is used in plots below
    if (is.na(data$Survived[i])) data$Color[i] = 5
    else if (data$Survived[i] == 0) data$Color[i] = -1
    else if (data$Survived[i] == 1) data$Color[i] = 2
    count = count + 1
  }
}

#Classify unknown woman-child-groups
library(ggplot2)
library(gridExtra)
data$GroupName = substring( data$GroupId,0,regexpr('-',data$GroupId)-1)
data$Color[is.na(data$Color) & data$Title=='woman'] <- 3
data$Color[is.na(data$Color) & data$Title=='boy'] <- 4
x = data$GroupId[data$GroupId!='noGroup']; x = unique(x); x=x[order(x)]
plotData <- list(); g <- list()
for (i in 1:3) plotData[[i]] <- data[data$GroupId %in% x[(27*(i-1))+1:27],]
for (i in 1:3) g[[i]] = ggplot(data=plotData[[i]], aes(x=0,y=factor(GroupName))) +
  geom_dotplot(dotsize=0.9,binwidth=1,binaxis='y',method="histodot",stackgroups=T,
               aes(fill=factor(Color),color=Title )) +
  scale_color_manual(values=c('gray70','blue','gray70'),limits=c('man','boy','woman')) +
  scale_fill_manual(values=c('#BB0000','#FF0000','#009900','#00EE00','gray70','gray70','white'),
                    limits=c('0','-1','1','2','3','4','5')) +
  scale_y_discrete(limits = rev(levels(factor(plotData[[i]]$GroupName)))) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.position='none')
grid.arrange(g[[1]],g[[2]],g[[3]],nrow=1,top='All 80 woman-child-groups in the test and training datasets combined (228 passengers).
Red = deceased female or boy, Green = survived, White or Gray = unknown survival, 
White or LightGreen or LightRed = different surname same ticket, Blue outline = boy')


# engineer titles on training set
train$Title <- 'man'
train$Title[train$Sex=='female'] <- 'woman'
train$Title[grep('Master',train$Name)] <- 'boy'
# Perform 25 trials of 10-fold cross validation
trials = 25; sum = 0
for (j in 1:trials){
  x = sample(1:890); s = 0
  for (i in 0:9){
    # engineer "woman-child-groups"
    train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
    train$GroupId = paste( train$Surname, train$Pclass,sub('.$','X',train$Ticket),train$Fare,train$Embarked,sep='-')
    train$GroupId[train$Title=='man'] <- 'noGroup'
    train$GroupFreq <- ave(1:891,train$GroupId,FUN=length)
    train$GroupId[train$GroupFreq<=1] <- 'noGroup'
    # add nannies and relatives to groups.
    train$TicketId = paste( train$Pclass,sub('.$','X',train$Ticket),train$Fare,train$Embarked,sep='-')
    for (k in which(train$Title!='man' & train$GroupId=='noGroup'))
      train$GroupId[k] = train$GroupId[train$TicketId==train$TicketId[k] & train$PassengerId != train$PassengerId[k]][1]
    train$GroupId[is.na(train$GroupId)] <- 'noGroup'
    train$GroupFreq <- ave(1:891,train$GroupId,FUN=length)
    # calculate training subset's group survival rate
    train$GroupSurvival <- NA
    train$GroupSurvival[-x[1:89+i*89]] <- ave(train$Survived[-x[1:89+i*89]],train$GroupId[-x[1:89+i*89]])
    # calculate testing subset's group survival rate from training set's rate
    for (k in x[1:89+i*89]){ 
      train$GroupSurvival[k] <- train$GroupSurvival[which(!is.na(train$GroupSurvival) & train$GroupId==train$GroupId[k])[1]]
      if (is.na(train$GroupSurvival[k])) train$GroupSurvival[k] <- ifelse(train$Pclass[k]==3,0,1)
    }
    # apply gender model plus WCG
    train$predict <- 0
    train$predict[train$Title=='woman'] <- 1
    train$predict[train$Title=='boy' & train$GroupSurvival==1] <- 1
    train$predict[train$Title=='woman' & train$GroupSurvival==0] <- 0
    c = sum(abs(train$predict[x[1:89+i*89]] - train$Survived[x[1:89+i*89]]))
    s = s + c
  }
  #cat( sprintf("Trial %d has 10-fold CV accuracy = %f\n",j,1-s/890))
  sum = sum + 1-s/890
}

#Prediction
data$GroupSurvival <- NA
data$Survived <- as.numeric(as.character(data$Survived))
data$GroupSurvival[1:891] <- ave(data$Survived[1:891],data$GroupId[1:891])
for (i in 892:1309) data$GroupSurvival[i] <- data$GroupSurvival[which(data$GroupId==data$GroupId[i])[1]]
data$GroupSurvival[is.na(data$GroupSurvival) & data$Pclass==3] <- 0
data$GroupSurvival[is.na(data$GroupSurvival) & data$Pclass!=3] <- 1
data$Predict <- 0
data$Predict[data$Sex=='female'] <- 1
data$Predict[data$Title=='woman' & data$GroupSurvival==0] <- 0
data$Predict[data$Title=='boy' & data$GroupSurvival==1] <- 1

#gender model
submit <- data.frame(PassengerId = 892:1309, Survived = data$Predict[892:1309])
write.csv(submit,'genderSurnameModel2.csv',row.names=F)
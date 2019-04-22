#SURVIVAL RATE BY CLASS
ggplot(full %>% filter(set=="train"), aes(Pclass, fill=Survived)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by Class") + 
  
  theme_minimal()

#Survival rate by sex
ggplot(full %>% filter(set=="train"), aes(Sex, fill=Survived)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by Sex") + 
  
  theme_minimal()

#Survival rate by age

tbl_age <- full %>%
  
  filter(set=="train") %>%
  
  select(Age, Survived) %>%
  
  group_by(Survived) %>%
  
  summarise(mean.age = mean(Age, na.rm=TRUE))



ggplot(full %>% filter(set=="train"), aes(Age, fill=Survived)) +
  
  geom_histogram(aes(y=..density..), alpha=0.5) +
  
  geom_density(alpha=.2, aes(colour=Survived)) +
  
  geom_vline(data=tbl_age, aes(xintercept=mean.age, colour=Survived), lty=2, size=1) +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_colour_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Density") +
  
  ggtitle("Survival Rate by Age") + 
  
  theme_minimal()


#Survival rate by age groups
ggplot(full %>% filter(set=="train" & !is.na(Age)), aes(`Age Group`, fill=Survived)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by Age Group") + 
  
  theme_minimal()

#Survival rate by SibSp
ggplot(full %>% filter(set=="train"), aes(SibSp, fill=Survived)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by SibSp") + 
  
  theme_minimal()

#Survival rate by Parch
ggplot(full %>% filter(set=="train"), aes(Parch, fill=Survived)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by Parch") + 
  
  theme_minimal()

#Survival rate by Embarked
ggplot(full %>% filter(set=="train"), aes(Embarked, fill=Survived)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by Embarked") + 
  
  theme_minimal()

#Survival rate by Title
ggplot(full %>% filter(set=="train") %>% na.omit, aes(title, fill=Survived)) +
  
  geom_bar(position="fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by Title") + 
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Survival rate by Family
ggplot(full %>% filter(set=="train") %>% na.omit, aes(`FamilySize`, fill=Survived)) +
  
  geom_bar(position="fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by Family Group") + 
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



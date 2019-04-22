ggplot(full %>% filter(set=="dataset"), aes(Pclass, fill=Survived)) +
  
  geom_bar(position = "fill") +
  
  scale_fill_brewer(palette="Set1") +
  
  scale_y_continuous(labels=percent) +
  
  ylab("Survival Rate") +
  
  geom_hline(yintercept=crude_survrate, col="white", lty=2, size=2) +
  
  ggtitle("Survival Rate by Class") + 
  
  theme_minimal()
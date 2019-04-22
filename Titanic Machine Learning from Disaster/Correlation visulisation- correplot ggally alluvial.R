library('corrplot')

tbl_corr <- full %>%
  
  filter(set=="train") %>%
  
  select(-PassengerId, -SibSp, -Parch) %>%
  
  select_if(is.numeric) %>%
  
  cor(use="complete.obs") %>%
  
  corrplot.mixed(tl.cex=0.85)

library(GGally)
lm.plt <- function(data, mapping, ...){
  plt <- ggplot(data = data, mapping = mapping) + 
    geom_point(shape = 20, alpha = 0.7, color = 'darkseagreen') +
    geom_smooth(method=loess, fill="red", color="red") +
    geom_smooth(method=lm, fill="blue", color="blue") +
    theme_minimal()
  return(plt)
}
tbl_numeric <- full %>%
  
  filter(set=="train") %>%
  
  select(-PassengerId, -SibSp, -Parch) %>%
  
  select_if(is.numeric)
ggpairs(tbl_numeric, tbl_numeric, lower = list(continuous = lm.plt))

library(alluvial)
tbl_summary <- full %>%
  
  filter(set=="train") %>%
  
  group_by(Survived, Sex, Pclass, `Age Group`, title) %>%
  
  summarise(N = n()) %>% 
  
  ungroup %>%
  
  na.omit



alluvial(tbl_summary[, c(1:5)],
         
         freq=tbl_summary$N, border=NA,
         
         col=ifelse(tbl_summary$Survived == "Yes", "blue", "gray"),
         
         cex=0.65,
         
         ordering = list(
           
           order(tbl_summary$Survived, tbl_summary$Pclass==1),
           
           order(tbl_summary$Sex, tbl_summary$Pclass==1),
           
           order(tbl_summary$title, tbl_summary$Pclass==1),
           NULL,
           
           NULL))
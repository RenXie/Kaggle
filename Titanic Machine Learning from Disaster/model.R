############ LIBRARIES #####

library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)
library(gbm)

# GBM Model
start =  Sys.time()
gbm_model  =  gbm(item_cnt_day ~ shop_id + item_id,
                  data = sales_data,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 5000,
                  interaction.depth = 3, 
                  bag.fraction = 0.7,
                  train.fraction = 0.8,
                  # cv.folds = 5,
                  n.cores = NULL,
                  verbose = T)

end = Sys.time()
print(end - start)

result2 = predict(gbm_model,newdata = test_data[,c("shop_id","item_id")], n.trees = 5000 )

sub2 = data.frame(ID = test_data$ID, 
                  item_cnt_month =  result2)

write.csv(sub2, "submission.csv", row.names = F)
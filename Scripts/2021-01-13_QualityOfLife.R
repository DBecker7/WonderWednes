library(GGally)
library(dplyr)

qol <- read.csv("Data/2021-01-13-QualityOfLife.csv")

head(qol)
glimpse(qol)

qol2 <- qol %>% 
    mutate_if(function(x){length(unique(x)) < 6}, factor)

ggpairs(qol2[qol2$VISIT == "Week 16", c(2, 3, 15)], mapping = aes(colour = TRT))
ggpairs(qol[qol$VISIT == "Week 16", 5:14])
ggpairs(qol2[qol2$VISIT == "Week 16", 5:14])
ggpairs(qol[qol$VISIT == "Week 16", 5:14], mapping = aes(colour = qol[qol$VISIT == "Week 16",]$TRT))
ggpairs(qol2[qol2$VISIT == "Week 16", 5:14], mapping = aes(colour = qol2[qol2$VISIT == "Week 16",]$TRT))


#### Goal 1: Multidimensional nature of DLQI ####
# As integers, they're all highly correlated
# As categorical variables, need an intelligent way to summarise
    # Something like PCA or NMF, but for categories?
    # Collection of stacked bar plots?
    # Percent agreement between categories?
    # Compound Poisson model for discrete categories?
#### Goal 2: Show the effect of treatment, incorporate multidimensionality ####


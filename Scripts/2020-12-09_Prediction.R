library(ggplot2)
library(dplyr)
library(GGally)

rex <- read.csv("Data/2020-12-09_Prediction")

head(rex)
glimpse(rex)

rex <- rex %>% 
    mutate(RE = factor(RE), 
        hist = factor(hist), 
        mult.foc = factor(mult.foc),
        acc.in.situ = factor(acc.in.situ),
        lymph.inv = factor(lymph.inv),
        estr.rec = factor(estr.rec), 
        prog.rec = factor(prog.rec))


ggpairs(rex, mapping = aes(colour = RE))


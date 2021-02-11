library(tidyr)
library(dplyr)
library(ggplot2)

#missme <- read.csv('https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2021/2021-02-10/missing_data.csv')
#write.csv(missme, file = "Data/2021-02-10_missme.csv", row.names = FALSE)

missme <- read.csv("Data/2021-02-10_missme.csv")

glimpse(missme)


apply(missme, 2, function(x) sum(is.na(x)))

#missme$NAs <- apply(missme, 1, function(x) sum(is.na(x)))
#missme$row <- 1:nrow(missme)


rowna <- apply(missme, 2, is.na) %>% 
    as.data.frame()
rowna$NAs <- apply(rowna, 1, sum)
rowna$row <- 1:nrow(rowna)
rowna <- rowna %>% 
    pivot_longer(cols = !c("row", "NAs")) 
roworder <- rowna %>% 
    group_by(row) %>% 
    summarise(total = sum(value), .groups = "drop") %>% 
    mutate(roword = rank(total, ties.method = "first"))
roworder

myvars <- unique(rowna$name)
dots <- myvars[grepl(x = myvars, pattern = "\\.")]
nodots <- myvars[!grepl(x = myvars, pattern = "\\.")]

dotord <- strsplit(dots, "\\.") %>% 
    sapply(function(x) x[length(x)]) %>% 
    as.numeric() %>% 
    order()
dots <- dots[dotord]

varorder <- c(dots, nodots)
varorder


rowna %>% 
    left_join(roworder, by = "row") %>% 
    filter(NAs > 0) %>% 
    mutate(name = factor(name, levels = varorder, ordered = TRUE)) %>% 
    ggplot() + 
        aes(y = name, x = roword, fill = value, colour) + 
        geom_tile(colour = "white") +
        scale_fill_manual(values = c("white", "black")) +
        theme_bw()

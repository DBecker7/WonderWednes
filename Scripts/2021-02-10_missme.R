library(tidyr)
library(dplyr)
library(ggplot2)

# Challenge
    # 1. Show different patterns of missing values
    # 2. Visualize the potential impact of missing values on the comparison of the two treatment arms.

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
nodots <- myvars[!grepl(x = myvars, pattern = "\\.")] %>% sort()
nodots

dotord <- strsplit(dots, "\\.") %>% 
    sapply(function(x) x[length(x)]) %>% 
    as.numeric() %>% 
    order()
dots <- dots[dotord]

varorder <- c(dots, nodots)
varorder


rowna %>% 
    left_join(roworder, by = "row") %>% 
    filter(NAs > 0, startsWith(name, "pain.bin")) %>% 
    mutate(name = factor(name, levels = varorder, ordered = TRUE)) %>% 
    ggplot() + 
        aes(y = name, x = roword, fill = value, colour) + 
        geom_tile(colour = "white") +
        theme_bw() +
        labs(title = "Missingness, by row",
            subtitle = "Only rows with missing values are shown",
            x = "Patient, ordered by amount of missingness",
            y = NULL,
            fill = NULL) +
        coord_flip() +
        scale_fill_manual(values = c("white", "black"), 
            labels = c("", "Missing")) +
        scale_y_discrete(labels = paste0("Visit ", 1:10)) +
        scale_x_continuous(labels = "", breaks = 0)

rowna %>% 
    left_join(roworder, by = "row") %>% 
    filter(NAs > 0, startsWith(name, "pain.bin")) %>% 
    mutate(name = factor(name, levels = varorder, ordered = TRUE)) %>% 
    ggplot() + 
        aes(y = name, x = roword, fill = value, colour) + 
        geom_tile(colour = "white") +
        theme_bw() +
        labs(title = "Missingness, by patient",
            subtitle = "Only rows with missing values are shown",
            x = "Patient, ordered by amount of missingness",
            y = NULL,
            fill = NULL) +
        coord_flip() +
        scale_fill_manual(values = c("white", "black"), 
            labels = c("", "Missing")) +
        scale_y_discrete(labels = paste0("Visit ", 1:10)) +
        scale_x_continuous(labels = "", breaks = 0)

glimpse(missme)

# Vis 2: Directed graph

# Idea: from 0 missingness, to _blank missingness, from 1 in a row, to _
pains <- select(missme, starts_with("pain.bin"))
painna <- as.data.frame(apply(pains, 2, is.na))
names(painna) <- paste0("Visit ", 1:10)
painna$`Visit 0` <- FALSE
painna <- select(painna, `Visit 0`, everything())

edgemat <- matrix(NA, ncol = 3)
for(row in 1:nrow(pains)){
    for(col in 2:(ncol(pains) + 1)){
        from <- paste0(names(painna)[col-1], 
            ifelse(painna[row, col-1], " absent", " present"))
        to <- paste0(names(painna)[col], 
            ifelse(painna[row, col], " absent", " present"))
        value <- 1
        edgemat <- rbind(edgemat, c(from, to, value))
    }
}

edgedf <- as.data.frame(edgemat)
names(edgedf) <- c("source", "target", "value")
edgedf$value <- as.numeric(edgedf$value)
edges <- edgedf %>% 
    group_by(source, target) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    as.data.frame()
edges <- edges[complete.cases(edges),]

nodes <- data.frame(name = sort(unique(c(edges$source, edges$target))))
nodes$group <- ifelse(grepl("present", nodes$name), "a", "b")
my_colour <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["grey", "red"])'

edges$group <- ifelse(grepl("present", edges$target), "a", "b")

edges$source <- unlist(sapply(edges$source, 
    function(x) which(nodes$name == x) - 1))
edges$target <- unlist(sapply(edges$target, 
    function(x) which(nodes$name == x) - 1))


library(networkD3)
sankeyNetwork(Links = edges, Nodes = nodes, 
    Source = "source", Target = "target", Value = "value", NodeID = "name", 
    fontSize = 8, nodePadding = 30, nodeWidth = 5, 
    NodeGroup = "group", LinkGroup = "group", colourScale = my_colour,
    height = 500, width = 1300)

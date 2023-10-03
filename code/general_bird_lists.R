

library(tidyverse)

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")

full_bird_list1 <- read.csv("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/bird_taxa_list.csv")

ardeids <- bird_taxa_filter(full_bird_list1, join_taxa = c("alpha.code", "alpha.code"), keep_taxa = "Ardeidae")


shorebirds <- bird_taxa_filter(full_bird_list1, join_taxa = c("alpha.code", "alpha.code"), keep_taxa = c("Scolopacidae", "Recurvirostridae", "Charadriidae", "Haematopodidae"))


duck_taxa <- filter(full_bird_list1, grepl("Duck|Teal", common.name)) %>% 
  select(order, family, subfamily) %>% 
  distinct()

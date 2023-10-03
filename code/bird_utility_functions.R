


# given a data frame containing a column with bird species identifier plus any other data, filter to various taxonomic groups. 
# can filter on multiple taxonomic levels simultaneously
# allows multiple options for species identifier, and has capacity to specify name of species identifier in data frame on the fly (not ahead of time)

# source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.r")

library(tidyverse)
#library(tabulizer)
#library(rlang)
#library(rvest)
#library(here)

# compile full bird list
compile_bird_list <- function(){
      full_bird_list1 <- read.csv("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/bird_taxa_list.csv") %>% 
  dplyr::select(-taxonomic.order) 
bird_lumpies <- read.csv("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/bird_lumpies.csv") %>%
  dplyr::select(-group.spp)

manual_species <- read.csv("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/manual_species.csv")

full_bird_list <- rbind(full_bird_list1, bird_lumpies, manual_species) %>% 
  mutate_if(is.factor, as.character) %>% 
  filter(!is.na(species)) %>% 
  distinct()
saveRDS(full_bird_list, "C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/full_bird_list")

}
compile_bird_list()


# bird taxa filter ----
bird_taxa_filter <- function(data_file, 
                             join_taxa = c("alpha.code", "species"), 
                             keep_taxa,
                             drop_cols) {

# arguments
  # data_file: your bird data with an allowable species identifier, either AOU Alpha Code (Alpha.Code), Numeric Code (Species.Number), Common Name (Common.Name), or Scientific Name (Scientific.Name)
  # join_taxa: 2 character string, first character indicates which species identifier is common between data file and full_bird_list, second character indicates name of this field in data_file
  #keep_taxa: names of the taxa to want to filter to; can be names of different taxonomic levels, e.g. c("Charadriiformes", "Falconidae"); can also leave this blank to keep all species in data_file but add common names, sci names, other taxonomic info
  # drop_cols: which columns from full_bird_list do you want to remove; default keeps only Alpha.Code and Common.Name
  
  
  
full_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/full_bird_list")


burd_list_join = join_taxa[1]
data_join = join_taxa[2]


zdata_file <- data_file %>% 
  rename(!!burd_list_join := all_of(data_join))

foo <- left_join(zdata_file, full_bird_list)


if(missing(keep_taxa)){
  foo <- foo
} else {
foo <- foo %>% 
  filter_all(any_vars(. %in% keep_taxa))%>% 
  droplevels()
}



if(missing(drop_cols)){
  foo <- foo
} else {
foo <- foo %>% 
  dplyr::select(-one_of(drop_cols))
}
 
return(foo)
}

#xxx <- read.csv("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/bird_taxa_list.csv") %>% select(species = alpha.code) %>% bird_taxa_filter(filter_taxa = "Charadriiformes")

# bird names from text ----
bird_names_from_text <- function(x) {

full_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/full_bird_list")


bird_names <- rbind(full_bird_list %>% 
                      select(code.name = common.name),
                    full_bird_list %>% 
                      select(code.name = alpha.code)) %>% 
  mutate(code.name = tolower(code.name)) %>% 
  summarise(paste(code.name, collapse = "|"))

out_names <- paste(str_extract_all(tolower(x), bird_names[[1]]))
out_names <- gsub("c[()]", "", out_names)
out_names <- gsub("[()]", "", out_names)
out_names <- gsub(paste(c("character0", '"'), collapse = "|"), "", out_names)
out_names <- gsub(", ", "_", out_names)
return(out_names)
}

# bird_names_from_text("Bald Eagle and White-tailed Kite and WREN are birds.")


# translate bird names between common name, alpha code, sci name, etc. 
# from.name and to.name can be any of c("alpha.code", "common.name", "order", "family", "subfamily", "genus", "species", "species.number")
translate_bird_names <- function(x, from.name, to.name) {

full_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/full_bird_list")

from_names <- tolower(full_bird_list[,from.name])
to_names <- full_bird_list[,to.name]

#names(to_names) <- from_names

#str_replace_all(tolower(x), to_names)
plyr::mapvalues(tolower(x), from_names, to_names, warn_missing = FALSE)

}

# translate_bird_names("WREN", "alpha.code", "species")




is_spp_group <- function(df) {
lumpies <- read.csv("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/bird_lumpies.csv") %>% 
  mutate(is.spp.group = T) %>% 
  select(alpha.code, is.spp.group, group.spp)

df <- df %>% 
  left_join(., lumpies) %>% 
  droplevels() %>% 
  mutate(is.spp.group = ifelse(is.na(is.spp.group), F, is.spp.group))

}

#
add_group_name <- function(df){
  lumpies <- read.csv("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/bird_lumpies.csv") %>% 
  mutate(is.spp.group = T) %>% 
  select(alpha.code, is.spp.group, group.spp)

long_lumpies <- lumpies %>% 
  select(-is.spp.group) %>% 
  separate(group.spp, into = paste("spp", seq(1, 4), sep = "_")) %>% 
  pivot_longer(cols = contains("spp")) %>%
  filter(!is.na(value)) %>% 
  select(-name) %>% 
  rename(group.alpha.code = alpha.code, alpha.code = value)

expanded_lumpies <- long_lumpies %>% 
  distinct(group.alpha.code) %>% 
  mutate(alpha.code = group.alpha.code) %>% 
  rbind(., long_lumpies) %>% 
  arrange(group.alpha.code) %>% 
  inner_join(., df)
  
}

# fix incorrect 4-letter codes

fix_4letter_codes <- function(df) {
  df <- df %>% 
    mutate(alpha.code = as.character(alpha.code)) %>% 
  mutate(alpha.code = case_when(alpha.code == "REHE" ~ "REDH",
                             alpha.code == "BRCO" ~ "BRAC",
                             alpha.code == "BRAN" ~ "BLBR",
                             alpha.code == "GWTE" ~ "AGWT",
                             alpha.code == "OLDS" ~ "LTDU",
                             alpha.code == "NOSH" ~ "NSHO",
                             alpha.code == "TEAL" ~ "UNTE",
                             alpha.code == "HADU" ~ "HARD",
                             alpha.code == "COMO" ~ "COGA",
                             alpha.code == "COSN" ~ "WISN",
                             alpha.code == "CAGO" ~ "CANG",
                             alpha.code == "WESJ" ~ "CASJ",
                             TRUE ~ alpha.code)) 
    
}


# make_bird_list #####################################################

make_bird_list <- function(from = c("web"), read_from = NA, write_list = TRUE, write_to) {
# this is mostly a support function for bird_taxa_filter  
# create list of bird species with taxonomic info and AOU alpha code
# info sourced from 2 websites (links below)
# INPUT:  
# use from = "web" to read tables direct from websites, 
#  from = "disk" to read from location saved to disk; specify location with read_from, save csv's as birdpop_list and aou_list
#  use write_list = TRUE to save to disk, specify location with write_to
#  write_list = FALSE returns the data frame
# OUTPUT: data frame with columns: common.name, alpha.code, species, order, family, subfamily, genus
    
# from here: https://www.birdpop.org/pages/birdSpeciesCodes.php
# IBP list has 4-letter codes
if(from == "web") {  
birdpop <- extract_tables("https://www.birdpop.org/docs/misc/Alpha_codes_tax.pdf")
birdpop2 <- do.call(rbind, birdpop[-length(birdpop)])
birdpop_df <- birdpop2 %>% 
  data.frame() %>% 
  rename(common.name = 2, alpha.code = 3, species = 4) %>% 
  dplyr::select(common.name, alpha.code, species) %>% 
  mutate(alpha.code = gsub("\\*", "", alpha.code))
}
if(from == "disk") {  
birdpop_df <- read.csv(paste(read_from, "birdpop_list.csv", sep = "")) %>% 
  dplyr::select(alpha.code = SPEC, common.name = COMMONNAME)
}
  
#--  
# from here: http://checklist.aou.org/taxa
# AOU list has higher taxa classifications
if(from == "web") {
aou <- read.csv("http://checklist.aou.org/taxa.csv?type=charset%3Dutf-8%3Bsubspecies%3Dno%3B") %>% 
  dplyr::select(common.name = common_name, order, family, subfamily, genus, species)
} 
 if(from == "disk") { 
aou <- read.csv(paste(read_from, "NACC_list_species.csv", sep = "")) %>% 
  dplyr::select(common.name = common_name, order, family, subfamily, genus, species)
}

# bbl list has taxonomic numbers  
url <- "https://www.pwrc.usgs.gov/bbl/manual/speclist.cfm"
bbl <- url %>%
  xml2::read_html() %>%
  html_nodes(xpath='//*[@id="spectbl"]') %>%
  html_table()  
bbl_list <- bbl[1] %>% 
  data.frame() %>% 
  rename_all(tolower) %>% 
  select(alpha.code, common.name, species.number, taxonomic.order)

  
bird_list <- full_join(birdpop_df, aou) %>% 
  full_join(bbl_list) %>% 
  mutate(species.number = ifelse(alpha.code == "NOFL", 4125, species.number),
         species.number = ifelse(alpha.code == "DEJU", 5677, species.number))
if(write_list == TRUE) {
write.csv(bird_list, write_to, row.names = F)
} else {
  return(bird_list)
}
}

#bird_list <- make_bird_list(from = "web", write_list = TRUE, write_to = "C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/bird_taxa_list.csv")

#bird_list <- make_bird_list(from = "disk", read_from = "C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/", write_list = TRUE, write_to = "C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/utility_function_data/bird_taxa_list.csv")




library("tidyverse")

# Set Up ------------------------------------------------------
FTP_files <- read_delim("/home/daniel/code/BW6/BW6_documentation/Documents/FTP_SERVER_FILES.csv", delim = ",")

FTP_files %>% count(file_type)
FTP_files %>%
    filter(file_type == "GWAS" | file_type == "GWAS_BW3") %>%
    count(file_type)


GWAS_FILES <- FTP_files %>% filter(file_type == "GWAS" | file_type == "GWAS_BW3")
GWAS_FILES %>%
    count(STUDY) %>%
    select(STUDY) %>%
    pull()





overview_db <- read_delim("/home/daniel/code/BW6/Data/Study_overview_Dan.csv", delim = "\t")
overview_db
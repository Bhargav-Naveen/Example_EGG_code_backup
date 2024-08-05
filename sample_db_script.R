
# Load Libraries: ------------------------------------------------------
library(tidyverse)
library(glue)

# Define Directories: ------------------------------------------------------
database_dir <- "/home/daniel/code/BW6/BW6_database/data"

# define paths to files
STUDY_INFO_path <- glue("{database_dir}/STUDY_INFO.tsv")
FTP_FILES_path <- glue("{database_dir}/FTP_FILES.tsv")
Demo_QC_table_path <- glue("{database_dir}/DEMO_QC_DB/full_table_cleaned.tsv")
Demographic_table_path <- glue("{database_dir}/DEMO_QC_DB/demographic_cleaned.tsv")
QC_table_path <- glue("{database_dir}/DEMO_QC_DB/technical_cleaned.tsv")


# Load Data: ------------------------------------------------------
STUDY_INFO_DF <- read_delim(STUDY_INFO_path, delim = "\t")
FTP_FILES_DF <- read_delim(FTP_FILES_path, delim = "\t")
Demo_QC_DF <- read_delim(Demo_QC_table_path, delim = "\t")
Demo_cleaner_DF <- read_delim(Demographic_table_path, delim = "\t")


# Combine: ------------------------------------------------------

STUDY_INFO_DF$PROCESSING_ID[duplicated(STUDY_INFO_DF$PROCESSING_ID)]
# Select distinct rows for STUDY info
STUDY_INFO_DF <- STUDY_INFO_DF %>%
    select(PROCESSING_ID, META_STUDY_ID) %>%
    distinct()

demo_cols <- c("PROCESSING_ID", "BW_fathers_Sample_size")
FULL_DB <- FTP_FILES_DF %>%
    full_join(Demo_cleaner_DF[demo_cols], by = "PROCESSING_ID") %>%
    full_join(STUDY_INFO_DF %>% select(PROCESSING_ID, META_STUDY_ID), by = "PROCESSING_ID")

# Explore: ------------------------------------------------------

# Check ethnicities
FULL_DB %>% count(ETHNICITY)

# how does EU MATERNAL look?
bw_paternal_gest <- FULL_DB %>%
    # filter(ETHNICITY %in% c("european", "finnish", "white", "caucasian")) %>%
    filter(tolower(INDIVIDUAL) == "father") %>%
    filter(COVARIATES == "sex")

# get total n
bw_paternal_gest %>%
    distinct(STUDY_ID, .keep_all = TRUE) %>%
    summarise(total_n = sum(as.numeric(BW_fathers_Sample_size)))

# get files
bw_paternal_gest$file_name_fixed
bw_paternal_gest$README_file


colnames(bw_paternal_gest)
helen_db <- bw_paternal_gest %>% select(PROCESSING_ID, STUDY_ID, file_name_fixed, BW_fathers_Sample_size)
write_delim(helen_db, file = "/home/daniel/code/BW6/Data/paternal_bw_list_23092021", delim = "\t")
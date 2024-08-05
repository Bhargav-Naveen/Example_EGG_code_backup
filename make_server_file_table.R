# Load Libraries ----------------------------------------------------------
library("tidyverse")
library("dplyr")
library("readxl")
library("tidyr")
library("readr")
library("vroom")
library("glue")

# Load Arguments ----------------------------------------------------------
# list files
print("Starting R Script for cleaning file names")

# args <- commandArgs(TRUE)
# master_df_path <- args[1]
# PROJECT_INPUT_DIR <- args[2]

# Cook paths
# master_df_path <- "/home/daniel/code/BW6/Data/FTP_server_files.csv"
# FTP_file_path <- "/home/daniel/code/BW6/BW6_database/data/FTP_FILES.tsv"
# PROJECT_INPUT_DIR <- "/home/daniel/remote_servers/cook_ftp/BW"
# output_dir <- "/home/daniel/code/BW6/Data/"




FTP_file_path <- "/gpfs/mrc0/projects/Research_Project-MRC158833/beml201/EGG/BW6_database/data/FTP_FILES.tsv"
PROJECT_INPUT_DIR <- "/gpfs/mrc0/projects/Research_Project-MRC158833/beml201/EGG/data/raw"
BW_3_PROJECT_INPUT_DIR <- PROJECT_INPUT_DIR
STUDY_INFO_PATH <- "/gpfs/mrc0/projects/Research_Project-MRC158833/beml201/EGG/BW6_database/data/STUDY_INFO.tsv"

FTP_BW_FOLDER <- PROJECT_INPUT_DIR


all_files_main <- list.files(FTP_BW_FOLDER)
# all_files_main <- c(all_files_main,bw3_files)
# make dataframe
master_df <- tibble(file_name_ftp = all_files_main)
master_df$file_type <- ""
master_df %>% filter(file_type == "")

# make type column
# data types.GWAS, Readme, Info, Plots,
master_df <- master_df %>%
    mutate(file_type = if_else(str_detect(file_name_ftp, pattern = "^EGG_HRC.*\\.txt(\\.gz)?$"), "GWAS", file_type)) %>%
    mutate(file_type = if_else(str_detect(file_name_ftp, pattern = "^EGG_1KGP.*\\.txt(\\.gz)?$"), "GWAS", file_type)) %>%
    mutate(file_type = if_else(str_detect(file_name_ftp, pattern = "README|readme"), "Readme", file_type)) %>%
    mutate(file_type = if_else(str_detect(file_name_ftp, pattern = "\\.(png|jpeg|jpg|pdf|tiff?)$"), "Plots", file_type)) %>%
    mutate(file_type = if_else(str_detect(file_name_ftp, pattern = "\\.xlsx$"), "Info", file_type))



# fix file names
master_df <- master_df %>%
    mutate(file_name_fixed = if_else(file_type == "GWAS", file_name_ftp, "")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "EGG_HRC_BWX", replacement = "EGG_HRC_BW6")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "\\_(?=BW\\.|BW_|CHILD)", replacement = "\\.")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "(?<=CHILD|MOTHER|FATHER)\\_", replacement = "\\.")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "CW20", replacement = "CW\\.20")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "SS12", replacement = "SS\\.12")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "PCs_TEENAGE_EUROPEAN_IPK_20210323", replacement = "PCs\\.TEENAGE\\.EUROPEAN\\.IPK\\.20210323")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "PCs_TEENAGE_EUROPEAN_IPK_20220530", replacement = "PCs\\.TEENAGE\\.EUROPEAN\\.IPK\\.20220530")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "PCs_TEENAGE_EUROPEAN_IPK_20220615", replacement = "PCs\\.TEENAGE\\.EUROPEAN\\.IPK\\.20220615")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "GS\\.GSAV2", replacement = "GS_GSAV2")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "GS_GSAV2\\.SS", replacement = "GS_GSAV2\\.European\\.SS")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "BW_chrX\\.child\\.sex\\.RaineStudy", replacement = "BW\\.child\\.sex\\.chrX_RaineStudy")) %>%
    mutate(across(c(file_name_fixed), str_replace_all, pattern = "BW_chrX\\.child\\.sex_gest\\.RaineStudy", replacement = "BW\\.child\\.sex_gest\\.chrX_RaineStudy"))




# master_df %>%
#    filter(file_type == "GWAS") %>%
#    count(PREFIX)

#  make temp column and split into columns
master_df <- master_df %>%
    mutate(file_name_temp = file_name_fixed) %>%
    # mutate(across(c(file_name_temp), str_replace_all, pattern = "EGG_HRC_BW6\\.", replacement = "")) %>%
    mutate(across(c(file_name_temp), str_replace_all, pattern = "\\.gz$", replacement = "")) %>%
    mutate(across(c(file_name_temp), str_replace_all, pattern = "\\.txt$", replacement = ""))



# master_df %>% filter(file_type == "GWAS")
master_df <- master_df %>% separate(col = "file_name_temp", sep = "\\.", into = c("PREFIX", "PHENO", "INDIVIDUAL", "COVARIATES", "STUDY", "ETHNICITY", "ANALYST", "DATE"), remove = TRUE)

# fix columns
master_df <- master_df %>%
    mutate(across(c(PHENO), str_replace_all, pattern = "zBW|ZBW", replacement = "BW")) %>%
    mutate(across(c(PHENO), str_to_upper)) %>%
    mutate(across(c(COVARIATES), str_replace_all, pattern = "_5PC|_PCs", replacement = "")) %>%
    mutate(across(c(INDIVIDUAL, COVARIATES, ETHNICITY), str_to_lower)) %>%
    mutate(across(c(ETHNICITY), str_replace_all, pattern = "eur$|eu_hrc$", replacement = "european")) %>%
    mutate(across(c(DATE), str_replace_all, pattern = "_G[PT]strictPC", replacement = "")) %>%
    mutate(across(c(DATE), str_replace_all, pattern = "12052020", replacement = "20200512"))

master_df <- master_df %>%
    mutate(across(c(ETHNICITY), str_replace_all, pattern = "ac_caapa", replacement = "afro-caribbean")) %>%
    mutate(across(c(ETHNICITY), str_replace_all, pattern = "ma_1000gp3", replacement = "mexican")) %>%
    mutate(across(c(ETHNICITY), str_replace_all, pattern = "th_1000gp3", replacement = "thai"))




# update fixed file name
master_df <- master_df %>% unite("file_name_fixed", c("PREFIX", "PHENO", "INDIVIDUAL", "COVARIATES", "STUDY", "ETHNICITY", "ANALYST", "DATE"), sep = ".", remove = FALSE, na.rm = TRUE)
master_df <- master_df %>% mutate(file_name_fixed = if_else(file_type == "GWAS", paste(file_name_fixed, "tsv.gz", sep = "."), file_name_fixed))


master_df <- master_df[c("STUDY", "PHENO", "INDIVIDUAL", "COVARIATES", "ETHNICITY", "ANALYST", "DATE", "file_name_ftp", "file_name_fixed", "file_type")] %>% arrange(STUDY, INDIVIDUAL, COVARIATES, file_type)



# add file check column
master_df$file_check_complete <- "FALSE"
# add file index column
master_df$FN_ID <- master_df %>% str_glue_data("FN_{rownames(.)}")

# fix covariates
master_df <- master_df %>% mutate(across(c(COVARIATES), str_replace_all, pattern = "sex_batch", replacement = "sex")) %>%
            mutate(across(c(COVARIATES), str_replace_all, pattern = "sex_ga_batch", replacement = "sex_gest"))

# ADD BW3 Data ----------------------------------------------------------


# for BW3
bw3_files <- list.files(BW_3_PROJECT_INPUT_DIR)
bw3_df <- tibble(file_name_ftp = bw3_files)
bw3_df$file_type <- ""
bw3_df %>% filter(file_type == "")
bw3_df <- bw3_df %>%
    filter(str_detect(file_name_ftp, pattern = "^CLEANED")) %>% 
    mutate(file_type = if_else(str_detect(file_name_ftp, pattern = "EGG_1000G_BW3"), "GWAS_BW3", file_type)) %>%
    mutate(file_name_fixed = "NA")
bw3_df <- bw3_df %>%
    mutate(PHENO = "BW") %>%
    mutate(INDIVIDUAL = "CHILD") %>%
    mutate(GENDER = case_when(str_detect(file_name_ftp, pattern = "\\.male|\\.MALE") == TRUE ~ "MALE", str_detect(file_name_ftp, pattern = "\\.female|\\.FEMALE") == TRUE ~ "FEMALE")) %>%
    mutate(COVARIATES = "NA") %>%
    mutate(STUDY = str_extract(file_name_ftp, pattern = "(?<=RESULTS\\.).*(?=\\.F?E?MALE|\\.f?e?male)")) %>%
    mutate(ETHNICITY = "european") %>%
    mutate(ETHNICITY = if_else(str_detect(file_name_ftp, pattern = "MAROC"), "moroccan", ETHNICITY)) %>%
    mutate(ETHNICITY = if_else(str_detect(file_name_ftp, pattern = "TURKISH"), "turkish", ETHNICITY)) %>%
    mutate(ETHNICITY = if_else(str_detect(file_name_ftp, pattern = "SURN"), "surinamese", ETHNICITY)) %>%
    mutate(ANALYST = "") %>%
    mutate(ANALYST = if_else(STUDY == "ABCD", "MHZ", ANALYST)) %>%
    mutate(ANALYST = if_else(STUDY == "B58C", "DPS", ANALYST)) %>%
    mutate(ANALYST = if_else(STUDY == "GENERATIONR", "MNK", ANALYST)) %>%
    mutate(ANALYST = if_else(STUDY == "SCORM", "MA", ANALYST)) %>%
    mutate(DATE = str_extract(file_name_ftp, pattern = "[:digit:]{8}")) %>%
    mutate(DATE = if_else(STUDY == "SCORM", "08052013", DATE))

bw3_df <- bw3_df %>%
    mutate(INDIVIDUAL = if_else(INDIVIDUAL == "CHILD", "child", INDIVIDUAL)) %>%
    mutate(COVARIATES = "sex")

# recode new file names
bw3_df <- bw3_df %>% mutate(file_name_fixed = glue("EGG_1KGP_BW3.{PHENO}.{INDIVIDUAL}_{GENDER}.{COVARIATES}.{STUDY}.{ETHNICITY}.{ANALYST}.{DATE}.tsv.gz"))


bw3_df %>% select(file_name_ftp, GENDER)
bw3_df <- bw3_df[c("STUDY", "PHENO", "INDIVIDUAL", "COVARIATES", "ETHNICITY", "ANALYST", "DATE", "file_name_ftp", "file_name_fixed", "file_type")]



ftp_files <- bind_rows(bw3_df, master_df)
ftp_files <- full_join(bw3_df, master_df)




# ADD Readme ----------------------------------------------------------

# make GWAS_file
GWAS_files <- ftp_files %>% filter(file_type == "GWAS" | file_type == "GWAS_BW3")


# readme files
README_files <- ftp_files %>% filter(file_type == "Readme")
README_files$file_name_ftp
ignore_list <- c("FS", "PS", "DBDS")


unique_study <- unique(GWAS_files$STUDY)
README_out <- NULL
for (current_study in unique_study) {
    print(current_study)
    if (str_detect(current_study, pattern = "GOYA")) {
        print("pre")
        temp_df <- README_files[str_detect(README_files$file_name_ftp, pattern = "GOYA"), "file_name_ftp"]
        print("GOOG")
        temp_df$STUDY <- current_study    
        README_out <- rbind(README_out, temp_df)
        print("post")
    } else if (is.element(current_study, ignore_list)) {
        print(glue("skipping ", current_study))
    } else {
        # README_files <- README_files %>% mutate(STUDY = if_else(str_detect(file_name_ftp, pattern = {{ current_study }}), {{ current_study }}, STUDY))
        temp_df <- README_files[str_detect(README_files$file_name_ftp, pattern = {{ current_study }}), "file_name_ftp"]
        temp_df$STUDY <- current_study
        README_out <- rbind(README_out, temp_df)
    }
}
# add missing readme files
missing_study <- as_tibble(unique_study[!(unique_study %in% README_out$STUDY)])
missing_study$file_name_ftp <- NA
colnames(missing_study) <- c("STUDY", "file_name_ftp")
# missing_combined <- rbind(missing_study,missing_readme)
README_out <- rbind(README_out, missing_study)

# missing readme
missing_readme <- README_files[!(README_files$file_name_ftp %in% README_out$file_name_ftp), ] %>% select("file_name_ftp", "STUDY")
missing_readme

SSI_samples <- c("FS", "IHPS_CIDR", "IHPS_casesCtrls_MEGAex", "Mono_ctrls", "OPI", "PPD_moms", "PE_ctrls")
GO_samples <- c("GDBroad", "GDIllum", "GDAffy", "GDAFFY", "GS_GSAV2")
INDIAN_samples <- c("MMNP", "PMNS", "PS", "VBC", "MBRC")
BW3_samples <- c("ABCD", "B58C", "GENERATIONR", "SCORM")
Daniel_samples <- c("GIFTS", "ALSPAC")

README_out <- README_out %>% mutate(file_name_ftp = if_else(STUDY %in% SSI_samples, "README_BW_SSI_samples.txt", file_name_ftp)) %>% # ssi
    mutate(file_name_ftp = if_else(STUDY %in% "COPSAC_severe", "README_COPSACsevere.txt", file_name_ftp)) %>% # copsac severe
    mutate(file_name_ftp = if_else(STUDY %in% GO_samples, "README_walker's_GoDARTS_GoSHARE.txt", file_name_ftp)) %>% # godarts
    mutate(file_name_ftp = if_else(STUDY %in% INDIAN_samples, "README_Indian_Cohorts_CCMB.txt", file_name_ftp)) %>% # Indian
    mutate(file_name_ftp = if_else(STUDY %in% "chrX_RaineStudy", "EGG_HRC_BW6.README.RaineStudy.CW.20200217.txt", file_name_ftp)) %>% # RAINE chrx
    mutate(file_name_ftp = if_else(STUDY %in% BW3_samples, "SEE_BW3_DOCUMENTATION", file_name_ftp)) %>% # BW3
    mutate(file_name_ftp = if_else(STUDY %in% Daniel_samples, "TALK_TO_DANIEL_LEIRER", file_name_ftp)) # Daniel Samples

# check readme
print(README_out, n = 100)

# remove duplicate study ID
README_out[duplicated(README_out$STUDY), ]
README_out %>% filter(STUDY == "INMA")
# GEN3G, INMA, PANIC
README_out <- README_out %>% filter(!(STUDY == "INMA" & file_name_ftp == "README_EGG_HRC_BW6.ZBW.INMA_GSA.european.SMA.20210504.txt")) %>% # filter out INMA
    filter(!(STUDY == "PANIC" & file_name_ftp == "EGG_HRC_BW6.README.PANIC.TK.20200624.txt.gz")) %>% # PANIC
    filter(!(STUDY == "Gen3G" & file_name_ftp == "Gen3G README.docx")) %>% # Gen3G
    mutate(file_name_ftp = if_else(STUDY == "Gen3G", "Gen3G_README.txt : Gen3G README.docx", file_name_ftp)) %>% # Gen3G
    rename(README_file = file_name_ftp) %>% # rename column
    filter(!(README_file == "EGG_HRC_BW6.README.CHB.mother.XIW.20211014.txt")) %>%
    filter(!(README_file == "EGG_HRC_BW6.README.SWS.EA.20210524.txt.gz")) %>%
    filter(!(README_file == "README_EGG_HRC_BW6.BW.father.sex.UKBB.european.GW.20211021.txt"))

README_out

# Merge into GWAS file
GWAS_files <- GWAS_files %>% full_join(README_out, by = "STUDY")

# add DBDS
GWAS_files %>%
    filter(STUDY == "DBDS") %>%
    select(STUDY, INDIVIDUAL, README_file)
GWAS_files <- GWAS_files %>%
    mutate(README_file = if_else((STUDY == "DBDS" & INDIVIDUAL == "father"), "EGG_HRC_BW6.README.DBDS.father.XIW.20211014.txt", README_file)) %>%
    mutate(README_file = if_else((STUDY == "DBDS" & INDIVIDUAL == "mother"), "EGG_HRC_BW6.README.DBDS.mother.XIW.20211014.txt", README_file))

print(GWAS_files %>% select(STUDY, README_file), n = 300)

# ADD INFO  ----------------------------------------------------------

# ADD IMAGES  ----------------------------------------------------------




# ADD PROCESSING_ID  ----------------------------------------------------------
# ADD extra columns to GWAS_ID
GWAS_files$SEX <- NA
GWAS_files$CHROMOSOMES <- NA
GWAS_files$INFO_file <- NA


# add ID to GWAS_file
id_dictionary <- read_delim(STUDY_INFO_PATH, delim = "\t")
id_dictionary <- id_dictionary %>%
    rename("STUDY" = "GWAS_FILE_ID") %>%
    select(PROCESSING_ID, STUDY_ID, STUDY) %>%
    drop_na() %>%
    # distinct(STUDY, .keep_all = TRUE) %>%
    arrange(STUDY_ID)

GWAS_files <- GWAS_files %>%
    distinct() %>%
    unite("STUDY", c(STUDY, ETHNICITY), remove = FALSE) %>%
    mutate(STUDY = if_else(STUDY == "HAPO_european" & ANALYST == "RNB", "HAPO_european_EXETER", STUDY))

# check there are no cohorts in GWAS_files but not in id_dictionary. 
setdiff(GWAS_files %>% pull("STUDY"), id_dictionary %>% pull("STUDY"))
intersect(GWAS_files %>% pull("STUDY"), id_dictionary %>% pull("STUDY"))

GWAS_files <- id_dictionary %>% inner_join(GWAS_files, by = "STUDY")



## Save file


# select cols
required_cols <- c("PROCESSING_ID", "STUDY_ID", "PHENO", "INDIVIDUAL", "COVARIATES", "ETHNICITY", "ANALYST", "DATE", "SEX", "CHROMOSOMES", "file_name_ftp", "file_name_fixed", "file_type", "file_check_complete", "FN_ID", "README_file", "INFO_file")
required_cols[!required_cols %in% colnames(GWAS_files)]
required_cols %in% colnames(GWAS_files)

GWAS_files <- GWAS_files %>% select(required_cols)
#write_delim(GWAS_files, glue(output_dir, "FTP_FILES.tsv"), delim = "\t")
write_delim(GWAS_files, FTP_file_path, delim = "\t")

print("clean file names script DONE")


# file exclusions  ----------------------------------------------------------
# FTP_file_df <- read_delim(FTP_file_path, delim = "\t")
#FTP_file_df <- read_delim(glue(output_dir, "FTP_FILES.tsv"), delim = "\t")
FTP_file_df <- GWAS_files
FTP_file_df$keep_col <- "KEEP"
print(FTP_file_df %>%
    filter(STUDY_ID == "Raine") %>%
    select(file_name_ftp, keep_col), n = 22)
rain_files_keep <- c(
    "EGG_HRC_BW6.BW_chrX.child.sex.RaineStudy.european.CW20200110.txt.gz",
    "EGG_HRC_BW6.BW_chrX.child.sex_gest.RaineStudy.european.CW20200110.txt.gz",
    "EGG_HRC_BW6.BW.child.sex.RaineStudy.european.CW20200110.txt.gz",
    "EGG_HRC_BW6.BW.child.sex_gest.RaineStudy.european.CW20200110.txt.gz",
    "EGG_HRC_BW6.BW.father.sex.RaineStudy.european.CW.20200616.txt.gz",
    "EGG_HRC_BW6.BW.father.sex.chrX_RaineStudy.european.CW.20200616.txt.gz",
    "EGG_HRC_BW6.BW.father.sex_gest.RaineStudy.european.CW.20200616.txt.gz",
    "EGG_HRC_BW6.BW.father.sex_gest.chrX_RaineStudy.european.CW.20200616.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex.RaineStudy.european.CW.20200616.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex.chrX_RaineStudy.european.CW.20200616.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex_gest.RaineStudy.european.CW.20200630.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex_gest.chrX_RaineStudy.european.CW.20200616.txt.gz"
)

FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(str_detect(file_name_ftp, "RaineStudy"), "DROP", keep_col)) %>%
    mutate(keep_col = if_else(file_name_ftp %in% rain_files_keep, "KEEP", keep_col))


FTP_file_df <- FTP_file_df %>% mutate(keep_col = if_else(str_detect(file_name_fixed, "ALSPAC2021.*20211221"), "DROP", keep_col)) %>% # remove old ALSPAC2021 2021221
    mutate(keep_col = if_else(str_detect(file_name_fixed, "EFSOCH.*20211013"), "DROP", keep_col)) %>% # remove old EFSOCH 20211013
    mutate(keep_col = if_else(str_detect(file_name_fixed, "EFSOCH.*20210707"), "DROP", keep_col)) %>% # remove old EFSOCH 20210707
    mutate(keep_col = if_else(str_detect(file_name_fixed, "GIFTS.*20211013"), "DROP", keep_col)) %>% # remove old GIFTS 20211013
    mutate(keep_col = if_else(str_detect(file_name_fixed, "MMNP.*20210828"), "DROP", keep_col)) %>% # remove old MMNP 20210828
    mutate(keep_col = if_else(str_detect(file_name_fixed, "PMNS.*20210828"), "DROP", keep_col)) %>% # remove old PMNS 20210828
    mutate(keep_col = if_else(str_detect(file_name_fixed, "PS.*20210828"), "DROP", keep_col))  %>% # remove old PS 20210828
    mutate(keep_col = if_else(str_detect(file_name_fixed, "VBC.*20210828"), "DROP", keep_col)) %>% # remove old VBC 20210828
    mutate(keep_col = if_else(str_detect(file_name_fixed, "MBRC.*20210828"), "DROP", keep_col)) %>% # remove old MBRC 20210828
    mutate(keep_col = if_else(str_detect(file_name_fixed, "hbcs.*20210309"), "DROP", keep_col)) %>% # remove old HSBC 20210309
    mutate(keep_col = if_else(str_detect(file_name_fixed, "TEENAGE.*20210323"), "DROP", keep_col)) %>% # remove old teenage 20210323
    mutate(keep_col = if_else(str_detect(file_name_fixed, "TEENAGE.*20220530"), "DROP", keep_col)) # remove old teenage 20220530

# remove pona
pona_exclude <- c(
    "EGG_HRC_BW6.pw.child.sex.PONA.african.GH.20200728.txt.gz",
    "EGG_HRC_BW6.pw.child.sex_gest.PONA.african.GH.20200728.txt.gz",
    "EGG_HRC_BW6.bw.child.sex.PONA.african.GH.20200128.txt.gz",
    "EGG_HRC_BW6.bw.child.sex_gest.PONA.african.GH.20200128.txt.gz"
)
FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% pona_exclude, "DROP", keep_col))


# remove dbds files
dbds_exclude <- c( 
    "EGG_HRC_BW6.BW.father.sex.DBDS.european.XIW.20211014.txt.gz",
    "EGG_HRC_BW6.BW.father.sex_gest.DBDS.european.XIW.20211014.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex.DBDS.european.XIW.20211014.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex_gest.DBDS.european.XIW.20211014.txt.gz"
)
FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% dbds_exclude, "DROP", keep_col))


# remove chb files
chb_exclude <- c( 
    "EGG_HRC_BW6.BW.mother.sex.CHB.european.XIW.20211014.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex_gest.CHB.european.XIW.20211014.txt.gz"

)
FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% chb_exclude, "DROP", keep_col))


# remove NFBC
nfbc_exclude <- c("EGG_HRC_BW6.BW.child.sex.NFBC1966.finnish.JR.20200204.txt.gz",
                "EGG_HRC_BW6.BW.child.sex_gest.NFBC1966.finnish.JR.20200204.txt.gz",
                "EGG_HRC_BW6.BW.child.sex.NFBC1986.finnish.JR.20200204.txt.gz",
                "EGG_HRC_BW6.BW.child.sex_gest.NFBC1986.finnish.JR.20200204.txt.gz")

FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% nfbc_exclude, "DROP", keep_col))


# remove panic
panic_exclude <- c("EGG_HRC_BW6.BW.CHILD.sex.PANIC.FINNISH.TK.20200624.txt.gz",
                "EGG_HRC_BW6.BW.CHILD.sex_gest.PANIC.FINNISH.TK.20200624.txt.gz")

FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% panic_exclude, "DROP", keep_col))

# remove gen3g

gen3g_exclude <- c(
    "EGG_HRC_BW6.BW.mother.sex.Gen3G.european.RF.20200116.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex.Gen3G.european.RF.20200504.txt",
    "EGG_HRC_BW6.BW.mother.sex_gest.Gen3G.european.RF.20200116.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex_gest.Gen3G.european.RF.20200504.txt"
)

FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% gen3g_exclude, "DROP", keep_col))

# remove ukbb

UKBB_exclude <- c(
    "EGG_HRC_BW6.BW.father.sex.UKBB.european.GW.20211021.txt.gz",
    "EGG_HRC_BW6.BW.father.sex.UKBB.european.GW.20211028.txt.gz"
)
FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% UKBB_exclude, "DROP", keep_col))

# remove genr
generation_r_exclude <- c(
    "CLEANED.EGG_1000G_BW3.RESULTS.GENERATIONR.FEMALE.MNK.14102013.txt.gz",
    "CLEANED.EGG_1000G_BW3.RESULTS.GENERATIONR.MALE.MNK.12102013.txt.gz",
    "EGG_HRC_BW6.BW.CHILD.SEX_5PC.GENR.EUR.KT.21012020.txt.gz",
    "EGG_HRC_BW6.BW.CHILD.SEX_GEST_5PC.GENR.EUR.KT.21012020.txt.gz",
    "EGG_HRC_BW6.BW.child.sex.GENR4.european.HJL.20210930.txt.gz",
    "EGG_HRC_BW6.BW.child.sex_gest.GENR4.european.HJL.20210930.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex.GENR4.european.HJL.20210930.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex_gest.GENR4.european.HJL.20210930.txt.gz"
)
FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% generation_r_exclude, "DROP", keep_col))

# viking exclude
viking_exclude <- c(
    "EGG_HRC_BW6.BW.child.sex.VIKING.european.AL.20210211.txt.gz"
)
FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% viking_exclude, "DROP", keep_col))

# remove project viva
ProjectViva_exclude <- c(
    "EGG_HRC_BW6.BW.child.sex.ProjectViva.white.RF.20210416.txt.gz",
    "EGG_HRC_BW6.BW.child.sex_gest.ProjectViva.white.RF.20210416.txt.gz"
)
FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% ProjectViva_exclude, "DROP", keep_col))




# remove chop

CHOP_exclude <- c(
    "EGG_HRC_BW6.BW.child.sex_gest.CHOP.admixed_american.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.child.sex_gest.CHOP.african.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.child.sex_gest.CHOP.asian.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.child.sex_gest.CHOP.european.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.child.sex.CHOP.admixed_american.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.child.sex.CHOP.african.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.child.sex.CHOP.asian.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.child.sex.CHOP.european.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.father.sex.CHOP.african.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.father.sex.CHOP.european.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex.CHOP.african.JPB.20210310.txt.gz",
    "EGG_HRC_BW6.BW.mother.sex.CHOP.european.JPB.20210310.txt.gz"
)
FTP_file_df <- FTP_file_df %>%
    mutate(keep_col = if_else(file_name_ftp %in% CHOP_exclude, "DROP", keep_col))


# add file index column

FTP_file_df <- FTP_file_df %>%
    group_by(PROCESSING_ID, INDIVIDUAL) %>%
    mutate(FN_ID = row_number()) %>%
    mutate(FN_ID = glue("FN_{PROCESSING_ID}_{INDIVIDUAL}_{COVARIATES}_{DATE}_{FN_ID}", "__")) %>%
    ungroup()

write_delim(FTP_file_df, FTP_file_path, delim = "\t")

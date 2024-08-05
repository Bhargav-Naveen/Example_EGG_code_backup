# Study_info_file
required_cols <- c("META_STUDY_ID", "STUDY_ID", "PROCESSING_ID", "Study_full_name", "Ethnicity")


id_dictionary <- read_delim("/home/daniel/code/BW6/Data/id_dictionary.tsv", delim = "\t")
id_dictionary <- id_dictionary %>% arrange(META_STUDY_ID, STUDY_ID)
write_delim(id_dictionary, "/home/daniel/code/BW6/Data/STUDY_INFO.tsv", delim = "\t")
unique(id_dictionary$STUDY_ID)

id_dictionary$STUDY_ID
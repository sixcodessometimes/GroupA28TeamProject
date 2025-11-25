cwur <- read.csv("cwurData.csv")
sample_table <- cwur[ , c("world_rank", "institution", "country", 
                          "publications", "score", "year")]
head(sample_table, 5)


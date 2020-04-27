library(tidyverse)
library(readxl)
library(boxr)
library(jsonlite)

box_auth()
##codebook <- read_excel("Preliminary Recruitment Codebook_v1_072619.xlsx")
codebook=box_read_excel(file_id = 497660089154) %>% as_tibble()
colnames(codebook) <- gsub("[ /]|\\(.*\\)","",colnames(codebook))
cnames <- colnames(codebook)

##codebook <- codebook %>% group_by(VariableName) %>% summarise(x = paste(FormatValue,collapse = ":::") )


# if the Variable name is NA replace it with the last observation carried forward...
# if the Format value is N/A replace it with NA..
zz <- codebook %>%
  mutate(VariableName=zoo::na.locf(VariableName),
         FormatValue=ifelse(FormatValue=="N/A",NA,FormatValue)) %>%
  select(VariableName,FormatValue) %>% filter(!is.na(FormatValue)) %>% group_by(VariableName) %>% nest(data=c(FormatValue)) %>%
  mutate(data=map(data,~pull(.x))) %>% rename(FormatValue=data)

codebook <- codebook %>% filter(!is.na(VariableName)) %>% select(-FormatValue) %>% left_join(zz,by = "VariableName")
codebook <- codebook %>% mutate(ConceptId=row_number()) %>% select(ConceptId,cnames)
write_json(codebook,"test.json",pretty=TRUE)


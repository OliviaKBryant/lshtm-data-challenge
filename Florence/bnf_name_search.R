# Load in data 
BNFcodes <- read.csv("20220115_1642266807923_BNF_Code_Information.csv")

# Look at BNF Substances by name
namecheck <- BNFcodes %>%
  # Exclude respiratory, nutritional, topical chapters
  filter(!(BNF.Chapter.Code %in% c(3, 9, 11:23))) %>% 
  select(BNF.Paragraph.Code, BNF.Paragraph, BNF.Chemical.Substance.Code, BNF.Chemical.Substance) %>%
  distinct(BNF.Chemical.Substance.Code, .keep_all = T) %>%
  filter(str_detect(BNF.Chemical.Substance, "cort|sone|lone|onide|terol|asol|pred"))
nrow(namecheck) #42 items

# Look at items in chapters that are not included in initial inclusion criteria 
namecheck %>%
  filter(!(str_detect(BNF.Paragraph.Code, "10502|^603|100102"))) %>%
  select(BNF.Paragraph.Code, BNF.Paragraph, BNF.Chemical.Substance)
  
# By looking at the paragraph names, those are not targets of this project
# In fact, some are not corticosteroids but have similar parts of name 
#   i.e. those in BNF Chapters 4 and 5
# 42 total - 14 excluded = 28, matches with initial inclusion criteria
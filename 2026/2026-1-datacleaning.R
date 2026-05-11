rm(list=ls())

################################################ update dataset name
data <- read.csv("Data/2026/Responses_2026.05.11.csv")

#write.csv(names(data), "Data/2026/Original_Columns.csv", row.names = F)

################################################
## Clean response dataset
################################################

## rename columns
colmatching <- read.csv("Data/2026/Renamed_Columns.csv")
names(data) <- as.character(colmatching$new[match(names(data), colmatching$original)])
rm(colmatching)
## drop blank columns
data <- data %>% select(-c("core_talent", "core_culture", "core_alumni", "core_fundraising",
                           "full_talent", "full_culture", "full_alumni", "full_fundraising",))
## convert blank cells to NA
data[data==""] <- NA

## RECODE VALUES

################################################ update institution names
## institution names
data <- data %>% mutate(institution = institution %>%
                          recode_values(
                                "London School of Economics and Political Science" ~ "LSE",
                                "University of St Andrews" ~ "St Andrews",
                                "University of Manchester" ~ "Manchester",
                                "London Business School" ~ "LBS",
                                "Trinity Development & Alumni " ~ "Trinity",
                                "University of Sheffield" ~ "Sheffield",
                                "University of Nottingham" ~ "Nottingham",
                                "University of Cambridge - Development and Alumni Relations" ~ "Cambridge",
                                "University of Birmingham, Development and Alumni Relations Office" ~ "Birmingham",
                                "University of Warwick" ~ "Warwick",
                                "Royal College of Art " ~ "RCA",
                                "Royal Academy of Music" ~ "RAM",
                                "The University of Edinburgh" ~ "Edinburgh",
                                "King's College London" ~ "KCL",
                                "University of Bristol" ~ "Bristol",
                                "University of Leeds" ~ "Leeds"
))

## index type
data <- data %>% mutate(index_type = index_type %>%
                          recode_values(
                            "Full Index (58 questions)" ~ "Full",
                            "Core Index (35 questions)" ~ "Core"
))

## responses
scoring <- data.frame(original = c("Yes", "No", "Partially"), new = c(1, 0, 0))
data <- data %>% mutate(across(
  core_talent_01:full_fundraising_12,
  ~ recode_values(.x, from = scoring$original, to = scoring$new)
))
rm(scoring)

## n_consulted needs to have a string at the end so that Excel doesn't force it into a "date" format
data <- data %>% mutate(n_consulted = paste0(n_consulted, " people"))

## convert to correct variable types
data <- data %>% mutate(across(
  c(dplyr::starts_with("core"), dplyr::starts_with("full"), "relevant"),
  ~ as.numeric(.x)
))
data <- data %>% mutate(across(
  c("institution", "index_type", "n_consulted", "time"),
  ~ as.factor(.x)
))

data <- data %>% arrange(index_type, institution)

## export full data file
write.csv(data, "Results/2026/Cleaned_FullData.csv", row.names = F)


################################################
## Create summary score dataset
################################################
# scoring calculations - sum scores and then divide by n in category

data <- data %>%
  mutate(
    score = round(
      # sum of scores
      rowSums(dplyr::select(., dplyr::starts_with(c("core_", "full_"))), na.rm = T) /
        # divided by length of category - determined using if/else statement to match to index type
        ifelse(index_type == "Core",
        length(names(data %>% dplyr::select(., dplyr::starts_with("core")))),
        length(names(data %>% dplyr::select(., dplyr::starts_with("full"))))
        ) *
        # multiplied by 10 and rounded to one decimal
        10, 1),
    talent = round(
      rowSums(dplyr::select(., dplyr::contains("talent")), na.rm = T) /
        ifelse(index_type == "Core",
               length(names(data %>% dplyr::select(., dplyr::starts_with("core_talent")))),
               length(names(data %>% dplyr::select(., dplyr::starts_with("full_talent"))))
        ) *
        10, 1),
    culture = round(
      rowSums(dplyr::select(., dplyr::contains("culture")), na.rm = T) /
        ifelse(index_type == "Core",
               length(names(data %>% dplyr::select(., dplyr::starts_with("core_culture")))),
               length(names(data %>% dplyr::select(., dplyr::starts_with("full_culture"))))
        ) *
        10, 1),
    alumni = round(
      rowSums(dplyr::select(., dplyr::contains("alumni")), na.rm = T) /
        ifelse(index_type == "Core",
               length(names(data %>% dplyr::select(., dplyr::starts_with("core_alumni")))),
               length(names(data %>% dplyr::select(., dplyr::starts_with("full_alumni"))))
        ) *
        10, 1),
    fundraising = round(
      rowSums(dplyr::select(., dplyr::contains("fundraising")), na.rm = T) /
        ifelse(index_type == "Core",
               length(names(data %>% dplyr::select(., dplyr::starts_with("core_fundraising")))),
               length(names(data %>% dplyr::select(., dplyr::starts_with("full_fundraising"))))
        ) *
        10, 1),
  )

## relocate summary responses to front of dataframe
data <- data %>% relocate(
  score, talent, culture, alumni, fundraising,
  .after = index_type
)
## drop item-level responses
data <- data %>% dplyr::select(-c(core_talent_01:full_fundraising_12))
## reorder for clean export
data <- data %>% arrange(index_type, institution)

## export cleaned summary dataset
write.csv(data, "Results/2026/Cleaned_Summary.csv", row.names = F)

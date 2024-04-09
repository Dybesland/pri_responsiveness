#===============================================================================
# Script: speech_scraping.R
# Date: March 2024
# Author: Sara Dybesland
# Paper: "Miss represented or Misrepresented? Gendered Prioritiy Responsiveness 
#         in the Norwegian Parliament"
# Purpose: Scrape speeches from the Norwegian Parliament and process text data.
# Requires: stortingscrape, rvest, tidyverse, stringr, dplyr, xml2, future.apply
#===============================================================================


#===============================================================================
# PREPARATION
#===============================================================================
# Set the locale to Norwegian for proper date and text handling. This helps with reading Norwegian dates and text formats correctly.
Sys.setlocale(locale = "no_NO.UTF-8") # For Mac
# Sys.setlocale(locale = "no_NO.utf8") # For Windows

# Load necessary R packages for data manipulation, text analysis, and plotting.
library(readr)       # For reading CSV files.
library(dplyr)       # For data manipulation.
library(tidyr)       # For tidying the data.
library(quanteda)    # For quantitative text analysis.
library(readtext)    # For reading text data.
library(keyATM)      # For topic modeling.
library(magrittr)    # Provides the %>% pipe operator for chaining commands.
library(ggplot2)     # For data visualization.
library(gt)          # For creating tables.
library(gtsummary)   # For summarizing data frames as tables.
library(tidyverse)   # A collection of packages for data science.
library(lme4)        # For linear mixed-effects models.
library(texreg)      # For formatting regression output.
library(effects)     # For visualizing regression effects.
library(wordcloud) # For creating the word cloud
library(tidytext)
# Load the knitr package
library(knitr)
library(kableExtra)

# Load the datasets: speech data and survey share data from CSV files.
speech_data <- read_csv("data/processed/parla_nor.csv")
survey_data_shares <- read_csv("data/raw/survey_data_shares.csv")

#===============================================================================
# Create time variables corresponding to survey data to represent: 
#===============================================================================

# Create a data frame with unique rounds and their start and end dates from the survey data.
unique_rounds_df <- survey_data_shares %>%
  select(round_n, start_date, end_date) %>%
  distinct()     
print(unique_rounds_df)

# Add new columns to unique_rounds_df that calculate the days until the next end date and lag the round number.
unique_rounds_df <- unique_rounds_df %>%
  arrange(end_date) %>%
  mutate(
    survey_days_to_next_end_date = lead(end_date) - end_date,  # Days to next survey's end date.
    survey_next_end_date = lead(end_date),                     # The next survey's end date.
    survey_lag_round = lag(round_n)                            # Previous survey round number.
  ) %>%
  rename(
    survey_round_n = round_n,
    survey_end_date = end_date, 
    survey_start_date = start_date
  )


# Speeches heald during survey rounds. 
speech_data <- speech_data %>%
  mutate(
    survey_round_n = case_when(
      between(publication_date, as.Date("2013-11-06"), as.Date("2014-01-05")) ~ 1,
      between(publication_date, as.Date("2014-03-10"), as.Date("2014-03-31")) ~ 2,
      between(publication_date, as.Date("2015-03-09"), as.Date("2015-04-08")) ~ 4,
      between(publication_date, as.Date("2015-10-28"), as.Date("2015-11-23")) ~ 5,
      between(publication_date, as.Date("2016-03-01"), as.Date("2016-03-19")) ~ 6,
      between(publication_date, as.Date("2016-11-01"), as.Date("2016-12-01")) ~ 7,
      between(publication_date, as.Date("2017-05-11"), as.Date("2017-06-06")) ~ 9,
      between(publication_date, as.Date("2017-10-31"), as.Date("2017-11-23")) ~ 10,
      between(publication_date, as.Date("2018-10-17"), as.Date("2018-11-05")) ~ 13,
      between(publication_date, as.Date("2019-01-15"), as.Date("2019-02-11")) ~ 14,
      between(publication_date, as.Date("2020-01-15"), as.Date("2020-02-22")) ~ 17,
      between(publication_date, as.Date("2020-06-02"), as.Date("2020-06-29")) ~ 18,
      between(publication_date, as.Date("2021-01-26"), as.Date("2021-03-08")) ~ 20,
      between(publication_date, as.Date("2021-11-02"), as.Date("2021-11-30")) ~ 22,
      between(publication_date, as.Date("2022-01-12"), as.Date("2022-01-31")) ~ 23,
      between(publication_date, as.Date("2022-05-23"), as.Date("2022-06-15")) ~ 24,
      between(publication_date, as.Date("2022-10-31"), as.Date("2022-11-28")) ~ 25,
      between(publication_date, as.Date("2023-02-16"), as.Date("2023-03-14")) ~ 26,
      between(publication_date, as.Date("2023-06-02"), as.Date("2023-06-23")) ~ 27,
      TRUE ~ NA_integer_  # Assign NA to dates that don't fall within any interval
    )
  )


# Create an indicator of which round to be represented. 
speech_data <- speech_data %>%
  mutate(
    survey_round_to_represent = case_when(
      publication_date <= as.Date("2014-01-05") ~ NA_integer_,
      between(publication_date, as.Date("2014-01-06"), as.Date("2014-03-31")) ~ 1,
      between(publication_date, as.Date("2014-04-01"), as.Date("2015-04-08")) ~ 2,
      between(publication_date, as.Date("2015-04-09"), as.Date("2015-11-23")) ~ 4,
      between(publication_date, as.Date("2015-11-24"), as.Date("2016-03-19")) ~ 5,
      between(publication_date, as.Date("2016-03-20"), as.Date("2016-12-01")) ~ 6,
      between(publication_date, as.Date("2016-12-02"), as.Date("2017-06-06")) ~ 7,
      between(publication_date, as.Date("2017-06-07"), as.Date("2017-11-23")) ~ 9,
      between(publication_date, as.Date("2017-11-24"), as.Date("2018-11-05")) ~ 10,
      between(publication_date, as.Date("2018-11-06"), as.Date("2019-02-11")) ~ 13,
      between(publication_date, as.Date("2019-02-12"), as.Date("2020-02-22")) ~ 14,
      between(publication_date, as.Date("2020-02-23"), as.Date("2020-06-29")) ~ 17,
      between(publication_date, as.Date("2020-06-30"), as.Date("2021-03-08")) ~ 18,
      between(publication_date, as.Date("2021-03-09"), as.Date("2021-11-30")) ~ 20,
      between(publication_date, as.Date("2021-12-01"), as.Date("2022-01-31")) ~ 22,
      between(publication_date, as.Date("2022-02-01"), as.Date("2022-06-15")) ~ 23,
      between(publication_date, as.Date("2022-06-16"), as.Date("2022-11-28")) ~ 24,
      between(publication_date, as.Date("2022-11-29"), as.Date("2023-03-14")) ~ 25,
      between(publication_date, as.Date("2023-03-15"), as.Date("2023-06-23")) ~ 26,
      publication_date > as.Date("2023-06-23") ~ 27,
      TRUE ~ NA_integer_  # Assign NA to dates that don't fall within any interval
    )
  )



# For robustness check, I create a time stap where only speeches 90 days after 
# end of survey round is included. 

speech_data <- speech_data %>%
  mutate(
    time_stamp_90days = case_when(
      publication_date <= as.Date("2014-01-05") ~ NA_integer_,
      between(publication_date, as.Date("2014-01-06"), as.Date("2014-01-06") + days(60)) ~ 1,
      between(publication_date, as.Date("2014-04-01"), as.Date("2014-04-01") + days(60)) ~ 2,
      between(publication_date, as.Date("2015-04-09"), as.Date("2015-04-09") + days(60)) ~ 3,
      between(publication_date, as.Date("2015-11-24"), as.Date("2015-11-24") + days(60)) ~ 4,
      between(publication_date, as.Date("2016-03-20"), as.Date("2016-03-20") + days(60)) ~ 5,
      between(publication_date, as.Date("2016-12-02"), as.Date("2016-12-02") + days(60)) ~ 6,
      between(publication_date, as.Date("2017-06-07"), as.Date("2017-06-07") + days(60)) ~ 7,
      between(publication_date, as.Date("2017-11-24"), as.Date("2017-11-24") + days(60)) ~ 8,
      between(publication_date, as.Date("2018-11-06"), as.Date("2018-11-06") + days(60)) ~ 9,
      between(publication_date, as.Date("2019-02-12"), as.Date("2019-02-12") + days(60)) ~ 10,
      between(publication_date, as.Date("2020-02-23"), as.Date("2020-02-23") + days(60)) ~ 11,
      between(publication_date, as.Date("2020-06-30"), as.Date("2020-06-30") + days(60)) ~ 12,
      between(publication_date, as.Date("2021-03-09"), as.Date("2021-03-09") + days(60)) ~ 13,
      between(publication_date, as.Date("2021-12-01"), as.Date("2021-12-01") + days(60)) ~ 14,
      between(publication_date, as.Date("2022-02-01"), as.Date("2022-02-01") + days(60)) ~ 15,
      between(publication_date, as.Date("2022-06-16"), as.Date("2022-06-16") + days(60)) ~ 16,
      between(publication_date, as.Date("2022-11-29"), as.Date("2022-11-29") + days(60)) ~ 17,
      between(publication_date, as.Date("2023-03-15"), as.Date("2023-03-15") + days(60)) ~ 18,
      publication_date > as.Date("2023-06-23") ~ 19,
      TRUE ~ NA_integer_  # Assign NA to dates that don't fall within any interval
    )
  )

speech_data <- left_join(speech_data, unique_rounds_df, 
                         by = c("survey_round_to_represent" = "survey_round_n"))

# For survey_round_to_represent count
speech_data <- speech_data %>%
  group_by(survey_round_to_represent) %>%
  mutate(count_survey_round = n()) %>%
  ungroup() # Ungroup to prevent unexpected behavior in future operations

# For time_stamp_90days count
speech_data <- speech_data %>%
  group_by(time_stamp_90days) %>%
  mutate(count_time_stamp_90days = n()) %>%
  ungroup()


speech_data %>%
  select(time_stamp_90days, count_time_stamp_90days) %>%
  unique()

# Create the mapping based on the sorted unique values
unique_rounds <- sort(unique(speech_data$survey_round_to_represent))
round_mapping <- setNames(seq_along(unique_rounds), unique_rounds)

# Apply the mapping to create 'time_stamp'
speech_data <- speech_data %>%
  mutate(time_stamp = round_mapping[as.character(survey_round_to_represent)])

speech_data <- speech_data %>%  filter(!is.na(time_stamp))

## PRESENT TABLE of speaches over period:

response_periods <- speech_data %>% select(survey_round_to_represent,
                                           time_stamp,
                                           time_stamp_90days,
                                           survey_start_date,
                                           survey_end_date, 
                                           survey_next_end_date,
                                           survey_days_to_next_end_date, 
                                           count_survey_round, 
                                           count_time_stamp_90days)


response_periods <- response_periods %>%
  distinct() %>%
  filter(!if_any(c(time_stamp_90days), is.na)) %>% 
  select(survey_round_to_represent, 
         survey_start_date,
         survey_end_date, 
         survey_next_end_date, 
         survey_days_to_next_end_date, 
         count_survey_round, 
         count_time_stamp_90days, 
         time_stamp)%>%
  arrange(survey_round_to_represent) %>% 
  rename("NCP Survey Round" = survey_round_to_represent, 
         "Start  Round" = survey_start_date, 
         "End  Round" = survey_end_date, 
         "End Next  Round" = survey_next_end_date, 
         "Days Between  Endings" = survey_days_to_next_end_date, 
         "N Speeches Between  Rounds" = count_survey_round, 
         "N Speeches  Round End + 60 Days" = count_time_stamp_90days, 
         "Time Stamp in Dynamic keyATM" = time_stamp)

response_periods[is.na(response_periods)] <- "NA"

# Print the data frame as a LaTeX table
# With Booktabs 
kable(response_periods, "latex", booktabs=TRUE, linesep = "") %>%  
  save_kable("output/tables/time_stamp.tex")

#===============================================================================
# PREPARE DATA FOR keyATM 
#===============================================================================
names(speech_data)
# Select variables from speech_data and convert them to the appropriate data types.
speech_data <- speech_data %>%
  mutate(id = row_number()) %>% 
  select(id, mp_gender, mp_party, mp_committee_id1, mp_committee_id2, mp_committee_id3, mp_committee_id4,
         government_name, government_ideology, publication_year, survey_round_to_represent, time_stamp, time_stamp_90days, speech_text) %>%  # Select relevant columns.
  mutate(
    mp_gender = as.factor(mp_gender),  # Convert 'gender' to a factor for categorical analysis.
    mp_party = as.factor(mp_party), 
    mp_committee_id1 = as.factor(mp_committee_id1), 
    mp_committee_id2 = as.factor(mp_committee_id2), 
    mp_committee_id3 = as.factor(mp_committee_id3), 
    mp_committee_id4 = as.factor(mp_committee_id4), 
    government_name = as.factor(government_name), 
    government_ideology = as.factor(government_ideology),
    publication_year = as.factor(publication_year), 
    survey_round_to_represent = as.numeric(survey_round_to_represent), 
    time_stamp = as.numeric(time_stamp), 
    speech_text = as.character(speech_text))


# Create a corpus from the speech data for text analysis.
speech_corpus <- corpus(speech_data, text_field = "speech_text")

# Load a custom stopword list containing names of Members of Parliament, which we want to exclude from analysis.
names_mps <- readLines("data/raw/names_mps_list.txt")

# Tokenize the corpus: breaking down the text into individual words or tokens while removing unwanted elements.
speech_tokens <- tokens(
  speech_corpus,
  remove_numbers = TRUE,          # Remove all numbers.
  remove_punct = TRUE,            # Remove all punctuation marks.
  remove_symbols = TRUE,          # Remove symbolic characters.
  remove_separators = TRUE,       # Remove separator characters.
  remove_url = TRUE               # Remove URLs.
) %>%
  tokens_tolower() %>%            # Convert all tokens to lowercase to ensure uniformity.
  tokens_remove(
    c(stopwords(language = "no"), # Remove Norwegian stopwords provided by the 'snowball' package.
      names_mps                   # Remove names of Members of Parliament from the tokens.
    )
  ) %>%
  tokens_select(min_nchar = 2)    # Select tokens that have two or more characters.


# Load the pre-annotated list which maps each token to its lemma (base form).
load("data/raw/annotation.Rda")

# Replace tokens in the corpus with their lemmatized form based on the loaded annotation.
speech_tokens <- tokens_replace(speech_tokens, annotation$token, annotation$lemma)

# Load additional stopwords specific to the author's research or specific speech context.
author_stopwords <- readLines("data/raw/author_stopword_list.txt")
# Remove these additional stopwords from the tokenized text.
speech_tokens <- speech_tokens %>%
  tokens_remove(author_stopwords)

# Construct a document-feature matrix (DFM) from the processed tokens.
# A DFM is a matrix that represents the frequency of terms across documents in the corpus.
speech_dfm <- dfm(speech_tokens) %>%
  dfm_trim(min_doclength = 5)  # Remove documents (speeches) with fewer than 5 terms to focus on more substantial texts.

# Print the number of features (unique terms) in the DFM.

# Prepare the data for keyATM, which is used for advanced topic modeling.
# This involves subsetting the DFM to include only documents with at least one token remaining after preprocessing.
speech_dfm <- quanteda::dfm_subset(speech_dfm, ntoken(speech_dfm) > 1)


# Convert the DFM to the format required by keyATM for topic modeling analysis.
keyATM_docs <- keyATM_read(texts = speech_dfm)
summary(keyATM_docs)


#===============================================================================
# KEYWORD SELECTION for keyATM with weighted LDA ( reciew this process)
#===============================================================================
set.seed(225)  # set the seed before split the dfm
docs_withSplit <- keyATM_read(texts = speech_dfm,
                              split = 0.3)  # split each document

lda_key_select <- weightedLDA(
  docs              = docs_withSplit$W_split,  # 30% of the corpus
  number_of_topics  = 21,  # the number of potential themes in the corpus
  model             = "base",
  options           = list(seed = 250)
)
top_words(lda_key_select, n= 20)  # top words can aid selecting keywords


#===============================================================================
# KEYWORD SELECTION for keyATM with weighted LDA and knowledge 
#===============================================================================

keywords <- list(
  Health     = c("helse", "helsevesen", "sjukehus", "sykehus"),
  Elderly    = c("eldreomsorge", "sykehjem", "sjukeheim"),
  Environment = c("miljø", "klima", "bærekraft", "fornybar", "utslipp", "klode", "grønn"),
  Education   = c("utdanning", "skole", "forskning", "elev", "lærer"),
  Infrastructure = c("samferdsel", "transport", "infrastruktur", "veg", "trafikk", "byggje", "bygge"), 
  Immigration     = c("innvandring", "asylpolitikk", "flyktning"),
  Economy    = c("skatt", "avgift", "finans", "økonomi", "økonomisk", "gjeld"),
  Labour = c("ansatt", "arbeidsplass", "arbeidsmarked","arbeidsplassar", "tilsette"),
  Welfare   = c("fattigdom", "velferdsstat", "trygd", "pensjon", "omfordeling", "bo"),
  Equality = c("likestilling", "diskriminering", "rasisme", "homofil"), 
  Culture     = c("idrett", "frivillighet", "teater", "film", "tv", "avis"),
  Defence    = c("forsvar", "militær"),
  Crime = c("lagmannsrett", "politi", "sikkerhetstjeneste", "kriminalitet"),
  Trade   = c("næringsliv", "industri", "næringsdrivende", "bedrift", "næring"),
  Foreign = c("utenrikspolitikk", "eu", "bistand", "nato", "internasjonal","fred", "krig"), 
  Tradition     = c("kristendom", "religion", "kongehus", "kirke"),
  Family   = c("barnevern", "familie", "ekteskap", "barnehage", "oppvekst"),
  Freedom = c("ytringsfrihet", "alkohol", "trosfrihet","frihet"), 
  Privatisation = c("privatisering", "effektivisering", "eierskap", "statlig"),
  Regional = c("distrikt", "desentralisering", "sentralisering", "regionalpolitikk")
)

#===============================================================================
# Dynamic KEY atm
#===============================================================================

# Split corpuses into male and female speeakers: 

# Subset for male MPs
speech_dfm_male <- dfm_subset(speech_dfm, mp_gender == "male")
# Subset for female MPs
speech_dfm_female <- dfm_subset(speech_dfm, mp_gender == "female")


# MALE SPEECHES 

# Extract and align document variables
# Assuming 'mp_gender' is a docvar in your speech_corpus
male_vars <- data.frame(
  doc_id = docnames(speech_corpus),
  mp_gender = docvars(speech_corpus, "mp_gender"),  # Assuming 'mp_gender' exists
  survey_round_to_represent = docvars(speech_corpus, "survey_round_to_represent"),
  publication_year = docvars(speech_corpus, "publication_year"),
  time_stamp = docvars(speech_corpus, "time_stamp")
)

# Filter to keep only records where mp_gender is male
male_vars <- male_vars %>% 
  as_tibble() %>% 
  filter(mp_gender == "male")

# Exclude records with NAs in key variables
male_vars <- male_vars %>%
  filter(!is.na(survey_round_to_represent) & !is.na(time_stamp))


male_vars_period <- male_vars %>%
  filter(doc_id %in% docnames(speech_dfm_male)) %>%
  arrange(publication_year, survey_round_to_represent, time_stamp) %>%
  select(publication_year, survey_round_to_represent, time_stamp)

# Check the head of the cleaned dataset
head(male_vars_period)


male_num_unique_periods <- length(unique(male_vars_period$time_stamp))


unique(male_vars_period$survey_round_to_represent)

keyATM_docs_male <- keyATM_read(texts = speech_dfm_male)
summary(keyATM_docs_male)

class(male_vars_period$time_stamp)
out_male <- keyATM(docs = keyATM_docs_male, 
                   no_keyword_topics = 0, 
                   keywords = keywords, 
                   model = "dynamic", 
                   model_settings = list(time_index = male_vars_period$time_stamp, 
                                         num_states = male_num_unique_periods),
                   options = list(seed = 123, store_theta = TRUE, thinning = 5))

top_words(out_male)


save(out_male, file = "data/processed/out_male.RData")

load("data/processed/out_male.RData")
male_fig_timetrend <- plot_timetrend(out_male, time_index_label = male_vars_period$time_stamp, xlab = "time_stamp")
male_fig_timetrend

male_speech_data <- values_fig(male_fig_timetrend)
save(male_speech_data, file = "data/processed/est_male_speeches.RData")
#=================

# FEMALE TOPIC MODEL: 


# Extract and align document variables
# Assuming 'mp_gender' is a docvar in your speech_corpus
female_vars <- data.frame(
  doc_id = docnames(speech_corpus),
  mp_gender = docvars(speech_corpus, "mp_gender"),  # Assuming 'mp_gender' exists
  survey_round_to_represent = docvars(speech_corpus, "survey_round_to_represent"),
  publication_year = docvars(speech_corpus, "publication_year"),
  time_stamp = docvars(speech_corpus, "time_stamp")
)

# Filter to keep only records where mp_gender is female
female_vars <- female_vars %>% 
  as_tibble() %>% 
  filter(mp_gender == "female")

# Exclude records with NAs in key variables
female_vars <- female_vars %>%
  filter(!is.na(survey_round_to_represent) & !is.na(time_stamp))


female_vars_period <- female_vars %>%
  filter(doc_id %in% docnames(speech_dfm_female)) %>%
  arrange(publication_year, survey_round_to_represent, time_stamp) %>%
  select(publication_year, survey_round_to_represent, time_stamp)

# Check the head of the cleaned dataset
head(female_vars_period)


female_num_unique_periods <- length(unique(female_vars_period$time_stamp))


unique(female_vars_period$survey_round_to_represent)

keyATM_docs_female <- keyATM_read(texts = speech_dfm_female)
summary(keyATM_docs_female)

out_female <- keyATM(docs = keyATM_docs_female, 
                     no_keyword_topics = 0, 
                     keywords = keywords, 
                     model = "dynamic", 
                     model_settings = list(time_index = female_vars_period$time_stamp, 
                                           num_states = female_num_unique_periods),
                     options = list(seed = 123, store_theta = TRUE, thinning = 5))

top_words(out_female)


save(out_female, file = "data/processed/out_female.RData")


load("data/processed/out_female.RData")

female_fig_timetrend <- plot_timetrend(out_female, time_index_label = female_vars_period$time_stamp, xlab = "Year")
female_fig_timetrend
female_speech_data <- values_fig(female_fig_timetrend)
save(female_speech_data, file = "data/processed/est_female_speeches.RData")
#===============================================================================
# Key ATM covariates
#===============================================================================

vars <- docvars(speech_corpus)
head(vars)

vars %>%
  as_tibble() %>%
  select(mp_gender, mp_party) -> vars_selected
table(vars_selected)

head(model.matrix(~ mp_gender + mp_party, data = vars_selected))

out_covariates <- keyATM(
  docs              = keyATM_docs,
  no_keyword_topics = 20,
  keywords          = keywords,
  model             = "covariates",
  model_settings    = list(covariates_data    = vars_selected,
                           covariates_formula = ~ mp_gender + mp_party ),
  options = list(seed = 123, store_theta = TRUE, thinning = 5),
  keep              = c("Z", "S")
)

save(out_covariates, file = "data/processed/out_covariates.RData")

strata_tw <- by_strata_TopicWord(
  out, keyATM_docs,
  by = as.vector(vars_selected$Party)
)


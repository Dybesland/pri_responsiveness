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

# R Interprets Norwegian letters
Sys.setlocale(locale = "no_NO.UTF-8") # For Mac
# Sys.setlocale(locale = "no_NO.utf8") # For Windows

# Load packages 
library(stortingscrape)  # v0.3.0
library(rvest)           # v1.0.3 
library(tidyverse)       # v2.0.0 
library(stringr)         # v1.5.1 
library(dplyr)           # v1.1.4 
library(xml2)            # v1.3.6 
library(future.apply)    # v1.11.1
library(readr)           # v2.1.5
library(lubridate)       # v1.9.3
library(readxl)          # v1.4.3

# ===============================================================================
# Scrape Parliamentary Session Publications
# ===============================================================================
# Define and scrape parliamentary session years, then process and store publication metadata.

# Define session years for scraping.
session_years <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019",
                   "2019-2020", "2020-2021", "2021-2022", "2022-2023", "2023-2024")

# Retrieve metadata for specified session publications using parallel processing for efficiency.
publication_ids <- future_lapply(session_years, get_session_publications)
publication_ids <- do.call(rbind, publication_ids)  # Combine lists into a single data frame.

# Filter out publications not available in formats XML and HTML/XML.
publication_ids <- publication_ids[publication_ids$publication_format %in% c("XML", "HTML//XML"), ]

# Extract list of publication IDs for retrieval.
ids <- publication_ids$publication_id

# Prepare a list to store the content of each publication.
pub_list <- list()

# Loop through each publication ID, fetch the content, and store it using the ID as the key.
for (id in ids) {
  pub <- get_publication(id)  # Fetch the publication using its ID.
  pub_list[[id]] <- pub  # Store the fetched publication in the list.
}


# -------------------------------------------------------------------------------
# Scrape Pre-2016 Parliamentary Session Publications (_b2016)
# -------------------------------------------------------------------------------
# Process publications from before 2016 due to differences in HTML/XML structure from after 2016.

# Prepare an empty data frame for storing extracted information.
df_b2016 <- data.frame(publication_id = character(),
                       meeting_case_title = character(), 
                       speech_type = character(),
                       speech_text = character(),
                       speaker_name = character(),
                       stringsAsFactors = FALSE)

# Loop through each XML document in the publication list (pub_list).
for (publication_id in names(pub_list)) {  
  publication_xml <- pub_list[[publication_id]]  # Extract the XML document from the list.
  
  # Find all 'meeting' nodes within the XML document to process individual meetings.
  meetings <- xml_find_all(publication_xml, ".//mote")
  
  # Iterate through each 'meeting' node to extract relevant details.
  for (meeting in meetings) {
    meeting_case_title <- xml_text(xml_find_first(meeting, ".//saktit"), trim = TRUE)
    
    # Collect 'innlegg' (speech) and 'presinnl' (presentation) nodes from the current meeting.
    speeches <- c(xml_find_all(meeting, ".//innlegg"), xml_find_all(meeting, ".//presinnl"))
    
    # Iterate through each speech node to extract and compile speech information.
    for (speech in speeches) {
      speech_type <- xml_name(speech)  # Identifies the type of speech ('innlegg' or 'presinnl').
      speech_type_2 <- xml_attr(speech, "type")  # Additional type classification, if available.
      speech_text <- xml_text(xml_find_first(speech, ".//a"), trim = TRUE)  # Extracts the speech text.
      speaker_name <- xml_text(xml_find_first(speech, ".//navn"), trim = TRUE)  # Extracts the speaker's name.
      
      # Construct a new row with the extracted information and add it to the data frame.
      new_row_b2016 <- data.frame(publication_id = publication_id,  # Publication ID from the pub_list.
                                  meeting_case_title = meeting_case_title, 
                                  speech_type = speech_type,
                                  speech_type_2 = speech_type_2,
                                  speech_text = speech_text,
                                  speaker_name = speaker_name,
                                  stringsAsFactors = FALSE)
      df_b2016 <- rbind(df_b2016, new_row_b2016)  # Append the new row to the data frame.
    }
  }
}


# -------------------------------------------------------------------------------
# Scrape Post-2016 Parliamentary Session Publications (_a2016)
# -------------------------------------------------------------------------------
# Process publications from after 2016 due to differences in HTML/XML structure from after 2016

# Initialize an empty data frame for storing extracted information, including speaker IDs.
df_a2016 <- data.frame(publication_id = character(),
                       meeting_case_title = character(), 
                       speech_type = character(),
                       speech_text = character(),
                       speaker_name = character(),
                       speaker_id = character(),
                       stringsAsFactors = FALSE)


# Iterate through each XML document listed in 'pub_list'.
for (publication_id in names(pub_list)) {  
  publication_xml <- pub_list[[publication_id]]  # Extract the XML document from the list.
  
  # Find all 'meeting' nodes within the XML document to process individual meetings.
  meetings <- xml_find_all(publication_xml, ".//mote")
  
  # For each 'meeting' node, extract relevant details.
  for (meeting in meetings) {
    meeting_case_title <- xml_text(xml_find_first(meeting, ".//saktittel"), trim = TRUE)
    
    # Collect various types of speech nodes ('hovedinnlegg', 'presinnlegg', 'replikk') from the current meeting.
    speeches <- c(xml_find_all(meeting, ".//hovedinnlegg"), xml_find_all(meeting, ".//presinnlegg"), xml_find_all(meeting, ".//replikk"))
    
    # Iterate through each collected speech node to compile detailed speech information.
    for (speech in speeches) {
      speech_type <- xml_name(speech)  # Identifies the speech's main type ('hovedinnlegg', 'presinnlegg', 'replikk').
      speech_type_2 <- xml_name(speech)  # Redundant here, could be meant for additional type distinctions.
      speech_text <- xml_text(xml_find_first(speech, ".//a"), trim = TRUE)  # Extracts the actual text of the speech.
      speaker_name <- xml_text(xml_find_first(speech, ".//navn"), trim = TRUE)  # Extracts the speaker's name.
      speaker_id <- xml_attr(xml_find_first(speech, ".//navn"), "personid")  # Extracts the unique speaker ID.
      
      # Construct a new row with the gathered information for appending to the data frame.
      new_row_a2016 <- data.frame(publication_id = publication_id,  # Uses the name from 'pub_list' as 'doc_id'.
                                  meeting_case_title = meeting_case_title, 
                                  speech_type = speech_type,
                                  speech_type_2 = speech_type_2,
                                  speech_text = speech_text,
                                  speaker_name = speaker_name,
                                  speaker_id = speaker_id, 
                                  stringsAsFactors = FALSE)
      df_a2016 <- rbind(df_a2016, new_row_a2016)  # Append the new row to the data frame.
    }
  }
}

#===============================================================================
# TIDYING SPEECHES 
#===============================================================================
# ------------------------------------------------------------------------------
# Tidying Speech Data Before 2016
# ------------------------------------------------------------------------------
# Clean and structure the speech data from the period before 2016.

# Joining 'df_b2016' with 'publication_ids' to add relevant publication information.
df_b2016 <- left_join(df_b2016, publication_ids, by = "publication_id")

# Categoriing speech data into periods based on session_id.
df_b2016 <- df_b2016 %>%
  mutate(
    period_id = case_when(
      session_id %in% c("2013-2014","2015-2016","2014-2015") ~ "2013-2017",
      TRUE ~ NA_character_  # Set period_id to NA if none of the above conditions are met
    )
  )

# Cleaning speaker names by removing parentheses, colons, brackets, and trailing whitespaces.
df_b2016$speaker_name <- sub("\\(.*$", "", df_b2016$speaker_name)
df_b2016$speaker_name <- sub("\\:.*$", "", df_b2016$speaker_name)
df_b2016$speaker_name <- sub("\\[.*$", "", df_b2016$speaker_name)
df_b2016$speaker_name <- trimws(df_b2016$speaker_name)

# Normalising speaker roles based on predefined roles in the name coloumn and correcting speaker names to standard format.
df_b2016 <- df_b2016 %>%
  mutate(
    # Extract roles based on specific starting words.
    role = str_extract(speaker_name, "^(Statsminister|Statsråd|Utenriksminister|Tredje visepresident|Første visepresident|Stortingspresident|Tidligere miljø- og utviklingsminister|Møtelederen|Tidligere miljøvernminister|Møteleder)"),
    # Remove the extracted role from the name.
    speaker_name = str_replace(speaker_name, "^(Statsminister|Statsråd|Utenriksminister|Tredje visepresident|Første visepresident|Stortingspresident|Tidligere miljø- og utviklingsminister|Møtelederen|Tidligere miljøvernminister|Møteleder)\\s*", ""),
    # Convert empty strings to NA in speaker_name after removal operation.
    speaker_name = na_if(speaker_name, ""),
    # Ensure no NAs are created in the role column (replace NA with an empty string if you prefer).
    role = if_else(is.na(role), "", role)
  )

# Correcting speaker names due to different spellings found in the data.
df_b2016<- df_b2016%>%
  mutate(speaker_name = case_when(
    speaker_name == "Abid Q. Raja" ~ "Abid Raja",
    speaker_name == "Abid Q.Raja" ~ "Abid Raja",
    speaker_name == "Anne Odenmarck" ~ "Anne Berit Odenmarck",
    speaker_name == "Arild Stokkan-Grande" ~"Arild Grande", 
    speaker_name == "Christopher Wand" ~ "Christopher Amundsen Wand",
    speaker_name == "Elisabeth Aspaker" ~ "Elisabeth Vik Aspaker", 
    speaker_name == "Else-May Botten" ~ "Else-May Norderhus", 
    speaker_name == "Frank J. Jensen" ~ "Frank J. Jenssen", 
    speaker_name == "Freddy de Reuiter" ~ "Freddy de Ruiter", 
    speaker_name == "Fredrik Bjørnebekk" ~ "Fredrik Bjørnebekk-Waagen",
    speaker_name == "Geir S. Toskedal" ~ "Geir Sigbjørn Toskedal",
    speaker_name == "Gina Knutson Barstad" ~ "Gina Barstad",
    speaker_name == "Helge Andre Njåstad"  ~ "Helge André Njåstad",
    speaker_name == "Helge Torheim"  ~ "Helge Thorheim",
    speaker_name == "Ine M. Eriksen Søreide"  ~ "Ine Eriksen Søreide",
    speaker_name == "Ingebjørg Godskesen"  ~ "Ingebjørg Amanda Godskesen",
    speaker_name == "Ingjerd Schou"  ~ "Ingjerd Schie Schou",
    speaker_name == "Ingrid Kjerkol" ~ "Ingvild Kjerkol", 
    speaker_name == "Jan-Arild Ellingsen" ~ "Jan Arild Ellingsen", 
    speaker_name ==  "Jenny Ellaug Følling"  ~ "Jenny Følling", 
    speaker_name ==  "Jette Christensen"  ~ "Jette F. Christensen",
    speaker_name ==  "Jorid Holstad Nordmelan"  ~ "Jorid Juliussen Nordmelan",
    speaker_name ==  "Ketil Solvik Olsen"  ~ "Ketil Solvik-Olsen",
    speaker_name ==  "Kjell Idar Juvik"  ~ "Kjell-Idar Juvik",
    speaker_name ==  "Laila Marie Reiertsen"   ~ "Laila  Reiertsen",
    speaker_name ==  "Lasse Juliussen"    ~ "Lasse Nordmelan Juliussen",
    speaker_name ==  "Linda C. Hofstad Helleland"    ~ "Linda Hofstad Helleland" ,
    speaker_name ==  "Line Henriette Hjemdal"  ~ "Line Henriette Holten" ,
    speaker_name ==  "Lise Wiik"   ~ "Lise Solveig Wiik" ,
    speaker_name ==  "Mange Rommetveit"   ~ "Magne Rommetveit" ,
    speaker_name ==  "Marie Brekke"   ~ "Marie Ljones Brekke"  ,
    speaker_name ==  "Madassar Kapur" ~ "Mudassar Kapur"  ,
    speaker_name ==  "Olaug V. Bollestad"  ~ "Olaug Vervik Bollestad"  ,
    speaker_name ==  "Ove Bernt Trellevik" ~ "Ove Trellevik"  ,
    speaker_name ==  "Per Olaf Lundteigen Sp"   ~ "Per Olaf Lundteigen"  ,
    speaker_name ==  "Peter Christian Frølich"    ~ "Peter Frølich" ,
    speaker_name ==  "Rangdi Krogstad"  ~ "Rangdi  Krogstad" ,
    speaker_name ==  "Regina Aleksandrova"  ~ "Regina Alexandrova" ,
    speaker_name ==  "Rasmus Hanson" ~ "Rasmus Hansson" ,
    speaker_name ==  "Rasmus Hanssen" ~ "Rasmus Hansson" ,
    speaker_name ==  "Ruth Mari Grung"  ~ "Ruth Grung",
    speaker_name ==  "Tom E.B. Holthe" ~ "Tom E. B. Holthe" ,
    speaker_name ==  "Ulf Leirstein"  ~ "Ulf Isak Leirstein" ,
    speaker_name ==  "Une Aina Bastholm"  ~ "Une Bastholm" ,
    speaker_name ==  "Veronica Pedersen" ~ "Veronica Pedersen Åsheim" ,
    speaker_name ==  "Øystein Hansen"    ~ "Øystein Langholm Hansen" ,
    speaker_name ==  "Madussar Kapur" ~ "Mudassar Kapur",
    TRUE ~ speaker_name  #  Keep all other names as they are
  ))


# Load MP data and add speaker names for merging.
mp_data <- read_csv("data/raw/mp_data.csv")
mp_data$speaker_name <- mp_data$name

# Merging 'df_b2016' with 'mp_data' based on matching speaker names and period IDs to get more info on Mp.
df_b2016 <- right_join(mp_data, df_b2016, by = c("speaker_name","period_id"))

# Role  and speech type adjustments.
df_b2016 <- df_b2016 %>%
  mutate(
    role = na_if(role, ""),  # Convert empty strings to NA in role
    role = case_when(
      speech_type == "presinnl" ~ "president",  # Change role where speech_type is 'presinnl'
      type == "Representant" ~ "representative",  # Change role based on 'type'
      type == "Vararepresentant" ~ "substitute representative",  # Change role based on 'type'
      is.na(name) & is.na(role) ~ "other",  # Set role to 'other' if both name and role are NA
      TRUE ~ role  # Otherwise, keep the original role
    ), 
    speech_type_2 = case_when(
      speech_type == "presinnl"~"presinnlegg", 
      TRUE ~ speech_type_2
    )
  )


# Selecting and renaming columns in 'df_b2016'.
df_b2016<- df_b2016%>%
  select(
    response_date = response_date.x,
    version = version.x,
    publication_id,
    publication_available_date,
    publication_date, 
    publication_format, 
    publication_title, 
    meeting_case_title, 
    publication_type,
    period_id, 
    session_id,
    mp_id, 
    mp_name = name, 
    mp_firstname = firstname, 
    mp_lastname = lastname, 
    mp_birth = birth, 
    mp_death = death,
    mp_gender = gender,
    mp_party = party_id, 
    mp_county = county, 
    mp_county_id = county_id, 
    mp_substitute = substitute_mp,
    mp_committee_id1 = committee_id_1, 
    mp_committee_id2 = committee_id_2, 
    mp_committee_id3 = committee_id_3, 
    mp_committee_id4 = committee_id_4,
    mp_committee_name1 = committee_name_1,
    mp_committee_name2 = committee_name_2,
    mp_committee_name3 = committee_name_3, 
    mp_committee_name4 = committee_name_4,
    speaker_name, 
    speaker_role = role, 
    speech_type = speech_type_2, 
    speech_text
  )


# Save df_b2016 
write_csv(df_b2016, "data/raw/df_b2016.csv")
save(df_b2016, file = "data/raw/df_b2016.Rdata" )

# ------------------------------------------------------------------------------
# Subsection: Tidying Speech Data After 2016
# ------------------------------------------------------------------------------
# Cleaning and structure the speech data from the period after 2016.

# Joining 'df_b2016' with 'publication_ids' to add relevant publication information.
df_a2016 <- left_join(df_a2016, publication_ids, by = "publication_id")

# Categoriing speech data into periods based on session_id.
df_a2016 <- df_a2016 %>%
  mutate(
    period_id = case_when(
      session_id %in% c("2016-2017") ~ "2013-2017",
      session_id %in% c("2017-2018", "2018-2019", "2019-2020", "2020-2021") ~ "2017-2021",
      session_id %in% c("2021-2022", "2022-2023", "2023-2024") ~ "2021-2025",
      TRUE ~ NA_character_  # Set period_id to NA if none of the above conditions are met
    )
  )

# Clean the speech text by removing any characters before the first closing parenthesis or colon followed by a space.
df_a2016$speech_text <- sub("^[^):]*[:)]\\s*", "", df_a2016$speech_text)
# Remove all colon characters from the speech text.
df_a2016$speech_text <- sub(":", "", df_a2016$speech_text)


# Remove any text (and the text's enclosing parentheses) that appears after an opening parenthesis.
df_a2016$speaker_name <- sub("\\(.*", "", df_a2016$speaker_name)
# Remove any text following a closing parenthesis in speaker names.
df_a2016$speaker_name <- sub("\\).*", "", df_a2016$speaker_name)
# Eliminate any content within square brackets and the brackets themselves in speaker names.
df_a2016$speaker_name <- sub("\\[.*", "", df_a2016$speaker_name)
# Remove any trailing characters after a closing square bracket in speaker names
df_a2016$speaker_name <- sub("\\].*", "", df_a2016$speaker_name)
# Replace newline characters in speaker names with a space.
df_a2016$speaker_name <- gsub("\\n", " ", df_a2016$speaker_name)
# Replace carriage return characters in speaker names with a space.
df_a2016$speaker_name <- gsub("\\r", " ", df_a2016$speaker_name)
# Condense multiple consecutive whitespace characters in speaker names.
df_a2016$speaker_name <- gsub("\\s+", " ", df_a2016$speaker_name)
# Remove any characters following a colon in speaker names.
df_a2016$speaker_name <- sub("\\:.*", "", df_a2016$speaker_name)


# Trim whitespaces and set 'speaker_name' to NA if it contains more than 5 words for data cleanliness.
df_a2016 <- df_a2016 %>%
  mutate(
    speaker_name = str_trim(speaker_name),  # Trim leading and trailing whitespaces first
    speaker_name = if_else(str_count(speaker_name, "\\S+") > 5, NA_character_, speaker_name)
  )

# Move speakers roles from speaker_name to role
df_a2016 <- df_a2016 %>% mutate(
  # Extract roles based on specific starting words, handling potential case and space variations
  role = str_extract(speaker_name, regex("^(Statsminister|Statsråd|Utenriksminister|Tredje visepresident|Første visepresident|Stortingspresident|Tidligere miljø- og utviklingsminister|Møtelederen|Møteleiaren|Tidligere miljøvernminister|Møteleder|Lederen|Fung\\.\\s*leder|Presidenten|Fung leder|Fleire|Flere|Fleire frå salen|Flere fra salen|fra salen|frå salen|Statsård|Avdelingsdirektør|Andre visepresident|Femte visepresident|Fjerde visepresident)", ignore_case = TRUE)),
  # Remove the extracted role from the name, handling potential case and space variations
  speaker_name = str_replace(speaker_name, regex("^(Statsminister|Statsråd|Utenriksminister|Tredje visepresident|Første visepresident|Stortingspresident|Tidligere miljø- og utviklingsminister|Møtelederen|Møteleiaren|Tidligere miljøvernminister|Møteleder|Lederen|Fung\\.\\s*leder|Presidenten|Fung leder|Fleire|Flere|Fleire frå salen|Flere fra salen|fra salen|frå salen|Statsård|Avdelingsdirektør|Andre visepresident|Femte visepresident|Fjerde visepresident)\\s*", ignore_case = TRUE), ""),
  # Ensure no NAs are created in the role column (replace NA with an empty string if you prefer)
  role = if_else(is.na(role), "", role)
) %>%
  mutate(speaker_name = trimws(speaker_name), speech_text = trimws(speech_text))


# Remove space following the first letter in 'speaker_name' if present
df_a2016 <- df_a2016 %>%
  mutate(
    speaker_name = if_else(str_detect(speaker_name, "^\\S{1}\\s"),
                           str_replace(speaker_name, "^(\\S)\\s", "\\1"),
                           speaker_name)
  )


# Correcting speaker names due to different spellings found in the data.
df_a2016 <- df_a2016%>%
  mutate(speaker_name = case_when(
    speaker_name == "Abid Q. Raja" ~ "Abid Raja",
    speaker_name == "Else-May Botten" ~ "Else-May Norderhus", 
    speaker_name == "Ine M. Eriksen Søreide"  ~ "Ine Eriksen Søreide",
    speaker_name == "Ingjerd Schou"  ~ "Ingjerd Schie Schou",
    speaker_name ==  "Linda C. Hofstad Helleland"    ~ "Linda Hofstad Helleland" ,
    speaker_name ==  "Ulf Leirstein"  ~ "Ulf Isak Leirstein",
    speaker_name == "Arnfinn Nergård" ~ "Arnfinn Nergård", 
    speaker_name == "Bent Høie"  ~ "Bent Høie",
    speaker_name == "Bjørn Arild Gram" ~ "Bjørn Arild Gram",
    speaker_name ==  "Bård Vegar Solhjell"    ~ "Bård Vegar Solhjell" ,
    speaker_name ==  "Carl I Hagen"  ~ "Carl I. Hagen",
    speaker_name ==  "Carl. I. Hagen"  ~ "Carl I. Hagen",
    speaker_name ==  "Emilie Enger Mehl"  ~ "Emilie Mehl",
    speaker_name ==  "Gro Anita Mykjåland"  ~"Gro-Anita Mykjåland",
    speaker_name ==  "Nils. T. Bjørke"   ~ "Nils T. Bjørke",
    speaker_name ==  "Ola Borten Moe"   ~ "Ola Borten Moe",
    speaker_name ==  "Per-Willly Amundsen"    ~ "Per-Willy Amundsen" ,
    speaker_name ==  "Trine-Lise Sundnes"     ~ "Trine Lise Sundnes" ,
    # Add more conditions as needed
    TRUE ~ speaker_name  # This keeps all other names as they are
  ))


# Merging 'df_b2016' with 'mp_data' based on matching speaker names and period IDs to get more info on Mp.
df_a2016 <- right_join(mp_data, df_a2016, by = c("speaker_name","period_id"))


# Role  and speech type adjustments.
df_a2016 <- df_a2016 %>%
  mutate(
    role = na_if(role, ""),  # Convert empty strings to NA in role
    role = case_when(
      speech_type == "presinnl" ~ "president",  # Change role where speech_type is 'presinnl'
      type == "Representant" ~ "representative",  # Change role based on 'type'
      type == "Vararepresentant" ~ "substitute representative",  # Change role based on 'type'
      is.na(name) & is.na(role) ~ "other",  # Set role to 'other' if both name and role are NA
      TRUE ~ role  # Otherwise, keep the original role
    )
  )

# Selecting and renaming columns in 'df_a2016'.
df_a2016<- df_a2016%>%
  select(
    response_date = response_date.x,
    version = version.x,
    publication_id,
    publication_available_date,
    publication_date, 
    publication_format, 
    publication_title, 
    meeting_case_title, 
    publication_type,
    period_id, 
    session_id,
    mp_id, 
    mp_name = name, 
    mp_firstname = firstname, 
    mp_lastname = lastname, 
    mp_birth = birth, 
    mp_death = death,
    mp_gender = gender,
    mp_party = party_id, 
    mp_county = county, 
    mp_county_id = county_id, 
    mp_substitute = substitute_mp,
    mp_committee_id1 = committee_id_1, 
    mp_committee_id2 = committee_id_2, 
    mp_committee_id3 = committee_id_3, 
    mp_committee_id4 = committee_id_4,
    mp_committee_name1 = committee_name_1,
    mp_committee_name2 = committee_name_2,
    mp_committee_name3 = committee_name_3, 
    mp_committee_name4 = committee_name_4,
    speaker_name, 
    speaker_role = role, 
    speech_type = speech_type_2, 
    speech_text
  )


# Save df_a2016
write_csv(df_a2016, "data/raw/df_a2016.csv")
save(df_a2016, file = "data/raw/df_a2016.Rdata" )



# ------------------------------------------------------------------------------
# Subsection: Combining and Cleaning Speech Data Before and After 2016
# ------------------------------------------------------------------------------
# Combine speech data from before and after 2016 into one dataset.
speech_data <- rbind(df_a2016, df_b2016)

# Convert 'publication_date' from string to Date format for consistency and easier handling.
speech_data <- speech_data %>%
  mutate(publication_date = as.Date(publication_date))

# Standardize 'speaker_role' by translating Norwegian roles to English and converting all to lowercase.
speech_data <- speech_data %>%
  mutate(
    speaker_role = case_when(
      speaker_role == "Lederen"                               ~ "Leader",
      speaker_role == "Statsråd"                              ~ "Minister",
      speaker_role == "Fung. leder" | speaker_role == "Fung leder" ~ "Acting Leader",
      speaker_role == "Møtelederen" | speaker_role == "Møteleiaren" ~ "Meeting Leader",
      speaker_role == "Presidenten" | speaker_role == "presidenten" ~ "President",
      speaker_role == "Flere" | speaker_role == "Fleire"       ~ "Several",
      speaker_role == "Møteleder"                             ~ "Meeting Leader",
      speaker_role == "Statsminister"                         ~ "Prime Minister",
      speaker_role == "Avdelingsdirektør"                      ~ "Department Director",
      speaker_role == "Tidligere miljøvernminister"            ~ "Former Minister of the Environment",
      speaker_role == "Tidligere miljø- og utviklingsminister" ~ "Former Minister of Environment and Development",
      speaker_role == "Utenriksminister"                       ~ "Foreign Minister",
      TRUE                                                     ~ speaker_role  # Default case to keep original values
    ),
    speaker_role = tolower(speaker_role),  # Ensure uniformity in casing for role names
  )

# Remove non-letter characters from the beginning of 'speech_text' for standardization and clarity.
speech_data <- speech_data %>%
  mutate(
    speech_text = str_replace(speech_text, "^[^A-Za-z]+", "")
  )

# Remove rows where 'speech_text' is an empty string to ensure data quality and relevance.
speech_data <- speech_data %>%
  filter(speech_text != "")

# Translate gender to English 
speech_data <- speech_data %>%
  mutate(mp_gender = case_when(
    mp_gender == "kvinne" ~ "female",
    mp_gender == "mann"   ~ "male",
    TRUE                  ~ mp_gender  # Keep original value (including NA)
  ))

# Save the cleaned and combined speech data to CSV and RData formats for future analysis.
write_csv(speech_data, "data/raw/speech_data.csv")
save(speech_data, file = "data/raw/speech_data.Rdata")


# Take out representatives speeches and stortingsmeetings: Calling it Parla_nor
parla_nor<- speech_data %>%
  filter((str_starts(publication_id, "s") | str_starts(publication_id, "refs")) &
           (speaker_role %in% c("representative", "substitute representative")))

# Add government/parliament information
parla_nor <- parla_nor %>%
  mutate(
    government_name = case_when(
      period_id == "2021-2025" ~ "Støre I",
      period_id == "2017-2021" ~ "Solberg II",
      period_id == "2013-2017" ~ "Solberg I",
      TRUE ~ NA_character_  # Assign NA for other cases or leave as is if no other periods are considered
    ),
    government_ideology = case_when(
      period_id == "2021-2025" ~ "labour/left",
      period_id == "2017-2021" ~ "conservative/right",
      period_id == "2013-2017" ~ "conservative/right",
      TRUE ~ NA_character_  # Assign NA for other cases or leave as is if no other periods are considered
    ), 
    share_seats_women = case_when(
      period_id == "2021-2025" ~ 45.6,
      period_id == "2017-2021" ~ 41.1,
      period_id == "2013-2017" ~ 39.6,
      TRUE ~ NA_real_  # Use NA_real_ for numeric NA
    )
  )

parla_nor$share_seats_men <- 100-parla_nor$share_seats_women
parla_nor$total_n_seats <- 169

# Add year 
parla_nor <- parla_nor %>%
  mutate(publication_year = year(as.Date(publication_date)))


# Save data
write_csv(parla_nor, "data/processed/parla_nor.csv")
save(parla_nor, file = "data/processed/parla_nor.Rdata")









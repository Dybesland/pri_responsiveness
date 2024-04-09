#===============================================================================
#  File:    Get MP Info 
#
#  Date:    March, 2024
#
#  Paper:   Miss represented or Misrepresented? Gendered Prioritiy
#           Responsiveness in the Norwegian Parliament
# 
#  Author:  Sara Dybesland
#  Purpose: Use stortingsscrape to get mp info 

#===============================================================================
library(tidyr)
library(dplyr)
library(stortingscrape)
library(future.apply)

# GET MP INFO: 
parlperiods <- get_parlperiods()
parlperiods <- parlperiods[1:3,4]
mp_inf <- lapply(parlperiods, function(pp) {
  get_parlperiod_mps(pp, substitute = TRUE)
})
mp_inf <- do.call(rbind, mp_inf)

mp_bio <- future_lapply(mp_inf$mp_id, get_mp_bio)
mp_bio <- do.call(rbind, mp_bio)
mp_bio <- as.data.frame(mp_bio)

save(mp_bio, file = "data/raw/mp_large_df.Rdata")
# Subset and combine 


mp_position <- mp_bio %>% select(root, parl_positions) %>% 
  unnest(root) %>% 
  unnest(parl_positions, keep_empty = TRUE) %>%
  filter(parl_period_id %in% c("2013-2017", "2017-2021", "2021-2025")) %>% 
  filter(committee_type == "FAG") %>% 
  select(id, committee_id, committee_name, parl_period_id, 
         mp_id = id)%>% 
  rename(period_id = parl_period_id) %>% 
  distinct()

table(mp_position$period_id, mp_position$committee_id)

mp_periods <- mp_bio %>% select(root, parl_periods) %>% 
  unnest(root) %>% 
  unnest(parl_periods, keep_empty = TRUE) %>%
  filter(parl_period_id %in% c("2013-2017", "2017-2021", "2021-2025")) %>%
  select(id, county, parl_period_id, type) %>% 
  rename(period_id = parl_period_id, 
         mp_id = id) %>% 
  distinct()


mp_data <- left_join(mp_periods, mp_position, by = c("period_id", "mp_id"))

mp_data <- left_join(mp_inf, mp_data, by = c("period_id", "mp_id"))


mp_data$name <- paste(mp_data$firstname , mp_data$lastname )

mp_data <- mp_data %>%
  group_by(period_id, mp_id) %>%
  mutate(committee_index = row_number()) %>%
  ungroup()

# Pivot both committee_id and committee_name columns wider
mp_data <- mp_data %>%
  pivot_wider(
    id_cols = c(response_date, version, mp_id, lastname, firstname, name, birth, death, gender, county_id, party_id, substitute_mp, county, type, period_id, ),  # Keeps these columns as identifiers
    names_from = committee_index,
    values_from = c(committee_id, committee_name),
    names_sep = "_",  # Separate original column name and index with an underscore
  )
names(mp_data)

mp_data <- mp_data %>% distinct(name, period_id, .keep_all = TRUE)

write_csv(mp_data, "data/raw/mp_data.csv")

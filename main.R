library(tidyverse)
library(tidycensus)

gov <- read_csv("data/state_gov.csv")
names(gov) <- c(
  "ind",
  "ward",
  "evers",
  "michels",
  "beglinger",
  "write_in"
)

gov_long <- gov |> 
  select(-ind) |> 
  pivot_longer(cols = c(2:5), names_to = "candidate", values_to = "votes") |> 
  mutate(office = "governor") |> 
  group_by(ward) |> 
  mutate(share = votes / sum(votes)) |> 
  select(-votes) |> 
  pivot_wider(names_from = candidate, values_from = share) |> 
  mutate(gov_margin = evers - michels) |> 
  select(ward, gov_margin)


sen <- read_csv("data/us_senator.csv")
names(sen) <- c(
  "ind",
  "ward",
  "barnes",
  "johnson",
  "write_in"
)

sen_long <- sen |> 
  select(-ind) |> 
  pivot_longer(cols = c(2:4), names_to = "candidate", values_to = "votes") |> 
  mutate(office = "senator") |>
  group_by(ward) |> 
  mutate(share = votes / sum(votes)) |> 
  select(-votes) |> 
  pivot_wider(names_from = candidate, values_from = share) |> 
  mutate(sen_margin = barnes - johnson) |> 
  select(ward, sen_margin)


both <- left_join(sen_long, gov_long) |> 
  mutate(diff = gov_margin - sen_margin)


wards <- tigris::voting_districts(state = "WI", county = "Milwaukee")

joined <- wards |> 
  mutate(w = str_extract(NAME20, "[:digit:]+") |> 
           str_remove_all(pattern = "^0+")) |> 
  select(w) |> 
  right_join(both |> 
               mutate(w = str_extract(ward, "[:digit:]+")))

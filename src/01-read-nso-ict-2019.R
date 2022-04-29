# malawi data -------------------------------------------------------------

# support ref: https://github.com/szimmer/CongressionalApportionment/blob/master/01_ReadCensusPDF.R

pdf_path <- "http://www.nsomalawi.mw/images/stories/data_on_line/economics/ICT/ICT%20Household%20Survey%202019.pdf"

library(pdftools)
library(tidyverse)

PDFInfo <- pdftools::pdf_info(pdf_path)
txt <- pdftools::pdf_text(pdf_path)

page130 <- txt[[130]]

page130split <- str_split(page130, pattern = "\\n", simplify = TRUE) %>% as.vector()

tidy_data <- tibble(raw_text = page130split) %>% 
  filter(row_number() >= 9) %>%
  filter(row_number() <= 32) %>%

  mutate(text_squish = str_squish(raw_text)) %>% 
  select(-raw_text) %>% 
  mutate(text_squish = str_replace_all(text_squish, "City", "")) %>% 
  mutate(text_squish = str_replace_all(text_squish, "Bay", "")) %>% 
  mutate(text_squish = str_squish(text_squish)) %>% 
  #mutate(text_squish = str_split(text_squish, pattern = "\\s")) %>% View()
  separate(text_squish, into = c("district",
                                 "owns_radio",
                                 "listens_to_radio",
                                 "watches_tv",
                                 "computer_use",
                                 "mobile_phone",
                                 "internet_access",
                                 "mobile_money",
                                 "postal_service_use"), sep = "\\s") %>% 
  mutate(district = case_when(
    district == "Nkhata" ~ "Nkhata Bay",
    district == "Mzuzu" ~ "Mzuzu City",
    district == "Lilongwe" & owns_radio == "65.0" ~ "Lilongwe City",
    district == "Zomba" & owns_radio == "56.8" ~ "Zomba City",
    district == "Blantyre" & owns_radio == "56.3" ~ "Blantyre City",
    TRUE ~ district
  )) %>% 
  pivot_longer(cols = !district,
               names_to = "variable",
               values_to = "percent")

readr::write_csv(tidy_data, file = "data/tidy_data/nso-malawi-ict-survey-2019.csv")

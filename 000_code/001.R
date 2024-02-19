library(tidyverse)
library(janitor)
library(readxl)
library(MASS)

setwd("~/1_r/project1/000_code")
data_dir <- "../001_data/"

#Import files
odf21 <- read_xlsx(paste0(data_dir, "data2021.xlsx"))
odf22 <- read_xlsx(paste0(data_dir, "data2022.xlsx"))
odf23 <- read_xlsx(paste0(data_dir, "data2023.xlsx"))

odf <- read_xlsx(paste0(data_dir, "datavr.xlsx")) 

# Function that widens the columns "mutatie_reden_omschrijving" and "relatiecode
# _plaats" because they contain two values. If there are more than two 
cleanread <- function(dfxx){
  janitor::clean_names(dfxx
  ) |>
  separate_wider_delim(
    "mutatie_reden_omschrijving",
    delim = " ",
    names = c("mutatie_reden", "mutatie_omschrijving"),
    too_few = "align_start",
    too_many = "merge"
  ) |> 
  separate_wider_delim(
    "relatiecode_plaats",
    delim = "_ ",
    names = c("relatiecode", "relatieplaats"),
    too_few = "align_start",
    too_many = "merge"
  )}

#Data cleaning
df21 <- cleanread(odf21)
df22 <- cleanread(odf22)
df23 <- cleanread(odf23)

#Stock dataset import and cleaning
df <- odf |> 
  janitor::clean_names() |> 
  dplyr::select(
    "artikelcode",
    "artikel_omzet_groep_id",
    "inkoopprijs",
    "verkoop_prijs",
    "consumenten_adviesprijs",
    "locatie_i",
    "locatie_ii",
    "nonactief",
    "verpakkingseenheid",
    "afbeelding",
    "nieuw",
    "aanbieding",
    "opruiming",
    "datum_voorraad_controle"
  )
df2122 <- full_join(df21, df22)
dfv0 <- full_join(df2122, df23)
dfv1 <- left_join(dfv0, df, by = "artikelcode")

dfv1$mutatie_omschrijving <- if_else(is.na(dfv1$mutatie_omschrijving), "toename", dfv1$mutatie_omschrijving)
dfv1$mutatie_omschrijving <- if_else(dfv1$mutatie_omschrijving == "(toename)", "toename", dfv1$mutatie_omschrijving)
dfv1$mutatie_omschrijving <- if_else(dfv1$mutatie_omschrijving == "(afname)", "afname", dfv1$mutatie_omschrijving)                
dfv1$mutatie_reden <- if_else(dfv1$mutatie_reden == "Voorraadverschil", "Voorraadmutatie", dfv1$mutatie_reden)
dfv1$mutatie_reden <- if_else(dfv1$mutatie_reden == "Incourant", "Voorraadmutatie", dfv1$mutatie_reden)
dfv1$relatiecode <- if_else(is.na(dfv1$relatiecode), "242", dfv1$relatiecode)
dfv1$relatieplaats <- if_else(is.na(dfv1$relatieplaats), "Magazijn", dfv1$relatieplaats)
dfv1$relatieplaats <- if_else(dfv1$relatiecode == 236, "Kassa", dfv1$relatieplaats)

dfv1$weekdag <- wday(dfv1$datum, label = TRUE)
dfv1$dag <- mday(dfv1$datum)
dfv1$maand <- month(dfv1$datum)
dfv1$jaar <- year(dfv1$datum) 

begin <- dfv1 |> 
  slice_head(by = artikelcode
             ) |> 
  dplyr::select("artikel_begin_stand",
                "artikelcode"
              ) |> 
  rename(begin = artikel_begin_stand)

dfv1 <- left_join(dfv1, begin, by = "artikelcode")

dfv1 <- dfv1 |> 
  mutate(supply_at = cumsum(aantal) + begin, .by = "artikelcode")



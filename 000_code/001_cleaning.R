library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(lmtest)   # Provides Breusch-Pagan and RESET Tests
library(moments)  # Provides skewness and kurtosis
library(sandwich) # Provided HC variance estimators
library(prophet)

setwd("~/1_r/project1/000_code")
data_dir <- "../001_data/"

# In- and outflow data for 2021, 22, and 23.
odf21 <- read_xlsx(paste0(data_dir, "data2021.xlsx"))
odf22 <- read_xlsx(paste0(data_dir, "data2022.xlsx"))
odf23 <- read_xlsx(paste0(data_dir, "data2023.xlsx"))

# Additional information for products
odf <- read_xlsx(paste0(data_dir, "datavr.xlsx")) 

# Function that widens the columns "mutatie_reden_omschrijving" and 
# "relatiecode_plaats" because they contain two values. 

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

df21 <- cleanread(odf21)
df22 <- cleanread(odf22)
df23 <- cleanread(odf23)


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
    "datum_voorraad_controle"
  )

# Joining the datasets together to create dfv1, 3 years of in- outflow data +
# additional information, join key = artikelcode
df2122 <- full_join(df21, df22)
dfv0 <- full_join(df2122, df23)
dfv1 <- left_join(dfv0, df, by = "artikelcode")

# Treating missing data and standardizing categorical values
dfv1$mutatie_omschrijving <- 
  if_else(is.na(dfv1$mutatie_omschrijving), "toename", dfv1$mutatie_omschrijving)
dfv1$mutatie_omschrijving <- 
  if_else(dfv1$mutatie_omschrijving == "(toename)", "toename", dfv1$mutatie_omschrijving)
dfv1$mutatie_omschrijving <- 
  if_else(dfv1$mutatie_omschrijving == "(afname)", "afname", dfv1$mutatie_omschrijving)                
dfv1$mutatie_reden <- 
  if_else(dfv1$mutatie_reden == "Voorraadverschil", "Voorraadmutatie", dfv1$mutatie_reden)
dfv1$mutatie_reden <- 
  if_else(dfv1$mutatie_reden == "Incourant", "Voorraadmutatie", dfv1$mutatie_reden)
dfv1$relatiecode <- 
  if_else(is.na(dfv1$relatiecode), "242", dfv1$relatiecode)
dfv1$relatieplaats <- 
  if_else(is.na(dfv1$relatieplaats), "Magazijn", dfv1$relatieplaats)
dfv1$relatieplaats <- 
  if_else(dfv1$relatiecode == 236, "Kassa", dfv1$relatieplaats)
# Werkbon and pakbon are unrealized transactions and will thus not be included
# in the dataset
dfv1 <- dfv1 |> 
  filter(!mutatie_reden == "Werkbon"
         ) |> 
  filter(!mutatie_reden == "Pakbon")


# Creates the variable inflow and outflow, inflow is any action whereby money
# is earned and outflow is any action whereby money is lost
# All values are positive doubles
dfv1 <- dfv1 |> 
  mutate(inflow =
           if_else(mutatie_reden == "Factuur" & aantal < 0,
                   aantal * (verkoop_prijs*-1),
                   0
                   )
         )|> 
  mutate(inflow =
           if_else(mutatie_reden == "Voorraadmutatie" & aantal > 0,
                   aantal * (inkoopprijs),
                   inflow
                   )
         )|> 
  mutate(outflow =
           if_else(mutatie_reden == "Factuur" & aantal > 0,
                   aantal * (verkoop_prijs),
                   0
                   )
         ) |> 
  mutate(outflow =
           if_else(mutatie_reden == "Voorraadmutatie" & aantal < 0,
                   aantal * (inkoopprijs*-1),
                   outflow
                   )
         ) |> 
  mutate(outflow =
           if_else(mutatie_reden == "Ontvangst",
                   aantal * (inkoopprijs),
                   outflow
                   )
         )

dfv1 <- dfv1 |> 
  select(
  "relatiecode",
  "relatieplaats",
  "artikelcode",
  "aantal",
  "datum",
  "mutatie_reden",
  "mutatie_omschrijving",
  "inflow",
  "outflow",
  "artikel_omzet_groep_id",
  "inkoopprijs",
  "verkoop_prijs",
  "consumenten_adviesprijs",
  "locatie_i",
  "locatie_ii",
  "verpakkingseenheid",
  "afbeelding",
  "datum_voorraad_controle"
)

dfsales <- dfv1 |> filter(mutatie_reden == "Factuur")
dfpurchase <- dfv1 |> filter(mutatie_reden == "Ontvangst")
dferror <- dfv1 |> filter(mutatie_reden == "Voorraadmutatie")

write_rds(dfsales, "../001_data/sales.RDS")

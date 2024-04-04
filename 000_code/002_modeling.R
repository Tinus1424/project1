################################################################################
# Testing some packages and their functions
#
################################################################################
library(tidymodels)
library(tidyverse)
library(modeltime)
library(timetk)
library(lubridate)
tidymodels_prefer()

setwd("~/1_r/project1/000_code")

rds_data = "../001_data/sales.RDS"

df <- read_rds(rds_data)


data <- df |> 
  mutate(
    datum = floor_date(ymd(datum), "month")
  ) |> 
  filter(
    artikelcode == 20429311,
  ) |> 
  select(
    datum,
    aantal
  ) |> 
  group_by(
    datum
  ) |> 
  summarise(
    sum_aantal = sum(aantal)
  )


data |> plot_time_series(datum, sum_aantal)


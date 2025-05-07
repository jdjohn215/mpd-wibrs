rm(list = ls())

library(tidyverse)

# match historic WIBRS to geocodes

wibrs.historic <- read_csv("https://data.milwaukee.gov/dataset/5a537f5c-10d7-40a2-9b93-3527a4c89fbd/resource/395db729-a30a-4e53-ab66-faeb5e1899c8/download/wibrarchive.csv")
mai <- read_csv("mai/addresses-with-coords.csv.gz")

wibr.location.join <- wibrs.historic |>
  left_join(mai, by = join_by(Location == address_string)) |>
  mutate(final_x = if_else(is.na(x), RoughX, x),
         final_y = if_else(is.na(y), RoughY, y))

sum(!is.na(wibr.location.join$x))/nrow(wibr.location.join) # matched to a more detailed geocode

wibr.valid.location <- wibr.location.join |>
  filter(!is.na(final_x)) |>
  select(-c(RoughX, RoughY, x, y)) |>
  mutate(Arson = as.integer(Arson),
         AssaultOffense = as.integer(AssaultOffense),
         Burglary = as.integer(Burglary),
         CriminalDamage = as.integer(CriminalDamage),
         Homicide = as.integer(Homicide),
         LockedVehicle = as.integer(LockedVehicle),
         Robbery = as.integer(Robbery),
         SexOffense = as.integer(SexOffense),
         Theft = as.integer(Theft),
         VehicleTheft = as.integer(VehicleTheft))

write_csv(wibr.valid.location, "wibr-historic-valid-location.csv.gz")

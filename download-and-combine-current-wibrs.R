rm(list = ls())

library(readr)
library(dplyr)

# match current WIBRS to geocodes

wibrs.current <- read_csv("https://data.milwaukee.gov/dataset/e5feaad3-ee73-418c-b65d-ef810c199390/resource/87843297-a6fa-46d4-ba5d-cb342fb2d3bb/download/wibr.csv")
mai <- read_csv("mai/addresses-with-coords.csv.gz")

wibr.location.join <- wibrs.current |>
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

# read in historic wibrs
wibrs.historic <- read_csv("wibr-historic-valid-location.csv.gz")

wibrs.all <- bind_rows(wibrs.historic, wibr.valid.location)
deduplicate <- wibrs.all |>
  group_by(IncidentNum) |>
  slice_max(order_by = ReportedDateTime, n = 1, with_ties = F)

write_csv(deduplicate, "wibrs-complete.csv.gz")

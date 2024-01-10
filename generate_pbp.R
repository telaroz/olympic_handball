# Women Tokyo 2020

library(handbaloner)
library(stringr)
library(furrr)
future::plan(future::multisession)
match_numbers <- 60000:61147

base_url <- "https://www.ihf.info/competitions/women/307/2020-olympic-games-tokyo---women039s-tournament/20353/match-center/"

furrr::future_walk(paste0(base_url, match_numbers), 
                   handbaloner::scrape_from_ihf, 
                   folder = "tokyo/women")

pbps <- purrr::map_df(list.files("tokyo/women", "PBP", full.names = TRUE),
                      handbaloner::generate_tidy_pbp,
                      match_id_pattern = 'Match ', gender = "W")
data <- pbps
verify_pbp <- function(data){
  data <- data[,
               .(match_id, score, half, team, teams, is_home, number_of_possession,
                 possession, start_of_possession, end_of_possession,
                 lead, numeric_time)]
  
  data <- data[,.SD[.N], .(match_id, number_of_possession)]
  
  data[, start_of_possession := data.table::fifelse(stringr::str_length(start_of_possession) == 8,
                                                    stringr::str_sub(start_of_possession, 1, 5),
                                                    start_of_possession)]
  
  
  data[, end_of_possession := data.table::fifelse(stringr::str_length(end_of_possession) == 8,
                                                  stringr::str_sub(end_of_possession, 1, 5),
                                                  end_of_possession)]
  
  data[, possession_length := as.numeric(lubridate::ms(end_of_possession)) -
         as.numeric(lubridate::ms(start_of_possession))]
  
  data[, sum(possession_length)]
  data[, sum(possession_length), .(match_id, half)]
}


# Correcciones

# "Match " no detected in pdf sheet
data[teams == "ROC - FRA" & is.na(match_id), match_id := 38]
# Remove match 37 to add it again
data <- data[teams != "NOR - SWE"]
match_37 <- handbaloner::generate_tidy_pbp(list.files("tokyo/women", "PBP", full.names = TRUE)[37], match_id_pattern = "Match ")
match_37[, match_id := 37]

data <- data.table::rbindlist(list(data, match_37))

handbaloner::plot_paces(data, 38)
verify_pbp(data)

data.table::fwrite(data, "tokyo/womens_tokyo_2020.csv")

# Men Tokyo 2020
future::plan(future::multisession)
numeros_partidos <- 60000:61541

base_url <- "https://www.ihf.info/competitions/men/308/2020-olympic-games-tokyo---men039s-tournament/20351/match-center/"

furrr::future_walk(paste0(base_url, numeros_partidos), handbaloner::scrape_from_ihf, folder = "tokyo/men")


pbps_men <- purrr::map_df(list.files("tokyo/men", "PBP", full.names = TRUE), handbaloner::generate_tidy_pbp,
                      match_id_pattern = 'Match ')
data_men <- pbps_men

verify_pbp(data_men)

# Correcciones

# No se habían detectado 
data_men[teams == "EGY - ESP" & is.na(match_id), match_id := 37]
data_men[teams == "FRA - DEN" & is.na(match_id), match_id := 38]

verify_pbp(data_men)

match_21 <- handbaloner::generate_tidy_pbp(list.files("tokyo/men", "PBP", full.names = TRUE)[21], match_id_pattern = "Match ")


data_men <- data.table::rbindlist(list(data_men, match_21))
verificaciones(data_men)

data.table::fwrite(data_men, "tokyo/mens_tokyo_2020.csv")

# Setup
library(tidyverse)
library(rvest)
library(readxl)
library(lubridate)


################ Salary Data #################
##############################################
url <- "https://www.spotrac.com/nhl/rankings/player/_/year/2023/sort/cash_total"

webpage <- read_html(url)

players_data <- webpage %>%
    html_nodes(xpath = '//*[@id="table"]/div[1]/ul/li')


players_text <- players_data %>%
    html_text()

# Remove ranking numbers and trailing whitespace from each row
players_text <- players_text %>%
    sub("^[0-9]+\\s*", "", .)

# splitting text into separate columns
players_df <- data.frame(
    raw_text = players_text %>%
        str_replace_all("\\n", "") %>%
        str_squish() %>%
        sub("^[0-9]+\\s*", "", .),
    stringsAsFactors = FALSE
)

players_df <- players_df %>%
    mutate(
        Name = str_extract(raw_text, "[^,]+"), 
        Salary = str_extract(raw_text, "\\$.*")
    ) 


players_df$Name <- gsub("\\b[A-Z]{3}\\b", "", players_df$Name)

players_df$Salary <- as.numeric(gsub("[\\$,]", "", players_df$Salary))

players_df <- players_df[,2:3]


################## TOI Data ##################
##############################################
toi_data <- read_xlsx("toi_data.xlsx")
toi_data <- as.data.frame(toi_data)

toi_data$GP <- as.numeric(toi_data$GP)

toi_data <- toi_data |>
    mutate(
        avg_shift = period_to_seconds(hms(paste0("00:", Shift))),
        even_toi_sec = period_to_seconds(hms(paste0("00:", even_toi))),
        pp_toi_sec = period_to_seconds(hms(paste0("00:", pp_toi))),
        sh_toi_sec = period_to_seconds(hms(paste0("00:", sh_toi))),
    )

toi_data2 <- toi_data %>% 
    select(-c(Shift, even_toi, pp_toi, sh_toi))


########## Joining and Saving as csv #########
##############################################
players_df$Name <- trimws(players_df$Name)

joined_df <- inner_join(players_df, toi_data2, by = c("Name" = "Player"))

# checking if duplicates
# duplicates <- joined_df[duplicated(joined_df), ]
# nrow(duplicates)
# print(duplicates)

nhl_data <- joined_df

write.csv(nhl_data, "nhl_data.csv", row.names = FALSE)
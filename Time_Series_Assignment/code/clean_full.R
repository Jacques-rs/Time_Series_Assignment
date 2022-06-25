

full_clean <- function(df){

    df <- df %>%
        mutate(obs = as.double(gsub(.$obs, pattern = "Q", replacement = "0"))) %>%
        mutate(obs = ceiling_date(lubridate::yq(obs), unit = "quarter") - 1) %>%
        rename(date = obs)

    return(df)
}
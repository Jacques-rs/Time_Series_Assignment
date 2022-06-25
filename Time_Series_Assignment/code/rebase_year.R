
# Creating a function that will convert the base year
# for the df from 2015 to 1995


lreer_rebase <- function(df, New_Base_Start = 19950131,
                         New_Base_End = 19951231){

    new_base_year <- df %>%  filter(Date >= lubridate::ymd(New_Base_Start)
                                  & Date <= lubridate::ymd(New_Base_End))
    new_base_factor <- 100/mean(new_base_year$Value)

    df <- df %>% mutate(Value = Value*new_base_factor)

    return(df)
}


# Convert the date column to the correct format

lreer_wrangle_date <- function(df){

    df <- df |> transmute(Date = as.Date(paste("01-", Date, sep = ""), format = "%d-%b-%Y"),
                          Value = Value) |>
        transform( Date = ceiling_date(Date, "month") - days(1) ) |>
        arrange(Date) |>
        as_tibble()

    return(df)

}
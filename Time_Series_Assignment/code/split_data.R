

split_data_endog <- function(df){

    df <- df[,1:13]

    return(df)
}


split_data_exog <- function(df){

    df <- df %>% cbind(.[,1], .[,14:20]) %>%
        gather(season, season_dum, 2:4) %>%
        gather(outlier, outlier_dum, 2:5)

    return(df)
}




split_data_endog <- function(df){

    df <- df[,1:13]

    return(df)
}


split_data_exog <- function(df){

    df2 <- df %>% dplyr::select(1 | 14:20) %>%
        mutate(Outliers = DUMRER1 + DUMRER2 + DUMFBYA + DUMNFAOFPY,
               .keep = "unused")


    #%>%
        # gather(season, season_dum, 2:4) %>%
        # gather(outlier, outlier_dum, 2:5)

    return(df2)
}


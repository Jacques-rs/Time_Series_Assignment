

split_data_endog <- function(df){

    df <- df[,1:13]

    return(df)
}


split_data_exog <- function(df){

    df <- cbind(df[,1], df[,14:20])

    return(df)
}


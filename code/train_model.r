### r pre-run
library(dynlm)
library(formula.tools)
library(imputeTS)
df_mean <- read.csv("data/spinach_weather_pintung.csv")


train <- function(df_mean, penalty, penalty_all, n=1, fill="locf", no_summary=FALSE){
    time_value <- c()
    col_names <- names(df_mean)
    for (i in 1:length(col_names)){
        if (substring(col_names[i], nchar(col_names[i]) - 3, nchar(col_names[i])) == "time"){
                time_value <- c(time_value, i)
        }
    }
    df_mean <- df_mean[, -time_value]
    df_mean_copy <- df_mean
    
    if (fill=="locf") df_mean$price <- na.locf(df_mean$price, option = "locf")
    else if (fill=="nocb") df_mean$price <- na.locf(df_mean$price, option = "nocb")
    else if (fill=="linear") df_mean$price <- na.interpolation(df_mean$price, option = "linear")
    else if (fill=="spline") df_mean$price <- na.interpolation(df_mean$price, option = "spline")
    else if (fill=="ma_simple") df_mean$price <- na.ma(df_mean$price, weight = "simple")
    else if (fill=="ma_linear") df_mean$price <- na.ma(df_mean$price, weight = "linear")
    else if (fill=="ma_exp") df_mean$price <- na.ma(df_mean$price, weight = "exponential")


    na_col <- sapply(df_mean, function(x){sum(is.na(x))})
    na_col <- names(na_col[na_col==0])
    df_mean <- df_mean[ ,na_col]
    # df_mean[is.na(df_mean)] <- 0

    ts_df <- df_mean
    ts_df$t_spread <- ts_df$tmax - ts_df$tmin
    ts_df$stnpres_spread <- ts_df$stnpresmax - ts_df$stnpresmin

    train_data <- ts_df[1:84, ]
    test_data <- ts_df[85:nrow(ts_df), ]

    for (i in 1:ncol(df_mean)){
    ts_df[, i] <- ts(df_mean[, i])
    train_data[, i] <- ts(train_data[, i]) 
    test_data[, i] <- ts(test_data[, i])
    }

    ### r reg
    reg1 <- dynlm(price ~ L(price, 1) + L(stnpres, 1) + L(seapres, 1) + L(stnpresmax, 1) + L(stnpresmin, 1) + L(temperature, 1) + L(tmax, 1) + L(tmin, 1) + L(tddewpoint, 1) + L(ws, 1) + L(wd, 1) + L(wsgust, 1) + L(wdgust, 1)  + L(precphour, 1) + L(precpday, 1)   + L(precp1daymax, 1) + L(rh, 1) + L(evapa, 1) + L(sunshine, 1) + L(globlrad, 1) + L(uvimaxmean, 1) + L(uvimax, 1) + L(cloudamount, 1)  + L(t_spread, 1) + L(stnpres_spread, 1), data = train_data)
    reg2 <- dynlm(price ~ L(price, 2) + L(stnpres, 2) + L(seapres, 2) + L(stnpresmax, 2) + L(stnpresmin, 2) + L(temperature, 2) + L(tmax, 2) + L(tmin, 2) + L(tddewpoint, 2) + L(ws, 2) + L(wd, 2) + L(wsgust, 2) + L(wdgust, 2)  + L(precphour, 2) + L(precpday, 2)   + L(precp1daymax, 2) + L(rh, 2) + L(evapa, 2) + L(sunshine, 2) + L(globlrad, 2) + L(uvimaxmean, 2) + L(uvimax, 2) + L(cloudamount, 2)  + L(t_spread, 2) + L(stnpres_spread, 2), data = train_data)
    reg3 <- dynlm(price ~ L(price, 3) +L(stnpres, 3) + L(seapres, 3) + L(stnpresmax, 3) + L(stnpresmin, 3) + L(temperature, 3) + L(tmax, 3) + L(tmin, 3) + L(tddewpoint, 3) + L(ws, 3) + L(wd, 3) + L(wsgust, 3) + L(wdgust, 3)  + L(precphour, 3) + L(precpday, 3) + L(precp1daymax, 3) + L(rh, 3) + L(evapa, 3) + L(sunshine, 3) + L(globlrad, 3) + L(uvimaxmean, 3) + L(uvimax, 3) + L(cloudamount, 3) + L(t_spread, 3) + L(stnpres_spread, 3), data = train_data)
    reg4 <- dynlm(price ~ L(price, 4) + L(stnpres, 4) + L(seapres, 4) + L(stnpresmax, 4) + L(stnpresmin, 4) + L(temperature, 4) + L(tmax, 4) + L(tmin, 4) + L(tddewpoint, 4) + L(ws, 4) + L(wd, 4) + L(wsgust, 4) + L(wdgust, 4)  + L(precphour, 4) + L(precpday, 4)   + L(precp1daymax, 4) + L(rh, 4) + L(evapa, 4) + L(sunshine, 4) + L(globlrad, 4) + L(uvimaxmean, 4) + L(uvimax, 4) + L(cloudamount, 4)  + L(t_spread, 4) + L(stnpres_spread, 4), data = train_data)


    for(i in c(1:n)){
    ### r tstep and ICs
    #penalty <- 3 #AIC when penalty==2
    penalty1 <- log(length(reg1$residuals)) #BIC
    penalty2 <- log(length(reg2$residuals))
    penalty3 <- log(length(reg3$residuals))
    penalty4 <- log(length(reg4$residuals))
    d <- "backward"

    s <- capture.output(tstep1 <- step(reg1, direction=d, k=penalty)) 
    s <- capture.output(tstep2 <- step(reg2, direction=d, k=penalty)) 
    s <- capture.output(tstep3 <- step(reg3, direction=d, k=penalty)) 
    s <- capture.output(tstep4 <- step(reg4, direction=d, k=penalty)) 
    rm(s)



    ### r coeff
    coeff1 <- gsub("\\)", "", gsub(", [0-9]", "", gsub(".*\\(", "" ,names(tstep1$coefficient))))
    coeff2 <- gsub("\\)", "", gsub(", [0-9]", "", gsub(".*\\(", "" ,names(tstep2$coefficient))))
    coeff3 <- gsub("\\)", "", gsub(", [0-9]", "", gsub(".*\\(", "" ,names(tstep3$coefficient))))
    coeff4 <- gsub("\\)", "", gsub(", [0-9]", "", gsub(".*\\(", "" ,names(tstep4$coefficient))))

    coeff1 <- coeff1[coeff1!="Intercept"]
    coeff2 <- coeff2[coeff2!="Intercept"]
    coeff3 <- coeff3[coeff3!="Intercept"]
    coeff4 <- coeff4[coeff4!="Intercept"]

    coeff_union <- Reduce(union, list(coeff1, coeff2, coeff3, coeff4))

    ### r reg with fewer vars
    create_formula <- function(dep_string, vars_strings, lag_period=0){
    if (lag_period>0){
        f <- paste0("L(", vars_strings, ",", as.character(lag_period), ")")
    } else {
        f <- vars_strings
    }
    f <- paste0(f, collapse="+")
    f <- paste(dep_string, f, sep="~")
    formula(f)
    }

    f1 <- create_formula("price", coeff_union, 1)
    f2 <- create_formula("price", coeff_union, 2)
    f3 <- create_formula("price", coeff_union, 3)
    f4 <- create_formula("price", coeff_union, 4)

    reg1 <- dynlm(f1, data=train_data)
    reg2 <- dynlm(f2, data=train_data)
    reg3 <- dynlm(f3, data=train_data)
    reg4 <- dynlm(f4, data=train_data)
    }

    ### r reg2
    coeff_t1 <- names(tstep1$coefficients)
    coeff_t2 <- names(tstep2$coefficients)
    coeff_t3 <- names(tstep3$coefficients)
    coeff_t4 <- names(tstep4$coefficients)

    coeff_t1 <- coeff_t1[coeff_t1!="(Intercept)"]
    coeff_t2 <- coeff_t2[coeff_t2!="(Intercept)"]
    coeff_t3 <- coeff_t3[coeff_t3!="(Intercept)"]
    coeff_t4 <- coeff_t4[coeff_t4!="(Intercept)"]
    coeff_t_union <- Reduce(union, list(coeff_t1, coeff_t2, coeff_t3, coeff_t4)) 

    f <- create_formula("price", coeff_t_union) 
    reg_all <- dynlm(as.formula(as.character(f)), data=train_data)
    #penalty_all <- log(length(reg_all$residuals)) # BIC
    s <- capture.output(tstep_all <- step(reg_all, k=penalty_all)); rm(s)
    if(!no_summary) print(summary(tstep_all))

    ### r predict
    lag <- 1
    pre <- predict(tstep_all, test_data)
    pre_naive <- ts_df$price[c(84:107)]

    get_mse <- function(y_hat, y){
        mean(sum((y-y_hat)^2))
    }
    withdraw <- union(which(is.na(df_mean_copy$price)), which(is.na(df_mean_copy$price))-lag)-84
    test_price <- test_data$price[!c(1:24) %in% withdraw]

    print(get_mse(pre[!c(1:24) %in% withdraw], test_price))
    print(get_mse(pre_naive[!c(1:24) %in% withdraw], test_price))
    score <- get_mse(pre[!c(1:24) %in% withdraw], test_price)
    invisible(list(tstep_all, data.frame(test_price=df_mean_copy$price[c(85:108)], pre=pre, pre_naive=pre_naive), score))
}


### grid search
temp <- 0
min_score <- 1000000
model <- train(df_mean, 1, 1, fill="locf", no_summary=TRUE)
min_model <- model
for (type in c("locf", "nocb", "linear", "spline", "ma_simple", "ma_linear", "ma_exp")){
    for(i in c(1:10)){
        skip <- FALSE
        for(j in c(1:10)){
            cat("**Model-", type, " - ", as.character(i), " - ", as.character(j), "\n")
            tryCatch({
                        model <- train(df_mean, i, j, fill=type, no_summary=TRUE)
                        plotNA.imputations(min_model[[2]]$test_price, min_model[[2]]$pre, ylim=c(0, 150))
                        if(model[[3]] < min_score){
                            min_score <- model[[3]]
                            min_model <- model
                            min_param <- list(i, j, type)
                        }
                     }, warning = function(x){
                     }, error = function(x){
                        assign("skip", TRUE, env=globalenv())
                     }, finally ={
                         if(skip==TRUE | temp==model[[3]]) break
                         temp <- model[[3]]
                     }
            )
        }
    }
}
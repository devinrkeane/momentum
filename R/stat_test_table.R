library(stringr);library(data.table)

stat_test_table <- function(data = sigma_factor_returns, tests = list("shapiro.test", "jarque.bera.test"), 
                            start = "1998-01-01", end = "2016-12-31", series.names){
    for(ii in 1:length(tests)){
        test <- match.fun(test.name)
        
        stat_tests <- list(
            all_test_stat <- apply(data, 2, function(x) test(x)[[1]]),
            all_test_pvalue <- apply(data, 2, function(x) test(x)[[2]]),
            segment_test_stat <- apply(data[paste(start, "/", end, sep = "")], 2, function(x) test(x)[[1]]),
            segment_pvalue <-  apply(data[paste(start, "/", end, sep = "")], 2, function(x) test(x)[[2]])
        )

        normal_table <- lapply(stat_tests, function(x) round(as.matrix(x), 4) %>% as.data.frame) %>% bind_cols(.)

        rownames(normal_table) <- series_names

        colnames(normal_table) <- c(paste(str_sub(start, 0, 4), "-", str_sub(end, 0, 4), " Test-stat", sep = ""),
                                "p-value", 
                                paste(str_sub(start, 0, 4), "-", str_sub(end, 0, 4), " Test-stat", sep = ""),
                                "p-value")

       print(knitr::kable(normal_table, 
             caption = "test statistics and p-values, 1998-2016", 
             digits = 3))
    }
}



for(test.name in tests){
    print(test.name)
    }
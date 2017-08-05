library(readxl);library(quantmod);library(PerformanceAnalytics);library(RColorBrewer);library(xlsx);library(ggplot2)
library(dplyr);library(purrr);library(texreg);library(astsa);library(timeSeries);library(tseries);library(tidyr)


SP500.index <- monthlyReturn(na.omit(getSymbols("^GSPC", from="1950-01-01",
                                               to="2016-11-30", auto.assign=FALSE)[,6]))


factor.returns <- as.xts(read.zoo("MoM_Value_Size_monthly_returns_reduced.csv", header=TRUE,
                                  index.column=1, sep=",", format = "%m/%d/%Y"))

#set global plotting parameters
windowsFonts(font1=windowsFont("Perpetua"))
par(mfrow = c(1,1), mar=c(3,4,3,3)+.1, family="font1")



chart.CumReturns(factor.returns[,c(2,5,6,14)], lwd = 2,  wealth.index = TRUE,
                 col = brewer.pal(4, "Spectral"),
                 main = "Growth of $1, 1927-2016 for Momentum, Value, Size and Market Factors",
                 ylab = "Growth of $1",
                 xlab = "Year")


legend(50, 70000, legend = c("Momentum (>70th Percentile Prior 2-12 Month)", 
                             "Size (<30th Percentile Market Cap)",
                             "Value (>70th Percentile Book to Market Ratio)", 
                             "Market (NYSE, AMEX, NASDAQ Composite)"), 
       col = brewer.pal(4, "Spectral"), 
       border.col = "grey", pch = 3,
       cex = .7,
       pt.cex = 1) 

returns.table <- table.AnnualizedReturns(factor.returns[,c(2,5,6,14)])
downside.table <- table.DownsideRisk(factor.returns[,c(2,5,6,14)])
distribution.table <- table.Distributions(factor.returns[,c(2,5,6,14)])

all.table <- rbind(returns.table, downside.table, distribution.table)[-c(4:5,6,8,19,20,14,13),]


textplot(all.table, cmar = 1,
         main = "Risk Metrics For Momentum, Value, Size and Market Returns, 1927-2016", 
         wrap = FALSE)

factor.spread.table <- table.AnnualizedReturns(factor.returns.1927to2016[,c(1,2,3,6)])
factor.spread.table[1:2,] = round(factor.spread.table[1:2,]*100,4)
textplot(factor.spread.table, cmar = 1, cex=2, max.cex=1.75, wrap = FALSE)
title("Summary For Momentum, Value, Size and Market Spread Returns, 1927-2016", cex=2)


#back test 2006-2016
portfolio_return <- as.xts(read.zoo("portfolio.return.csv", index.column = 1, 
                                    header=T, sep=",", format = "%Y-%m-%d"))

returns.table2 <- table.AnnualizedReturns(portfolio_return["2006/2007"])
downside.table2 <- table.DownsideRisk(portfolio_return)
distribution.table2 <- table.Distributions(portfolio_return)
all.table2 <- rbind(returns.table2, downside.table2, distribution.table2)[-c(4:5,6,8,19,20,14,13),]

 
textplot(all.table2, cmar =3, rmar = 1,
         main = "Risk Metrics For Momentum, Value, Size and Market Returns, 1927-2016", 
         wrap.colnames = 10, wrap.rownames = 50)
title("Performance and Risk Metrics, Monthly Returns, Oct 2006-Dec 2016")

chart.CumReturns(portfolio_return, lwd = 2,  wealth.index = TRUE,
                 col = brewer.pal(6, "Set1"),
                 main = "Growth of $1, 2006-2016 for Momentum, Value, Size and Market Factors",
                 ylab = "Growth of $1",
                 xlab = "Year")


legend(0, 2.5, legend = c("Arcwood DAA, Asset Class Selection Only", 
                          "DAA With Trend and Asset Class Selection",
                          "Momentum (>70th Percentile Prior 2-12 Month)", 
                          "Size (<30th Percentile Market Cap)",
                          "Value (>70th Percentile Book to Market Ratio)", 
                          "Market (NYSE, AMEX, NASDAQ Composite)"), 
       col = brewer.pal(6, "Set1"), 
       border.col = "grey", pch = 20,
       cex = 1.25,
       pt.cex = 2) 

chart.Drawdown(portfolio_return, main="2006-2016 Drawdowns for Momentum, Value, Size and Market Factors")
daa.legend

###CAPM and Fama French Summaries####
par(mfrow=c(1,1))


decades <- function(month.period = 1:120, input = factor.returns.1927to2016, main="FF Factors"){
        chart.CumReturns(input[month.period,], main = main,lwd=.85,legend.loc = "none")
        print(table.AnnualizedReturns(input[month.period,]))
}

return.decades = list(1:120, 121:240, 241:360, 361:480, 481:600, 601:720, 721:840, 841:960, 960:1079)

chart.titles <- list("1927-1936", "1937-1946", "1947-1956", 
                     "1957-1966", "1967-1976", "1977-1986", 
                     "1987-1996", "1997-2006", "2007-2016" )
layout(matrix(c(1:9), nrow = 3, 3, byrow = T))
pwalk(list(return.decades, main=chart.titles), decades)

colnames(factor.returns.1927to2016)

invoke_map(decades, list(month.period=return.decades, main=chart.titles))


#histograms

factor.returns.df <- as_data_frame(factor.returns.1927to2016) %>% 
        mutate(date = index(factor.returns.1927to2016)) %>% 
        gather(factor, return, -date)

ggplot(factor.returns.df, aes(x=return, fill=factor)) + 
        geom_density(binwidth = .005, alpha = 0.6, aes(y=..scaled..)) 
        facet_grid(factor~.) +
        scale_fill_manual(values=rep("skyblue",6)) +
        scale_x_continuous(limits = c(-0.3, 0.3)) + 
        theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 10),
              strip.background = element_blank(), 
              legend.position = "none") + stat_function(fun=dnorm, args = )


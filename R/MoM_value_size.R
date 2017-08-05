MoM_Value_Size_monthly_returns <- read_excel("MoM_Value_Size_monthly_returns.xlsx", 
                                             +     col_types = c("date", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric", "numeric", 
                                                                 +         "numeric", "numeric"))

round(MoM_Value_Size_monthly_returns[-1],2)
colnames(MoM_Value_Size_monthly_returns)[13]="Avg 7-Hi Prior"
MoM_Value_Size_monthly_returns = as.xts(MoM_Value_Size_monthly_returns[-1], order.by = MoM_Value_Size_monthly_returns$Date) %>% ./100

factor.returns = MoM_Value_Size_monthly_returns[,c(11,12,14,16,32,34),]

factor.returns.table = data.frame(t(table.AnnualizedReturns(factor.returns)))

levels(factor.returns.table) = colnames(factor.returns.table)

factor.returns.table$factors = factor(rownames(factor.returns.table))




library(plotly)

dat1 <- data.frame(
        sex = factor(c("Female","Female","Male","Male")),
        time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
        total_bill = c(13.53, 16.81, 16.24, 17.42)
)

# Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()
ggplot(data=factor.returns.table, aes(x=factors, y=Annualized.Std.Dev, group=factors)) +
        geom_bar(colour="black", stat="identity",
                 position=position_dodge(),
                 size=.3) +                        # Thinner lines
        xlab("Time of day") + ylab("Total bill") + # Set axis labels
        ggtitle("Average bill for 2 people") +     # Set title
        theme_bw()

ggplotly()

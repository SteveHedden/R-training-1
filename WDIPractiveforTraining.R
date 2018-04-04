
#Pull in 4 series from World Development Indicators (WDI) API. Maternal mortality ratio,
#under-5 mortality rate, total fertility rate, and GDP per capita
wdi <- WDI(country="all", indicator=c("SH.STA.MMRT",
                                      "SH.DYN.MORT",
                                      "SP.DYN.TFRT.IN",
                                      "NY.GDP.PCAP.CD"), start=1990, end=2015)

#Save first 2 rows of the dataframe and print
wdi2 <- wdi[1:2,]
print(wdi2)

#Save the year 2015 data as new dataframe and print head
wdi3 <- wdi[wdi$year == 2015,]
head(wdi3)

#Save each series as a variable
maternalMort <- wdi3$SH.STA.MMRT
U5MR <- wdi3$SH.DYN.MORT
gdpPCP <- wdi3$NY.GDP.PCAP.CD
tfr <- wdi3$SP.DYN.TFRT.IN


#Create 2 regressions with maternal mortality as the dependent variable and print summary results
fit1 <- lm(maternalMort ~ tfr)
fit2 <- lm(log(maternalMort) ~ (tfr) + log(gdpPCP))
summary(fit1)
summary(fit2)

#Create a visual of the data using TFR as x-axis and maternal mort as y-axis
ggplot(wdi3, aes(x = (tfr), y = maternalMort)) + 
  geom_point(aes(size = wdi3$NY.GDP.PCAP.CD),shape=1) + 
  geom_smooth(formula = y ~ x, color = "red")

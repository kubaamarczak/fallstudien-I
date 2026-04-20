# RELOAD DATA FRESH FIRST
zensus_wide <- readr::read_csv("zensus2022_wide.csv")
zensus_long <- readr::read_csv("zensus2022_long.csv")

#TASK1
#check structures of variables 
str(zensus_wide)

#check if scales of variables are appropriate by checking min max median
summary(zensus_wide)

#we can see that OstDe is int, we must change it to factor
zensus_wide$Ostdeutschland <- factor(zensus_wide$Ostdeutschland,
                                     levels = c(0,1),
                                     labels = c("West", "East"))

zensus_long$Ostdeutschland <- factor(zensus_long$Ostdeutschland,
                                     levels = c(0, 1),
                                     labels = c("West", "East"))

#create Region column in both datasets
zensus_wide$Region <- zensus_wide$Ostdeutschland
zensus_long$Region <- zensus_long$Ostdeutschland

#reorder columns so Region appears in place of Ostdeutschland
library(dplyr)
zensus_wide <- zensus_wide %>%
  select(ARS, Name, Bundesland, Region, Bev_Tausend, Abitur_Quote, 
         OhneAbschl_Quote, Abitur_GenderGap, Erwerbstaetig_Quote, 
         Erwerbslos_Quote, Nichterwerb_Quote, Erwerbstaetig_GenderGap, 
         Erwerbslos_GenderGap)

#verify both
str(zensus_wide$Region)
str(zensus_long$Region)

#no missing values check
colSums(is.na(zensus_wide))

#categorical variables: ARS, Name, Bundesland, Region
#metric variables: all remaining 9 variables
#no further cleaning necessary


#TASK 2
library(ggplot2)
ggplot(zensus_long, aes(x="", y = Wert)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Boxplot of all metric variables")

#exact outliers using IQR rule for each variable
library(dplyr)

#Bev_Tausend
Q1 <- quantile(zensus_wide$Bev_Tausend, 0.25)
Q3 <- quantile(zensus_wide$Bev_Tausend, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(Bev_Tausend < (Q1 - 1.5 * IQR_val) | Bev_Tausend > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, Bev_Tausend) %>%
  arrange(desc(Bev_Tausend))

#Abitur_Quote
Q1 <- quantile(zensus_wide$Abitur_Quote, 0.25)
Q3 <- quantile(zensus_wide$Abitur_Quote, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(Abitur_Quote < (Q1 - 1.5 * IQR_val) | Abitur_Quote > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, Abitur_Quote) %>%
  arrange(desc(Abitur_Quote))

#OhneAbschl_Quote
Q1 <- quantile(zensus_wide$OhneAbschl_Quote, 0.25)
Q3 <- quantile(zensus_wide$OhneAbschl_Quote, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(OhneAbschl_Quote < (Q1 - 1.5 * IQR_val) | OhneAbschl_Quote > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, OhneAbschl_Quote) %>%
  arrange(desc(OhneAbschl_Quote))

#Abitur_GenderGap
Q1 <- quantile(zensus_wide$Abitur_GenderGap, 0.25)
Q3 <- quantile(zensus_wide$Abitur_GenderGap, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(Abitur_GenderGap < (Q1 - 1.5 * IQR_val) | Abitur_GenderGap > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, Abitur_GenderGap) %>%
  arrange(Abitur_GenderGap)

#Erwerbstaetig_Quote
Q1 <- quantile(zensus_wide$Erwerbstaetig_Quote, 0.25)
Q3 <- quantile(zensus_wide$Erwerbstaetig_Quote, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(Erwerbstaetig_Quote < (Q1 - 1.5 * IQR_val) | Erwerbstaetig_Quote > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, Erwerbstaetig_Quote) %>%
  arrange(Erwerbstaetig_Quote)

#Erwerbslos_Quote
Q1 <- quantile(zensus_wide$Erwerbslos_Quote, 0.25)
Q3 <- quantile(zensus_wide$Erwerbslos_Quote, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(Erwerbslos_Quote < (Q1 - 1.5 * IQR_val) | Erwerbslos_Quote > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, Erwerbslos_Quote) %>%
  arrange(desc(Erwerbslos_Quote))

#Nichterwerb_Quote
Q1 <- quantile(zensus_wide$Nichterwerb_Quote, 0.25)
Q3 <- quantile(zensus_wide$Nichterwerb_Quote, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(Nichterwerb_Quote < (Q1 - 1.5 * IQR_val) | Nichterwerb_Quote > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, Nichterwerb_Quote) %>%
  arrange(desc(Nichterwerb_Quote))

#Erwerbstaetig_GenderGap
Q1 <- quantile(zensus_wide$Erwerbstaetig_GenderGap, 0.25)
Q3 <- quantile(zensus_wide$Erwerbstaetig_GenderGap, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(Erwerbstaetig_GenderGap < (Q1 - 1.5 * IQR_val) | Erwerbstaetig_GenderGap > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, Erwerbstaetig_GenderGap) %>%
  arrange(Erwerbstaetig_GenderGap)

#Erwerbslos_GenderGap
Q1 <- quantile(zensus_wide$Erwerbslos_GenderGap, 0.25)
Q3 <- quantile(zensus_wide$Erwerbslos_GenderGap, 0.75)
IQR_val <- Q3 - Q1
zensus_wide %>%
  filter(Erwerbslos_GenderGap < (Q1 - 1.5 * IQR_val) | Erwerbslos_GenderGap > (Q3 + 1.5 * IQR_val)) %>%
  select(Name, Bundesland, Erwerbslos_GenderGap) %>%
  arrange(Erwerbslos_GenderGap)

#how to deal?
#since this is official census data, all values are real and valid.
#keep all outliers in the dataset
#interpret them in context of location and city
#we can use median instead of average, median is more robust. use mad instead of sd



#TASK 3 

#boxplots for each variable
library(ggplot2)
ggplot(zensus_long, aes(x = Region, y = Wert, fill = Region)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Boxplots of all variables by East/West",
       x = "", 
       y = "Value",
       fill = "Region") +
  theme_minimal()

#histograms for each variable

#Abitur_Quote
ggplot(zensus_wide, aes(x = Abitur_Quote, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Abitur_Quote by East/West",
       x = "Abitur_Quote (%)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()

#OhneAbschl_Quote
ggplot(zensus_wide, aes(x = OhneAbschl_Quote, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "OhneAbschl_Quote by East/West",
       x = "OhneAbschl_Quote (%)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()

#Abitur_GenderGap
ggplot(zensus_wide, aes(x = Abitur_GenderGap, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Abitur_GenderGap by East/West",
       x = "Abitur_GenderGap (pp)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()

#Bev_Tausend
ggplot(zensus_wide, aes(x = Bev_Tausend, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Bev_Tausend by East/West",
       x = "Population (thousands)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()

#Erwerbstaetig_Quote
ggplot(zensus_wide, aes(x = Erwerbstaetig_Quote, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Erwerbstaetig_Quote by East/West",
       x = "Erwerbstaetig_Quote (%)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()

#Erwerbslos_Quote
ggplot(zensus_wide, aes(x = Erwerbslos_Quote, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Erwerbslos_Quote by East/West",
       x = "Erwerbslos_Quote (%)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()

#Nichterwerb_Quote
ggplot(zensus_wide, aes(x = Nichterwerb_Quote, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Nichterwerb_Quote by East/West",
       x = "Nichterwerb_Quote (%)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()

#Erwerbstaetig_GenderGap
ggplot(zensus_wide, aes(x = Erwerbstaetig_GenderGap, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Erwerbstaetig_GenderGap by East/West",
       x = "Erwerbstaetig_GenderGap (pp)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()

#Erwerbslos_GenderGap
ggplot(zensus_wide, aes(x = Erwerbslos_GenderGap, fill = Region, color = Region)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, aes(y = after_stat(density))) +
  geom_density(adjust = 1.4, fill = NA) +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Erwerbslos_GenderGap by East/West",
       x = "Erwerbslos_GenderGap (pp)", y = "Density",
       fill = "Region", color = "Region") +
  theme_minimal()


#what do we see?
#Abitur_Quote
#west has much higher abitur rates than east
#west is more spread out, east is compact and lower

#Abitur_GenderGap
#west is more negative men have higher abitur rates in west
#east is closer to zero more equal between genders

#Bev_Tausend
#both heavily right skewed, most districts small, few very large
#berlin is the extreme outlier

#Erwerbslos_Quote
#very similar between east and west

#Erwerbstaetig_Quote

#west clearly higher,more people employed in west
#east shifted noticeably to the left

#Nichterwerb_Quote
#east clearly higher more people not in labour force at all
#direct opposite of the employment finding above

#Erwerbstaetig_GenderGap
#both negative men more employed than women everywhere
#west more negative bigger gender gap in west
#east closer to zero more equal in east

#OhneAbschl_Quote
#west has more outliers and higher values
#east lower and more compact

#Erwerbslos_GenderGap
#west positive men slightly more unemployed than women
#east negative women more unemployed than men
#clear structural difference between regions


#TASK 4
#create pairwise scatterplots and find the relationships
pairs(zensus_wide[, 5:13],
      lower.panel = NULL,
      col = ifelse(zensus_wide$Region == "East", "steelblue", "salmon"),
      pch = 16,
      cex = 0.5,
      main = "Pairwise scatterplots of all metric variables",
      oma = c(3, 3, 5, 3))

par(xpd = TRUE)
legend(x = "bottom",
       legend = c("West", "East"),
       col = c("salmon", "steelblue"),
       pch = 16,
       cex = 0.9,
       bty = "n",
       horiz = TRUE)

#what do we observe from the pairwise scatterplot?

#Erwerbstaetig_Quote vs Nichterwerb_Quote
#strongest relationship in the entire plot, tight diagonal line going downward
#as employment goes up, inactivity goes down, almost perfectly
#both east and west follow the same line closely

#Abitur_Quote vs OhneAbschl_Quote
#downward slope, more high education means less people with no certificate
#east clusters at lower abitur and lower ohneabschl values

#Abitur_Quote vs Erwerbslos_Quote
#slight downward slope, higher education linked to lower unemployment
#east and west somewhat separated

#OhneAbschl_Quote vs Erwerbslos_Quote
#upward slope, more people without certificates linked to higher unemployment

#east/west separation
#blue east dots cluster clearly lower than pink west in Abitur_Quote
#in Erwerbstaetig_Quote and Nichterwerb_Quote east and west visibly separated
#in Erwerbslos_Quote east and west overlap heavily, similar unemployment rates

#Bev_Tausend
#flat scattered lines with all other variables, no relationship
#population size tells us nothing about education or employment
#berlin visible as extreme outlier in all panels of this row

#gender gap variables
#mostly scattered clouds across all pairs, weak relationships overall


#TASK 5

#correlation plot with significance stars
library(psych)

#copy with shorter names, original data untouched
plot_data <- zensus_wide[, 5:13]
colnames(plot_data) <- c(
  "Bev_Tausend", "Abitur_Quote", "OhneAbschl",
  "Abitur_Gap", "Erwerbst_Quote", "Erwerbslos_Quote",
  "Nichterw_Quote", "Erwerbst_Gap", "Erwerbslos_Gap"
)

cor.plot(plot_data,
         numbers = TRUE,
         upper = FALSE,
         main = "Correlation plot of all metric variables",
         show.legend = FALSE,
         cex = 0.8,
         xlas = 2,
         stars = TRUE)

#significance analysis
result <- corr.test(zensus_wide[, 5:13])
round(result$r, 4)
round(result$p, 4)

#findings
#research conventions(cohen 1988) 

#r = 0.10 to 0.29 weak
#r = 0.30 to 0.49 moderate
#r = 0.50 to 1.00 strong
#sign indicates direction, magnitude indicates strength

#Erwerbstaetig_Quote vs Nichterwerb_Quote = -0.98, p = 0.000
#strongest correlation in the entire dataset, highly significant
#as employment goes up, inactivity goes down almost perfectly

#OhneAbschl_Quote vs Erwerbslos_Quote = 0.53, p = 0.000
#moderate positive, significant
#more people without certificates linked to higher unemployment

#Abitur_Quote vs Abitur_GenderGap = -0.47, p = 0.000
#moderate negative, significant
#higher abitur districts tend to have smaller gender gap

#Abitur_Quote vs OhneAbschl_Quote = 0.41, p = 0.000
#moderate positive, significant

#Erwerbstaetig_Quote vs Erwerbslos_Quote = -0.42, p = 0.000
#moderate negative, significant
#higher employment linked to lower unemployment

#Erwerbslos_GenderGap vs Erwerbstaetig_Quote = 0.54, p = 0.000
#moderate positive, significant

#Erwerbslos_GenderGap vs Nichterwerb_Quote = -0.58, p = 0.000
#moderate negative, significant

#Bev_Tausend vs most variables
#all near zero and not significant
#population size has no meaningful relationship with any other variable


##TASK 6 - causality discussion
#no code needed, purely a discussion question for the report

#can we conclude causality from our results?
#no, we cannot conclude causality because:

#reason 1 - observational data
#the census is not a controlled experiment
#we simply observed districts as they are
#no variables were manipulated

#reason 2 - confounding variables
#a third hidden variable could be causing both variables to move together
#example: poverty could cause both low education and high unemployment
#we cannot separate these effects with our data

#reason 3 - reverse causality
#we cannot tell which direction the relationship goes
#does low education cause unemployment?
#or does unemployment cause people to leave school early?
#both directions are plausible

#reason 4 - no time dimension
#our data is a snapshot from one point in time (may 2022)
#causality requires observing change over time

#conclusion
#we can only say variables are associated or correlated
#not that one causes the other
#establishing causality would require experimental data,
#longitudinal data, or more advanced causal inference methods


#TASK 7
#identify at least 3 variables that influence unemployment rate
#top 3 based on correlation: OhneAbschl_Quote, Erwerbstaetig_Quote, Abitur_Quote

#univariate methods

#scatterplot: OhneAbschl_Quote vs Erwerbslos_Quote
ggplot(zensus_wide, aes(x = OhneAbschl_Quote, y = Erwerbslos_Quote,
                        color = Region)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "OhneAbschl_Quote vs Erwerbslos_Quote",
       x = "Share without school certificate (%)",
       y = "Unemployment rate (%)",
       color = "Region") +
  theme_minimal()

#scatterplot: Abitur_Quote vs Erwerbslos_Quote
ggplot(zensus_wide, aes(x = Abitur_Quote, y = Erwerbslos_Quote,
                        color = Region)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Abitur_Quote vs Erwerbslos_Quote",
       x = "Abitur rate (%)",
       y = "Unemployment rate (%)",
       color = "Region") +
  theme_minimal()

#boxplot: Region vs Erwerbslos_Quote
boxplot(Erwerbslos_Quote ~ Region, data = zensus_wide,
        col = c("salmon", "steelblue"),
        main = "Unemployment rate by East/West",
        xlab = "Region",
        ylab = "Unemployment rate (%)")

#multivariate methods

#reset graphics device before cor.plot
dev.off()
par(mar = c(14, 14, 4, 2))

#correlation plot - shows all candidate variables together
library(psych)
cor.plot(zensus_wide[, c("Erwerbslos_Quote", "OhneAbschl_Quote", 
                         "Abitur_Quote", "Erwerbstaetig_Quote",
                         "Nichterwerb_Quote")],
         numbers = TRUE,
         upper = FALSE,
         main = "Correlations with unemployment rate",
         show.legend = TRUE,
         cex = 0.8,
         stars = TRUE,
         xlas = 2)

#multiple linear regression - quantifies influence of all 3 variables simultaneously
par(mar = c(5, 4, 4, 2))
model <- lm(Erwerbslos_Quote ~ 
              Abitur_Quote + 
              OhneAbschl_Quote + 
              Erwerbstaetig_Quote,
            data = zensus_wide)
summary(model)
#significant coefficients confirm which variables truly influence unemployment
#while controlling for the other variables simultaneously

#regression with region interaction
#checks if east/west differences change the relationships
model2 <- lm(
  Erwerbslos_Quote ~ 
    Abitur_Quote * Region + 
    OhneAbschl_Quote * Region,
  data = zensus_wide
)
summary(model2)

#conclusion task 7
#OhneAbschl_Quote is the strongest and most consistent predictor of unemployment
#Erwerbstaetig_Quote is a significant negative predictor
#Abitur_Quote is NOT significant when controlling for the other variables
#the effect of no-certificate rate is significantly stronger in east than west
#in East Germany the effect of low education on unemployment
#is twice as strong as in West Germany (0.59 vs 0.30)


##task 8 

library(psych)

#describeBy() from psych package gives descriptive statistics
#gives  mean, median, sd, min, max etc.

describeBy(zensus_wide[, 5:13],
           group = zensus_wide$Region)

#aggregate() calculates  statistic for each group
#calculate median and sd for each variable split by east/west

#median comparison
aggregate(zensus_wide[, 5:13],
          by = list(Region = zensus_wide$Region),
          FUN = median)

#standard deviation is from mean // use MAD comparison
aggregate(zensus_wide[, 5:13],
          by = list(Region = zensus_wide$Region),
          FUN = mad)

#task 8 findings

#Abitur_Quote
#west median 34.20% vs east median 26.45%
#west significantly higher, nearly 8 percentage points difference
#west more spread out, more variation between western districts

#OhneAbschl_Quote
#west median 6.25% vs east median 4.65%
#west higher, more people without any school certificate in west
#east more homogeneous, less variation between eastern districts

#Abitur_GenderGap
#west median -2.70 vs east median +0.20
#in west men have higher abitur rates
#in east women and men almost equal
#likely legacy of gdr education system promoting gender equality

#Erwerbstaetig_Quote
#west median 51.80% vs east median 47.25%
#west clearly higher, more people employed in west
#east also less spread out

#Erwerbslos_Quote
#west median 5.80% vs east median 6.10%
#almost identical, surprisingly little difference between east and west
#west has more extreme outliers

#Nichterwerb_Quote
#west median 45.40% vs east median 49.75%
#east clearly higher, more people economically inactive in east
#directly mirrors the employment finding
#east more homogeneous

#Erwerbstaetig_GenderGap
#west median -3.00 vs east median -2.30
#both negative, men more employed than women everywhere
#west gap larger, bigger gender inequality in west
#east closer to zero, more equal, likely gdr legacy

#Erwerbslos_GenderGap
#west median 0.60 vs east median 0.00
#west positive, men slightly more unemployed than women
#east at zero, no gender difference in unemployment

#overall
#east has lower education but less inequality between districts
#east has lower employment but similar unemployment to west
#east has more gender equality in both education and labour market
#west has higher education and employment but more polarisation
#differences reflect gdr legacy and ongoing structural challenges
#more than 30 years after reunification

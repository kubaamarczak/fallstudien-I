## TASK 1 : CLEANING DATA

#cleaning data first from the text files first
#read raw ECA&D temperature files for 6 stations in NRW
#extract station name, STAID, and elevation from each file header
#combine into one tidy data frame keyed on (station, year)
#recode -999999 as NA and convert temperatures from 0.01°C to °C
#drop empty 2026 rows

#save as stations_clean.csv for use in later scripts

library(readr)    # for reading text files
library(dplyr)    # for data manipulation
library(tidyr)    # for reshaping
library(stringr)  # for parsing the header strings

data_dir <- "Data"
files <- list.files(data_dir, pattern = "^indexTG_.*\\.txt$", full.names = TRUE)
files

#helper: extract metadata (station name, STAID, elevation) from header
parse_header <- function(path) {
  hdr  <- readLines(path, n = 25)
  text <- paste(hdr, collapse = "\n")
  
  station <- str_match(text, "STATION\\s+GERMANY\\s+(.+?)\\s+\\(STAID:\\s*(\\d+)\\)")
  elev    <- str_match(text, "ELEVATION:\\s*m\\s*(-?\\d+)")
  
  tibble(
    station_name = str_trim(station[, 2]),
    staid        = as.integer(station[, 3]),
    elevation_m  = as.integer(elev[, 2])
  )
}

#column names for the 21 data columns in each file
col_names <- c("souid", "year", "annual", "winter_half", "summer_half",
               "djf", "mam", "jja", "son",
               "jan","feb","mar","apr","may","jun",
               "jul","aug","sep","oct","nov","dec")

#read one station file: header + data + cleanup
read_station <- function(path) {
  meta <- parse_header(path)
  
  raw <- suppressWarnings(
    read_table(path,
               skip = 30,
               col_names = col_names,
               col_types = cols(.default = col_double()))
  )
  
  #keep only 21 named cols
  #drop blank trailing row
  #NA + convert to celsius
  raw %>%
    select(all_of(col_names)) %>%
    filter(!is.na(souid)) %>%
    mutate(across(annual:dec,
                  ~ ifelse(.x == -999999, NA_real_, .x / 100))) %>%
    mutate(station_name = meta$station_name,
           staid        = meta$staid,
           elevation_m  = meta$elevation_m)
}

#test on one file to see
test <- read_station(files[1])
glimpse(test)

#read all six files and make into one table
stations <- lapply(files, read_station) %>% bind_rows()

#add a short readable station label for plots and grouping
stations <- stations %>%
  mutate(station = recode(station_name,
                          "DUISBURG-FRIEMERSHEIM" = "Duisburg",
                          "DORTMUND"              = "Dortmund",
                          "ESSEN-BREDENEY"        = "Essen",
                          "ARNSBERG"              = "Arnsberg",
                          "BRILON"                = "Brilon",
                          "KAHLER ASTEN (WST)"    = "Kahler Asten"
  ))

#sanity check
stations %>%
  group_by(station, elevation_m) %>%
  summarise(year_min = min(year),
            year_max = max(year),
            n_years  = n(),
            .groups  = "drop") %>%
  arrange(elevation_m)

#drop the trailing 2026 rows because only until winter and we don't need for seasonal/annual analysis
stations <- stations %>%
  filter(year != 2026)

#move station info columns to the front
stations <- stations %>%
  relocate(station, station_name, staid, elevation_m)

#save the cleaned data so don't have to re-parse next time
write_csv(stations, file.path(data_dir, "stations_clean.csv"))

#final check
nrow(stations)
summary(stations[, c("annual", "djf", "jja")])


## TASK 1 : OUTLIER DETECTION
#check for impossible values and visualize outlier years per station
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#load the cleaned data we saved earlier
stations <- read_csv(file.path("Data", "stations_clean.csv"))


#reshape to long format for facet plotting
seasonal_long <- stations %>%
  select(station, year, annual, djf, mam, jja, son) %>%
  pivot_longer(cols = c(annual, djf, mam, jja, son),
               names_to = "season",
               values_to = "temp")

#order seasons logically and stations by elevation
seasonal_long <- seasonal_long %>%
  mutate(season = factor(season, levels = c("annual", "djf", "mam", "jja", "son")),
         station = factor(station, levels = c("Duisburg","Dortmund","Essen",
                                              "Arnsberg","Brilon","Kahler Asten")))

ggplot(seasonal_long, aes(x = station, y = temp, fill = station)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.5) +
  facet_wrap(~ season, scales = "free_y") +
  labs(x = NULL,
       y = "Temperature (°C)",
       fill = "Station") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


## task1 findings

#all values fall inside the physically plausible range for NRW temperatures
#no impossible values detected, so no data errors to correct
#red dots in plot are real extreme years, not measurement errors
#stations are ordered correctly by elevation: Duisburg (warmest) to Kahler Asten (coldest)
#winter (djf) has the widest spread and most outliers, summer (jja) is the tightest
#cold winter outliers cluster at the long-record stations (Arnsberg, Kahler Asten)
#a few warm autumn outliers in recent years at the lowland stations (likely post-2010 warm autumns)
#keep all outliers because they are real climate signal, not data errors



## TASK 2 : Descriptive Analysis with Seasonal (3-months time frame)

#focus on annual mean, winter (djf) and summer (jja) temperatures
#summary statistics per station and variable
desc_long <- stations %>%
  select(station, year, annual, djf, jja) %>%
  pivot_longer(cols = c(annual, djf, jja),
               names_to = "variable",
               values_to = "temp")

desc_long %>%
  group_by(station, variable) %>%
  summarise(n      = sum(!is.na(temp)),
            mean   = mean(temp, na.rm = TRUE),
            median = median(temp, na.rm = TRUE),
            sd     = sd(temp, na.rm = TRUE),
            min    = min(temp, na.rm = TRUE),
            max    = max(temp, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(variable, station)

#findings:
#means decrease cleanly with elevation across all three variables
#Duisburg (31m) is the warmest; Kahler Asten (839m) is the coldest by ~6°C in annual mean
#winter (djf) has the largest SD (~1.6-2.0°C), about twice that of summer
#annual SD is smallest (~0.6-1.0°C) because 12-month averaging smooths year-to-year variation
#mean and median nearly identical at every station, suggesting roughly symmetric distributions
#sample sizes vary (56-129) due to different observation windows per station


## TASK 2 : DESCRIPTIVE ANALYSIS
#focus on annual mean, winter (djf) and summer (jja) temperatures
#draw histograms with density curve overlay to inspect distribution shape
#one row per station, one column per variable
ggplot(desc_long, aes(x = temp)) +
  geom_histogram(aes(y = after_stat(density), fill = station),
                 bins = 12, color = "white", alpha = 0.8) +
  geom_density(color = "black", linewidth = 0.6) +
  facet_grid(station ~ variable, scales = "free") +
  labs(x = "Temperature (°C)",
       y = "Density",
       fill = "Station") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"))

#findings:
#all 18 panels are roughly bell-shaped and centered around the mean
#djf (winter) panels are visibly wider (sd ~1.6-2.0°C) than annual or jja (~0.6-1.2°C)
#some annual distributions show mild right-skew (Duisburg, Kahler Asten) likely due to recent warming
#a few panels show mild bimodality (Arnsberg annual, Kahler Asten jja) but no extreme deviations
#overall shapes are consistent with a normal distribution at all six stations
#qq plots in the next step will confirm this more formally


## TASK 2 : Q-Q PLOTS
#qq plots to check normality visually
#one panel per station, three colored series (annual, djf, jja) per panel
#points should fall close to the reference line if the variable is normally distributed
ggplot(desc_long, aes(sample = temp, color = variable)) +
  stat_qq(size = 1, alpha = 0.7) +
  stat_qq_line(linewidth = 0.5) +
  facet_wrap(~ station, scales = "free_y", ncol = 3) +
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles (°C)",
       color = "Variable") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"))

#findings:
#all three variables track their reference lines closely at every station
#djf (winter) is the cleanest fit to normal at every station
#jja (summer) fits well, with 1-2 points above the line at warmer stations (recent heat waves)
#annual shows mild upward curve at the right end across all stations
#this reflects the recent warming trend pushing the upper tail above a normal fit
#step pattern visible in djf for long-record stations (Arnsberg, Kahler Asten) is a discreteness artefact, not a problem
#conclusion: normal distribution is a reasonable approximation for all three variables at all six stations
#this justifies parametric tests (e.g. t-test) in later tasks


## TASK 2 : SC PLOTS W CORRELATION
#bivariate: pairwise scatterplots between annual, djf and jja
#one point per station-year, colored by station
library(patchwork)

p_annual_djf <- ggplot(stations, aes(x = djf, y = annual, color = station)) +
  geom_point(size = 1.6, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, alpha = 0.5) +
  labs(x = "Winter (DJF) temperature (°C)",
       y = "Annual mean temperature (°C)") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p_annual_jja <- ggplot(stations, aes(x = jja, y = annual, color = station)) +
  geom_point(size = 1.6, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, alpha = 0.5) +
  labs(x = "Summer (JJA) temperature (°C)",
       y = "Annual mean temperature (°C)") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p_djf_jja <- ggplot(stations, aes(x = jja, y = djf, color = station)) +
  geom_point(size = 1.6, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, alpha = 0.5) +
  labs(x = "Summer (JJA) temperature (°C)",
       y = "Winter (DJF) temperature (°C)",
       color = "Station") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 13)

p_annual_djf + p_annual_jja + p_djf_jja

## CORR VALUES
#correlation table per station and pair
stations %>%
  group_by(station) %>%
  summarise(annual_djf = cor(annual, djf, use = "complete.obs"),
            annual_jja = cor(annual, jja, use = "complete.obs"),
            djf_jja    = cor(djf, jja, use = "complete.obs"),
            .groups = "drop")

#findings:
#annual mean correlates strongly with both djf and jja (~0.6-0.7) at all stations
#this is expected since the annual mean is an average that includes both seasons
#djf and jja correlate only weakly within a station (~0.1-0.4)
#meaning a cold winter does not predict a cold summer of the same year
#all six stations show the same correlation structure, suggesting common regional climate physics
#Arnsberg has the lowest annual-jja correlation (0.43), likely because its record ends in 1998
#implication: winter and summer trends should be analysed separately in later tasks



## TASK 3 : MISSING VALUES

#quantify missingness per station for the three variables we focus on
stations %>%
  group_by(station) %>%
  summarise(n_total       = n(),
            n_miss_annual = sum(is.na(annual)),
            n_miss_djf    = sum(is.na(djf)),
            n_miss_jja    = sum(is.na(jja)),
            pct_miss_annual = round(100 * sum(is.na(annual)) / n(), 1),
            .groups = "drop") %>%
  arrange(desc(pct_miss_annual))


#findings:
#missingness is concentrated at two stations: Kahler Asten (18%, mostly 1887-1925) and Arnsberg (7.6%, mostly WWII years)
#the other four stations have <5% missing annual values and are essentially complete
#gaps are mostly entire years rather than isolated months within a year
#missingness is not random: clustered in early instrument era and WWII

#three options for handling missing values:
#option 1: listwise deletion - drop any row with a missing value in the variable analyzed
#  simple, transparent, unbiased if missingness is random, but loses observations
#option 2: pairwise deletion - use all available data for each individual analysis
#  different analyses use slightly different sample sizes
#option 3: imputation - fill in gaps using a model (interpolation, regression, multiple imputation)
#  recovers observations but introduces uncertainty and can fabricate signal if done poorly

#strategy: pairwise deletion (use all available years for each analysis)
#justification:
#- gaps are station-wide (whole years) so within-year imputation across variables is not feasible
#- cross-station imputation would bias the spatial comparison in task 6 (e.g. using Essen 1945 to fill Arnsberg 1945 would artificially shrink the inter-station difference)
#- remaining sample sizes (n>530 per variable) are large enough for reliable inference
#- gaps cluster in periods (pre-1925 Kahler Asten, WWII) that we would not include in modern climate windows anyway
#- imputation as a robustness check is unnecessary for the same reason

#implementation: use na.rm = TRUE and use = "complete.obs" throughout
#no formal imputation method is applied



## TASK 4 : COMPARE TWO TIME PERIODS AT ONE STATION
#chosen station: Kahler Asten (longest record, high-elevation mountain station)
#chosen periods: 1931-1960 (early reference) vs 1991-2020 (recent climate normal)
#both windows are 30 years long (WMO climate normal length)
#both windows fall after the patchy pre-1925 era so data are essentially complete

#filter data for Kahler Asten and tag each year with its period
ka <- stations %>%
  filter(station == "Kahler Asten",
         (year >= 1931 & year <= 1960) | (year >= 1991 & year <= 2020)) %>%
  mutate(period = case_when(
    year >= 1931 & year <= 1960 ~ "1931-1960",
    year >= 1991 & year <= 2020 ~ "1991-2020"
  )) %>%
  select(station, year, period, annual, djf, jja)

#descriptive statistics per period
ka %>%
  pivot_longer(cols = c(annual, djf, jja),
               names_to = "variable",
               values_to = "temp") %>%
  group_by(variable, period) %>%
  summarise(n      = sum(!is.na(temp)),
            mean   = mean(temp, na.rm = TRUE),
            median = median(temp, na.rm = TRUE),
            sd     = sd(temp, na.rm = TRUE),
            min    = min(temp, na.rm = TRUE),
            max    = max(temp, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(variable, period)

#boxplots per variable and period
ka_long <- ka %>%
  pivot_longer(cols = c(annual, djf, jja),
               names_to = "variable",
               values_to = "temp") %>%
  mutate(variable = factor(variable, levels = c("annual", "djf", "jja")))

ggplot(ka_long, aes(x = period, y = temp, fill = period)) +
  geom_boxplot(alpha = 0.7, width = 0.5) +
  geom_jitter(width = 0.1, size = 0.8, alpha = 0.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = NULL,
       y = "Temperature (°C)",
       fill = "Period") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 12)


#findings:
#all three variables are warmer in 1991-2020 than in 1931-1960
#shift in mean: annual +0.76°C, djf +1.00°C (largest), jja +0.84°C
#variability (SD, box width) is roughly unchanged between periods
#the 1991-2020 median sits at or above the 1931-1960 upper quartile for annual and jja
#djf shows the biggest mean shift but most distributional overlap (winter is the noisiest variable)
#sample sizes (26-30 per period) are well-balanced
#chosen periods (30-year WMO climate normals) avoid the patchy pre-1925 era at Kahler Asten
#these results motivate a formal test in task 5 to assess significance





## TASK 5 : HYPOTHESIS TEST FOR TEMPORAL CHANGE
#H0: mean temperature in 1931-1960 equals mean in 1991-2020
#H1: the two means differ (two-sided)
#one test per variable: annual, djf, jja

#candidate procedures considered:
#- Student two-sample t-test: assumes independence, normality, equal variances
#- Welch two-sample t-test:   assumes independence, normality (drops equal variance)
#- Mann-Whitney U test:       non-parametric, assumes independence + similar shape

#chosen test: Welch two-sample t-test
#- normality is plausible from task 2 q-q plots and histograms
#- Welch is the safer default than Student even when variances are equal
#- parametric test uses values not ranks, more powerful when normality holds
#- two periods are 30+ years apart at the same station so independence is reasonable


#step 1: check normality with Shapiro-Wilk per period and variable
ka %>%
  pivot_longer(cols = c(annual, djf, jja),
               names_to = "variable",
               values_to = "temp") %>%
  filter(!is.na(temp)) %>%
  group_by(variable, period) %>%
  summarise(W       = shapiro.test(temp)$statistic,
            p_value = shapiro.test(temp)$p.value,
            .groups = "drop")

#findings (normality):
#5 of 6 groups are consistent with normality (p > 0.05)
#djf 1931-1960 fails (p = 0.002) due to historic cold winters (1940, 1942, 1956, 1963)
#sample sizes (27-30 per group) are large enough for the Central Limit Theorem to apply
#a Mann-Whitney robustness check is added for djf below


#step 2: check equal variances with F-test per variable
ka %>%
  pivot_longer(cols = c(annual, djf, jja),
               names_to = "variable",
               values_to = "temp") %>%
  filter(!is.na(temp)) %>%
  group_by(variable) %>%
  summarise(F_stat   = var.test(temp ~ period)$statistic,
            p_value  = var.test(temp ~ period)$p.value,
            .groups  = "drop")

#findings (variance):
#all three variables show p > 0.05, so equal-variance assumption is supported
#both Student and Welch t-tests would be valid, Welch chosen as the safer default


#step 3: run Welch t-test per variable
ka_long <- ka %>%
  pivot_longer(cols = c(annual, djf, jja),
               names_to = "variable",
               values_to = "temp") %>%
  filter(!is.na(temp))

test_results <- ka_long %>%
  group_by(variable) %>%
  summarise(mean_early = mean(temp[period == "1931-1960"]),
            mean_late  = mean(temp[period == "1991-2020"]),
            diff       = mean_late - mean_early,
            t_stat     = t.test(temp ~ period, var.equal = FALSE)$statistic,
            df         = t.test(temp ~ period, var.equal = FALSE)$parameter,
            p_value    = t.test(temp ~ period, var.equal = FALSE)$p.value,
            ci_low     = t.test(temp ~ period, var.equal = FALSE)$conf.int[1],
            ci_high    = t.test(temp ~ period, var.equal = FALSE)$conf.int[2],
            .groups = "drop")

test_results


#step 4: robustness check for djf with Mann-Whitney U test
#(because shapiro flagged the 1931-1960 djf group)
wilcox.test(temp ~ period,
            data = ka_long %>% filter(variable == "djf"))


#findings (test results):
#all three Welch t-tests reject H0 at the 5% level
#- annual: mean_early = 5.19°C, mean_late = 5.95°C, diff = +0.76°C, p < 0.001
#- djf:    mean_early = -2.43°C, mean_late = -1.43°C, diff = +1.00°C, p = 0.018
#- jja:    mean_early = 12.66°C, mean_late = 13.50°C, diff = +0.80°C, p = 0.005
#all 95% CIs for the difference exclude zero
#annual has the smallest p (most precise estimate, least year-to-year noise)
#djf has the largest mean shift but widest CI (winter is the noisiest variable)
#Mann-Whitney check for djf agrees (p = 0.039), so the t-test conclusion is reliable
#  despite Shapiro flagging non-normality in the 1931-1960 djf group

#substantive conclusion:
#temperatures at Kahler Asten increased significantly between 1931-1960 and 1991-2020
#warming is detectable in annual, winter, and summer separately
#the magnitude (~0.8-1.0°C) is consistent with global warming over the same interval



## TASK 6 : SPATIAL COMPARISON ACROSS STATIONS
#H0: all six stations have the same mean annual temperature
#H1: at least one pair of station means differs

#chosen window: 1961-1990 (WMO standard climate normal, all 6 stations covered)

#filter data
spatial <- stations %>%
  filter(year >= 1961, year <= 1990) %>%
  select(station, year, annual) %>%
  filter(!is.na(annual))

#sample sizes per station
spatial %>%
  group_by(station) %>%
  summarise(n      = n(),
            mean   = mean(annual),
            sd     = sd(annual),
            .groups = "drop") %>%
  arrange(mean)


#step 1: check normality per station (Shapiro-Wilk)
spatial %>%
  group_by(station) %>%
  summarise(W       = shapiro.test(annual)$statistic,
            p_value = shapiro.test(annual)$p.value,
            .groups = "drop")


#step 2: check equal variances across all six groups (Levene's test)
#install.packages("car") if needed
library(car)
leveneTest(annual ~ station, data = spatial)


#step 3: run one-way ANOVA
anova_fit <- aov(annual ~ station, data = spatial)
summary(anova_fit)


#step 4: post-hoc test (Tukey HSD) to identify which stations differ
TukeyHSD(anova_fit)

#findings:
#all six stations are consistent with normality (Shapiro p > 0.36 for all)
#variances are essentially equal across stations (Levene p = 0.976)
#ANOVA strongly rejects H0: F(5,172) = 280.1, p < 2e-16
#R^2 = 686.5/(686.5+84.3) = 0.89 -> 89% of variance in annual temp is explained by station
#mean annual temperature ranges from 4.89°C (Kahler Asten) to 10.94°C (Duisburg)
#span of about 6°C across the six stations in 1961-1990

#Tukey HSD post-hoc:
#14 of 15 pairwise comparisons are significant at p < 0.001
#only non-significant pair: Essen vs Dortmund (diff = 0.18°C, p = 0.92)
#  this makes physical sense: similar elevation (150m vs 120m) and same Ruhr lowland
#five effectively distinct climate groups identified:
#  {Kahler Asten} < {Brilon} < {Arnsberg} < {Essen, Dortmund} < {Duisburg}

#substantive conclusion:
#spatial differences across the six NRW stations are highly significant
#stations cluster by elevation, consistent with the lapse rate hypothesis tested in task 8
#the spatial range (~6°C across 800m of elevation) is much larger than the temporal range
#  found at Kahler Asten in task 5 (~0.8-1.0°C over 60 years)



## TASK 7 : MULTIPLE TESTING INFLATION

#concept: when many tests are run, the family-wise error rate inflates
#under independence: P(>=1 false positive) = 1 - (1 - alpha)^k

#illustrate the inflation
alpha <- 0.05
k     <- c(1, 3, 5, 10, 15, 20)
fwer  <- 1 - (1 - alpha)^k
data.frame(n_tests = k, fwer = round(fwer, 3))

#count of tests actually performed in this report:
#- task 5: 3 Welch t-tests (annual, djf, jja) + 1 Wilcoxon robustness check
#- task 6: 1 omnibus ANOVA + 15 pairwise comparisons (Tukey HSD)
#total nominal tests: 3 + 15 = 18 (excluding assumption checks)

#Tukey HSD already corrects family-wise error rate within the post-hoc step,
#so the 15 spatial p-values are already protected at alpha = 0.05.

#for task 5 (3 t-tests on the same Kahler Asten data) we apply corrections:
p_raw <- c(annual = 0.000808,
           djf    = 0.018,
           jja    = 0.00483)

bonferroni <- p.adjust(p_raw, method = "bonferroni")
holm       <- p.adjust(p_raw, method = "holm")
bh         <- p.adjust(p_raw, method = "BH")

data.frame(variable   = names(p_raw),
           p_raw      = p_raw,
           bonferroni = round(bonferroni, 4),
           holm       = round(holm, 4),
           BH         = round(bh, 4))


#findings:
#running 18 tests at alpha=0.05 would give ~60% chance of at least one false positive
#Tukey HSD already corrects the 15 spatial pairwise tests so no further adjustment needed
#after applying corrections to the 3 temporal t-tests:
#- annual and jja remain clearly significant under every correction method
#- djf is significant under Holm and BH but borderline under Bonferroni (p = 0.054)
#substantive conclusions from tasks 5 and 6 are unchanged after correction






## TASK 8 : ADIABATIC COOLING (ELEVATION EFFECT)


#textbook adiabatic lapse rate: temperature decreases ~0.65°C per 100m elevation
#question: does elevation alone explain the spatial differences across stations?

#use the same 1961-1990 window as task 6 for consistency
station_means <- stations %>%
  filter(year >= 1961, year <= 1990, !is.na(annual)) %>%
  group_by(station, elevation_m) %>%
  summarise(mean_annual = mean(annual),
            sd_annual   = sd(annual),
            n           = n(),
            .groups = "drop") %>%
  arrange(elevation_m)
station_means


#fit linear regression: mean annual temp ~ elevation
fit <- lm(mean_annual ~ elevation_m, data = station_means)
summary(fit)

#extract slope and convert to °C per 100m for comparison with textbook
slope_per_m   <- coef(fit)["elevation_m"]
slope_per_100 <- slope_per_m * 100
cat("Estimated slope:", round(slope_per_100, 3), "°C per 100m\n")
cat("Textbook lapse rate:", -0.65, "°C per 100m\n")


#compute predicted vs observed and residuals
station_means <- station_means %>%
  mutate(predicted = predict(fit),
         residual  = mean_annual - predicted)
station_means


#plot: observed temperature vs elevation, with fitted line + textbook line
ggplot(station_means, aes(x = elevation_m, y = mean_annual)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 0.8,
              alpha = 0.15) +
  geom_abline(intercept = coef(fit)[1], slope = -0.0065,
              color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(color = station), size = 4) +
  geom_text(aes(label = station), nudge_y = 0.3, size = 3.5) +
  labs(x = "Elevation (m)",
       y = "Mean annual temperature 1961--1990 (°C)",
       color = "Station") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


#findings:
#estimated lapse rate: -0.70°C per 100m, very close to textbook value of -0.65°C per 100m
#R^2 = 0.968: elevation explains 96.8% of variance in mean annual temperature across stations
#regression is highly significant (p = 0.0004)
#blue fitted line and red textbook line are nearly parallel -> lapse rate is a good model

#residuals reveal where other factors matter:
#Duisburg (+0.49°C warmer than predicted): likely urban heat island effect
#  -> heavily industrialised Rhine/Ruhr confluence, westernmost station closest to North Sea
#Arnsberg (-0.69°C cooler than predicted): likely cold air pooling in the Ruhr valley
#  -> cold dense air drains into valleys at night, lowering mean temperature
#other four stations (Dortmund, Essen, Brilon, Kahler Asten) fit the lapse rate almost exactly

#conclusion:
#adiabatic cooling (elevation) is the dominant driver of spatial temperature differences in NRW
#it explains ~97% of the variance across stations
#the remaining ~3% is explained by local factors: urban heat island (Duisburg), valley cold pooling (Arnsberg)
#other factors (latitude, distance from coast) are minor within this relatively small region




## TASK 9 : EFFECT SIZES
#p-values tell us whether an effect exists, effect sizes tell us how big it is
#we report both raw mean differences (in °C, preserves the unit) and Cohen's d (standardized)
#raw differences are the primary measure since temperature is directly interpretable
#Cohen's d supplements this for comparison across variables with different variability
#conventions for Cohen's d: 0.2 = small, 0.5 = medium, 0.8 = large, >1.0 = very large

#--- TEMPORAL EFFECT SIZES (task 5: Kahler Asten 1931-1960 vs 1991-2020) ---

#Cohen's d function (pooled SD)
cohens_d <- function(x, y) {
  nx <- length(na.omit(x))
  ny <- length(na.omit(y))
  pooled_sd <- sqrt(((nx - 1) * var(x, na.rm = TRUE) +
                       (ny - 1) * var(y, na.rm = TRUE)) / (nx + ny - 2))
  (mean(y, na.rm = TRUE) - mean(x, na.rm = TRUE)) / pooled_sd
}

early <- ka %>% filter(period == "1931-1960")
late  <- ka %>% filter(period == "1991-2020")

temporal_effects <- tibble(
  variable   = c("annual", "djf", "jja"),
  mean_early = c(mean(early$annual, na.rm = TRUE),
                 mean(early$djf,    na.rm = TRUE),
                 mean(early$jja,    na.rm = TRUE)),
  mean_late  = c(mean(late$annual,  na.rm = TRUE),
                 mean(late$djf,     na.rm = TRUE),
                 mean(late$jja,     na.rm = TRUE)),
  raw_diff   = c(mean(late$annual,  na.rm = TRUE) - mean(early$annual, na.rm = TRUE),
                 mean(late$djf,     na.rm = TRUE) - mean(early$djf,    na.rm = TRUE),
                 mean(late$jja,     na.rm = TRUE) - mean(early$jja,    na.rm = TRUE)),
  cohens_d   = c(cohens_d(early$annual, late$annual),
                 cohens_d(early$djf,    late$djf),
                 cohens_d(early$jja,    late$jja))
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

temporal_effects


#--- SPATIAL EFFECT SIZE (task 6: ANOVA eta-squared) ---
#eta-squared = proportion of total variance explained by station
#analogous to R^2 in regression, ranges 0-1

anova_summary <- summary(anova_fit)[[1]]
ss_station    <- anova_summary["station",    "Sum Sq"]
ss_residual   <- anova_summary["Residuals",  "Sum Sq"]
eta_squared   <- ss_station / (ss_station + ss_residual)

cat("Spatial effect size (eta-squared):", round(eta_squared, 3), "\n")
cat("Interpretation: station explains",
    round(eta_squared * 100, 1), "% of variance in annual temperature\n")

#raw spatial range across stations (1961-1990)
cat("Raw spatial range: Duisburg mean =", round(max(station_means$mean_annual), 2),
    "°C, Kahler Asten mean =", round(min(station_means$mean_annual), 2),
    "°C, difference =",
    round(max(station_means$mean_annual) - min(station_means$mean_annual), 2), "°C\n")


#findings:
#temporal effect sizes (Kahler Asten, 1931-1960 vs 1991-2020):
#- annual:  raw diff = +0.76°C, Cohen's d = 0.955 (large)
#- djf:     raw diff = +1.00°C, Cohen's d = 0.651 (medium-large)
#- jja:     raw diff = +0.80°C, Cohen's d = 0.775 (large)
#all three temporal effects are in the medium-to-large range
#winter has the largest raw shift but lowest Cohen's d because it is the most variable season
#annual Cohen's d of 0.955 is close to 1.0 - nearly one standard deviation of warming

#spatial effect size (Aufgabe 6, 1961-1990):
#eta-squared = 0.891: station identity explains 89.1% of variance in annual temperature
#raw spatial range: 5.99°C (Kahler Asten 4.89°C to Duisburg 10.88°C)
#this is an extremely large effect - far larger than the temporal warming signal

#comparison of temporal vs spatial effects:
#spatial range (~6°C) is about 8x larger than the temporal warming at Kahler Asten (~0.76°C)
#elevation-driven spatial differences dominate over climate change signal at this regional scale
#raw differences in °C are the primary effect size measure here (directly interpretable)
#Cohen's d supplements this for cross-variable comparison (adjusts for different SDs per season)




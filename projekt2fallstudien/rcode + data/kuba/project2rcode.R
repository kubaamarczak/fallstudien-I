# ============================================================
# Klimaunterschiede – Datenaufbereitung für Fallstudien I
# Quellen: https://www.ecad.eu/download/millennium/millennium.php
# Regionalebene: 6 Orte (Kahler Asten, Dortmund, Duisburg, Essen, Arnsberg, Brilon)
# ============================================================
## -----------------------------------------------------------------------------
## Daten importieren
df_kahler_asten <- read.table("../data/indexTG_000812.txt", skip = 32,  header = FALSE)
df_kahler_asten

df_dortmund <- read.table("../data/indexTG_004021.txt", skip = 32,  header = FALSE)
df_dortmund

df_duisburg <- read.table("../data/indexTG_004030.txt", skip = 32,  header = FALSE)
df_duisburg

df_essen <- read.table("../data/indexTG_004074.txt", skip = 32,  header = FALSE)
df_essen

df_arnsberg <- read.table("../data/indexTG_004172.txt", skip = 32,  header = FALSE)
df_arnsberg

df_brilon <- read.table("../data/indexTG_004897.txt", skip = 32,  header = FALSE)
df_brilon
## -----------------------------------------------------------------------------

## 1. Datenaufbereitung

## Spalten umbenennen
library(dplyr)
renameCols <- function(df_name) {
  df_name <- df_name %>%
    rename(
      souid = V1,
      year = V2,
      annual = V3,
      winter_half_year = V4,
      summer_half_year = V5,
      winter_djf = V6,
      spring_mam = V7,
      summer_jja = V8,
      autumn_son = V9,
      january = V10,
      february = V11,
      march = V12,
      april = V13,
      may = V14,
      june = V15,
      july = V16,
      august = V17,
      september = V18,
      october = V19,
      november = V20,
      december = V21
    )
}

df_kahler_asten <- renameCols(df_kahler_asten)
df_dortmund <- renameCols(df_dortmund)
df_duisburg <- renameCols(df_duisburg)
df_essen <- renameCols(df_essen)
df_arnsberg <- renameCols(df_arnsberg)
df_brilon <- renameCols(df_brilon)

## In 1 Grad Celsius umwandeln
convertCelsius <- function(df_name) {
  df_name <- df_name %>% 
    mutate(across(c(annual, winter_half_year, summer_half_year, winter_djf,
                    spring_mam, summer_jja, autumn_son, january, february, march, 
                    april, may, june, july, august, september, october, november, 
                    december), ~ .x / 100)
    )
}


df_kahler_asten <- convertCelsius(df_kahler_asten)
df_dortmund <- convertCelsius(df_dortmund)
df_duisburg <- convertCelsius(df_duisburg)
df_essen <- convertCelsius(df_essen)
df_arnsberg <- convertCelsius(df_arnsberg)
df_brilon <- convertCelsius(df_brilon)

## Fehlende Werte als NAs behandeln
df_kahler_asten[df_kahler_asten == -9999.99] <- NA
df_dortmund[df_dortmund == -9999.99] <- NA
df_duisburg[df_duisburg == -9999.99] <- NA
df_essen[df_essen == -9999.99] <- NA
df_arnsberg[df_arnsberg == -9999.99] <- NA
df_brilon[df_brilon == -9999.99] <- NA

## Als data.frame 
df_kahler_asten <- as.data.frame(df_kahler_asten)
df_kahler_asten
df_dortmund <- as.data.frame(df_dortmund)
df_dortmund
df_duisburg <- as.data.frame(df_duisburg)
df_duisburg
df_essen <- as.data.frame(df_essen)
df_essen
df_arnsberg <- as.data.frame(df_arnsberg)
df_arnsberg
df_brilon <- as.data.frame(df_brilon)
df_brilon

## Exportieren als csv
library(readr)

write_csv(df_kahler_asten, "kahler_asten.csv")
write_csv(df_dortmund, "dortmund.csv")
write_csv(df_duisburg, "duisburg.csv")
write_csv(df_essen, "essen.csv")
write_csv(df_arnsberg, "arnsberg.csv")
write_csv(df_brilon, "brilon.csv")

## 
kahler_asten <- read.csv("kahler_asten.csv")
kahler_asten
dortmund <- read.csv("dortmund.csv")
dortmund
duisburg <- read.csv("duisburg.csv")
duisburg
essen <- read.csv("essen.csv")
essen
arnsberg <- read.csv("arnsberg.csv")
arnsberg
brilon <- read.csv("brilon.csv")
brilon

df_weather <- bind_rows(
  kahler_asten = kahler_asten,
  essen = essen,
  dortmund = dortmund,
  duisburg = duisburg,
  arnsberg = arnsberg,
  brilon = brilon,
  .id = "station"
)

df_weather %>%
  group_by(station)

df_weather$station <- factor(
  df_weather$station,
  levels = c("duisburg", "essen", "dortmund", "arnsberg", "brilon", "kahler_asten")
)

## Ausreißer
library("ggplot2")

boxplt <- function(variab, variabname) {
  ggplot(df_weather, aes(x = station, y = {{ variab }})) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    labs(
      y = variabname
    ) +
    scale_y_continuous(limits = c(-15, 25)) +
    theme_minimal() +
    theme(
      text = element_text(size=9),
      legend.text = element_text(size=8),
      legend.title = element_text(size=10),
      axis.text = element_text(size=7),
      panel.grid = element_blank(),
      axis.text.x = element_text(size=8),
      axis.ticks.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.line.x = element_line(color = "black"),
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

## Jahresmitteltemperatur
a1 <- boxplt(annual, "Jahresmitteltemperatur [°C]")

## Winter Halbjahr Mitteltemperatur
a2 <- boxplt(winter_half_year, "Mitteltemperatur Winterhalbjahr [°C]")

## Sommer Halbjahr Mitteltemperatur
a3 <- boxplt(summer_half_year, "Mitteltemperatur Sommerhalbjahr [°C]")

## Winter Quartal Mitteltemperatur
b1 <- boxplt(winter_djf, "Mitteltemperatur Winterquartal (Dez. - Feb.) [°C]")

## Frühling Quartal Mitteltemperatur
b2 <- boxplt(spring_mam, "Mitteltemperatur Frühlingsquartal (Mrz. - Mai) [°C]")

## Sommer Quartal Mitteltemperatur
b3 <- boxplt(summer_jja, "Mitteltemperatur Sommerquartal (Jun. - Aug.) [°C]")

## Herbst Quartal Mitteltemperatur
b4 <- boxplt(autumn_son, "Mitteltemperatur Herbstquartal (Sep. - Nov.) [°C]")

## Januar Mitteltemperatur
c1 <- boxplt(january, "Mitteltemperatur Januar [°C]")
c1

## Februar Mitteltemperatur
c2 <- boxplt(february, "Mitteltemperatur Februar [°C]")
c2

## März Mitteltemperatur
c3 <- boxplt(march, "Mitteltemperatur März [°C]")
c3

## April Mitteltemperatur
c4 <- boxplt(april, "Mitteltemperatur April [°C]")
c4

## Mai Mitteltemperatur
c5 <- boxplt(may, "Mitteltemperatur Mai [°C]")
c5

## Juni Mitteltemperatur
c6 <- boxplt(june, "Mitteltemperatur Juni [°C]")
c6

## Juli Mitteltemperatur
c7 <- boxplt(july, "Mitteltemperatur Juli [°C]")
c7

## August Mitteltemperatur
c8 <- boxplt(august, "Mitteltemperatur August [°C]")
c8

## September Mitteltemperatur
c9 <- boxplt(september, "Mitteltemperatur September [°C]")
c9

## Oktober Mitteltemperatur
c10 <- boxplt(october, "Mitteltemperatur Oktober [°C]")
c10

## November Mitteltemperatur
c11 <- boxplt(november, "Mitteltemperatur November [°C]")
c11

## Dezember Mitteltemperatur
c12 <- boxplt(december, "Mitteltemperatur Dezember [°C]")
c12

library("patchwork")
(a1)

(a2 + a3)

(b1 + b2)
(b3 + b4)

(c1 + c2 + c3)
(c4 + c5 + c6) 
(c7 + c8 + c9) 
(c10 + c11 + c12) 

## Z-Score
scale(duisburg$annual)
scale(essen$annual)
scale(dortmund$annual)
scale(arnsberg$annual)
scale(brilon$annual)
scale(kahler_asten$annual)

## Spannweite des Z-Scores
range(na.omit(scale(duisburg$annual)))
range(na.omit(scale(essen$annual)))
range(na.omit(scale(dortmund$annual)))
range(na.omit(scale(arnsberg$annual)))
range(na.omit(scale(brilon$annual)))
range(na.omit(scale(kahler_asten$annual)))

## Es gibt Ausreißer. 

## -----------------------------------------------------------------------------

## 2. Deskriptive Analyse

## Ausgesucht wurden neben der Jahresmittelwerte die saisonalen Mittelwerte

## --------------------------------------------------
## i. Statistiken der uni- und bivariaten Statistik
## ----------------------------
## a. univariat
summarze <- function(variab) {
df_weather %>%
  summarise(
    n_full = length({{ variab }}),
    nas = length({{ variab }}) - length(na.omit({{ variab }})),
    min = min({{ variab }}, na.rm = TRUE),
    mean = mean({{ variab }}, na.rm = TRUE),
    median = median({{ variab }}, na.rm = TRUE),
    max = max({{ variab }}, na.rm = TRUE),
    sd = sd({{ variab }}, na.rm = TRUE),
    mad = mad({{ variab }}, na.rm = TRUE),
    iqr = IQR({{ variab }}, na.rm = TRUE)
  )
}

## Jahresmitteltemperatur
summarze(annual)

## Saisonale Mitteltemperaturen
summarze(winter_djf)
summarze(spring_mam)
summarze(summer_jja)
summarze(autumn_son)

## ----------------------------
## b. bivariat
library("GGally")
ggpairs(
  df_weather,
  columns = c("annual", "winter_djf", "spring_mam", "summer_jja", "autumn_son"),
  columnLabels = c("Jahr", "Winterquartal", "Frühlingsquartal", "Sommerquartal", "Herbstquartal"),
  aes(color = station, alpha = 0.6)
)

## --------------------------------------------------
## ii. Graphische Verfahren
library("ggplot2")

timelne <- function(variab, variabname) {
ggplot(df_weather, aes(x = year, y = {{ variab }})) +
  geom_line(aes(color = station), alpha = 0.7) +
  labs(
    x = "Jahr",
    y = variabname
  ) +
  theme_minimal() +
    scale_y_continuous(limits = c(-10, 22)) +
  theme(
    text = element_text(size=9),
    legend.text = element_text(size=8),
    legend.title = element_text(size=10),
    axis.text = element_text(size=7),
    panel.grid = element_blank(),
    axis.text.x = element_text(size=8),
    axis.ticks.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 8))
  )
}

## Zeitreihe Jahresmitteltemperatur
ggplot(df_weather, aes(x = year, y = annual)) +
  geom_line(aes(color = station), alpha = 0.7) +
  labs(
    x = "Jahr",
    y = "Jahresmitteltemperatur [°C]"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size=9),
    legend.text = element_text(size=8),
    legend.title = element_text(size=10),
    axis.text = element_text(size=7),
    panel.grid = element_blank(),
    axis.text.x = element_text(size=8),
    axis.ticks.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 8))
  )

## Zeitreihe saisonale Mittel
t1 <- timelne(winter_djf, "Mitteltemperatur Winterquartal [°C]")
t2 <- timelne(spring_mam, "Mitteltemperatur Frühlingsquartal [°C]")
t3 <- timelne(summer_jja, "Mitteltemperatur Sommerquartal [°C]")
t4 <- timelne(autumn_son, "Mitteltemperatur Herbstquartal [°C]")

library("patchwork")
(t1 + t2) /
  (t3 + t4)

## iii. Normalverteilung plausibel?
## Histogramm & KDE
## Jahresmitteltemperatur
ggplot(df_weather, aes(x = annual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25) +
  geom_density() +
  theme_minimal() +
  facet_wrap(~station)

## Saisonale Mitteltemperaturen
ggplot(df_weather, aes(x = winter_djf)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25) +
  geom_density() +
  theme_minimal() +
  facet_wrap(~station)

ggplot(df_weather, aes(x = spring_mam)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25) +
  geom_density() +
  theme_minimal() +
  facet_wrap(~station)

ggplot(df_weather, aes(x = summer_jja)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25) +
  geom_density() +
  theme_minimal() +
  facet_wrap(~station)

ggplot(df_weather, aes(x = autumn_son)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25) +
  geom_density() +
  theme_minimal() +
  facet_wrap(~station)

## QQ-Plots
## Jahresmitteltemperatur
ggplot(df_weather, aes(sample = annual)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  facet_wrap(~station)

## Saisonale Mitteltemperaturen
ggplot(df_weather, aes(sample = winter_djf)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  facet_wrap(~station)

ggplot(df_weather, aes(sample = spring_mam)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  facet_wrap(~station)

ggplot(df_weather, aes(sample = summer_jja)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  facet_wrap(~station)

ggplot(df_weather, aes(sample = autumn_son)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  facet_wrap(~station)

## Anhand der Histogramme und KDEs ist eine Normalverteilung der Daten plausibel,
## da pro Station die typische Glockenform sichtbar ist. 
## Zusätzlich sind die QQ-Plots auch nicht sehr verdächtig.
## => Approximativ kann die Normalverteilung angenommen werden

## -----------------------------------------------------------------------------

## 3. Umgang mit fehlenden Werten

## Fehlende Werte spielen bei Wetterdaten eine große Rolle. anhand der Zeitreihen
## sind mehrere Jahre erkennbar, welche lückenhaft dargestellt sind. 











## -----------------------------------------------------------------------------

## 4. Signifikante Veränderung anhand eines Standortes










## -----------------------------------------------------------------------------

## 5. Hypothesentests definieren








## -----------------------------------------------------------------------------

## 6. Vergleich zeitliche und räumliche Unterschiede & Hypothesentest











## -----------------------------------------------------------------------------

## 7. Diskussion des multiplen Testens












## -----------------------------------------------------------------------------

## 8. Adiabatische Abkühlung als Confounder?














## -----------------------------------------------------------------------------

## 9. Berichtlegung






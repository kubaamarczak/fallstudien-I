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

df_weather

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
summarze <- function(df, variab) {
  df %>%
    summarise(
      n_full = length(.data[[variab]]),
      nas = sum(is.na(.data[[variab]])),
      min = min(.data[[variab]], na.rm = TRUE),
      mean = mean(.data[[variab]], na.rm = TRUE),
      median = median(.data[[variab]], na.rm = TRUE),
      max = max(.data[[variab]], na.rm = TRUE),
      sd = sd(.data[[variab]], na.rm = TRUE),
      mad = mad(.data[[variab]], na.rm = TRUE),
      iqr = IQR(.data[[variab]], na.rm = TRUE)
    )
}

## Deskriptive Maßzahlen

library(purrr)

iterate <- list(
  duisburg = duisburg,
  essen = essen,
  dortmund = dortmund,
  arnsberg = arnsberg,
  brilon = brilon,
  kahler_asten = kahler_asten
)

vars <- c("annual", "winter_djf", "summer_jja")

results_annual <- map_dfr("annual", function(var) {
  map_dfr(
    iterate,
    function(df) summarze(df, var),
    .id = "station"
  ) %>%
    mutate(variable = var, .before = 2)
})

results_winter_djf <- map_dfr("winter_djf", function(var) {
  map_dfr(
    iterate,
    function(df) summarze(df, var),
    .id = "station"
  ) %>%
    mutate(variable = var, .before = 2)
})

results_summer_jja <- map_dfr("summer_jja", function(var) {
  map_dfr(
    iterate,
    function(df) summarze(df, var),
    .id = "station"
  ) %>%
    mutate(variable = var, .before = 2)
})

results_annual
results_winter_djf
results_summer_jja

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

ggplot(df_weather, aes(x = summer_jja)) +
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

ggplot(df_weather, aes(sample = summer_jja)) +
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

## Es stellt sich die Frage, ob und wenn ja, wie mit fehlenden Werten umgegangen 
## werden könnte.

## Eine Möglichkeit wäre es, eine lineare Interpolation anzuwenden. Jedoch 
## schwanken die Werte bei Wetterdaten sehr stark, was keine sinnvolle 
## Imputation darstellen würde. 

## Eine sinnvolle Möglichkeit wäre die Imputation mittels (vorhandener) Wetterdaten
## aus umliegenden Orten. 

## 









## -----------------------------------------------------------------------------

## 4. Signifikante Veränderung anhand eines Standortes

## Ausgesuchter Standort: Essen

## Vergleichzeiträume
## Von: 1950 - 1980
## Bis: 1990 - 2020

## Auswahl der Zeiträume in Essen
essen_50_80 <- essen %>% filter(between(year, 1950, 1980))
essen_90_20 <- essen %>% filter(between(year, 1990, 2020))

## Die Wahl der Zeiträume hängt damit zusammen, dass von ca. 1950 an Deutschland
## ein Wirtschaftswunder hatte und sich so industriell weiterentwickeln konnte.
## So wurden vermehrt Autos als Transportmittel verwendet, die Industrie wurde
## immer präsenter und das kurbelte natürlich die Klimaerwärmung an. 
## Ab 1980 wurde dann das Klima immer mehr zum Thema. So war das Klima schon 
## durch all die Jahre zuvor belastet, ab 1990 ist der Zeitraum dann so gewählt, 
## dass die moderne Zeit abgedeckt wurde mit blühender Industrie, Transport
## (Autos, Flugzeuge) und vielem mehr. Die Klimaerwärmung ist ein Thema wie noch 
## nie.

## -----------------------------------------------------------------------------

## 5. Hypothesentests definieren

## Ziel ist es zu prüfen, ob sich das Klima über die Zeiträume hinweg statistisch
## signifikant unterscheiden, vor allem in Bezug auf eine Klimaerwärmung.
## Es folgt eine Überprüfung der Annahmen für den t-Test sowie die Ausführung
## des Welch t-Tests und Wilcoxon Rangsummentests.

## F-Test (Varianzhomogenität)
##
## H0: 
## H1:
var.test(x = essen_50_80$annual, y = essen_90_20$annual, alternative = "less")
## p = 0.2275 => H0 beibehalten: Es besteht ein statistisch signifikanter 
## Hinweis darauf, dass die Varianzen inhomogen sind. Folgend findet der 
## Welch t-Test Anwendung, welcher keine Varianzhomogenität voraussetzt. 

## Welch t-Test
##
## H0: µ1 ≥ µ2 vs.
## H1: µ1 < µ2
t.test(x = essen_50_80$annual, y = essen_90_20$annual, alternative = "less")
## p = 3.451e-08 < 0.05 => H0 ablehnen: Es besteht ein statistisch Hinweis
## darauf, dass sich die Mittelwerte der beiden Zeiträume signifikant unterscheiden.

## Wilcoxon Rangsummentest
##
## H0:
## H1: 
wilcox.test(x = essen_50_80$annual, y = essen_90_20$annual, alternative = "less")
## p = 1.928e-07 < 0.05 => H0 ablehnen: Es besteht ein statistisch Hinweis
## darauf, dass sich die Mittelwerte der beiden Zeiträume signifikant unterscheiden.


## -----------------------------------------------------------------------------

## 6. Vergleich zeitliche und räumliche Unterschiede & Hypothesentest

## --------------------------------------------------
## Zeitraum: ca. 1950 - 1995
## => In dem Zeitraum sind bei allen Wetterstationen genug Daten, um einen
## sinnvollen räumlichen Vergleich angehen zu können.

## Zeitraum: 1995 - 2020
## => In dem Zeitraum wird nur mit den vier Stationen gearbeitet, welche 
## vorhandene Daten haben.

## --------------------------------------------------
## Zeitraum: ca. 1950 - 1995
df_compare_50_95 <- df_weather %>%
  filter(between(year, 1950, 1995)) %>%
  group_by(year) %>%
  filter(all(!is.na(annual))) %>%
  ungroup()
df_compare_50_95

## -----------------------------------
## ANOVA
## Der ANOVA-Test ist ein Test für Gruppenvergleiche. 
## Der ANOVA-Test setzt die Normalverteilung voraus, welche bei diesem Datensatz
## annähernd gegeben ist.
## H0: Es gibt keinen Unterschied in den Mittelwerten zwischen den Gruppen.
## H1: Es gibt einen Unterschied in den Mittelwerten zwischen den Gruppen.
## H0:
## H1:

anova(lm(annual ~ station, data = df_compare_50_95))
## p < 2.2e-16 *** => H0 ablehnen: Es besteht ein statistischer Hinweis darauf,
## dass sich die Gruppen im Mittelwert unterschieden.

## Tukey-Test
## Der Tukey-Test gibt Auskunft darüber, welche Gruppen sich unterscheiden.
## H0: 
## H1: 
## H0: 
## H1:

TukeyHSD(aov(annual ~ station, data = df_compare_50_95))
## nur dortmund-essen nicht signifikant => Kein statistisch signifikanter Unterschied
## sonst => statistisch signifikanter Unterschied

## -----------------------------------
## Kruskal-Wallis
## Der Kruskal-Wallis Test ist eine robuste Alternative für den ANOVA-Test.
## 
## H0: Die Verteilungen/Mediane unterscheiden sich nicht.
## H1: Die Verteilungen/Mediane unterscheiden sich.
## H0: 
## H1:

kruskal.test(annual ~ station, data = df_compare_50_95)
## p < 2.2e-16 *** => H0 ablehnen: Es besteht ein statistischer Hinweis darauf,
## dass sich die Gruppen im Mittelwert unterschieden.

## Paarweiser Wilcoxon-Test
## Der paarweise Wilcoxon-Test gibt Auskunft darüber, welche Gruppen sich unterscheiden.
## H0: 
## H1: 
## H0: 
## H1:

pairwise.wilcox.test(df_compare_50_95$annual, df_compare_50_95$station)
## nur dortmund-essen nicht signifikant => Kein statistisch signifikanter Unterschied
## sonst => statistisch signifikanter Unterschied


## --------------------------------------------------
## Zeitraum: 1995 - 2020
df_compare_95_20 <- df_weather %>%
  filter(between(year, 1995, 2020)) %>%
  group_by(year) %>%
  filter(all(!is.na(annual))) %>%
  ungroup()
df_compare_95_20

## -----------------------------------
## ANOVA
## Der ANOVA-Test ist ein Test für Gruppenvergleiche. 
## Der ANOVA-Test setzt die Normalverteilung voraus, welche bei diesem Datensatz
## annähernd gegeben ist.
## H0: Es gibt keinen Unterschied in den Mittelwerten zwischen den Gruppen.
## H1: Es gibt einen Unterschied in den Mittelwerten zwischen den Gruppen.
## H0:
## H1:

anova(lm(annual ~ station, data = df_compare_95_20))
## p < 2.2e-16 *** => H0 ablehnen: Es besteht ein statistischer Hinweis darauf,
## dass sich die Gruppen im Mittelwert unterschieden.

## Tukey-Test
## Der Tukey-Test gibt Auskunft darüber, welche Gruppen sich unterscheiden.
## H0: 
## H1: 
## H0: 
## H1:

TukeyHSD(aov(annual ~ station, data = df_compare_95_20))
## nur dortmund-essen nicht signifikant => Kein statistisch signifikanter Unterschied
## sonst => statistisch signifikanter Unterschied

## -----------------------------------
## Kruskal-Wallis
## Der Kruskal-Wallis Test ist eine robuste Alternative für den ANOVA-Test.
## 
## H0: Die Verteilungen/Mediane unterscheiden sich nicht.
## H1: Die Verteilungen/Mediane unterscheiden sich.
## H0: 
## H1:

kruskal.test(annual ~ station, data = df_compare_95_20)
## p < 2.2e-16 *** => H0 ablehnen: Es besteht ein statistischer Hinweis darauf,
## dass sich die Gruppen im Mittelwert unterschieden.

## Paarweiser Wilcoxon-Test
## Der paarweise Wilcoxon-Test gibt Auskunft darüber, welche Gruppen sich unterscheiden.
## H0: 
## H1: 
## H0: 
## H1:

pairwise.wilcox.test(df_compare_95_20$annual, df_compare_95_20$station)
## nur dortmund-essen nicht signifikant => Kein statistisch signifikanter Unterschied
## sonst => statistisch signifikanter Unterschied


## -----------------------------------------------------------------------------

## 7. Diskussion des multiplen Testens

## Es stellt sich die Frage, inwiefern Testen am gleichen Datensatz das Problem
## des multiplen Testens verstärkt. So wird am gleichen Datensatz mehrmals 
## auf auf Unterschiede innerhalb einer Gruppe, auf einen Unterschied zwischen 
## Gruppen und das noch in verschiedenen Zeiträumen getestet. Wenn es um 
## verschiedene Hypothesenpaare geht, dann gehört das nicht zum klassischen
## Problem des multiplen Testens dazu, da eben verschiedene Hypothesen getestet 
## werden. Wenn es jedoch um gleiche Tests an einem Datensatz geht (zB Tukey),
## dann wird das multiple Testen relevanter. Bei Tukey wird ein adjustierter
## p-Wert verwendet, welcher für das multiple Testen adjustiert. Genauso
## adjustiert der paarweise Wilcoxon-Test mittels "holm" den p-Wert gegenüber
## mehreren Tests.


## -----------------------------------------------------------------------------

## 8. Adiabatische Abkühlung als Confounder?

## Höhendaten
## Duisburg: 31 m
## Essen: 150 m
## Dortmund: 120 m
## Arnsberg: 218 m
## Brilon: 472 m
## Kahler Asten: 839 m

df_weather_elev <- df_weather %>% 
  mutate(
    elevation = case_when(
      station == "duisburg" ~ 31,
      station == "essen" ~ 150,
      station == "dortmund" ~ 120,
      station == "arnsberg" ~ 218,
      station == "brilon" ~ 472,
      station == "kahler_asten" ~ 839,
    )
  )

df_weather_elev

model_elev <- lm(annual ~ elevation, data = df_weather_elev)
summary(model_elev)

coef(model_elev)["elevation"] * 100

## elevation: -0.0065406 °C / m hochsignifikant => -0.65 °C / 100 m 
##️️⚠️ Adiabatischer Effekt erklärt fast den ganzen räumlichen Unterschied


model_elev_stat <- lm(annual ~ elevation + station, data = df_weather_elev)
summary(model_elev_stat)

## Stationen wie Dortmund und Arnsberg tragen hochsignifikant zum räumlichen
## Temperaturunterschied bei, die Höhe auch immer noch.


anova(model_elev, model_elev_stat)

## Insgesamt trägt der adiabatische Effekt einen wesentlichen Teil zur Abkühlung
## der Temperatur bei. Dennoch lassen sich die Temperaturunterschiede zwischen
## den Städten nicht vollständig von dem adiabatischen Effekt erklären, da
## die ANOVA-Analyse gezeigt hat, dass unter Einbeziehung örtlicher Unterschiede
## signifikante Verschiedenheiten vorliegen. 


## --------------------------------------------------
## Landkarte NRW
## Pakete
library("ggplot2")
library("sf")
library("geodata")
library("rnaturalearth")
library("rnaturalearthdata")
library("osmdata")
library("lubridate")

## Letztes Jahr mit annual Daten für jede Station
df_weather %>% filter(year == 1997)

## Daten über Stationen
station_data <- data.frame(
  station = c("Duisburg", "Essen", "Dortmund", "Arnsberg", "Brilon", "Kahler Asten"),
  lon = c(6.76, 7.01, 7.46, 8.08, 8.57, 8.49),
  lat = c(51.43, 51.45, 51.51, 51.40, 51.39, 51.18),
  annual_mean = c(11.21, 10.32, 10.20, 9.20, 8.09, 5.58), ## 1997
  elevation = c(31, 150, 120, 218, 472, 839),
  change = c("0.019 °C", "0.024 °C", "0.028 °C", "0.000 °C", "0.022 °C", "0.012 °C")
)

## Als sf
station_data_sf <- st_as_sf(
  station_data,
  coords = c("lon", "lat"),
  crs = 4326
)

## Regressionsmodelle für tendenzielle Steigung 
model_essen <- lm(annual ~ year, data = essen)
summary(model_essen)
coef(model_essen)[2]

model_duisburg <- lm(annual ~ year, data = duisburg)
summary(model_duisburg)
coef(model_duisburg)[2]

model_dortmund <- lm(annual ~ year, data = dortmund)
summary(model_dortmund)
coef(model_dortmund)[2]

model_arnsberg <- lm(annual ~ year, data = arnsberg)
summary(model_arnsberg)
coef(model_arnsberg)[2]

model_brilon <- lm(annual ~ year, data = brilon)
summary(model_brilon)
coef(model_brilon)[2]

model_kahler_asten <- lm(annual ~ year, data = kahler_asten)
summary(model_kahler_asten)
coef(model_kahler_asten)[2]

## Landkarte importieren
germany_states <- gadm(
  country = "DEU",
  level = 1,
  path = tempdir()
)

germany_states_sf <- st_as_sf(germany_states)

nrw <- germany_states_sf %>%
  dplyr::filter(NAME_1 == "Nordrhein-Westfalen")

## Ruhr finden und importieren
q <- opq(
  bbox = c(6.65, 50.95, 8.75, 51.75),
  timeout = 120
) %>%
  add_osm_feature(key = "waterway", value = c("river", "stream", "canal")) %>%
  add_osm_feature(key = "name", value = "Ruhr")

osm_rivers <- osmdata_sf(q)

ruhr <- osm_rivers$osm_lines %>%
  filter(grepl("Ruhr", name, ignore.case = TRUE))

## Visualisierung der Karte
ggplot() +
  geom_sf(data = nrw, fill = "grey95", color = "grey60") +
  geom_sf(data = ruhr, color = "#5DA5DA", linewidth = 1) +
  geom_sf(data = station_data_sf, 
          aes(color = annual_mean, size = elevation)) +
  geom_sf_text(
    data = station_data_sf,
    aes(label = station),
    nudge_y = 0.08
  ) +
  geom_sf_text(
    data = station_data_sf,
    aes(label = change),
    nudge_y = 0.12,
    size = 3.2
  ) +
  coord_sf(xlim = c(5.8, 9.4), ylim = c(50.3, 52.5)) +
  scale_size_area(max_size = 12) +
  scale_color_viridis_c() +
  labs(
    color = "Jahresmitteltemperatur [°C]",
    size = "Höhe [m ü. NHN]"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


## -----------------------------------------------------------------------------

## 9. Berichtlegung






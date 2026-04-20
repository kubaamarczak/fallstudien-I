# ============================================================
# Zensus 2022 – Datenaufbereitung für Fallstudien I
# Quellen: Regionaltabelle_Bildung_Erwerbstaetigkeit.xlsx
# Regionalebene: Stadtkreis / kreisfreie Stadt / Landkreis
# ============================================================
#https://www.destatis.de/static/DE/zensus/gitterdaten/Regionaltabelle_Bildung_Erwerbstaetigkeit.xlsx
library(readxl)
library(dplyr)

# ---- Pfad zur heruntergeladenen Datei ----------------------
file_path <- "/Users/jakubmarczak/Downloads/Project1_data/Regionaltabelle_Bildung_Erwerbstaetigkeit.xlsx"

# ============================================================
# TEIL 1: Höchster Schulabschluss
# ============================================================

schulabs_raw <- read_excel(
  file_path,
  sheet = "CSV-Hoechster_Schulabschluss",
  col_names = FALSE
)

# Erste Zeile als Spaltennamen setzen
colnames(schulabs_raw) <- as.character(schulabs_raw[1, ])
schulabs_raw <- schulabs_raw[-1, ]   # Kopfzeile entfernen

# Nur Kreisebene behalten
schulabs <- schulabs_raw %>%
  filter(Regionalebene == "Stadtkreis/kreisfreie Stadt/Landkreis") %>%
  transmute(
    ARS          = `_RS`,
    Name         = Name,
    Regionalebene = Regionalebene,
    
    # Gesamtbevölkerung (15+)
    Bev_Gesamt   = as.numeric(SCHULABS_STP),
    Bev_Maenner  = as.numeric(SCHULABS_STP__M),
    Bev_Frauen   = as.numeric(SCHULABS_STP__W),
    
    # Noch in schulischer Ausbildung
    InAusb_Gesamt  = as.numeric(SCHULABS_STP__1),
    InAusb_Maenner = as.numeric(SCHULABS_STP__1_M),
    InAusb_Frauen  = as.numeric(SCHULABS_STP__1_W),
    
    # Mit allgemeinbildendem Schulabschluss (gesamt)
    MitAbschl_Gesamt  = as.numeric(SCHULABS_STP__2),
    MitAbschl_Maenner = as.numeric(SCHULABS_STP__2_M),
    MitAbschl_Frauen  = as.numeric(SCHULABS_STP__2_W),
    
    # Haupt-/Volksschulabschluss
    Haupt_Gesamt  = as.numeric(SCHULABS_STP__21),
    Haupt_Maenner = as.numeric(SCHULABS_STP__21_M),
    Haupt_Frauen  = as.numeric(SCHULABS_STP__21_W),
    
    # Realschulabschluss / Mittlere Reife
    Real_Gesamt  = as.numeric(SCHULABS_STP__23),
    Real_Maenner = as.numeric(SCHULABS_STP__23_M),
    Real_Frauen  = as.numeric(SCHULABS_STP__23_W),
    
    # Abitur / Fachhochschulreife
    Abitur_Gesamt  = as.numeric(SCHULABS_STP__24),
    Abitur_Maenner = as.numeric(SCHULABS_STP__24_M),
    Abitur_Frauen  = as.numeric(SCHULABS_STP__24_W),
    
    # Ohne allgemeinbildenden Schulabschluss
    OhneAbschl_Gesamt  = as.numeric(SCHULABS_STP__3),
    OhneAbschl_Maenner = as.numeric(SCHULABS_STP__3_M),
    OhneAbschl_Frauen  = as.numeric(SCHULABS_STP__3_W)
  )

# ============================================================
# TEIL 2: Erwerbsstatus
# ============================================================

erwerb_raw <- read_excel(
  file_path,
  sheet = "CSV-Erwerbsstatus",
  col_names = FALSE
)

colnames(erwerb_raw) <- as.character(erwerb_raw[1, ])
erwerb_raw <- erwerb_raw[-1, ]

erwerb <- erwerb_raw %>%
  filter(Regionalebene == "Stadtkreis/kreisfreie Stadt/Landkreis") %>%
  transmute(
    ARS = `_RS`,
    
    # Gesamtbevölkerung
    Bev_Gesamt_EW  = as.numeric(ERWERBSTAT_KURZ_STP),
    
    # Erwerbspersonen gesamt
    Erwerbspers_Gesamt  = as.numeric(ERWERBSTAT_KURZ_STP__1),
    Erwerbspers_Maenner = as.numeric(ERWERBSTAT_KURZ_STP__1_M),
    Erwerbspers_Frauen  = as.numeric(ERWERBSTAT_KURZ_STP__1_W),
    
    # Erwerbstätige
    Erwerbstaetig_Gesamt  = as.numeric(ERWERBSTAT_KURZ_STP__11),
    Erwerbstaetig_Maenner = as.numeric(ERWERBSTAT_KURZ_STP__11_M),
    Erwerbstaetig_Frauen  = as.numeric(ERWERBSTAT_KURZ_STP__11_W),
    
    # Erwerbslose
    Erwerbslos_Gesamt  = as.numeric(ERWERBSTAT_KURZ_STP__12),
    Erwerbslos_Maenner = as.numeric(ERWERBSTAT_KURZ_STP__12_M),
    Erwerbslos_Frauen  = as.numeric(ERWERBSTAT_KURZ_STP__12_W),
    
    # Nichterwerbspersonen
    Nichterwerb_Gesamt  = as.numeric(ERWERBSTAT_KURZ_STP__2),
    Nichterwerb_Maenner = as.numeric(ERWERBSTAT_KURZ_STP__2_M),
    Nichterwerb_Frauen  = as.numeric(ERWERBSTAT_KURZ_STP__2_W)
  )

# ============================================================
# TEIL 3: Zusammenführen & abgeleitete Quoten berechnen
# ============================================================

zensus2022 <- schulabs %>%
  left_join(erwerb, by = "ARS") %>%
  
  # Quoten berechnen (Basis: Bevölkerung 15+)
  mutate(
    # Anteil Abitur an Bevölkerung mit Abschluss (%)
    Abitur_Quote        = round(Abitur_Gesamt  / MitAbschl_Gesamt  * 100, 1),
    Abitur_Quote_M      = round(Abitur_Maenner / MitAbschl_Maenner * 100, 1),
    Abitur_Quote_F      = round(Abitur_Frauen  / MitAbschl_Frauen  * 100, 1),
    
    # Erwerbstätigenquote (Erwerbstätige / Bevölkerung gesamt, %)
    Erwerbstaetig_Quote   = round(Erwerbstaetig_Gesamt  / Bev_Gesamt_EW  * 100, 1),
    Erwerbstaetig_Quote_M = round(Erwerbstaetig_Maenner / Bev_Gesamt_EW * 100, 1),
    Erwerbstaetig_Quote_F = round(Erwerbstaetig_Frauen  / Bev_Gesamt_EW * 100, 1),
    
    # Erwerbslosenquote (Erwerbslose / Erwerbspersonen, %)
    Erwerbslos_Quote   = round(Erwerbslos_Gesamt  / Erwerbspers_Gesamt  * 100, 1),
    Erwerbslos_Quote_M = round(Erwerbslos_Maenner / Erwerbspers_Maenner * 100, 1),
    Erwerbslos_Quote_F = round(Erwerbslos_Frauen  / Erwerbspers_Frauen  * 100, 1)
  ) %>%

  # Zeilen mit fehlenden Werten in Kernvariablen entfernen
  filter(
    !is.na(Bev_Gesamt),
    !is.na(Erwerbstaetig_Gesamt)
  )

# ============================================================
# TEIL 4: Export
# ============================================================

write.csv(zensus2022, "zensus2022_Kreise.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("Fertig! Datei gespeichert: zensus2022_Kreise.csv\n")
cat(sprintf("Anzahl Kreise: %d\n", nrow(zensus2022)))
cat(sprintf("Anzahl Variablen: %d\n", ncol(zensus2022)))
cat("\nVariablenübersicht:\n")
print(colnames(zensus2022))

# ============================================================
# Zensus 2022 – Datenvorbereitung für Fallstudien I
# Input:  zensus2022_Kreise.csv
# Output: zensus2022_wide.csv  (Analysetabelle, eine Zeile pro Kreis)
#         zensus2022_long.csv  (Langtabelle für Visualisierungen)
# ============================================================

library(dplyr)
library(tidyr)

# ---- Einlesen ----------------------------------------------
data <- read.csv("zensus2022_Kreise.csv",
                 fileEncoding = "UTF-8",
                 colClasses   = c(ARS = "character"))

data$ARS        <- sprintf("%05s", data$ARS)
data$Bundesland <- substr(data$ARS, 1, 2)

# ---- Bundesland-Kürzel ------------------------------------
bl_labels <- data.frame(
  Bundesland = c("01","02","03","04","05","06","07","08",
                 "09","10","11","12","13","14","15","16"),
  BL_Name    = c("SH","HH","NI","HB","NRW","HE","RP","BW",
                 "BY","SL","BE","BB","MV","SN","ST","TH")
)

# Ostdeutschland: BB, BE, MV, SN, ST, TH (ARS 11-16)
ostdeutschland <- c("11","12","13","14","15","16")

# ============================================================
# TEIL 1: Breite Tabelle (Wide)
# Eine Zeile pro Kreis, eine Spalte pro Variable
# ============================================================

zensus_wide <- data %>%
  left_join(bl_labels, by = "Bundesland") %>%
  mutate(
    Ostdeutschland = as.integer(Bundesland %in% ostdeutschland),
    
    # Quoten neu berechnen (sauber, ohne Rundungsfehler aus CSV)
    Abitur_Quote          = round(Abitur_Gesamt  / MitAbschl_Gesamt  * 100, 1),
    OhneAbschl_Quote      = round(OhneAbschl_Gesamt / Bev_Gesamt     * 100, 1),
    Erwerbstaetig_Quote   = round(Erwerbstaetig_Gesamt / Bev_Gesamt_EW * 100, 1),
    Erwerbslos_Quote      = round(Erwerbslos_Gesamt / Erwerbspers_Gesamt * 100, 1),
    Nichterwerb_Quote     = round(Nichterwerb_Gesamt / Bev_Gesamt_EW  * 100, 1),
    
    # Gender Gaps (Frauen minus Männer, positiv = Frauen höher)
    Abitur_GenderGap      = round(Abitur_Quote_F      - Abitur_Quote_M,      1),
    Erwerbstaetig_GenderGap = round(Erwerbstaetig_Quote_F - Erwerbstaetig_Quote_M, 1),
    Erwerbslos_GenderGap  = round(Erwerbslos_Quote_F  - Erwerbslos_Quote_M,  1),
    
    # Bevölkerung in Tausend (analog zu Ausgaben_Verbrechen)
    Bev_Tausend           = round(Bev_Gesamt_EW / 1000, 1)
  ) %>%
  select(
    # Identifikatoren
    ARS, Name, BL_Name, Ostdeutschland,
    
    # Bevölkerung
    Bev_Tausend,
    
    # Bildung (Quoten)
    Abitur_Quote,
    OhneAbschl_Quote,
    Abitur_GenderGap,
    
    # Erwerbsstatus (Quoten)
    Erwerbstaetig_Quote,
    Erwerbslos_Quote,
    Nichterwerb_Quote,
    Erwerbstaetig_GenderGap,
    Erwerbslos_GenderGap
  ) %>%
  rename(Bundesland = BL_Name)

# ============================================================
# TEIL 2: Lange Tabelle (Long)
# Eine Zeile pro Kreis × Variable
# Geeignet für facet_wrap-Visualisierungen
# ============================================================

# Welche Spalten werden "lang" gedreht?
analyse_vars <- c(
  "Bev_Tausend",
  "Abitur_Quote",
  "OhneAbschl_Quote",
  "Abitur_GenderGap",
  "Erwerbstaetig_Quote",
  "Erwerbslos_Quote",
  "Nichterwerb_Quote",
  "Erwerbstaetig_GenderGap",
  "Erwerbslos_GenderGap"
)

zensus_long <- zensus_wide %>%
  pivot_longer(
    cols      = all_of(analyse_vars),
    names_to  = "Variable",
    values_to = "Wert"
  )

# ============================================================
# TEIL 3: Export
# ============================================================

write.csv(zensus_wide, "zensus2022_wide.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(zensus_long, "zensus2022_long.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("Fertig!\n")
cat(sprintf("Wide: %d Zeilen × %d Spalten\n", nrow(zensus_wide), ncol(zensus_wide)))
cat(sprintf("Long: %d Zeilen × %d Spalten\n", nrow(zensus_long), ncol(zensus_long)))
cat("\nVariablen in der Wide-Tabelle:\n")
print(colnames(zensus_wide))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Daten laden
library(readr)

df <- read_csv("/Users/jakubmarczak/Downloads/Project1_data/zensus2022_long.csv")
df2 <- read_csv("/Users/jakubmarczak/Downloads/Project1_data/zensus2022_wide.csv")

## 1. Datenaufbereitung
## Der Datensatz ist hinreichend bereinigt. Das einzige, was geändert wurde 
## ist die Variable 'Ostdeutschland', welche durch den Faktor 'Region' ersetzt
## wurde und die Werte Ost (1) und West (0) annehmen kann. Das macht es später 
## einfacher mit der Kategorisierung zwischen Ost- und Westdeutschland zu arbeiten.

df2$Region <- factor(
  df2$Ostdeutschland,
  levels = c(1, 0),
  labels = c("Ost", "West")
)

library(dplyr)
df2 <- df2 %>%
  select(ARS, Name, Bundesland, Region, Bev_Tausend, Abitur_Quote, 
         OhneAbschl_Quote, Abitur_GenderGap, Erwerbstaetig_Quote, 
         Erwerbslos_Quote, Nichterwerb_Quote, Erwerbstaetig_GenderGap, Erwerbslos_GenderGap)

print(df2, width = Inf)

## Kategorisch
## ARS, Name, Bundesland, Region (davor: Ostdeutschland)
## Metrisch
## Bev_Tausend, Abitur_Quote, OhneAbschl_Quote, Abitur_GenderGap, 
## Erwerbstaetig_Quote, Erwerbslos_Quote, Erwerbstaetig_GenderGap,
## Erwerbslos_GenderGap

## 2. Ausreißeranalyse
library(ggplot2)
o1 <- ggplot(df2, aes(x = "", y = Abitur_Quote)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "Abiturquote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

o2 <- ggplot(df2, aes(x = "", y = OhneAbschl_Quote)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "Ohne-Abschluss-Quote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

o3 <- ggplot(df2, aes(x = "", y = Erwerbstaetig_Quote)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "Erwerbstätigkeitsquote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

o4 <- ggplot(df2, aes(x = "", y = Erwerbslos_Quote)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "Erwerbslosigkeitsquote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

o5 <- ggplot(df2, aes(x = "", y = Nichterwerb_Quote)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "Nichterwerbsquote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

library(patchwork)
(o1 + o2 + o3) /
  (o4 + o5)

## Welche Städte / Kreise sind die Ausreißer?
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  which(x < lower | x > upper)
}

## Abiturquote
OL_Abitur_Quote <- find_outliers(df2$Abitur_Quote)
df2[OL_Abitur_Quote, c("Name", "Region", "Abitur_Quote")]

## Ohne-Abschluss-Quote
OL_OhneAbschl_Quote <- find_outliers(df2$OhneAbschl_Quote)
df2[OL_OhneAbschl_Quote, c("Name", "Region", "OhneAbschl_Quote")]

## Erwerbstätigkeitsquote (keine Ausreißer)
OL_Erwerbstaetig_Quote <- find_outliers(df2$Erwerbstaetig_Quote)
df2[OL_Erwerbstaetig_Quote, c("Name", "Region", "Erwerbstaetig_Quote")]

## Erwerbslosigkeitsquote
OL_Erwerbslos_Quote <- find_outliers(df2$Erwerbslos_Quote)
df2[OL_Erwerbslos_Quote, c("Name", "Region", "Erwerbslos_Quote")]

## Nichterwerbsquote (keine Ausreißer)
OL_Nichterwerb_Quote <- find_outliers(df2$Nichterwerb_Quote)
df2[OL_Nichterwerb_Quote, c("Name", "Region", "Nichterwerb_Quote")]

## West- und Ostdeutschland
library(ggplot2)
ol1 <- ggplot(df2, aes(x = Region, y = Abitur_Quote)) +
  geom_boxplot() +
  labs(
    x = "Region Deutschlands",
    y = "Abiturquote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ol2 <- ggplot(df2, aes(x = Region, y = OhneAbschl_Quote)) +
  geom_boxplot() +
  labs(
    x = "Region Deutschlands",
    y = "Ohne-Abschluss-Quote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ol3 <- ggplot(df2, aes(x = Region, y = Erwerbstaetig_Quote)) +
  geom_boxplot() +
  labs(
    x = "Region Deutschlands",
    y = "Erwerbstätigkeitsquote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ol4 <- ggplot(df2, aes(x = Region, y = Erwerbslos_Quote)) +
  geom_boxplot() +
  labs(
    x = "Region Deutschlands",
    y = "Erwerbslosigkeitsquote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ol5 <- ggplot(df2, aes(x = Region, y = Nichterwerb_Quote)) +
  geom_boxplot() +
  labs(
    x = "Region Deutschlands",
    y = "Nichterwerbsquote (%)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

library(patchwork)
(ol1 + ol2 + ol3) /
  (ol4 + ol5)

## Welche Städte / Kreise sind die Ausreißer?
df2_west <- df2 %>%
              filter(Region == "West")

df2_east <- df2 %>%
              filter(Region == "Ost")

## Abiturqoute
## West
OLW_Abitur_Quote <- find_outliers(df2_west$Abitur_Quote)
df2_west[OLW_Abitur_Quote, c("Name", "Region", "Abitur_Quote")]
## Ost
OLE_Abitur_Quote <- find_outliers(df2_east$Abitur_Quote)
df2_east[OLE_Abitur_Quote, c("Name", "Region", "Abitur_Quote")]

## Ohne-Abschluss-Quote
## West
OLW_OhneAbschl_Quote <- find_outliers(df2_west$OhneAbschl_Quote)
df2_west[OLW_OhneAbschl_Quote, c("Name", "Region", "OhneAbschl_Quote")]
## Ost
OLE_OhneAbschl_Quote <- find_outliers(df2_east$OhneAbschl_Quote)
df2_east[OLE_OhneAbschl_Quote, c("Name", "Region", "OhneAbschl_Quote")]

## Erwerbstätigkeitsquote
##West
OLW_Erwerbstaetig_Quote <- find_outliers(df2_west$Erwerbstaetig_Quote)
df2_west[OLW_Erwerbstaetig_Quote, c("Name", "Region", "Erwerbstaetig_Quote")]
## Ost
OLE_Erwerbstaetig_Quote <- find_outliers(df2_east$Erwerbstaetig_Quote)
df2_east[OLE_Erwerbstaetig_Quote, c("Name", "Region", "Erwerbstaetig_Quote")]

## Erwerbslosigkeitsquote
## West
OLW_Erwerbslos_Quote <- find_outliers(df2_west$Erwerbslos_Quote)
df2_west[OLW_Erwerbslos_Quote, c("Name", "Region", "Erwerbslos_Quote")]
## Ost
OLE_Erwerbslos_Quote <- find_outliers(df2_east$Erwerbslos_Quote)
df2_east[OLE_Erwerbslos_Quote, c("Name", "Region", "Erwerbslos_Quote")]

## Nichterwerbsquote
## West
OLW_Nichterwerb_Quote <- find_outliers(df2_west$Nichterwerb_Quote)
df2_west[OLW_Nichterwerb_Quote, c("Name", "Region", "Nichterwerb_Quote")]
## Ost
OLE_Nichterwerb_Quote <- find_outliers(df2_east$Nichterwerb_Quote)
df2_east[OLE_Nichterwerb_Quote, c("Name", "Region", "Nichterwerb_Quote")]

## Es gibt Ausreißer. Es fallen vor allem größere Städte und weniger Kreise auf. 

## Da die Ausreißer echte Werte darstellen und keine Messfehler sind, sollten
## sie im Datensatz behalten werden und zur weiteren Analyse herangezogen werden.

## 3. Univariate Verteilungen
boxplt <- function(variab, variabname) {
  ggplot(df2, aes(y = {{ variab }})) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    labs(
      y = variabname
    ) +
    facet_wrap(~ Region,
               strip.position = "bottom") +
    theme_minimal() + 
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank()
    )
}

b1 <- boxplt(Bev_Tausend, "Bevölkerungszahl (in Tausend)")
b2 <- boxplt(Abitur_Quote, "Abiturquote (%)") 
b3 <- boxplt(OhneAbschl_Quote, "Ohne-Abschluss-Quote (%)") 
b4 <- boxplt(Abitur_GenderGap, "Abitur Gender Gap (%)") 
b5 <- boxplt(Erwerbstaetig_Quote, "Erwerbstätig (%)") 
b6 <- boxplt(Erwerbslos_Quote, "Erwerbslos (%)")
b7 <- boxplt(Nichterwerb_Quote, "Nichterwerb (%)")
b8 <- boxplt(Erwerbstaetig_GenderGap, "Erwerbstätig Gender Gap (%)")
b9 <- boxplt(Erwerbslos_GenderGap, "Erwerbslos Gender Gap (%)")

library(patchwork)
(b1 + b2 + b3) /
(b4 + b5 + b6) /
(b7 + b8 + b9)

histogramkde <- function(variab, variabname) {
  ggplot(df2, aes(x = {{ variab }})) +
    geom_histogram(alpha = 0.4, aes(y = after_stat(density), fill = Region)) +
    geom_density(adjust = 1.4, aes(color = Region)) +
    labs(
      x = variabname,
      y = "Dichte"
    ) +
    facet_wrap(~ Region,
               strip.position = "bottom") +
    scale_fill_manual(values = c("West" = "#5ac9c7", "Ost" = "#ec5b5b")) +
    scale_color_manual(values = c("West" = "#3f8d8b", "Ost" = "#a54040")) +
    theme_minimal() + 
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

p1 <- histogramkde(Bev_Tausend, "Bevölkerungszahl (in Tausend)")
p2 <- histogramkde(Abitur_Quote, "Abiturquote (%)")
p3 <- histogramkde(OhneAbschl_Quote, "Ohne-Abschluss-Quote (%)")
p4 <- histogramkde(Abitur_GenderGap, "Abitur Gender Gap (%)")
p5 <- histogramkde(Erwerbstaetig_Quote, "Erwerbstätig (%)")
p6 <- histogramkde(Erwerbslos_Quote, "Erwerbslos (%)")
p7 <- histogramkde(Nichterwerb_Quote, "Nichterwerb (%)")
p8 <- histogramkde(Erwerbstaetig_GenderGap, "Erwerbstätig Gender Gap (%)")
p9 <- histogramkde(Erwerbslos_GenderGap, "Erwerbslos Gender Gap (%)")

library(patchwork)
(p1 + p2 + p3) /
(p4 + p5 + p6) /
(p7 + p8 + p9)

## 4. paarweise Scatterplots / 5. paarweise Korrelationsanalyse
library(GGally)
ggpairs(
  df2,
  columns = c("Bev_Tausend", "Abitur_Quote", "OhneAbschl_Quote", "Abitur_GenderGap", "Erwerbstaetig_Quote", "Erwerbslos_Quote", "Nichterwerb_Quote", "Erwerbstaetig_GenderGap", "Erwerbslos_GenderGap"),
  columnLabels = c("Bevölkerung (tsd.)", "Abiturquote (%)", "Ohne-Abschluss-Quote (%)", "Abitur Gender Gap (PP)", "Erwerbstätigkeitsquote (%)", "Erwerbslosigkeitsquote (%)", "Nichterwerbsquote (%)", "Erwerbstätig Gender Gap (PP)", "Erwerbslos Gender Gap (PP)"),
  aes(color = Region, alpha = 0.6)
)

## Erste mögliche Zusamenhänge:
## I.
## Ohne Abschluss <-> Erwerbstätigkeit
## => niedrige Bildung -> weniger Beschäftigung

## II.
## Ohne Abschluss <-> Erwerbslosigkeit
## => niedrige Bildung -> mehr Arbeitslosigkeit

## III.
## Abiturquote <-> Abitur Gender Gap
## => Größere Gender Gap im Westen
## => Je mehr Abitur, desto größer die Gender Gap

numeric_df <- df2 %>%
  select(where(is.numeric))

library(psych)
cor_mat <- cor(numeric_df, use = "complete.obs")

cor_mat

## Cohens Konventionen (1998)
classify_corr <- function(r) {
  r <- abs(r)
  
  if (r < 0.1) return("trivial")
  else if (r < 0.3) return("small")
  else if (r < 0.5) return("medium")
  else return("large")
}

corr_classes <- matrix(
  sapply(cor_mat, classify_corr),
  nrow = nrow(cor_mat)
)

rownames(corr_classes) <- rownames(cor_mat)
colnames(corr_classes) <- colnames(cor_mat)

corr_classes

library(Hmisc)

res <- rcorr(as.matrix(numeric_df))

cor_mat <- res$r      ## Korrelationen
p_mat   <- res$P      ## p-Werte

stars <- ifelse(p_mat < 0.001, "***",
         ifelse(p_mat < 0.01, "**",
         ifelse(p_mat < 0.05, "*", "")))

combined <- matrix(
  ifelse(is.na(p_mat), "", paste0(round(cor_mat, 2), stars)),
  nrow = nrow(cor_mat)
)

rownames(combined) <- rownames(cor_mat)
colnames(combined) <- colnames(cor_mat)

diag(combined) <- ""

as.data.frame(combined)

library(corrplot)

corrplot(
  cor_mat,
  p.mat = p_mat,
  sig.level = 0.05,
  insig = "pch",
  tl.cex = 0.8,
  tl.col = "black",
  tl.srt = 45, 
  method = "color", 
  type = "lower",
  addCoef.col = "black",
  cl.pos = "n"
  )

## 6. Diskussion
## Ohne Abschluss <-> Erwerbstätigkeit
## deutet darauf hin, dass: niedrige Bildung -> weniger Beschäftigung

## Ohne Abschluss <-> Erwerbslosigkeit
## deutet darauf hin, dass: niedrige Bildung -> mehr Arbeitslosigkeit

## Abiturquote <-> Abitur Gender Gap
## deutet darauf hin, dass: Größere Gender Gap im Westen
## deutet darauf hin, dass: Je mehr Abitur, desto größer die Gender Gap

## Auf Basis der Korrelationsanalyse können keine kausalen Zusammenhänge festgestellt werden.
## Die Korrelationsanalyse fundiert auf linearen Zusammenhängen, was nicht unbedingt der wahren zugrundeliegenden Struktur der Daten entspricht.
## Es wären auch Drittvariablen nicht ausgeschlossen, die beide Variablen in den jeweiligen Scatterplots beeinflussen,
## was eine Begründung der Kausalität erschwert. 
## Die unterschiedlichen Zusammenhänge zwischen Ost- und Westdeutschland deuten auf eine
## hinreichend große Rolle von strukturellen Unterschieden hin, welche nicht durch einfache kausale
## Aussagen beschrieben werden können.

## 7. Variablen mit großem Einfluss auf Erwerbslosenquote identifizieren
## I.   Abiturquote (%)
## II.  Ohne-Abschluss-Quote (%)
## III. Erwerbstätigkeitsquote (%)

## Multiple Lineare Regression
model <- lm(Erwerbslos_Quote ~ 
              Abitur_Quote + 
              OhneAbschl_Quote + 
              Erwerbstaetig_Quote,
            data = df2)

summary(model)

## Wichtige Kennzahlen
## Gesamtes Modell:
## p-value: < 2.2e-16
## => gesamtes Modell ist hochsignifikant
## Multiple R-squared:  0.3381
## => das Modell erklärt ca. 33.8% der Varianz der Erwerbslosenquote
## => es existieren andere Faktoren, die die Varianz zusätzlich erklären
## Einzelne Variablen:
## OhneAbschl_Quote:    0.221623 ***
## => hochsignifikant und stark
## Erwerbstaetig_Quote  1.16e-08 ***
## => hochsignifikant, aber geht mit Erwerbslosigkeit einher
## Abitur_Quote:        0.0977
## => nicht signifikant auf 5%-Niveau
## ⚠️ Bei gleichzeitiger Betrachtung der 3 ausgewählten Variablen ist niedrige 
## Bildung ausschlaggebend

## Multiples Lineares Modell mit Effekten
model2 <- lm(
  Erwerbslos_Quote ~ 
    Abitur_Quote * Region + 
    OhneAbschl_Quote * Region,
  data = df2
)

summary(model2)

## Wichtige Kennzahlen
## Gesamtes Modell:
## p-value: < 2.2e-16
## => gesamtes Modell ist hochsignifikant
## Multiple R-squared:            0.3111
## => das Modell erklärt ca. 31.2% der Varianz der Erwerbslosenquote
## => es existieren andere Faktoren, die die Varianz zusätzlich erklären
## Einzelne Variablen:
## OhneAbschl_Quote:              0.59 ***
## => hochsignifikant und stark für Ostdeutschland
## RegionWest:OhneAbschl_Quote:   -0.29 **
## ~> 0.5925 - 0.2950 = 0.2975
## => signifikant, jedoch halb so stark wie in Ostdeutschland für Westdeutschland
## Abitur_Quote:                  ''
## => nicht signifikant auf 5%-Niveau
## ⚠️ In Ostdeutschland ist der Zusammenhang zwischen niedriger Bildung und 
## Arbeitslosigkeit deutlich stärker ausgebildet als in Westdeutschland

## Scatterplots
## Ohne-Abschluss-Quote -> Erwerbslosenquote
ggplot(df2, aes(x = OhneAbschl_Quote, y = Erwerbslos_Quote, color = Region)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(
    x = "Ohne-Abschluss-Quote (%)",
    y = "Erwerbslosenquote (%)",
    color = "Region"
  ) +
  scale_color_manual(values = c("West" = "#5ac9c7", "Ost" = "#ec5b5b"),
                     labels = c("Westdeutschland", "Ostdeutschland")) +
  theme_minimal()

## Erwerbstätigkeitsquote -> Erwerbslosenquote
ggplot(df2, aes(x = Erwerbstaetig_Quote, y = Erwerbslos_Quote, color = Region)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(
    x = "Erwerbstätigkeitsquote (%)",
    y = "Erwerbslosenquote (%)",
    color = "Region"
  ) +
  scale_color_manual(values = c("West" = "#5ac9c7", "Ost" = "#ec5b5b"),
                     labels = c("Westdeutschland", "Ostdeutschland")) +
  theme_minimal()

## Abiturquote -> Erwerbslosenquote 
ggplot(df2, aes(x = Abitur_Quote, y = Erwerbslos_Quote, color = Region)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(
    x = "Abiturquote (%)",
    y = "Erwerbslosenquote (%)",
    color = "Region"
  ) +
  scale_color_manual(values = c("West" = "#5ac9c7", "Ost" = "#ec5b5b"),
                     labels = c("Westdeutschland", "Ostdeutschland")) +
  theme_minimal()

## 8. Vergleich Ost- und Westdeutschland

## Vergleich anhand deskriptiver Maßzahlen
## Median
aggregate(cbind(Bev_Tausend, Abitur_Quote, OhneAbschl_Quote, Abitur_GenderGap, 
                Erwerbstaetig_Quote, Erwerbslos_Quote, Nichterwerb_Quote, 
                Erwerbstaetig_GenderGap, Erwerbslos_GenderGap) ~ Region, 
          data = df2, 
          FUN = median, na.rm = TRUE)
## Ostdeutschland ist Westdeutschland in Bezug auf die Abiturquote, 
## Erwerbstätigkeitsquote und Nichterwerbsquote systematisch unterlegen. 
## Jedoch haben in Ostdeutschland prozentuell mehr Personen einen Abschluss als
## in Westdeutschland. 
## Insgesamt lässt sich dennoch erkennen, dass sich die Unterschiede in den
## Medianen nicht maßgebend auf die Erwerbslosenquote auswirken, da diese zwischen
## Ost- und Westdeutschland ähnlich sind.

## MAD (Median Absolute Deviation)
aggregate(cbind(Bev_Tausend, Abitur_Quote, OhneAbschl_Quote, Abitur_GenderGap, 
                Erwerbstaetig_Quote, Erwerbslos_Quote, Nichterwerb_Quote, 
                Erwerbstaetig_GenderGap, Erwerbslos_GenderGap) ~ Region, 
          data = df2, 
          FUN = mad, na.rm = TRUE)
## Ostdeutschland hat kleinere Streuungen bezüglich der Abiturquote, 
## Ohne-Abschluss-Quote, Erwerbstätigkeitsquote und Nichterwerbsquote.
## Das deutet darauf hin, dass Westdeutschland in Bezug auf Bildung und 
## Beschäftigung stärkere regionale Unterschiede aufweist als Ostdeutschland.
## Jedoch ist die Streuung der Erwerbslosenquote nahezu identisch, was
## darauf hindeutet, dass die Arbeitslosigkeit innerhalb der Regionen 
## vergleichbar ist.

## Standardabweichung
aggregate(cbind(Bev_Tausend, Abitur_Quote, OhneAbschl_Quote, Abitur_GenderGap, 
                Erwerbstaetig_Quote, Erwerbslos_Quote, Nichterwerb_Quote, 
                Erwerbstaetig_GenderGap, Erwerbslos_GenderGap) ~ Region, 
          data = df2, 
          FUN = sd, na.rm = TRUE)
## Die Streuungen sind mit MAD vergleichbar, oft liegen die Streuungen in 
## Westdeutschland über denen in Ostdeutschland, die Streuung der 
## Erwerbslosenquote ist auch hier ähnlich. 

## Tests auf Gleichheit von Regionen
## Jetzt sollen die Befunde statistisch genauer untersucht werden, um die
## Hypothese zu prüfen, ob sich die Erwerbslosenquote zwischen den
## Regionen statistisch nicht unterscheidet, und wie das mit den anderen
## Variablen wie der Abiturquote, Ohne-Abschluss-Quote und Erwerbstätigkeitsquote
## aussieht. 

## Prüfen der Normalverteilungsannahme
histogramkde(Abitur_Quote, "Abiturquote (%)")
histogramkde(OhneAbschl_Quote, "Ohne-Abschluss-Quote (%)")
histogramkde(Erwerbstaetig_Quote, "Erwerbstätig (%)")
histogramkde(Erwerbslos_Quote, "Erwerbslos (%)")
## Aufgrund von Ausreißern (=> Verteilungsschiefe) kann keine NV-Annahme
## getroffen werden. Nachfolgend wird mit dem Wilcoxon Rangsummen Test gearbeitet,
## da dieser Test nichtparametrisch ist. Der t-Test wird lediglich dazu
## verwendet, die Konsistenz der Ergebnisse zu prüfen.

## Die Tests sind folgendermaßen aufgebaut:
## Wilcoxon Rangsummen Test
## H0: Die Wahrscheinlichkeit, dass ein zufälliger Wert aus Gruppe A größer ist als aus Gruppe B, ist gleich 0.5
## => P(X < Y) + 0.5P(X = Y) = 0.5
## H1: Die Verteilungen der beiden Gruppen sind unterschiedlich.
## => P(X < Y) + 0.5P(X = Y) != 0.5
## Welch t-Test (ungleiche Varianzen)
## H0: µ1 = µ2
## H1: µ1 != µ2
## Entscheidungsregel: H0 ablehnen, wenn p < 0.05 bei alpha = 0.05

## Tests für Unterschiede zw. Regionen bzgl. Abiturquote
wilcox.test(Abitur_Quote ~ Region, data = df2) ## p < 0.001
t.test(Abitur_Quote ~ Region, data = df2) ## p < 0.001
## H0 ablehnen: Bzgl. der Abiturquote unterscheiden sich die Regionen statistisch 
## signifikant.

## Tests für Unterschiede zw. Regionen bzgl. Ohne-Abschluss-Quote
wilcox.test(OhneAbschl_Quote ~ Region, data = df2) ## p < 0.001
t.test(OhneAbschl_Quote ~ Region, data = df2) ## p < 0.001
## H0 ablehnen: Bzgl. der Ohne-Abschluss-Quote unterscheiden sich die Regionen 
## statistisch signifikant.

## Tests für Unterschiede zw. Regionen bzgl. Erwerbstätigkeitsquote
wilcox.test(Erwerbstaetig_Quote ~ Region, data = df2) ## p < 0.001
t.test(Erwerbstaetig_Quote ~ Region, data = df2) ## p < 0.001
## H0 ablehnen: Bzgl. der Erwerbstätigkeitsquote unterscheiden sich die Regionen 
## statistisch signifikant.

## Tests für Unterschiede zw. Regionen bzgl. Erwerbslosenquote
wilcox.test(Erwerbslos_Quote ~ Region, data = df2) ## p > 0.05
t.test(Erwerbslos_Quote ~ Region, data = df2) ## p > 0.05
## H0 beibehalten: Bzgl. der Erwerbslosenquote unterscheiden sich die Regionen 
## nicht statistisch signifikant.

## ⚠️TAKEAWAY⚠️
## Obwohl sich Ost- und Westdeutschland signifikant in Bildungsniveau und 
## Erwerbstätigkeit unterscheiden, zeigt sich kein signifikanter Unterschied in 
## der Erwerbslosigkeitsquote.

## 9. Berichtlegung



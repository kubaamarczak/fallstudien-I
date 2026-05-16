# ============================================================
# Zensus 2022 – Datenaufbereitung für Fallstudien I
# Quellen: Regionaltabelle_Bildung_Erwerbstaetigkeit.xlsx
# Regionalebene: Stadtkreis / kreisfreie Stadt / Landkreis
# ============================================================
#https://www.destatis.de/static/DE/zensus/gitterdaten/Regionaltabelle_Bildung_Erwerbstaetigkeit.xlsx
library(readxl)
library(dplyr)

# ---- Pfad zur heruntergeladenen Datei ----------------------
file_path <- "Regionaltabelle_Bildung_Erwerbstaetigkeit.xlsx"

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


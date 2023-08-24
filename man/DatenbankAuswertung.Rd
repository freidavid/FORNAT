\name{DatenbankAuswertung}
\alias{DatenbankAuswertung}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Datenbankauswertung
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DatenbankAuswertung(input = NULL, output = NULL, combine = F, befischung = NULL, project = NULL, durchgang = NULL, species = NULL, color = T, analyze_all = F, fish_id = F, cutoff = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
The path to input file in excel format, which in this case is the latest version of the fishdatabase.
}
  \item{output}{
The path to store the output files.
}
  \item{combine}{
Default is FALSE. If combine is set to TRUE, all data of one project is combined.
}
  \item{befischung}{
This is used to specify a specific Befischungs-ID to analyse. It also works with multiple IDs, e.g. c(12,25,938:940)
}
  \item{project}{
Analyse all Befischungs-IDs of one project.
}
  \item{durchgang}{
Only analyse a certain durchgang, e.g. durchgang=c(1,2).
}
  \item{species}{
Subset the analysis to a certain species.
}
  \item{color}{
If set to TRUE (default), plots are in color, if set to FALSE, plots are black and white.
}
  \item{analyze_all}{
If set to TRUE, the function analayzez the whole data base (Default is FALSE).
}
  \item{fish_id}{
IF set to TRUE, the points of the plots are labelled with the specific fish-IDs. Default is FALSE..
}
  \item{cutoff}{
Cutoff to detemine the 0 and 0+ categories. Default is 100, which corresponds to 100 mm.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
DFR
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The function is currently defined as
function (input = NULL, output = NULL, combine = F, befischung = NULL, 
    project = NULL, durchgang = NULL, species = NULL, color = T, 
    analyze_all = F, fish_id = F, cutoff = 100) 
{
    packages <- c("readxl", "ggplot2", "RColorBrewer", "FSA", 
        "xlsx")
    not_installed <- packages[!(packages \%in\% installed.packages()[, 
        "Package"])]
    if (length(not_installed) > 0) {
        install.packages(not_installed)
    }
    lapply(packages, require, character.only = T)
    if (length(input) == 0) {
        warning("No input file provided, try to access fish database on FORNAT server", 
            call. = F)
        warning("Using 20230105_Fischdatenbank_Version3.xls as data-base", 
            call. = F)
    }
    if (length(output) == 0) {
        wd <- getwd()
        output = paste0(wd)
        warning(paste0("No path to write output provided, writing output to ", 
            wd, "/output.xls"))
    }
    if (length(project > 0) && length(befischung > 0)) {
        stop("You cannot specify both Projekt-ID and Befischungs-ID. Please chose only one option!")
    }
    if (length(project) == 0 && combine == T) {
        stop("You cannot combine over a project without specifying a project id. Please specify a project!")
    }
    data <- list()
    sheets <- excel_sheets(input)
    suppressMessages(suppressWarnings(for (i in 1:length(sheets)) {
        data[[i]] <- read_excel(input, sheet = i)
    }))
    names(data) <- sheets
    biometriedaten <- data$Biometrie
    befischungsdaten <- data$Befischung
    habitatdaten <- data$Habitatmessung
    suppressMessages(suppressWarnings(befischungsdaten$Datum <- read_excel(input, 
        sheet = which(sheets == "Befischung"), col_types = "date")$Datum))
    suppressMessages(suppressWarnings(biometriedaten$Durchgang <- read_excel(input, 
        sheet = which(sheets == "Biometrie"), col_types = "text")$Durchgang))
    biometriedaten$Fischart <- gsub("ü", "ue", biometriedaten$Fischart)
    biometriedaten$Fischart <- gsub("ü", "ue", biometriedaten$Fischart)
    biometriedaten$Fischart <- gsub("ö", "oe", biometriedaten$Fischart)
    biometriedaten$Fischart <- gsub("ä", "ae", biometriedaten$Fischart)
    biometriedaten$Fischart <- gsub("Ä", "Ae", biometriedaten$Fischart)
    biometriedaten$Fischart <- gsub("Ö", "Oe", biometriedaten$Fischart)
    biometriedaten$Fischart <- gsub("Ü", "Ue", biometriedaten$Fischart)
    if (length(project > 0)) {
        befischung <- befischungsdaten[which(befischungsdaten$`Projekt-ID` == 
            project), ]$`Befischung-ID`
        if (combine == T) {
            befischung <- befischungsdaten[which(befischungsdaten$`Projekt-ID` == 
                project), ]$`Befischung-ID`
            for (i in 1:length(befischung)) {
                biometriedaten[which(biometriedaten$Befischung_ID == 
                  befischung[i]), ]$Befischung_ID <- (-1)
                befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                  befischung[i]), ]$`Befischung-ID` <- (-1)
                habitatdaten[which(habitatdaten$`Befischung-ID` == 
                  befischung[i]), ]$`Befischung-ID` <- (-1)
            }
            befischung <- (-1)
        }
    }
    if (length(befischung) == 0) {
        befischung <- max(unique(befischungsdaten$`Befischung-ID`))
    }
    if (analyze_all == T) {
        befischung <- unique(befischungsdaten$`Befischung-ID`)
    }
    bio <- biometriedaten[which(biometriedaten$Befischung_ID == 
        befischung[1]), ]
    if (length(befischung) > 1) {
        for (i in 2:length(befischung)) {
            bio <- rbind(bio, biometriedaten[which(biometriedaten$Befischung_ID == 
                befischung[i]), ])
        }
    }
    if (length(durchgang) > 0) {
        if (length(durchgang) == 1) {
            bio <- bio[which(bio$Durchgang == durchgang), ]
        }
        if (length(durchgang) > 1) {
            bio_int <- bio[which(bio$Durchgang == durchgang[1]), 
                ]
            for (w in 2:length(durchgang)) {
                bio_int <- rbind(bio_int, bio[which(bio$Durchgang == 
                  durchgang[w]), ])
            }
            bio <- bio_int
        }
    }
    if (combine == T) {
        results <- vector(mode = "list", length = 1)
    }
    else {
        results <- vector(mode = "list", length = max(befischung))
    }
    for (j in 1:length(befischung)) {
        species <- unique(bio[which(bio$Befischung_ID == befischung[j]), 
            ]$Fischart)
        befischungs_id <- rep(befischung[j], length(species))
        gewaesser <- befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
            befischung[j]), ]$Gewässer
        if (length(unique(gewaesser)) == 1) {
            gewaesser <- rep(unique(gewaesser), length(species))
        }
        if (length(unique(gewaesser)) > 1) {
            gewaesser <- rep("more_than_one_waterbody", length(species))
        }
        ort <- befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
            befischung[j]), ]$`Orts-Bezeichnung`
        ort <- rep(ort, length(species))
        if (length(unique(ort)) > 1) {
            ort <- rep("more_than_one_location", length(species))
        }
        if (combine == F) {
            datum <- as.character(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$Datum)
            datum <- rep(datum, length(species))
        }
        if (combine == T) {
            datum <- as.character(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$Datum)
            datum <- rep(datum, length(species))
            if (length(unique(datum)) > 1) {
                datum <- rep("more_than_one_date", length(species))
            }
        }
        counts <- vector()
        average_length <- vector()
        min_length <- vector()
        max_length <- vector()
        average_weight <- vector()
        min_weight <- vector()
        max_weight <- vector()
        kondition <- vector()
        carle_strub_estimate <- vector()
        carle_strub_confinterval1 <- vector()
        carle_strub_confinterval2 <- vector()
        counts_0plus <- vector()
        counts_0 <- vector()
        carle_strub_100m <- vector()
        carle_strub_ha <- vector()
        weight_per_100m <- vector()
        weight_per_ha <- vector()
        carle_stub_weight_per_100m <- vector()
        carle_stub_weight_per_ha <- vector()
        counts_per_dg <- vector()
        kondition_sd <- vector()
        anzahl_durchgaenge <- vector()
        nr_of_dgs <- rep(NA, length(species))
        for (i in 1:length(species)) {
            counts[i] <- length(bio[which(bio$Befischung_ID == 
                befischung[j] & bio$Fischart == species[i]), 
                ]$Fischart)
            counts_0plus[i] <- length(bio[which(bio$Befischung_ID == 
                befischung[j] & bio$Fischart == species[i] & 
                bio$Laenge_mm >= cutoff), ]$Fischart)
            counts_0[i] <- length(bio[which(bio$Befischung_ID == 
                befischung[j] & bio$Fischart == species[i] & 
                bio$Laenge_mm < cutoff), ]$Fischart)
            zero_zero_plus_ratio <- round(counts_0/counts_0plus, 
                digits = 2)
            durchgaenge <- unique(bio[which(bio$Befischung_ID == 
                befischung[j]), ]$Durchgang)
            temporary <- vector()
            for (u in 1:length(durchgaenge)) {
                temporary[u] <- length(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i] & 
                  bio$Durchgang == durchgaenge[u]), ]$Fischart)
            }
            counts_per_dg[i] <- paste(temporary, collapse = "/")
            projekt <- vector()
            projekt <- unique(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$`Projekt-ID`)
            projekt <- rep(projekt, length(species))
            laenge <- befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$`Streckenlänge [m]`
            if (length(laenge) == 0) {
                fish_per_100m <- NA
                zero_plus_per_100m <- NA
            }
            else {
                fish_per_100m <- round(counts/as.numeric(laenge) * 
                  100, digits = 1)
                zero_plus_per_100m <- round(counts_0plus/as.numeric(laenge) * 
                  100, digits = 1)
            }
            if (combine == T) {
                if (is.numeric(laenge)) {
                  if (is.na(unique(laenge))) {
                    fish_per_100m <- NA
                    zero_plus_per_100m <- NA
                  }
                  else {
                    fish_per_100m <- round(counts/as.numeric(sum(laenge)) * 
                      100, digits = 1)
                    zero_plus_per_100m <- round(counts_0plus/as.numeric(sum(laenge)) * 
                      100, digits = 1)
                  }
                }
                else {
                  fish_per_100m <- rep(NA, length(species))
                  zero_plus_per_100m <- rep(NA, length(species))
                }
            }
            if (length(habitatdaten[which(habitatdaten$`Befischung-ID` == 
                befischung[j]), ]$`Mittlere Breite`) > 0) {
                breite <- round(habitatdaten[which(habitatdaten$`Befischung-ID` == 
                  befischung[j]), ]$`Mittlere Breite`/100, digits = 1)
                flaeche <- as.numeric(laenge) * as.numeric(breite)
                fl_faktor <- 10000/flaeche
                fish_per_ha <- round(counts * fl_faktor, digits = 1)
            }
            else {
                breite <- NA
                flaeche <- NA
                fl_faktor <- NA
                fish_per_ha <- NA
            }
            if (length(na.omit(bio[which(bio$Befischung_ID == 
                befischung[j] & bio$Fischart == species[i]), 
                ]$Laenge_mm)) > 0) {
                average_length[i] <- mean(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Laenge_mm))/10
                min_length[i] <- min(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Laenge_mm))/10
                max_length[i] <- max(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Laenge_mm))/10
            }
            else {
                average_length[i] <- NA
                min_length[i] <- NA
                max_length[i] <- NA
            }
            if (length(na.omit(bio[which(bio$Befischung_ID == 
                befischung[j] & bio$Fischart == species[i]), 
                ]$Gewicht_g)) > 0) {
                average_weight[i] <- mean(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Gewicht_g))
                min_weight[i] <- min(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Gewicht_g))
                max_weight[i] <- max(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Gewicht_g))
                weight_per_100m[i] <- round(sum(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Gewicht_g))/as.numeric(laenge) * 100/1000, 
                  digits = 1)
                weight_per_ha[i] <- round(sum(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Gewicht_g))/1000 * fl_faktor, digits = 1)
            }
            else {
                average_weight[i] <- NA
                min_weight[i] <- NA
                max_weight[i] <- NA
                weight_per_100m[i] <- NA
                weight_per_ha[i] <- NA
            }
            if (!is.na(bio[1, ]$Durchgang)) {
                durchgaenge <- unique(bio[which(bio$Befischung_ID == 
                  befischung[j]), ]$Durchgang)
            }
            else {
                durchgaenge <- NA
            }
            if (length(na.omit(durchgaenge)) > 0) {
                dgs <- vector(mode = "list", length = length(durchgaenge))
                for (g in 1:length(durchgaenge)) {
                  dgs[[g]] <- bio[which(bio$Befischung_ID == 
                    befischung[j] & bio$Fischart == species[i] & 
                    bio$Durchgang == durchgaenge[g]), ]
                }
                catch <- vector()
                for (g in 1:length(durchgaenge)) {
                  catch[g] <- length(dgs[[g]]$Laenge_mm)
                }
                if (length(durchgaenge) > 1) {
                  int <- removal(catch = catch, method = "CarleStrub")
                  cs <- cbind(summary(int), confint(int))
                  carle_strub_estimate[i] <- cs[1, 1]
                  carle_strub_confinterval1[i] <- round(cs[1, 
                    3], digits = 1)
                  carle_strub_confinterval2[i] <- round(cs[1, 
                    4], digits = 1)
                  carle_strub_100m[i] <- round((cs[1, 1]/as.numeric(laenge) * 
                    100), digits = 1)
                  carle_strub_ha[i] <- round((cs[1, 1] * fl_faktor), 
                    digits = 1)
                  carle_stub_weight_per_100m[i] <- round(average_weight[i]/1000 * 
                    cs[1, 1]/as.numeric(laenge) * 100, digits = 1)
                  carle_stub_weight_per_ha[i] <- round(average_weight[i]/1000 * 
                    cs[1, 1] * fl_faktor, digits = 1)
                }
                else {
                  carle_strub_estimate[i] <- NA
                  carle_strub_confinterval1[i] <- NA
                  carle_strub_confinterval2[i] <- NA
                  carle_strub_100m[i] <- NA
                  carle_strub_ha[i] <- NA
                  carle_stub_weight_per_100m[i] <- NA
                  carle_stub_weight_per_ha[i] <- NA
                }
                if (!is.na(durchgaenge[1])) {
                  anzahl_durchgaenge <- rep(length(durchgaenge), 
                    length(species))
                }
                else {
                  anzahl_durchgaenge <- rep(NA, length(species))
                }
                dg_names <- durchgaenge[1]
                if (length(durchgaenge) > 1) {
                  for (s in 2:length(durchgaenge)) {
                    dg_names <- paste0(dg_names, ",", durchgaenge[s])
                  }
                }
                if (!is.na(durchgaenge[1])) {
                  nr_of_dgs <- rep(dg_names, length(species))
                }
            }
            else {
                anzahl_durchgaenge[i] <- NA
                carle_strub_estimate[i] <- NA
                carle_strub_confinterval1[i] <- NA
                carle_strub_confinterval2[i] <- NA
                carle_strub_100m[i] <- NA
                carle_strub_ha[i] <- NA
                carle_stub_weight_per_100m[i] <- NA
                carle_stub_weight_per_ha[i] <- NA
            }
            if (length(na.omit(bio[which(bio$Befischung_ID == 
                befischung[j] & bio$Fischart == species[i]), 
                ]$Laenge_mm)) > 0 && length(na.omit(bio[which(bio$Befischung_ID == 
                befischung[j] & bio$Fischart == species[i]), 
                ]$Gewicht_g)) > 0) {
                kondition[i] <- round(mean(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Gewicht_g/((bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Laenge_mm/10)^3) * 100)), digits = 1)
                kondition_sd[i] <- round(sd(na.omit(bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Gewicht_g/((bio[which(bio$Befischung_ID == 
                  befischung[j] & bio$Fischart == species[i]), 
                  ]$Laenge_mm/10)^3) * 100)), digits = 1)
            }
            else {
                kondition[i] <- NA
            }
            if (length(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$`m ü. M.`) > 0) {
                meters_above_sealevel <- rep(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                  befischung[j]), ]$`m ü. M.`, length(species))
            }
            else (meters_above_sealevel <- rep(NA, length(species)))
            coordinates <- rep(paste0(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$`X-Koordinate`, " / ", befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$`Y-Koordinate`), length(species))
            if (length(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$Abfluss) > 0) {
                abfluss <- rep(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                  befischung[j]), ]$Abfluss, length(species))
            }
            else (abfluss <- rep(NA, length(species)))
            if (length(laenge) > 0) {
                streckenlaenge <- rep(as.numeric(laenge), length(species))
            }
            else (streckenlaenge <- rep(NA, length(species)))
            if (length(breite) > 0) {
                streckenbreite <- rep(as.numeric(breite), length(species))
            }
            else (streckenbreite <- rep(NA, length(species)))
            if (length(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$`Wassertemperatur [°C]`) > 
                0) {
                temperatur <- rep(round(as.numeric(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                  befischung[j]), ]$`Wassertemperatur [°C]`), 
                  digits = 1), length(species))
            }
            else (temperatur <- rep(NA, length(species)))
            if (length(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$`Leitfähigkeit (MicroS)`) > 
                0) {
                leitfaehigkeit <- rep(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                  befischung[j]), ]$`Leitfähigkeit (MicroS)`, 
                  length(species))
            }
            else (leitfaehigkeit <- rep(NA, length(species)))
            if (length(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                befischung[j]), ]$Fängigkeit) > 0) {
                faengigkeit <- rep(befischungsdaten[which(befischungsdaten$`Befischung-ID` == 
                  befischung[j]), ]$Fängigkeit, length(species))
            }
            else (faengigkeit <- rep(NA, length(species)))
            if (length(species > 0)) {
                if (combine == F) {
                  suppressWarnings(results[[(befischung[j])]] <- as.data.frame(cbind(befischungs_id, 
                    projekt, datum, gewaesser, ort, species, 
                    counts, counts_per_dg, fish_per_100m, average_length, 
                    min_length, max_length, average_weight, min_weight, 
                    max_weight, weight_per_100m, weight_per_ha, 
                    kondition, kondition_sd, anzahl_durchgaenge, 
                    nr_of_dgs, carle_strub_estimate, carle_strub_confinterval1, 
                    carle_strub_confinterval2, carle_strub_100m, 
                    carle_strub_ha, carle_stub_weight_per_100m, 
                    carle_stub_weight_per_ha, zero_plus_per_100m, 
                    zero_zero_plus_ratio, fish_per_ha, meters_above_sealevel, 
                    coordinates, abfluss, streckenlaenge, streckenbreite, 
                    temperatur, leitfaehigkeit, faengigkeit)))
                }
                else {
                  suppressWarnings(results[[1]] <- as.data.frame(cbind(befischungs_id, 
                    projekt, datum, gewaesser, ort, species, 
                    counts, counts_per_dg, fish_per_100m, average_length, 
                    min_length, max_length, average_weight, min_weight, 
                    max_weight, weight_per_100m, weight_per_ha, 
                    kondition, kondition_sd, anzahl_durchgaenge, 
                    nr_of_dgs, carle_strub_estimate, carle_strub_confinterval1, 
                    carle_strub_confinterval2, carle_strub_100m, 
                    carle_strub_ha, carle_stub_weight_per_100m, 
                    zero_plus_per_100m, carle_stub_weight_per_ha, 
                    zero_zero_plus_ratio, fish_per_ha, meters_above_sealevel, 
                    coordinates, abfluss, streckenlaenge, streckenbreite, 
                    temperatur, leitfaehigkeit, faengigkeit)))
                }
            }
        }
    }
    names(results) <- sapply(results, "[[", 1, 1)
    results[sapply(results, is.null)] <- NULL
    labels1 <- c("Datum", "Gewässer", "Stelle", "Koordinate (unten)", 
        "Höhe", "Abfluss", "Strecken Länge / Korridor Länge", 
        "Benetzte Breite", "Durchgänge", "Temperatur / Leitfähigkeit", 
        "Verhältnisse / Fängigkeit")
    labels2 <- c("Fischart", "Anzahl Fische pro Durchgang", "Anzahl Fische / 100 m", 
        "Anzahl >0+ / 100m", "Anzahl Fische / ha", "Biomasse / 100m", 
        "Biomasse / ha", "Kondition (Std. Abw.)", "Gewichtsspektrum", 
        "Grössenspektrum", "Verhältnis 0+ / >0+", "C&S Anzahl Fische in Strecke", 
        "C&S Anzahl Fische / 100m", "C&S Anzahl Fische / ha", 
        "C&S Biomasse / 100m", "C&S Biomasse / ha")
    for (v in 1:length(results)) {
        d <- results[[as.character(befischung[v])]]
        data1 <- c(d[1, ]$datum, d[1, ]$gewaesser, d[1, ]$ort, 
            d[1, ]$coordinates, paste0(d[1, ]$meters_above_sealevel, 
                " m ü. M."), d[1, ]$abfluss, paste0(d[1, ]$streckenlaenge, 
                " m"), paste0(d[1, ]$streckenbreite, " m"), d[1, 
                ]$anzahl_durchgaenge, paste0(d[1, ]$temperatur, 
                "C° - ", d[1, ]$leitfaehigkeit, "µS", "/cm"), 
            d[1, ]$faengigkeit)
        mat1 <- cbind(labels1, data1)
        data2 <- c(d[1, ]$species, paste0(d[1, ]$counts_per_dg, 
            " Fische"), paste(d[1, ]$fish_per_100m, " Fi./100 m"), 
            paste0(d[1, ]$zero_plus_per_100m, " Fi./100 m"), 
            paste0(d[1, ]$fish_per_ha, " Fi./ha"), paste0(d[1, 
                ]$weight_per_100m, " kg/100 m"), paste0(d[1, 
                ]$weight_per_ha, " kg/ha"), paste0(d[1, ]$kondition, 
                " (", d[1, ]$kondition_sd, ")"), paste0(d[1, 
                ]$min_weight, "-", d[1, ]$max_weight, " g"), 
            paste0(d[1, ]$min_length, "-", d[1, ]$max_length, 
                " cm"), d[1, ]$zero_zero_plus_ratio, paste0(d[1, 
                ]$carle_strub_estimate, " Fische"), paste0(d[1, 
                ]$carle_strub_100m, " Fi./100 m"), paste(d[1, 
                ]$carle_strub_ha, " Fi./ha"), paste0(d[1, ]$carle_stub_weight_per_100m, 
                " kg/100 m"), paste0(d[1, ]$carle_stub_weight_per_ha, 
                " kg/ha"))
        mat2 <- cbind(labels2, data2)
        out <- rbind(mat1, mat2)
        if (length(d$species) > 1) {
            for (y in 2:length(d$species)) {
                data1 <- c(d[y, ]$datum, d[y, ]$gewaesser, d[y, 
                  ]$ort, d[y, ]$coordinates, paste0(d[y, ]$meters_above_sealevel, 
                  " m ü. M."), d[y, ]$abfluss, paste0(d[y, ]$streckenlaenge, 
                  " m"), paste0(d[y, ]$streckenbreite, " m"), 
                  d[y, ]$anzahl_durchgaenge, paste0(d[y, ]$temperatur, 
                    "C° - ", d[y, ]$leitfaehigkeit, "µS", "/cm"), 
                  d[y, ]$faengigkeit)
                data2 <- c(d[y, ]$species, paste0(d[y, ]$counts_per_dg, 
                  " Fische"), paste0(d[y, ]$fish_per_100m, " Fi./100 m"), 
                  paste0(d[y, ]$zero_plus_per_100m, " Fi./100 m"), 
                  paste0(d[y, ]$fish_per_ha, " Fi./ha"), paste0(d[y, 
                    ]$weight_per_100m, " kg/100 m"), paste0(d[y, 
                    ]$weight_per_ha, " kg/ha"), paste0(d[y, ]$kondition, 
                    " (", d[y, ]$kondition_sd, ")"), paste0(d[y, 
                    ]$min_weight, "-", d[y, ]$max_weight, " g"), 
                  paste0(d[y, ]$min_length, "-", d[y, ]$max_length, 
                    " cm"), d[y, ]$zero_zero_plus_ratio, paste0(d[y, 
                    ]$carle_strub_estimate, " Fische"), paste0(d[y, 
                    ]$carle_strub_100m, " Fi./100m"), paste0(d[y, 
                    ]$carle_strub_ha, " Fi./ha"), paste0(d[y, 
                    ]$carle_stub_weight_per_100m, " kg/100 m"), 
                  paste0(d[y, ]$carle_stub_weight_per_ha, " kg/ha"))
                out <- cbind(out, c(data1, data2))
            }
        }
        data.frame(lapply(out, function(y) if (is.numeric(y)) 
            round(y, 3)
        else y))
        write.xlsx(out, file = paste0(output, "/", unique(d$projekt), 
            "_Resultat_Befischungs_ID_", befischung[v], ".xlsx"), 
            col.names = F, row.names = F)
    }
    print(results)
    for (i in 1:length(befischung)) {
        index <- as.numeric(befischung[i])
        if (combine == T) {
            index = -1
        }
        plot_data <- as.data.frame(results[[as.character(index)]])
        plot_data$projekt <- gsub("/", "-", plot_data$projekt)
        plot_data$projekt <- gsub(" ", "_", plot_data$projekt)
        plot_data$species <- gsub("ü", "ue", plot_data$species)
        plot_data$species <- gsub("ä", "ae", plot_data$species)
        plot_data$species <- gsub("ö", "oe", plot_data$species)
        if (length(plot_data$projekt) > 0) {
            if (unique(is.na(plot_data$datum))) {
                plot_data$datum <- "Kein_Datum"
            }
            if (color == T) {
                if (nrow(plot_data) >= 3 & nrow(plot_data) <= 
                  11) {
                  colors <- brewer.pal(nrow(plot_data), "PuOr")
                }
                if (nrow(plot_data) == 1) {
                  colors <- "black"
                }
                if (nrow(plot_data) == 2) {
                  colors <- c("black", "lightgrey")
                }
                if (nrow(plot_data) == 12) {
                  colors <- c(brewer.pal(11, "PuOr"), "black")
                }
                if (nrow(plot_data) == 13) {
                  colors <- c(brewer.pal(11, "PuOr"), "black", 
                    "lightgrey")
                }
                if (nrow(plot_data) >= 14) {
                  colors <- c(brewer.pal(11, "PuOr"), brewer.pal(nrow(plot_data) - 
                    11, "BrBG"))
                }
            }
            if (color == F) {
                colors <- gray.colors(nrow(plot_data), start = 0, 
                  end = 1)
            }
            p <- ggplot(data = plot_data, aes(x = factor(datum), 
                y = as.numeric(counts), fill = species)) + geom_bar(stat = "identity", 
                position = position_dodge(), color = "black") + 
                theme_bw() + scale_color_grey() + theme(axis.title.x = element_blank()) + 
                theme(axis.line.x = element_blank()) + theme(panel.grid = element_blank()) + 
                theme(panel.border = element_blank()) + theme(axis.line = element_line(colour = "black", 
                linewidth = 0.5)) + ggtitle(paste0(plot_data$projekt, 
                " - Fischarten")) + ylab("Anzahl") + theme(plot.title = element_text(size = 16)) + 
                theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16)) + 
                theme(legend.text = element_text(size = 16)) + 
                theme(legend.title = element_blank()) + theme(legend.position = "right") + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(plot.title = element_text(face = "bold")) + 
                theme(axis.text = element_text(size = 16)) + 
                scale_y_continuous(expand = c(0, 0), limits = c(0, 
                  max(as.numeric(plot_data$counts)) + 2)) + scale_fill_manual(values = colors)
            if (combine == F) {
                pdf(paste0(output, "/Fischarten_ID_", unique(plot_data$befischungs_id), 
                  "_", unique(plot_data$projekt), "_", unique(plot_data$datum), 
                  ".pdf"))
            }
            else {
                pdf(paste0(output, "/Fischarten_", unique(plot_data$projekt), 
                  ".pdf"))
            }
            par(mar = c(5.1, 4.5, 4, 1.5))
            print(p)
            par(mar = c(7, 4.5, 4, 1.5))
            help(par)
            barplot(as.numeric(plot_data$counts), col = colors, 
                names.arg = plot_data$species, main = paste0(unique(plot_data$projekt), 
                  " - ", unique(plot_data$datum)), ylab = "Anzahl", 
                las = 2)
            dev.off()
        }
    }
    for (i in 1:length(befischung)) {
        plot_data <- bio[which(bio$Befischung_ID == befischung[i]), 
            ]
        plot_data <- plot_data[!is.na(plot_data$Laenge_mm), ]
        if (length(plot_data$Befischung_ID) > 0) {
            index <- befischung[i]
            if (combine == T) {
                index = 1
            }
            project_data <- as.data.frame(results[[as.character(index)]])
            project_data$projekt <- gsub("/", "-", project_data$projekt)
            project_data$projekt <- gsub(" ", "_", project_data$projekt)
            if (unique(is.na(project_data$datum))) {
                project_data$datum <- "Kein_Datum"
            }
            species_list <- unique(plot_data$Fischart)
            pdf(paste0(output, "/Laengenverteilung_ID_", unique(plot_data$Befischung_ID), 
                "_", unique(project_data$projekt), "_", unique(project_data$datum), 
                ".pdf"))
            for (j in 1:length(species_list)) {
                single_species_data <- plot_data[which(plot_data$Fischart == 
                  species_list[j]), ]
                hist(single_species_data$Laenge_mm, breaks = seq(0, 
                  (max(na.omit(single_species_data$Laenge_mm)) + 
                    10), 10), main = paste0(unique(single_species_data$Fischart), 
                  " - Laengenverteilung"), ylab = "Anzahl", xlab = "Laengenkategorie [mm]")
            }
            dev.off()
        }
    }
    for (i in 1:length(befischung)) {
        plot_data <- bio[which(bio$Befischung_ID == befischung[i]), 
            ]
        plot_data <- plot_data[!is.na(plot_data$Laenge_mm), ]
        gewicht_1 <- na.omit(plot_data$Gewicht_g)
        if (length(plot_data$Befischung_ID) > 0) {
            index <- befischung[i]
            if (combine == T) {
                index = 1
            }
            project_data <- as.data.frame(results[[as.character(index)]])
            project_data$projekt <- gsub("/", "-", project_data$projekt)
            project_data$projekt <- gsub(" ", "_", project_data$projekt)
            if (unique(is.na(project_data$datum))) {
                project_data$datum <- "Kein_Datum"
            }
            if (length(gewicht_1) > 0) {
                pdf(paste0(output, "/Gewicht_Laengenverhaeltnis_ID_", 
                  unique(plot_data$Befischung_ID), "_", unique(project_data$projekt), 
                  "_", unique(project_data$datum), ".pdf"))
                species_list <- unique(plot_data$Fischart)
                for (j in 1:length(species_list)) {
                  single_species_data <- plot_data[which(plot_data$Fischart == 
                    species_list[j]), ]
                  gewicht_2 <- na.omit(single_species_data$Gewicht_g)
                  if (length(gewicht_2) > 0) {
                    plot(single_species_data$Laenge_mm, single_species_data$Gewicht_g, 
                      main = paste(species_list[j]), pch = 19, 
                      col = "darkblue", xlab = "Laenge [mm]", 
                      ylab = "Gewicht [g]")
                    if (fish_id == T) {
                      text(x = single_species_data$Laenge_mm, 
                        y = single_species_data$Gewicht_g, labels = single_species_data$Fisch_ID, 
                        cex = 0.5, pos = 1)
                    }
                  }
                }
                dev.off()
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

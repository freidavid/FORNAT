## FORNAT R-Code

R code written for FORNAT AG to evaluate the fish data base.

#### How to install using the package "devtools" by [Wickham, Hester & Bryan (2022)](https://devtools.r-lib.org/):

```install.packages("devtools")
library(devtools)
install_github("freidavid/FORNAT")
```

#### Load package and analyze data base:
In this case, the command would analyze "Befischung" with ID 937 and include only the first "Durchgang" (by default, all available "Durchgänge" are included).

```library(RFORNAT)
DatenbankAuswertung(befischung=937, durchgang=1 ,input=database.xlsx)
```
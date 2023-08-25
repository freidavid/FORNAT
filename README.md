## FORNAT R-Code

R code written for FORNAT AG to evaluate the fish data base.

#### How to install:

```library(devtools)
install_github("freidavid/FORNAT")
```

#### Load package and analyze data base (in this case "Befischung" with ID 937 and only the first "Durchgang"):

```library(RFORNAT)
DatenbankAuswertung(befischung=937, durchgang=1 ,input=/path/to/database.xlsx, output=/path/to/store/output)
```
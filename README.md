## FORNAT R-Code


R code written for FORNAT AG to evaluate the fish data base.



#### How to install using the package "devtools" by [Wickham, Hester & Bryan (2022)](https://devtools.r-lib.org/):

```
install.packages("devtools")
library(devtools)
install_github("freidavid/FORNAT")
```

#### Load package 
```
library(RFORNAT)
```

#### Analyze data base:
In this case, the command would analyze "Befischung" with ID 937 and include only the first "Durchgang" (by default, all available "Durchgänge" are included).

```
DatenbankAuswertung(befischung=937, durchgang=1 ,input=database.xlsx)
```

#### Load data from an excel file (.xlsx) into an existing geopackage (.gpkg):
The function takes the data from the excel file and adds it to the geopackage. The data needs to have the same columns as the table of the geopackage to which it is added.
The function has the following parameters:
* geopackage: The geopackage file. If it is not in the R working directory, it needs the complete path to the file.
* data: The excel file with the data to add. If it is not in the R working directory, it needs the complete path to the file.
* layer: The layer of the geopackage to which the data should be added.
* crs: The coordinate system to generate the point geometries. Defaults to 2056.

```
add_to_geopackage(geopackage="example.gpkg", data="data.xlsx", layer="layerxyz")
```

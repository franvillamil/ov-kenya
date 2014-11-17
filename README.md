Replication files for **Political strongholds and the severity of organized violence during the 2008 Kenyan crisis**, by Francisco Villamil.

Abstract:

>   In this article I propose a new mechanism as part of the explanations for the violence dynamics during the 2008 Kenyan crisis. The basic working assumption is that the logic of organized violence is different from the violence in riots and protests and that the factors determining the incidence of that type of violence cannot account for its severity. Using data from the Armed Conflict & Location Data (ACLED) project, I show that the severity of violence was positively related to the degree of ethnic heterogeneity, but only in those areas where one of the parties had mass support. Results not only point to one of the multiple mechanisms explaining violence in Kenya, but also shed light on the logic of the local dynamics of communal violence and highlight the need for a complex disaggregation of violence measures according to the different actors involved in the process.

### Replication files

This repository contains all the required files to replicate the analysis and figures included in the paper. It can be easily downloaded by clicking on the 'Download ZIP' button at the right. Alternatively, it can be cloned using the command line.

*Note:* The only necessary action if you download the ZIP file is to decompress the ```constituency.zip``` file. This folder has to be called 'constituency', and contains the shapefiles for the spatial overlay and the creation of the plots. The file was originally downloaded from ArcGis.com (http://www.arcgis.com/home/item.html?id=12a8bd218a944078b79f1f43e0a00786), but I had to include it here since I could not download it using R code.

The included R code files are (note that the running order is crucial):

```acled.R```  *(optional)* shows the creation of the dependent variable from the ACLED data set and creates the first 2 figures.

```analysis.R```  *(1st)* load the data and makes the main analysis presented in the text.

```cv.R``` *(2nd)* cross-validates the negative binomial models.

```plots.R``` *(3rd)* creates and saves all the figures related to the analysis (figures 3-6).

```appendix.R``` *(optional)* carries out the analysis and figures included in the appendix

### Data sources

- Fatalities: ACLED project (www.acleddata.com)
- Elections: National Electoral Commission
- Ethnics: 1989 Kenyan Census
- Poverty: Kenyan Integrated Household Budget Survey 2005/06 (variables at constituency level created in Kenya National Bureau of Statistics, 2008: Constituency report on well-being).

# RA Work on Housing Ownership 
This repo contains code and data used to analyze data on housing ownership and population factors correlated with housing ownership rates from the ACS. This was done as part of some RA work for Professor Anthony Zhang's research on how banking channels affect house ownership. 

## Exploratory Analysis
* The exploratory work done to explore the relationship between various variables and housing ownership can be found in `Scripts/exploratory.R`
    * I used two sources of data - ACS data from IPUMS Zillow county-level data on house prices
    * The ACS specification can be found in `Datasets/Imported/ACS/usa_00011.cbk`
    * Due to size limitations, the corresponding `Datasets/Imported/ACS/usa_00011.dat` file could not be uploaded to github but `Scripts/exploratory.R` downloads it into the folder
* In addition, while exploring the impact of various factors on housing ownership I also produced visuals for the results
    * Figures and tables of the output can be found in the `plots` and `tables` folders
    * The complete dataset with all the data used for creating the `plots` and `tables` is `houses_dataset.csv`
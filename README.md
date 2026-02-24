# On Climatic Niche Evolution of Mexican <i>Calochortus</i> (Liliaceae): Biogeographic History, Ecological Drivers of Speciation, and Floral Trait Evolution

This repository contains the data and scripts used for the analysis presented in: (insert manuscript citation)


## Installation & Requirements

Below is the software versions and libraries needed to run the scripts.

* **R version:** latest version
*    **Key Packages:** `ape`, `sf`, `hOUwie`, `RRgeo`, `dismo`
* **External Software:** MaxEnt

## Directory Structure

| Folder | Description |
| --- | --- |
| `00_Calochortus_Matrix` | <i>Calochortus</i> morphological character matrix. |
| `01_Species_Distribution_Phylogeny` | R script for coloring the branches of the phylogeny according to their geographic distribution. |
| `02_MX_Clade_AOO` | R script for estimating the Area of Origin (AOO) for the Mexican Clade of <i>Calochortus</i>. |
| `03_Driver_of_Speciations` | R scripts of the statistical tests for identifying drivers of speciation. |
| `04_RRPhylogeography` | R script using RRPhlogeography to describe the patterns of dispersal from the area of origin of species pairs. |
| `05_hOUwie` | Hidden Markov models for Ornstein-Uhlenbeck to jointly model the evolution of floral traits and climatic niche (using the `hOUwie` package). |

## Data Sources

Briefly describe where the raw data came from (e.g., GBIF, GenBank, or private field collections).

* **Occurrences:** [Link to GBIF download](https://doi.org/10.15468/dl.uhm6ek)
* **Environmental Data:** [Link to WorldClim files](https://www.worldclim.org/data/worldclim21.html)

## Contact
If you have an questions regarding the scripts, feel free to reach out to me at the email below. 
**Julianna Harden** - jjh374@cornell.edu

Connect with me on [LinkedIn](https://www.linkedin.com/in/juliharden)

**Happy Coding!**

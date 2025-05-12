# SG_income_inequality

## code
R and Python scripts for: Ye, Y., Walasek, L. & Brown, G.(2025). Social capital reduces, but inequality increases, status anxiety: Evidence from Foot Traffic to 24,000 Clothing Stores in the US.<br>

This repository includes four main scripts used in the analysis:<br>

01_brand_ratings.R<br>
Processes raw survey data on brand perceptions to generate a status rating for each clothing brand in the SafeGraph data.<br>
02_SG_visit_data.ipynb<br>
Cleans the raw foot traffic data from SafeGraph and computes county-level visit rates for four brand tiers, as well as brand accessibility measures.<br>
03_other_data.R<br>
Prepares additional public or previously compiled datasets used in the study, including U.S. Census data, Ortega parameters and the social capital index, ect.<br>
04_models_and_plots.R<br>
Merges all data sources and performs the full analysis, including model fitting and the generation of all visualizations presented in the paper.<br>


## Data
The file survey_raw_data.csv contains the raw data from participant ratings on brand status. Identifiable and sensitive information, such as response IDs, has been omitted.<br>

Third party data sources:<br>
1. Raw SafeGraph data can be accessed from https://www.deweydata.io/.<br>
2. Ortega parameters data can be accessed at https://github.com/kristinblesch/inequality/tree/main/Part1_LorenzCurves_USData/ortega_parameters.<br>
3. Social capital data. The JEC measure was obtained from the Joint Economic Committee’s report (https://www.jec.senate.gov/public/index.cfm/republicans/2018/4/the-geography-of-social-capital-in-america#toc-004-backlink). Chetty’s measures, derived from Facebook network data (Chetty et al., 2022), are available on the Social Capital Database (https://www.socialcapital.org/). Information on the Penn State index can be found at https://nercrd.psu.edu/data-resources/county-level-measure-of-social-capital/.


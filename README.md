# Introduction
We share our code for the visualization of counterfactuals scenarios of deaths in Puerto Rico following Hurricane Maria.

# Use
* Feel free to create a new branch for further incorporation and analysis.

# Data
The data includes the information required to calculate the historical ranges of variation from deaths occuring between September and December 2000-2017.

## Codebook
* Year = Year of Death Count
* census_pop = Population Estimates by the U.S. Census Bureau (not used)
* total_deaths = Total deaths by year
* sep_dec_deaths = Deaths during the September - December by year 

# Counterfactuals
We obtain the estimates and 95% confidence intervals around them from five peer-reviewed articles published between 2017 and April 2019. We substract these numbers from the death count for 2017. We then plot these **"what if"** over the historical distribution of deaths to determine whether the counterfactual falls within the historical distribution.

# Santos-Lozada and Howard 95% C.I. for the Estimate
Due to constant communications presented by another researchers we are sharing the document titled **Derivation of our calculations for intervals around excess death estimates** which we satifies the multiple requests we have recieved to justify the narrow ranges around our central estimate of 1139 excess deaths. 

The file can be accessed here: [Explainer](Santos_Howard_95RangesforEstimate.pdf).

# Correspondence
For any issues with the functionality of these scripts please create an [issue](https://github.com/alexisrsantos/counterfactuals_pr/issues). 

## License
The data collected and presented is licensed under the [Creative Commons Attribution 3.0 license](https://creativecommons.org/licenses/by/3.0/us/), and the underlying code used to format, analyze and display that content is licensed under the [MIT license](https://opensource.org/licenses/MIT).

## Metadata
This repository is maintained by [Dr. Alexis R. Santos](https://scholar.google.com/citations?user=oPZ-RDgAAAAJ&hl=en) and reviewed by [Dr. Jeffrey T. Howard](https://scholar.google.com/citations?user=l0A2z2YAAAAJ&hl=en). 

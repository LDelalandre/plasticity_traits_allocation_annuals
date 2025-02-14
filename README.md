### Plant response to nutrients differs among traits and depends on species’ nutrient requirements

This repository contains the code used for the article "Plant response to nutrients differs among traits and depends on species’ nutrient requirements", published in Annals of Botany in 2025 (doi: ).

The data are stored on the CNRS/InEE platform [InDoRES](), under the DOI:

You can clone the repository with git by pasting in your terminal:

	git clone https://github.com/LDelalandre/plasticity_traits_allocation_annuals
    
or 
just download the repository:
[plasticity_traits_allocation_annuals](git clone https://github.com/LDelalandre/plasticity_traits_allocation_annuals/archive/master.zip).

If you have [Rstudio](https://www.rstudio.com/) installed on your computer, you can then open `plasticity_traits_allocation_annuals.Rproj` with Rstudio.

You can contact me at <leo.delalandre@protonmail.com>

# Structure of the scripts
- import_data.R
- boxplot_in_situ.R: within-species trait variation in situ (boxplots and tests)
- origin_ferti_effects.R: mixed model to test the effects of origin and fertilization on trait values in the experiment
- boxplot_plasticity.R: boxplot of plasticity in the experiment, for the 3 traits with high proportion of variance explained by fertilization
- plots_allometry.R: allometry of above- and belowground exchange surfaces in biplots (surface against plant mass)
- network_plasticity.r: compare plasticity on different groups of traits, and see if some species are more plastic (includes the model with fixed effects of origin and ferti)

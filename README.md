# Algal-Dependency in Sub-Tropical, Arid Streams

Methods:
Tracing Algal prevalence in three (Semi-Arid, Transitional, and Sub-Humid) freshwater communities to predict effects of aridity on food webs. Calculations create outputs for the folloiwing: community composition, diversity, and abundance, Bayesian stable isotope mixing models, bootstrapped estimates of deuterium and nitrogen range, isotopic trophic levels. Outputs are used for visualization as figures and tables. markdown documents compile visualizations as figures, tables, and supplemental matierials documents. These analyses and accompanying prose are currently under peer review for scientific publishing.

Results:
Our goal was to quantify patterns in autochthonous production and consumption within stream communities along a natural precipitation gradient to better understand how streams might respond to aridification. In the Sub-Humid stream, algal resources are scarce and exclosures implicate a diverse assemblage of fish (dominated by centrarchids) positively impacts algae by regulating invertebrate herbivory. In contrast, algal resources are abundant at the Semi-Arid site where a less diverse fish assemblage (dominated by poeciliids) negatively effect algal resources. Exclosure effects are reversed at the Transition and Semi-Arid sites, suggesting the primary trophic effect of fish changes from insect predation to herbivory within streams under drier climate. $\delta^2$H, $\delta^{13}$C, and $\delta^{15}$N corroborate the dietary shift towards herbivory at drier sites which exhibit greater autochthonous assimilation and resource breadth as well as lower isotopic trophic level and food-chain length. Many invertebrate isotopic metric comparisons are statistically insignificant, lending little support for bottom-up cascades. However, patterns in Invertebrate niche dimensions oppose those of fish, implying top-down predatory controls at the Sub-Humid site and competitive overlap at the Semi-Arid and Transition sites. Based on these results, the consequences of aridification include increases in autochthonous production, increases in fish herbivory, and increases in invertebrate dietary overlap.

User Guide:
1. unzip Rdat
2. tweak Rcal/ scripts (require Rdat source files)
3. run Rupd/update_cal.R (generates calculations in Rout/)
4. tweak Rfig/ scripts (requires Rout files)
5. run Rupd/update_Rfig.R (generates visualizations in Rfig/)
6. tweak document formatting, figures, captions in Rmar/
7. run Rmar to generate final documents (MS word and Pdf files)



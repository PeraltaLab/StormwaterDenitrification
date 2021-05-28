# Stormwater Denitrification

This repository contains open-source code, data, & text files for the Stormwater Denitrification project (Town Creek, Greenville, North Carolina)

For information regarding the project, please visit: (update when manuscript is published)

## We address the following questions

* **Aim 1.)**: Examine the relationship between bacterial community composition, phylogenetic diversity, and denitrification potential along an urban stream reach during base and stormflow.

* **Aim 2.)**: Measure the phylogenetic diversity-function relationship in this urban stream ecosystem.

* **Aim 3.)**: Identify bacterial associations according to sediment or water sample type.

### Repo Contents

* **analyses:**: An R Markdown file that includes R script written by Mario Muscarella and Ariane Peralta containing functions used in analysis of soil and microbial sequence data.

* **bin:** 
	* *MothurTools.R*: An R script written by Mario Muscarella (Indiana University, now at University of Illinois) containing functions used in the analysis of community sequence data.

* **data:**: Files associated with soil and microbial data sets. 

* **figures:**: Figures (of plant community composition) generated according to R script located in R Markdown file.

* **mothur:**
  1. DatabaseDownload.sh - Downloads needed reference databases for mothur analysis. Also trims the alignment reference to improve performance.
  2. TC.Bacteria_A.Batch - First part of mothur analysis, includes alignment and chimera removal
  3. TC.Bacteria_B.Batch - Second part of mothur analysis, includes de novo OTU picking
  4. TC.files - Files file which assigns raw input into sample contigs

## Contributors

[Dr. Ariane Peralta](https://www.peraltalab.com): Associate Professor, Department of Biology, East Carolina University. Principal Investigator of the [Peralta Lab](http://www.peraltalab.com)

[Dr. Mario Muscarella](http://mmuscarella.github.io/): Assistant Professor, Institute or Arctic Biology, University of Alaska Fairbanks. Principal Investigator of the Muscarella Lab.

[Dr. Eban Z. Bean](https://abe.ufl.edu/people/bean-eban/): Assistant Professor, Department of Agricultural and Biological Engineering, University of Florida.

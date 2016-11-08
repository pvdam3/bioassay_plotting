# R plotting of bioassay data
This R script will take in a set of bioassay measurements in CSV format.

##Main directory
provide a main directory which contains:

1. /fw/ (csv files containing fresh weight measurements)
2. /di/ (csv files containing disease index measurements from 0-4)
3. optional: labels.csv which can overwrite the labels from the previously specified csv files.

The script will plot a 3x2 grid containing a row of freshweight average measurements and a row of stacked bargraphs containing the disease index measurements. The FW values were statistically evaluated for significance through a standard ANOVA and Tukey Honest Significant Difference (TukeyHSD) test. Labels (for example a, b, ab, c) were generated and plotted above the bar graph.

##Dependencies
Due to many workarounds of formatting the data like we want for plotting, we need the following R libraries installed and imported:

- library(ggplot2)
- library(reshape2)
- library(multcompView)
- library(plyr)
- library(tools)
- library(gridExtra)
- library(cowplot)
- library(gdata)
- library(dplyr)

##Sample output
A sample output can be found here: [sample_output.png](sample_output.png)
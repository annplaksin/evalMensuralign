# evalMensuralign
All the data needed for the evaluation of alignments of Mensural music.
This repo contains all the results and scripts for the surrogate data analysis performed in:
Plaksin, Anna Viktoria Katrin: "Modelle zur computergestützten Analyse von Überlieferungen 
der Mensuralmusik. Emprische Textforschung im Kontext phylogenetischer Verfahren.", Münster, 2021 
(Schriften zur Musikwissenschaft aus Münster 27), online: [urn:nbn:de:hbz:6-59029717067](http://nbn-resolving.de/urn:nbn:de:hbz:6-59029717067).

To analyses has been performed:
1. Test of normality of surrogate alignment distances: raw data in `normalityInput`. 
2. Analysis of variance of different levels of estimated similarity: raw data in `mainInput`.

The raw input data has been produced with the [Mensuraligner](https://github.com/annplaksin/Mensuralinger).

## Normality test

* checkNormality.R: Loops through input and performs Shapiro-Wilk tests and produces Q-Q plots.
* normalitySequel.R: Checks influence of parameter sets on normality, done with Chi² tests.

## Analysis of Variance

* collectMainData.R: Collects data, calculates Z scores and performs pre-checks.
* mainSignificanceAgainstRandom.R: Perform Wilcox test.
* mainAnalyzeZs.R: Further Kruskal-Wallis and Jonkheere-Terpsta test on collected Z scores.
* Kruskal_reverseGroups.R: Analysis of variance on reversed groups.
* mainShinyPlots.R: Create shiny result plots.
* reducedPlots.r: Shiny plots with reduced number of parameter sets.
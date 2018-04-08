# kintrade-model

This repository contains files related to a NetLogo model that was developed to examine the relationships between demography, kinship networks, and exchange in small-scale societies. The model was inspired by archaeological data on pottery trade in the North American Southwest. I have discussed the model in presentations at the 2017 and 2018 CAA meetings, as well as the 2017 SAA meetings.

The repository currently contains four files: 

1) kintrade 1.1.nlogo: the model itself. This version of the model is slightly updated from what I used for the 2017 presentations, but the changes are mostly cleaning up the code, adding comments, etc. It is exactly what I used for the 2018 CAA presentation; 

2) Allison 2018: a revised version of the 2017 CAA paper (also incorporating parts of the 2017 SAA presentation), which describes the model and analyses some results from the model. This paper is unpublished, but will soon (I hope) be published in the CAA 2017 proceedings. 

3) Netlogo Behavior Space data.csv: a large csv file containing the results of 21,600 runs of the simulation with various parameter settings. Most of the results reported in the Allison 2018 paper are the result of analyzing the data in this file, although the file includes many variables (and is likely to be confusing). Analysis focused on only a few of the variables and was done using Minitab statistical software, so it is difficult to reconstruct all the steps in the analysis. The paper describes some of the analysis, and I will attempt to add more information later about exactly which variables I actually looked at and how I did it.

4) kintrade network analysis.R: an R script that uses two of the files produced by the output procedure of the model (agentlist.csv and linklist.csv). The R script reads those files, and creates another csv file of it's own (AgentMetricsforx.csv, where x is the path to the subfolder containing the agentlist and linklist files). For this to work, the files produced by each run of the model need to be in a subfolder below the folder containing the R script. The AgentMetricsfor... file reports (for each agent) the current village of the agent, the number of pots owned, the number of pots once owned but traded away, the degree centrality, the shortest path length to any agent in one of the producer villages, and the betweenness centrality. The script also includes code that will draw the total kin network created by the simulation run, as well as the individual kin networks for the residents of Village 8 with the shortest paths back to producers (with the settings I have typically used, Village 8 is the most distant village from the producers). It also can draw the network for any specified by adding the agent number in the appropriate place (comments in the script should help with that).

The R script was important to the analyses that I discussed in my 2018 CAA presentation (no sharable text exists yet).

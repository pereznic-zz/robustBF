------- REPOSITORY ROBUST BAyeS FACTORS IN IHT FOR LINEAR REGRESSION COEFFICIENTS -------

Thanks for your interest in the repository that supports my paper on "Robust Bayes Factor for the 
Evaluation of Informative Hypotheses with respect to Linear Regression Coefficients". The Archive is 
organized in different sections corresponding to the workflow presented in the paper. The Archive is
composed by the following Files/Folders

1) "Bain_0.1.0.zip" -> 	Binary files to install as libarry packages in R for the computation of Bayes
			factors using the "Approximate Adjusted Fractional Bayes Factor".

2) "Simulations"    ->	Folder with all the files needed to perform the simulations done in the paper.
			Details on parts of the code and functions are documented in the code itself.
			To succesfully run the simulations the following has to be done:
				a) Open Rstudio and open project "Simulations.Rproj"
				b) Open every Scenario (1-30) and execute them - note that this is 
				   computationally intensive and depending on the scenario and 
				   computer specifications it might take some time
				c) After running all scenarios, result files are written in the same 
			 	   folder (6 files for each scenario).
				d) Open the file "ResultsAnalyzer.R" and run the analysis. Graphs
				   and results are stored in the same folder.
				e) NOTE: the file "DataRobustFunctions.R" contains the necessary code
				   for all the robust estimation and data generation details. The file	
				   does not have to be opened but the interested reader can refer to it 
				   for details in the algorithms and the implementation.

3) "Applied Example"->  This folder contains the original data retrieved from the OSF foundation as well
			as the data processing script that they used. The file "AppliedExample.R" 
			contains the necesarry code to perform the analyisis that were made in the
			paper.

4) "Plots"          ->  Contains three different codes for the generation of the plots regarding 
			Heteroscedastic residuals, Outlier outline and the Residual Function comparaison.
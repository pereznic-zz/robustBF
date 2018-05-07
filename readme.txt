Project duration: September 2017 - May 2018
Author: Nicolás Pérez
E-mail: n.perezescobar@uu.nl
Supervisor: Prof. Dr. Herbert Hoijtink
Organization: Utrecht University

Project title: A Robust Bayes Factor for the Evaluation of Informative Hypotheses with respect to 
Linear Regression Coefficients

-----------------------------------------------------------------------------------------------------------

PROJECT DESCRIPTION: 	In this paper we introduce a robust Bayes factor for informative hypotheses 
			testing (IHT) in the context of linear regression models. We first introduce 
			the concept of robust estimates for the linear regression coefficients, and 
			subsequently relate it to the Bayes factor as a valuable tool for IHT. The paper 
			focuses on comparing the performance of two robust Bayes factors with a non-robust 
			Bayes factor based on common ordinary least squares estimates. Comparisons are 
			made using a simulation study where outlier presence and heteroscedasticity 	
			conditions are manipulated. With the simulation results we show the advantage of 
			robust Bayes factors over non-robust Bayes factors and propose to investigate 
			further in the definition of criteria to select between the two robust Bayes 
			factors.

-----------------------------------------------------------------------------------------------------------

REPOSITORY DESCRIPTION

Thanks for your interest in the repository that supports my paper on "Robust Bayes Factor for the 
Evaluation of Informative Hypotheses with respect to Linear Regression Coefficients". The Archive is 
organized in different sections corresponding to the workflow presented in the paper. The Archive is
composed by the following Files/Folders

1) "Bain_0.1.0.zip" -> 	Binary files to install as library packages in R for the computation of Bayes
			factors using the "Approximate Adjusted Fractional Bayes Factor" retrieved from 
			https://informative-hypotheses.sites.uu.nl/software/bain/

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
				f) In the file "SimulationsResults.zip" reader can find result files that  
				   were used in the analysis in this paper. Unzipping the file and locating
				   the files in the same folder as the "ResultAnalyzer.R" would allow
				   the user to run the anayisis without running all scenarios again.

3) "Applied Example"->  This folder contains the original data retrieved from the OSF foundation 
			(https://osf.io/5dx4v/) as well as the data processing script that they used. 
			The file "AppliedExample.R" contains the necesarry code to perform the analyisis 
			that were made in the paper.

4) "Plots"          ->  Contains three different codes for the generation of the plots regarding 
			Heteroscedastic residuals, Outlier outline and the Residual Function comparison.

5) "ThesisNPE.pdf"  ->	Thesis paper in pdf format

-----------------------------------------------------------------------------------------------------------

OPEN DATA DISCLAIMER: 	As mentioned in the paper, public open data was used to elaborate the example.
			Our paper has no relationship with the original study or the collection of the
			data itself. We provide a copy of the data (retrieved on march 13, 2018) and the
			processing script. For further details we ask the reader to contact the authors
			of the original research. Information to contact and about the data repository for
			that study can be found in: https://osf.io/5dx4v/.

-----------------------------------------------------------------------------------------------------------

USED SOFTWARE VERSIONS

			- R Version 3.4.3
			- MASS package Version 7.3-47
			- Bain package Version 0.1.0 (zip file inlcuded)
			- robust package Version 0.4-18
			- dplyr package 0.7.4
			- TexStudio version 2.12.6

-----------------------------------------------------------------------------------------------------------

STORAGE: 		Research archive is available in https://github.com/pereznic/robustBF. An additional
			copy is stored by Utrecht University for a period of 10 years according to the
			data policy from the organization. If needed, reader can contact programme 
			coordinator (Porf. Dr. Irene Klugkist - i.klugkist@uu.nl).
 
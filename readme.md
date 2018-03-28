# Survival of the Fittest: Variable Selection on Agricultural Data from the Galápagos Islands

Variable selection is an important first step when analyzing datasets with a large number of 
potential predictor variables. We apply two techniques, Forward Selec- tion and Elastic Net,
to find the most important variables in a dataset detailing over 200 socioeconomic measurements 
for 755 farms on the Galápagos Islands. Model- ing five different outcome variables, we find 
the data available has the strongest linear relationships with the outcomes productivity and 
land use choices. For each of the outcome variables we present the top five predictor variables
 as well as a full set of coefficients for the optimally predictive model.
 
 ## R files
 * data\_creation.R: merge various data sources at UPA level to generate raw dataset 
 * data\_cleaning.R: perform data cleaning including filling missing values and setting appropriate variable type
 * modeling.function.R: perform all modeling (logistic/linear regression for Elastic Net/Forward Selection)
	 and output results
 * Each outcome variable is modeled in a separate R file that calls the modeling function:
 	1. production\_model.R
 	2. netincome\_model.R
 	3. workers\model.R
 	4. invasive\_model.R
 	5. landuse\_model.R
 * plot.cv.big.R: modified function for Elastic Net CV plots with larger text
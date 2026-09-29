
clear
set more off			   		// if more is on then output will be presented in chunks (showing only what fits on a single screen). If set to off, the you do not need to press more to show all output	

***************************
*  Lecture 1: question D  *
***************************

use PlannedEnv_Spain_India		// reading in the data
browse							//display the data

***********************
*question D2
***********************

codebook country					// this gives the labels for the categories of the country variable, and the frequencies (incl any missing values)	

* Alternatively one can use:
tab country, m                      // the m provides frequencies for missing labels as well 

count if indiv == .	  				// Counts missing values, coded (., .a, .b etc. in stata).
list id country indiv if indiv == . //Lists the id, country of origin, and missing value code of each person with a missing value. 

*to exclude the ID with missing value on country (we do not need it for this exercise and it complicates the use of the by command introduced later
drop if country >=.

***********************
*question D3
***********************

* Histogram with normal distribution

hist indiv if country==1, normal title(spain) name(spain, replace)        //the normal option shows the shape of a normal distribution based on the emperical mean and sd. The title option is to give the graph a title and the name option is to give it a different name than the default "Graph". The latter ensures that the figure is not overwritten but stays available in a tab. The replace ensure that the name india can be reused if the code is run again.
hist indiv if country==2, normal title(india) name(india, replace)     

*If you want to place both graphs next to each other in one image, you first have to save both images and then combine them

hist indiv if country==1, normal title(spain) saving(spain, replace)         
hist indiv if country==2, normal title(india) saving(india, replace)
gr combine spain.gph india.gph

*Skewness and kurtosis

summarize indiv if country==1, detail  // the detail provides detail summaries which include skewness and kurtosis
summarize indiv if country==2, detail

*or alternatively:
by country, sort: summarize indiv, detail

//The by command can be used with several functions and instructs STATA to the
//analysis for the two countries separately. However data need to be sorted 
//on the country variable: the sort option does so. 
//The by command treats missing values as a separate category, and therefore we have
//deleted the person with unknown country of origin earlier.

* which is similar to
bysort country: summarize indiv, detail

sktest indiv if country==1  // provides the p-value associated with the skewness statistic
sktest indiv if country==2  // unfortunately the sktest can not be used together with the by command

*Shapiro-Wilk test
swilk  indiv if country==1
swilk  indiv if country==2
*or alternatively
bysort country: swilk  indiv 

***********************
*question D4
***********************

* Outliers through box-plot

graph box indiv, over(country) marker(1,mlabel(id)) //the "over" specifies that you want a boxplot for both countries separately in one graph (the earlier used hist function does not support this). The marker amd mlabel specification ensure that person are identified by their ID number

* to have STATA list the outliers that are outside the 75th percentile + 4*iqr range, we first have to estimate what the 75th percentile and the Iqr is for both countries and store these in a variables
bysort country, sort: egen p75 = pctile(indiv), p(75) 
bysort country, sort: egen iqrange = iqr(indiv)
*then we sum these two variables
gen extr_indiv = p75 + 4*iqrange
*and ask Stata to list all persons with individualism scores higher than specified in the extr_indiv variable
list id country indiv if indiv > extr_indiv
* you will notice that person with missing individualism score are listed as well. Stata considers missing values to be very high values
* to remedy this we can do something like:
list id country indiv if indiv > extr_indiv & indiv <.
 * I typically drop variables that I no longer need so that I can use same labels later on again if need
drop p75 iqrange extr_indiv
 
* z-scores

egen zindiv_spain = std(indiv) if country==1       // this creates a variable namend zindiv_spain containing z-scores for individualism. The if statement is used to insert missing values for the India sample 
egen zindiv_india = std(indiv) if country==2       

* To get a summary of the two newly created variables and to see the range of z-scores use:

sum zindiv_spain zindiv_india

*the following list commands are used to obtain the ids of participants with Z-scores higher or larger than four

list id country zindiv_spain if abs(zindiv_spain)>=4 & zindiv_spain<.   //abs gives the absolute z-score
list id country zindiv_india if abs(zindiv_india)>=4 & zindiv_india<.   

*to generate a new variable outliers that identifies these outliers with "1" and non outliers with "0" use

gen outliers = 0  //generate a new variable with all zeros as data
replace outliers = 1 if abs(zindiv_spain)>4 & zindiv_spain<. //replace outliers in the Spain sample with 1's
replace outliers = 1 if abs(zindiv_india)>4 & zindiv_india<.

***********************
*question D5
***********************

* rerun normality checks without these id's

sktest indiv if country==1 & id~=1176
sktest indiv if country==2 & id~=2079 & id~=2200
bysort country: swilk indiv if id~=1176 & id~=2079 & id~=2200

*or more conveniently by using our newly created ouliers variable
sktest indiv if country==1 & outliers~=1
sktest indiv if country==2 & outliers~=1
bysort country: swilk  indiv if outliers~=1

***********************
*question D6
***********************

* "ladder of power" to see result of all sorts of transformations on normality

bysort country: ladder indiv if outliers~=1

* generate an new transformed variable using a square root transformation
* first find the lowest individualism score using the summarize function
sum indiv
* STATA temporarily stores all kinds of statistics after a command, so let us display the minimum value on the screen
display r(min) 
* you can use this directly as a constant in other functions. Let us use it to transform the variable using a log transformation as an example:
*gen indiv_log = log(indiv + 1-r(min))
*or a log3 transformation:
*gen indiv_log3 = log(indiv + 1-r(min))/log(3)
* Since the ladder command suggest a square root, let us do that (I found it to work better without making the lowest individualism value 1)
gen indiv_trans = sqrt(indiv)

*let check normality
bysort country: swilk  indiv_trans 


***********************
*question D7
***********************

* boxplot
graph box indiv_trans, over(country) marker(1,mlabel(id)) 

* list extreme outliers 
bysort country, sort: egen p75 = pctile(indiv_trans), p(75) 
bysort country, sort: egen iqrange = iqr(indiv_trans)
gen extr_indiv = p75 + 4*iqrange  
list id country indiv_trans if indiv_trans > extr_indiv & indiv_trans <.
drop p75 iqrange extr_indiv

* z-scores 

egen zindiv_trans_spain = std(indiv_trans) if country==1      
egen zindiv_trans_india = std(indiv_trans) if country==2       
sum zindiv_trans_spain zindiv_trans_india

gen outliers_trans = 0  
replace outliers_trans = 1 if abs(zindiv_trans_spain)>3 & zindiv_trans_spain~=. 
replace outliers_trans = 1 if abs(zindiv_trans_india)>3 & zindiv_trans_india~=.

list id country zindiv_trans_india zindiv_trans_spain if outliers_trans==1

bysort country: swilk  indiv_trans if outliers_trans~=1

***********************
*question D8
***********************

*to see the variances:
bysort country, sort: summarize indiv_trans, detail

*to conduct Levene's test of equality of variances:
robvar indiv_trans, by(country) 

***********************
*question D10
***********************

ttest indiv_trans, by(country)

ttest indiv_trans, by(country) welch //the welch option provides the welch corrected t-test which corrects for any violations of the equality of variance assumption

***********************
*question D11
***********************

*using the Welch corrected

ttest indiv_trans if outliers_trans~=1, by(country) welch 


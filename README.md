# SS_MSE

Contains code for performing a MSE using the Stock Synthesis framework as applied to sandbar shark

*Disclaimer: I did not follow best coding formatting guidelines in these scripts. Apologies *

## Functions included:
 
### BuildParFile

`BuildParFile()` is written specifically for this sandbar shark example. Code is clunky -- written before `SS_readpar()` update to `r4ss`. Note that there is a `BuildParFile()` designated for the Beverton-Holt "BH" stock-recruit relationship operating models, labeled BuildParFile_BH.R, also.   


### EditStarterFile

`EditStarterFile()` is designed to set the random number seed for the data-generating bootstrap process. 


### RunOM_NoHess 

`RunOM_NoHess()` runs the operating model while specifying the `-nohess` option. This is the data-generating bootstrap process. 


### BuildEMDatFile

This script contains the `BuildOM()` and `BuildEM()` functions.These functions take the information obtained from the 1st data-generating bootstrap process and inserts expected historical values in the OM and observed historical values in the EM. These functions are only to build the OM and EM data files in the first time-step. 


### UpdateEMDatFile

This script contains the `UpdateOM()` and `UpdateEM()` functions. These take the new information obtained from the data-generating bootstrap process and inserts expected future values in the OM and observed future data in the EM. These functions are called each time step except the first. 


### RunEM

`RunEM()` runs the estimation model after observation uncertainty has been added via the results of the data-generating process. 


### HCR

`HCR()` function applies the harvest control rule. This function is specifically designed for the sandbar shark MSE example. 


### ImplementHCR

`ImplementHCR()` is the code that takes the results from the HCR, and applies the ACL (quota) to the fisheries. This includes allocating commercial catch among commercial fisheries and estimating future catches that are not designated by the commercial quota (in the sandbar shark example: Mexican + Recreational catches; Menhaden bycatch). Note that this function must be specific to for each example and is not generalizeable. Further note that a limitation of this approach is that catch of all fisheries is assumed be constant within time blocks (i.e., no interim approach to updating commercial quota, and no yearly updated estimate of non-quota catches). 


### MSE_Master 

Designed to pull in all the other function scripts and run the MSE as looped across each time-step. 



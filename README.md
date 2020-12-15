# SEPTR
Simultaneous Estimation of Plasmid Transfer Rates

About: This numerical model is an extended version of Levin et al 1979, and takes into account plasmid bearing costs and includes temperature dependency of plasmid transfer rate constant. 
       The model simultaneously estimates the growth and plasmid transfer rate constants for mating experiments in liquid medium along with plasmid bearing costs and temperature dependency of the plasmid transfer rates.

Folders: 

        In - Contains the input data files, and keys for strain details and different model settings for which we compared the parameter estimates. 
        
        Out - Stores the outputs from the model run
         
        R - Stores the main executable file : do.r , along with the supporting functions designed for the model. 
         
        Rodeo - Stores the models - Proposed and original Levin's model (state variables - vars.txt, processes - pros.txt, functions - funs.txt, Stoichiometric table - stoi.txt, parameter definations- pars.txt). The text files are processed using rodeo package. 
         
Model algorithim: 

        1. Data from the input file are preprocessed. 
                  
        2. Using boundary parameter values from literature, an initial guess value (best fit parameter) is calculated based on latin hypercube sampling in the defined parameter space. 
                  
        3. FME package::modfit() is used for inverse fitting the different models to the preprocessed data. The best fit parameter from step 2 serves as an intitial guess for modfit. 
                  
        4. The fitting of the model is validated by comparing the modelled increase in D, R, T densities with respect to observed increase. 
                  
        5. Advanced Bayesian analysis is carried out using function modMCMC(). 
                  
The main executable file do.r in the R folder loads all the nessessary libraries and functions. The individual functions have inline comments explaining thier functions/working. 
Please feel free to contact the author with your queries and suggestions. 

Author: Sulagna Mishra; 
email: sulagna.mishra@tu-dresden.de

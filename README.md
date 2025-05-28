# Master thesis tracking code
The code and files mentioned here were all used in my master's thesis for tracking photons in different simulation setups


The tracking code was made with Fortran 77, and all files with .f points to tracking code
inp files are the input files where all the cards were determined for a set simulation
Sourcev01.f file is the spectrum fortran file
.txt file is the spectrum text file



### PMMA-phantom
Interactions happening inside the phantom were registered

Input file: 

Tracking code: 


### CT-image/Patient
Interactions happening inside the patient were registered
.vxl file

Input file: 

Tracking code:

Exit points out from patient surface:

### Operator
Exit points at patient surface with subsequent operator hit:



Radiation exposure to operator, to compare with shielded scenarios:

Input file:

Tracking code:

### Shielding of operator
Registering photons that hit the operator with shielding implemented:

Input file:

Tracking code:






## Particle Swarm Optimization (PSO) code in Fortran 95

A Fortran 95 code for Particle Swarm Optimization (PSO). The code is general and can be used with any fitness function. The fitness function is defined in a separate file for convenience. Note that the parameters of the main code must be modified according to the fitness file (the dimensions of the problem, limits of the solution space etc.). The parameters are defined at the start of the PSOclassicG.f95 file.


The data of each run is saved in a time-stamped folder. The folder contains a csv data file and a text logfile.

The included fitness.f95 file contains several well-known benchmark functions for testing the code. Note that the bounds of the solution space must be changed in the main file depending on which benchmark function is used.


## Compiling 
Use gfortran to compile the code:

```
gfortran -c PSOclassicG.f95
gfortran PSOclassicG.f95 -o PSOclassicG.exe
```
To run the program:
```
./PSOclassicG.exe to run 
```

## Sample Output

The program creates time-stamped folders, data files and log files to store the output of each run. 

```
 ======================================================
 Time stamp = 29Jun2018_10.44.48
 ======================================================
 ======================================================
 Algorithm parameters
 -----------------------------------------------------------------------------------                                                             ---------
 Problem dimension =           30
 Dynamic range =   -30.000000000000000      to   30.000000000000000
 Population size =           30
 Maximum allowed iterations =         5000
 Defined value to reach (VTR) =    9.9999999747524271E-007
 ======================================================

 ======================================================
 Performance parameters
 -----------------------------------------------------------------------------------                                                             ---------
 Total CPU time =    1.2500000000000000      seconds
 Overfly counter =           53
 No. of iterations required to reach VTR (NIR) =         3011
 No. of function evaluations (NFE) =        90280
 Global best value achieved after NIR =    1.0250801842914825E-006
 Global best value achieved after max_iter =   -8.2548385904601673E-008

 Global best solution =

   5.5082596704424785E-015
   5.5522369392845231E-015
  -5.5718613290661208E-015
   1.6246755064038873E-015
   8.0482217477294151E-016
  -4.1019462312090092E-015
  -4.4933247148115050E-015
   1.5038166299200255E-015
  -1.1679360362184465E-015
   9.0503278445041426E-015
  -7.8925604075080544E-015
   4.5185619581557949E-015
   2.2594169686377635E-015
  -1.0851562521858067E-015
  -9.2151565282134212E-016
   8.6883377975752917E-016
   2.0715596482704905E-015
   4.0434639980037548E-015
   6.8435164335953364E-016
   8.0064923689510604E-015
   4.1857757093709315E-015
   4.3259582715720220E-016
  -3.1461166552457938E-015
   4.6093380980337098E-016
   4.5872700038734570E-015
   4.8481846838986316E-015
   4.3196119587258173E-015
  -3.7507544585752364E-015
  -2.3136311711851269E-015
  -1.8352272546266788E-016

 ======================================================

```

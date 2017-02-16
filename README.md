# maape

MAAPE.R includes images of figure 1~10, exclude figure2.
A folder named "output data" will be generated and the Fiugres will be saved there.

If you run the file, the pop-up window will comes out.
You need to choose "MAAPE.R" file when you run the file.



From now on, I will explain how to run the file.

There are two ways to use the file.

You can open the MAAPE.R file and just run it.
  or
You can use R CMD BATCH.


1. Both of ways need R.
You can download R from https://cran.r-project.org/bin/windows/base/
link is based on Windows, however other OS also can use R.


2. List of Required packages

Mcomp
fma
fields
akima
plyr
forecast
xtable

If you need packages, you can install in R, by using the command
install.packages("Name_of_the_package", repos = "http://cran.us.r-project.org")

for example,
install.packages("Mcomp", repos = "http://cran.us.r-project.org")

or you can install all packages by using R CMD BATCH.
there is a file "install_package.R"
If you run the file by R CMD BATCH, you can install all of the packages by at once.


3. R CMD BATCH
You can run the file by using R CMD BATCH.
It will allow you to run the file at once.

You can get the result by following methods.

1. From the Desktop, right-click the Computer icon and select Properties. 
   If you don't have a Computer icon on your desktop, click the Start button, right-click the Computer option in the Start menu, and select Properties.
2. Click the Advanced System Settings link in the left column.
3. In the System Properties window, click on the Advanced tab, then click the Environment Variables button near the bottom of that tab.
4. Click the 'New' button on the ‘User variables’.
5. In the field ‘Variable Name:’, type PATH
6. In the field ‘Variable Value:’, You need to write the installation path of the R. 
   In my case, it was 'C:\Program Files\R\R-3.3.2\bin;' Do not omit semicolon after the path.
7. Open the command prompt, and type R CMD BATCH "the path that you have MAAPE.R" and enter
   for example, you can see
   C:\Users\Administrator>
   type 'R CMD BATCH "C:\Users\Administratior\Desktop\R programming\MAAPE\MAAPE.R" and enter.
8. You can see 'output data' folder on your MAAPE.R path, and Figures are in the folder.

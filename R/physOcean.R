c.leroy <-
function(T,S,z)
{
  1492.9 + 3*(T-10) - 0.006*(T-10)^2 - 0.04*(T-18)^2 +
    1.2*(S-35) - 0.01*(T-18)*(S-35) + z/61
}

c.mackenzie <-
function(T,S,z)
{
  1448.96 + 4.591*T - 0.05304*T^2 + 2.374e-4*T^3 +
    1.34*(S-35) + 0.0163*z + 1.675e-7*z^2 -
    0.01025*T*(S-35) - 7.139e-13*T*z^3
}

c.medwin <-
function(T,S,z)
{
  1449.2 + 4.6*T - (0.055*T)^2 + (0.00029*T)^3 +
    (1.34 - 0.01*T)*(S - 35) + 0.016*z
}

alfa.AM <- 
function(f, z, T, S = 35, pH = 8) {
## note z in km as in paper
## start with relaxation frequencies formulae (1) and (2)
  relaxFreq1 <- 0.78*sqrt(S/35)*exp(T/26)
  relaxFreq2 <- 42*exp(T/17)
## set up a function for ratios of sums and products 
## of frequncies and squared frequencies
  fratio <- function(relaxFreq, f) (relaxFreq*f^2)/(f^2 + relaxFreq^2)
## figureing out components of alfa
  alfa1 <- 0.106*fratio(relaxFreq1, f)*exp((pH - 8)/0.56)
  alfa2 <- 0.52*(1 + T/43)*(S/35)*fratio(relaxFreq2, f)*exp(-z/6)
  alfa3 <- 0.00049*(f^2)*exp(-1*(T/27 + z/17))
## return sum of components
  alfa1 + alfa2 + alfa3
}

alfa.FG <- 
function(f, z, T, S = 35, pH = 8) {
## Francois and Garrison formula for acoustic absorbtion coefficient
## based on Appendix 2A in Simmonds and MacClellan's 
## Fisheries Acoustics: Theory and Practice
## calculate parts of formula
## relaxation frequencies and such
## z in meters
  A1 <- (8.86/c.mackenzie(T, S, z))*10^(0.78*pH-5)
  f1 <- 2.8*sqrt(S/35)*10^(4 - 1245/(T + 273))
  P1 <- 1
  A2 <- 21.44*(S/c.mackenzie(T, S, z))*(1 + 0.025*T)
  P2 <- 1 - 1.37e-4*z + 6.2e-9*z^2
  f2 <- (8.17*10^(8 - 1990/(T + 273)))/(1 + 0.0018*(S - 35))
  P3 <- 1 - 3.83e-5*z + 49e-10*z^2
## note only for T < 20 deg C as of now
  A3 <- 4.937e-4 - 2.59e-5*T + 9.11e-7*T^2 - 1.5e-8*T^3
## for temperatures > 20 deg C, to be done
#  A3 <- 3.964e-4 - 1.146-5*T + 1.45-7*T^2 - 6.5e-10*T^3
## set up a function for ratios of sums and products 
## of frequncies and squared frequencies
  fratio <- function(relaxFreq, f) (relaxFreq*f^2)/(f^2 + relaxFreq^2)
## figureing out components of alfa as in App2A, formula (A2.1)
  alfa1 <- A1*P1*fratio(f1, f) 
  alfa2 <- A2*P2*fratio(f2, f) 
  alfa3 <- A3*P3*f^2
## return sum of components
  alfa1 + alfa2 + alfa3
}

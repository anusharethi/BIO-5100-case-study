# START of SCRIPT 

library(ggplot2) 



# Protein Concentrations 

prot <- c(0, 0.025, 0.125, 0.25, 0.5, 1, 1.5, 2)   



# Absorbance from my protein assay 

abs <- c(0, 0.004, 0.028, 0.058, 0.118, 0.215, 0.294, 0.418) 



# Convert into data.frame to plot with ggplot 

data <- as.data.frame(prot) 

data$abs <- abs 



#Calculate the line using the linear model function 

line <- lm(abs~prot) 



#Equation of a line y = mx + c 

#In our case abs = slope * prot + intercept 

# ukn.prot = (abs - intercept)/slope 

int <- summary(line)$coefficients[1] 

slope <- summary(line)$coefficients[2] 



#now calculate some unknown protein concs from absorbances 

#put the unknowns into a vector 

abs.ukns <- c(0.1712, 0.1865, 0.237, 0.2345) 



#rearrange the equation of the line to ukn.prot = (abs - intercept)/slope 

prot.ukns <- (abs.ukns - int)/slope 



# create the object with the graph in it.  

p <- ggplot(data=data,          # specify the data frame with data 
            
            aes(x=prot, y=abs)) +   # specify the x and y for the graph 
  
  geom_point() +          # make a scatter plot 
  
  stat_smooth(method = "lm") +  # add a linear model line 
  
  xlab("Protein concentration (microg/ml)") +   # label x-axis 
  
  ylab("Absorbance (570 nm)") +    # label y-axis 
  
  ggtitle("BCA assay") +  # add a title 
  
  theme_bw() +      # a simple theme 
  
  expand_limits(y=c(0,1)) +    # customise the y-axis 
  
  annotate(geom="text", x=1.5, y= 0.7, label="Abs         Prot",  color="red") 



#put the answers on the graph 

for (i in 1:length(abs.ukns)){ 
  
  p <- p + annotate(geom="text", x = 1.4, y = (0.7 - i/20), label=abs.ukns[i]) 
  
  p <- p + annotate(geom="text", x = 1.6, y = (0.7 - i/20), label=round(prot.ukns[i], 3)) 
  
} 



p # show us the graph... 



# END OF SCRIPT 
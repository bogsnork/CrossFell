
#One Way Anova----

#tell where the data come from
getwd()
datafilename="appendix1.data.txt"
data.ex1=read.table(datafilename,header=T)   #read the data into a table
aov.ex1 = aov(Alertness~Dosage,data=data.ex1)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)
#report the means and the number of subjects/cell
boxplot(Alertness~Dosage,data=data.ex1)
#graphical summary appears in graphics window


#Two Way Anova----

datafilename="appendix2.data.txt"
data.ex2=read.table(datafilename,header=T)   #read the data into a table
data.ex2                                      #show the data
aov.ex2 = aov(Alertness~Gender*Dosage,data=data.ex2)         #do the analysis of variance
summary(aov.ex2)                                    #show the summary table
print(model.tables(aov.ex2,"means"),digits=3)      
#report the means and the number of subjects/cell
boxplot(Alertness~Dosage*Gender,data=data.ex2) 
#graphical summary of means of the 4 cells
attach(data.ex2)
interaction.plot(Dosage,Gender,Alertness)  #another way to graph the means 
detach(data.ex2)

#One Way Repeated Measures ----
#Run the analysis:
datafilename="appendix3.data.txt"
data.ex3=read.table(datafilename,header=T)   #read the data into a table
data.ex3                                      #show the data
aov.ex3 = aov(Recall~Valence+Error(Subject/Valence),data.ex3)
summary(aov.ex3)
print(model.tables(aov.ex3,"means"),digits=3)       
#report the means and the number of subjects/cell
boxplot(Recall~Valence,data=data.ex3)          #graphical output

#Two way repeated measures ----

datafilename="appendix4.data.txt"
data.ex4=read.table(datafilename,header=T)   #read the data into a table
data.ex4                                      #show the data
aov.ex4=aov(Recall~(Task*Valence)+Error(Subject/(Task*Valence)),data.ex4 )

summary(aov.ex4)
print(model.tables(aov.ex4,"means"),digits=3)       
#report the means and the number of subjects/cell
boxplot(Recall~Task*Valence,data=data.ex4) #graphical summary of means of the 6 cells
attach(data.ex4)
interaction.plot(Valence,Task,Recall)    #another way to graph the interaction
detach(data.ex4)

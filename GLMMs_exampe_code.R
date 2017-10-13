###############################################################################################################
  ##############  Example script for analyzing Generalized Linear Mixed Models (GLMMs) in R ################
###############################################################################################################

# Parts of this code were developed with L. J. Knoll and M. Speekenbrink for 

# Fuhrmann, D.*, Knoll, L.J.*, Sakhardande, A., Stamp, F., Speekenbrink, M. & Blakemore, S-J. (2016). 
# A window of opportunity for cognitive training in adolescence. Psychological Science. *Joint first authors.

# This scripts simulates a developmental dataset.
# Suppose we conduct a training study with three different age groups (7-10yoa, 11-14yoa, 15-18yoa).
# Participants are randomly allocated to either training or a control condition.
# The researchers' hypothesize that 11-14yoa will show higher training effects than the other two age groups.
# The dependent variable is a cognitive task with 40 trials. This variable is dichotomous (correct/incorrect).
# They also want to control for IQ in case age groups differ in this respect

###############################################################################################################
### Load the R packages you will need

# Install packages - this only needs to be done once
install.packages("car")
install.packages("lme4")
install.packages("lsmeans")
install.packages("multcomp")
install.packages("Hmisc")
install.packages("Rmisc")
install.packages("doBy")
install.packages("ggplot2")

# Load packages for use
library(lme4)
library(lsmeans)
library(multcomp)
library(Hmisc)
library(doBy)
library(car)
library(Rmisc)
library(ggplot2)


###############################################################################################################
### Simulate a simple dataset
set.seed(1111)

id= rep(1:600, times=40) # create 600 participant ids, repeat them 40 times. We want to create 40 trials per ppt

x1 = sample(rep(7:18, length.out=600)) # assign each participant a random age between 7 and 18
age = rep(x1, times=40)

x2 = sample(rep(1:4, length.out=600)) # assign each participant to one of 4 schools
school = rep(x2, times=40)

x3 = sample(rep(1:2, length.out=600)) # assign each participant to either training or control group
treatment_group = rep(x3, times=40)

x4 = sample(rnorm(n = 600, mean = 100, sd = 10)) # assign each participant an IQ score
iq = rep(x4, times=40)

# Create 3, roughly even-sized categorical age groups
age_group <- as.numeric(cut2(age, g=3))

# Turn categorical variables into factors
id = as.factor(id)
age_group = as.factor(age_group)
school = as.factor(school)
treatment_group = as.factor(treatment_group)

# Give the levels of the categorical factors proper names
levels(age_group) <- c("7-10", "11-14", "15-18")
levels(school) <- c("School1", "School2", "School3", "School4")
levels(treatment_group) <- c("Training", "Control")

# Put all variables into one data frame called "data"
data = data.frame(id, iq, age, age_group, school, treatment_group)

# Create a helper variable called int - the interaction of age_group and treatment group
data$int = interaction(data$age_group, data$treatment_group)

# Create our dependent variable called correct, fill it with 50% 1s and 0s
data$correct = sample(0:1, 24000, replace=T, prob=c(0.5,0.5))

# Give  participants aged 11-10 who were in the training group a higher number of correct trials
data[data$int=="11-14.Training",]$correct = sample(0:1, 3680, replace=T, prob=c(0.45,0.55))

# Delete int - we won't need it any more
data$int = NULL


###############################################################################################################
### Summarize the data

# This step is not neccessary to run the GLMM but can speed up the computation time considerably
data_summary <- summaryBy (correct ~ id + iq + age_group + school + treatment_group, 
                           # summarize the dependent variable for each level of the other variables
                    data = data, 
                    FUN = c(sum, length, mean)) # get the sum of correct - this is the number of correct trials
                                                # get the total number of trials using "length"
                                                # get the mean for plotting


###############################################################################################################
### Plot data

# summarize the data for each age and training group
plot_data <- summarySE(data_summary, measurevar="correct.mean", groupvars=c("age_group","treatment_group"))

# convert the accuracy data to %
plot_data$correct.mean <- plot_data$correct.mean*100
plot_data$se <- plot_data$se*100


# plot using ggplot
ggplot(data=plot_data, aes(x=age_group, y=correct.mean, group=treatment_group)) +
  geom_bar(aes(fill=treatment_group),position = "dodge", stat="identity")+
  geom_errorbar(aes(ymin=correct.mean-se, ymax=correct.mean+se),
                width=.1,                  
                position=position_dodge(.9),stat="identity")+
  labs(x="Age group",y="Accuracy (%)")+
  coord_cartesian(ylim = c(40, 60))+
  theme(axis.ticks.x = element_blank())+
  theme(axis.title.y = element_text(face="bold",size=12, colour="black",vjust = 1.5))+
  theme(axis.text.y  = element_text(size=12))+
  theme(axis.text.x  = element_text(face="bold",size=12, colour="black"))+
  theme(axis.title.x = element_blank())+
  theme(legend.title = element_text(size=12, face="bold"))+
  theme(legend.text = element_text(size=12))+
  scale_fill_brewer(palette="Blues",name="Age group")


###############################################################################################################
### Run the GLMM

# Set the contrast-coding scheme for our categorical fixed effects
# The default is Dummy-coding 
# but it's usually better to use and orthogonal coding scheme like Helmert-coding
# For more information see 
# http://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
contrasts(data_summary$age_group)<-contr.helmert(3) 
# set Helmert contrasts for the three levels of age group
contrasts(data_summary$treatment_group)<-contr.helmert(2) 
# set Helmert contrasts for the two levels of training group

# Z-score iq
# Z-scoring can help interpreting effect because the transformed variable's mean will be 0
# It is particularly important when continuous variables are part of interactions,
# because z-scoring can reduce multicollinearity
data_summary$iq = scale(data_summary$iq, center = TRUE, scale = TRUE)

# Run the GLMM
model = glmer(cbind(correct.sum, correct.length-correct.sum) ~ 
                # We specify dependent variable as the number of correct trials (correct.sum)
                # and the number of incorrect trials (correct.length-correct.sum)
                
                     age_group * treatment_group + 
                # these are our fixed effects of interest and their interaction
                
                     iq +
                # we also want to control for IQ
        
                     (1|school/id),
                # these are our nested random effects. Participant ID is nested within school
              
                    data=data_summary, 
              
                    family = binomial) # use binomial as the dependent variable is dichotomous


###############################################################################################################
### Inspect the results

# Look at results in an ANOVA-style table
Anova(model,type=3)

# Inspect the intercept and the slopes ("Estimate") of our effects
summary(model)

# The contrasts of the main effects can be accessed using lsmeans
lsmeans(model, pairwise ~ age_group)

# Some contrasts of the interaction can be inspected using lsmeans
lsmeans(model, pairwise ~  age_group|treatment_group)

# More complex contrasts can be analysed using custom contrasts
dummydat <- aggregate(iq ~ age_group * treatment_group, data=data_summary, mean) 
# Create a matrix that contains our fixed effects
dummydat$iq <- 0 # Set IQ to 0, This means we are condering effects of interest for average levels of IQ
dummy=model.matrix( ~ age_group * treatment_group + iq, data=dummydat) # set up dummy codes to compare groups

# Now code the contrasts using subtraction
contrasts <- rbind(  
  "Accuracy after training compared to control is different in age group 7-10, than in age group 11-14"=
    ((dummy[dummydat$treatment_group == "Training" & dummydat$age_group =="7-10",]) -
       (dummy[dummydat$treatment_group=="Control"  & dummydat$age_group =="7-10",])) -
    ((dummy[dummydat$treatment_group == "Training" & dummydat$age_group =="11-14",]) -
       (dummy[dummydat$treatment_group=="Control"  & dummydat$age_group =="11-14",])),
  
  "Accuracy after training compared to control is different in age group 7-10, than in age group 15-18"=
    ((dummy[dummydat$treatment_group == "Training" & dummydat$age_group =="7-10",]) -
       (dummy[dummydat$treatment_group=="Control"  & dummydat$age_group =="7-10",])) -
    ((dummy[dummydat$treatment_group == "Training" & dummydat$age_group =="15-18",]) -
       (dummy[dummydat$treatment_group=="Control"  & dummydat$age_group =="15-18",])),
  
  "Accuracy after training compared to control is different in age group 11-14, than in age group 15-18"=
    ((dummy[dummydat$treatment_group == "Training" & dummydat$age_group =="11-14",]) -
       (dummy[dummydat$treatment_group=="Control"  & dummydat$age_group =="11-14",])) -
    ((dummy[dummydat$treatment_group == "Training" & dummydat$age_group =="15-18",]) -
       (dummy[dummydat$treatment_group=="Control"  & dummydat$age_group =="15-18",])))


summary(glht(model, contrasts))

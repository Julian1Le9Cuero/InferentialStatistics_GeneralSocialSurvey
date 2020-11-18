## Part 1: Data

## Part 2: Research question


## Part 3: Exploratory data analysis
# Note: respondent is abbreviated 'R'. Respondent's is abbreviated 'RS'.
load("C:/Users/julia/OneDrive/Escritorio/Inferential_Statistics/CourseProject/gss.Rdata")
library(statsr) # Summary statistics
library(ggplot2) # Data Visualization
library(dplyr) # Data manipulation

# Highest education degree (degree) and Total family income in constant dollars (coninc)
inc_degree <- gss %>% select(c(degree,coninc)) %>% filter(!is.na(degree))

# We can see a positive relationship between both variables.
# Median family income tends to be higher when R has more advanced degrees
inc_degree %>%
    ggplot(aes(x=degree,y=coninc,color=degree)) + geom_boxplot() +
    ggtitle(label="Highest education degree and Total family income (constant dollars)")

# Mean income also tends to increase with degree, however, variability also augments
inc_degree %>% group_by(degree) %>%
    summarise(n=n(),
              mean_income=mean(coninc,na.rm = TRUE),
              median_income=median(coninc,na.rm = TRUE),
              se=sd(coninc,na.rm = TRUE),
              min=min(coninc,na.rm = TRUE),
              max=max(coninc,na.rm = TRUE),
              iqr=IQR(coninc,na.rm = TRUE)) %>%
    arrange(desc(mean_income))

# Now let's check how RS identify their class by degree
degree_class <- gss %>% select(degree,class) %>% filter(!is.na(degree),!is.na(class))

# Although class is subjective, the more education, the less
# people identify themselves in either lower or working class
degree_class %>% ggplot(aes(x=degree,group=class,fill=class)) + geom_bar()

# And overall, there are more in "Middle Class" or "Upper Class" as education increases
degree_class %>% group_by(degree, class) %>% summarise(n=n()) %>%
    filter(class %in% c("Middle Class", "Upper Class")) %>% arrange(desc(n))

# Job find by Highest year of school completed

# Ease of getting a job has an uptrend as schooling years increase,
# "not easy" answers also augmented but it reaches a maximum at
# approximately 12 years of education but then decreases. Which tells us that is relatively
# easier to find a job as people is more educated.
jobfind_educ <- gss %>% select(jobfind,educ) %>%
    filter(!is.na(jobfind) & !is.na(educ)) %>% filter(educ %in% seq(0,20,5))

jobfind_educ %>% ggplot(aes(x=educ,group=jobfind,fill=jobfind)) +
    geom_bar(position = "dodge") + scale_y_log10()

## Part 4: Inference
# Is Mean family income different by degree?
# For this, I considered necessary performing ANOVA in order to compare 3 or more means
# Check conditions
# 1. Independence
# Sample size is less than 10% of the adult household population of the U.S
# The groups (categorical variable degree) are independent between them because
# it's a random and representative sample of the adult population of the U.S

# 2. Normal distribution of the observations
# By using qqnorm and qqline, we should see the points forming a line that's roughly straight
par(mfrow=c(2,3))

qqnorm(inc_degree[inc_degree$degree == "Lt High School","coninc"], main="Lt High School")
qqline(inc_degree[inc_degree$degree == "Lt High School","coninc"])

qqnorm(inc_degree[inc_degree$degree == "High School","coninc"], main="High School")
qqline(inc_degree[inc_degree$degree == "High School","coninc"])

qqnorm(inc_degree[inc_degree$degree == "Junior College","coninc"], main="Junior College")
qqline(inc_degree[inc_degree$degree == "Junior College","coninc"])

qqnorm(inc_degree[inc_degree$degree == "Bachelor","coninc"], main="Bachelor")
qqline(inc_degree[inc_degree$degree == "Bachelor","coninc"])

qqnorm(inc_degree[inc_degree$degree == "Graduate","coninc"], main="Graduate")
qqline(inc_degree[inc_degree$degree == "Graduate","coninc"])

# Hypothesis test for a test of normality
# Null hypothesis (H0): The data is normally distributed. If p> 0.05, normality can be assumed
# Alternative hypothesis (Ha): The data is not normally distributed

# There are more than 50 events per group, therefore Kolmogorov-Smirnov test will be used,
require(nortest)
by(data = inc_degree,INDICES = inc_degree$degree,FUN = function(x){ lillie.test(x$coninc)})

# Hypothesis tests show lack of normality because of the really low p-values,
# as a result, ANOVA cannot be used for this purpose, which was the most suitable method
# to check if means were different. So step 3 must be skipped (Equal variance between groups)

# Conclusions
# Even though ANOVA test could not be used, from the
# EDA we could evidence strong relations between education and
# someone's financial situation. However it may not be possible
# to extend that affirmation to the whole population because inference
# wasn't used.

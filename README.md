# Unit 4 Exercise 2 with Springboard
## Case study using the adult California Health Interview Survey from 2009
### Data available through http://healthpolicy.ucla.edu/

#### Download the .sav file from the website and load into R using the foreign package
library(foreign)
db <- file.choose()
adult_full = read.spss(db, to.data.frame=TRUE)

#### Only need these 10 columns for this exercise
keep_cols <- c("RBMI", "BMI_P", "RACEHPR2", "SRSEX", "SRAGE_P","MARIT2", 
               "AB1", "ASTCUR", "AB51", "POVLL")
adult <- adult[keep_cols]

### Exploring Data
#### Load the ggplot2 package
library(ggplot2)

#### Explore the dataset with summary and str
summary(adult)
str(adult)

#### Use two simple histograms to explore the data across age and BMI
ggplot(adult, aes(x = SRAGE_P)) +
  geom_histogram()

ggplot(adult, aes(x = BMI_P)) +
  geom_histogram()

#### Plot a binned-distribution of age, filling each bar according to the BMI categorization
ggplot(adult, aes(x = SRAGE_P, fill = factor(RBMI))) +
  geom_histogram(binwidth = 1)

### Data Cleaning
#### In the age distribution, it seems everyone above 84 was bucketed.
#### To solve this, remove all observations for which adult$SRAGE_P is smaller than or equal to 84.
adult <- adult[adult$SRAGE_P <= 84, ]

#### Also remove individuals with a BMI below 16 and above or equal to 52
adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]

#### Relabel the race variable to Latino, Asian, African American, White, Other, Alaskan/American Indian, and Pacific Islander
adult$RACEHPR2 <- factor(adult$RACEHPR2,
                         labels = c("Latino", "Asian", "African American", "White", "Alaskan/American Indian", "Other", "Pacific Islander"))

#### Relabel the BMI categories variable to Under-weight, Normal-weight, Over-weight, and Obese
adult$RBMI <- factor(adult$RBMI,
                     labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))

### Multiple Histograms
#### Assign a color scale to be used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

#### Create new theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),
                    legend.position = "none")

#### Create the histogram and add BMI_fill and customizations
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  fix_strips +
  BMI_fill +
  facet_grid(RBMI ~ .) +
  theme_classic()

### Alternative Histograms
#### 1 - Simple count histogram with no facets or themes
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill

#### 2 - Use plot 1 and show density
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1, aes(y = ..density..)) +
  BMI_fill

#### 3 - Faceted version of plot 1
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill +
  facet_grid(RBMI ~ .)

#### 4 - Faceted version of plot 2
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1, aes(y = ..density..)) +
  BMI_fill +
  facet_grid(RBMI ~ .)

#### 5 - plot 2 with position = "fill"
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1, aes(y = ..density..), position = "fill") +
  BMI_fill

#### 6 - plot 5 changed to an average assigned to y
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1, aes(y = ..count../sum(..count..)), position = "fill") +
  BMI_fill

### Calculate the facets manually
#### Create a contingency table of RBMI and SRAGE_P with table()
DF <- table(adult$RBMI, adult$SRAGE_P)

#### Use apply on DF to get frequency of each group
DF_freq <- apply(DF, 2, function(x) x/sum(x))

#### Load reshape2 and use melt on DF to create DF_melted
library(reshape2)
DF_melted <- melt(DF_freq)

#### Change names of DF_melted
names(DF_melted) <- c("FILL", "X", "value")

#### Use DF_melted to make a faceted plot
ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_bar(stat = "identity", position = "stack") +
  BMI_fill +
  facet_grid(FILL ~ .)

### Merimeko/Mosaic Plot
#### The initial contingency table using as.data.frame.matrix
DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))

#### Add some columns to DF for calculation puposes:
##### groupsSum (showing the sums of each row in DF),
DF$groupSum <- rowSums(DF)
##### xmax  (the max value of the sum)
DF$xmax <- cumsum(DF$groupSum)
##### and xmin (the min value of the sum)
DF$xmin <- DF$xmax - DF$groupSum
##### Remove the groupSum column
DF$groupSum <- NULL

#### Assign the row names to variable X
DF$X <- row.names(DF)

#### Melt the dataset
DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")

#### Use dplyr to calculate ymin and ymax and assign to DF_melted
library(dplyr)
DF_melted <- DF_melted %>%
  group_by(X) %>%
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

#### Plot DF_melted using the tufte theme from the ggthemes package
library(ggthemes)
ggplot(DF_melted, aes(ymin = ymin,
                      ymax = ymax,
                      xmin = xmin,
                      xmax = xmax,
                      fill = FILL)) +
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()

### Adding Statistics
#### Perform chi.sq test on RBMI and SRAGE_P and store in results
results <- chisq.test(table(adult$RBMI, adult$SRAGE_P))

#### Melt results$residuals and store as resid
resid <- melt(results$residuals)

#### Change names of resid
names(resid) <- c("FILL", "X", "residual")

#### Merge the two datasets:
DF_all <- merge(DF_melted, resid)

#### Update the last plot command to reference DF_all
ggplot(DF_all, aes(ymin = ymin,
                   ymax = ymax,
                   xmin = xmin,
                   xmax = xmax,
                   fill = residual)) +
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte()

### Adding Text
#### Calculate the midpoint on the x-axis in order to position labels on x axis
DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2

#### Position for labels on y axis
index <- DF_all$xmax == max(DF_all$xmax)
DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2

#### Edit the plot to add in the labels
ggplot(DF_all, aes(ymin = ymin, ymax = ymax, xmin = xmin,
                   xmax = xmax, fill = residual)) +
  geom_rect(col = "white") +
  geom_text(aes(x = xtext,
                label = X),
            y = 1,
            size = 3,
            angle = 90,
            hjust = 1,
            show.legend = FALSE) +
  geom_text(aes(x = max(xmax),
                y = ytext,
                label = FILL),
            size = 3,
            hjust = 1,
            show.legend  = FALSE) +
  scale_fill_gradient2() +
  theme_tufte() +
  theme(legend.position = "bottom")

### Generalizations
#### Generalize all the previous sections into a function called mosaicGG
mosaicGG <- function(data, X, FILL) {
  
  ##### Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  library(dplyr)
  DF_melted <- DF_melted %>%
    group_by(X) %>%
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  ##### Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  ##### Merge data
  DF_all <- merge(DF_melted, resid)
  
  ##### Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  ##### Plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
                          xmax = xmax, fill = residual)) +
    geom_rect(col = "white") +
    geom_text(aes(x = xtext, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}

### Use this function to do plots previously explored:
#### BMI described by age
mosaicGG(adult, "SRAGE_P", "RBMI")

#### Poverty described by age
mosaicGG(adult, "SRAGE_P", "POVLL")

#### mtcars: am described by cyl
mosaicGG(mtcars, "cyl", "am")

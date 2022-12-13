# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
library(tidyverse)
fullset = read.csv('./Data/housing_fullset.csv')

vars <- setdiff(names(iris), "Species")
vars1 <- setdiff(names(fullset %>% select_if(is.numeric)), "Numerical")
catvars <- setdiff(names(fullset %>% select_if(is.character)), "Categorical")
bvars <- setdiff(names(fullset %>% select_if(is.numeric)), "Hist Selection")
cvars <- setdiff(names(fullset %>% select_if(is.character)), "Categorical")


pageWithSidebar(
  headerPanel('Employee Stats Analysis Tool'),
  sidebarPanel(
    selectInput('xcol', 'Employee Statistic X-Axis', vars1),
    selectInput('ycol', 'Employee Statistic Y-Axis', vars1, selected = vars1[[2]]),
    selectInput('zcol', "Categorical Seperation", catvars),
    h3("See the distribution of other statistics!"),
    selectInput('acol', 'Employee Histogram Stat', bvars),
    selectInput('bcol', "Categorical Seperation", cvars),
  ),
  mainPanel(
    plotOutput('plot1'),
    plotOutput('plot2')
  )
)
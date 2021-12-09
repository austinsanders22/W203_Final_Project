## ----- Setup, include=FALSE------------------------------------------------------------------------------------------
knitr::purl("W203_Lab_2_Report.Rmd")
knitr::opts_chunk$set(echo = FALSE,fig.align="center")

MIN_REVENUE = 1
MIN_BUDGET = 1

COLOR_PG13 = "#F8766D"
COLOR_R = "#00CED1"

library(gridExtra)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(hash)
library(lmtest)
library(sandwich)
library(stargazer)
library(hash)
library(caTools)
library(GGally)
library(ggpubr)
library(kableExtra)

data <- read.csv(file = "../data/movies.csv")
cpi <- read.csv(file = "../data/CPI_by_Year.csv")

#data <- read.csv(file = "movies.csv")
#cpi <- read.csv(file = "CPI_by_Year.csv")


## ----Data Cleaning---------------------------------------------------------------------------------------------------
# Retrieve only the needed columns
df_raw = select(data, c('title', 'gross', 'budget', 'score', 'runtime', 'rating', 'year', 'votes'))

# Remove data points with world_revenue under MIN_REVENUE
df_raw <- subset(df_raw, df_raw$gross >= MIN_REVENUE & !is.na(df_raw$gross))

# Remove data points with budget under MIN_BUDGET
df_raw <- subset(df_raw, df_raw$budget >= MIN_BUDGET & !is.na(df_raw$budget))

# Keep PG-13 and R data points
df_raw <- subset(df_raw, df_raw$rating == "PG-13" | df_raw$rating == "R")

# Remove Duplicate Movie Titles
# Hash object for title : budget
h <- hash() 

# Clean dataframe
df = data.frame()

for(i in 1:nrow(df_raw)) {       # for-loop over rows
  title_key = df_raw[i,'title']
  
  if (TRUE == all(has.key( title_key, h ))) {
    # Title is already recorded

    # Search for existing row in clean dataframe with the same title
    for (k in 1:nrow(df)) {
      
      if (title_key == df[k,'title']) {
        # Replace row if the budget of the new value is higher than that of the
        # budget of the recorded title
        if (df_raw[i, 'budget'] > df[k, 'budget']) {
          
          # Delete found row in cleaned dataframe
          df = df[-c(k),]
          
          # Bind raw dataframe row to clean dataframe
          df <- rbind(df, df_raw[i,])
          
          # Revise title_key and budget to hash
          h[[title_key]] = df_raw[i,'budget']
        } else if (df_raw[i, 'budget'] > df[k, 'budget']
                   & df_raw[i, 'gross'] > df[k, 'gross']) {
          # Delete found row in cleaned dataframe
          df = df[-c(k),]
          
          # Bind raw dataframe row to clean dataframe
          df <- rbind(df, df_raw[i,])
          
          # Revise title_key and budget to hash
          h[[title_key]] = df_raw[i,'budget']
        }
        break
      }
      
    }
    
  } else {
    # Add title_key and budget to hash
    h[[title_key]] = df_raw[i,'budget']
    
    # Bind raw dataframe row to clean dataframe
    df <- rbind(df, df_raw[i,])
  }
}

# Apply Indicator variable for PG-13 (R is the base case)
df$PG13 = factor(ifelse(df$rating == "PG-13" , 1, 0))

# Perform CPI adjustment to both gross and budget variables.
df <- merge(x = df, y = cpi, by= 'year')
df <- transform(df, adj_gross = gross * adjust_to_2020_dollars, adj_budget = budget * adjust_to_2020_dollars)


## ----- EDA: Check main financial variables---------------------------------------------------------------------------
gross_histogram <- df %>%
  ggplot(aes(adj_gross)) +
  geom_histogram(color = "black", fill = "light blue", bins=30)

log_gross_histogram <- df %>%
  ggplot(aes(log(adj_gross))) +
  geom_histogram(color = "black", fill = "light blue", bins=30)

budget_histogram <- df %>%
  ggplot(aes(adj_budget)) +
  geom_histogram(color = "black", fill = "red", bins=30)

log_budget_histogram <- df %>%
  ggplot(aes(log(adj_budget))) +
  geom_histogram(color = "black", fill = "red",bins=30)

grid.arrange(gross_histogram, log_gross_histogram,
             budget_histogram, log_budget_histogram,
             nrow = 2, ncol = 2, top = "Financial Variable distributions")



## ----- EDA: Check other explanatory variables, warning=FALSE---------------------------------------------------------
# CHECK OTHER EXPLANATORY VARIABLES

df$ratings_distribution = factor(ifelse(df$rating == "R", "R", ifelse(df$rating == "PG-13", "PG-13", "G/PG")))


ratings_distribution_histogram <- df %>%
  ggplot(aes(x=ratings_distribution)) +
  geom_bar(color = "black", fill = c(COLOR_R, COLOR_PG13)) +
  theme(legend.position = "none")

score_histogram <- df %>%
  ggplot(aes(score)) +
  geom_histogram(color = "black", fill = "blue",bins=30)

year_histogram <- df %>%
  ggplot(aes(year)) +
  geom_histogram(color = "black", fill = "orange",bins=10)

votes_histogram <- df %>%
  ggplot(aes(votes)) +
  geom_histogram(color = "black", fill = "green",bins=10)

log_votes_histogram <- df %>%
  ggplot(aes(log(votes))) +
  geom_histogram(color = "black", fill = "green",bins=30)

runtime_histogram <- df %>%
  ggplot(aes(runtime)) +
  geom_histogram(color = "white", fill = "black",bins=30)

grid.arrange(ratings_distribution_histogram, score_histogram,  
             votes_histogram, log_votes_histogram,
             runtime_histogram, year_histogram,
             nrow = 3, ncol = 2, top = "Explanatory Variable Distributions")


## ----- Evaluate numeric variables for perfect collinearity, message = FALSE, warning = FALSE, error = FALSE----------

df_collinearity = select(df, c('adj_gross', 'adj_budget', 'score', 'runtime', 'year', 'votes'))

# Log-Transform `adj_gross`, `adj_budget`, and `votes`
df_collinearity$log_adj_gross = log(df_collinearity$adj_gross)
df_collinearity$log_adj_budget = log(df_collinearity$adj_budget)
df_collinearity$log_votes = log(df_collinearity$votes)

# Remove `gross`, `budget`, and `votes`
df_collinearity = select(df_collinearity, c('log_adj_gross', 'log_adj_budget', 'score', 'runtime', 'year', 'log_votes'))

# check for collinearity
ggpairs(df_collinearity) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## ----- Plot score and quality versus log(gross); color by rating, message = FALSE, warning = FALSE, error = FALSE----
p1 <- ggplot() +
      geom_point(data=df, aes(x=log(adj_budget), y=log(adj_gross), color=rating)) +
      xlab("Budget (Log-scale)") + ylab("Revenue (Log-scale)") +
      theme(legend.position = c(0.2, 0.15)) +
      scale_color_manual(values=c(COLOR_R, COLOR_PG13))

p2 <- ggplot() +
      geom_point(data=df, aes(x=score, y=log(adj_gross), color=rating)) +
      xlab("Score") + ylab("Revenue (Log-scale)") +
      theme(legend.position = c(0.2, 0.15)) +
      scale_color_manual(values=c(COLOR_R, COLOR_PG13))

ggarrange(p1,p2,nrow = 1, ncol = 2)


## ----- Create the models---------------------------------------------------------------------------------------------

#Model 1: Rating Only
# Base Case - R Movies
model_1 = lm(log(adj_gross) ~ PG13, data = df)

#Model 2: Rating and Quality
# Base Case - R Movies with rating of 0
model_2 = lm(log(adj_gross) ~ PG13 + score, data = df)

#Model 3: Rating and Quality with interaction terms
model_3 = lm(log(adj_gross) ~ PG13 + score + PG13*score, data = df)

#Model 4: Rating and Quality with budget
model_4 = lm(log(adj_gross) ~ PG13 + score + log(adj_budget), data = df)

#Model 5: Rating and Quality with budget and interaction terms
model_5 = lm(log(adj_gross) ~ PG13 + score + PG13 * score + log(adj_budget) + log(adj_budget) * PG13, data = df)



## ----- Function to interpret coefficients----------------------------------------------------------------------------
level_to_log <- function(coeff) {
  # Inputs the coefficient of an untransformed variable
  # Returns the % change in the log-transformed outcome variable per unit increase in coeff
  return ((exp(coeff)-1)*100)
}

log_log_pct_change <- function(coeff, pct_change) {
  # Inputs the coefficient of an untransformed variable and the percent change
  # Returns the % change in the log-transformed outcome variable given the % increase in the log-transformed explanatory variable
  # Basis: https://medium.com/@kyawsawhtoon/log-transformation-purpose-and-interpretation-9444b4b049c9
  input_ratio = 1+(pct_change/100)
  output_ratio = input_ratio ^ coeff
  return ((output_ratio-1)*100)
}



## ----create stargazer, results = "asis"------------------------------------------------------------------------------
stargazer(
  model_1, 
  model_2,
  model_3,
  model_4,
  model_5,
    covariate.labels = 
        c("PG13", "IMDb Score", "PG13 x Score", "PG13 x Log(Budget)",
          "Log(Budget)", "(Intercept)"),
  type = 'latex',
  header = FALSE, 
  omit.stat=c("f", "ser"),
  star.cutoffs = c(0.05, 0.01, 0.001) # the default isn't in line with w203
)


## ----F-test----------------------------------------------------------------------------------------------------------
anova(model_2, model_5, test='F')


## ----extra betas from model------------------------------------------------------------------------------------------
m5b0 = round(summary(model_5)$coefficients[1], digits = 3)
m5b1 = round(summary(model_5)$coefficients[2], digits = 3)
m5b2 = round(summary(model_5)$coefficients[3], digits = 3)
m5b3 = round(summary(model_5)$coefficients[5], digits = 3)
m5b4 = round(summary(model_5)$coefficients[4], digits = 3)
m5b5 = round(summary(model_5)$coefficients[6], digits = 3)
m5b0_i = round(exp(m5b0), digits = 3)
m5b1_i = round(level_to_log(m5b1), digits = 3)
m5b2_i = round(level_to_log(m5b2), digits = 3)
m5b3_i = round(level_to_log(m5b3), digits = 3)
m5b4_i = round(log_log_pct_change(m5b4,10), digits = 3)
m5b5_i = round(log_log_pct_change((m5b5+m5b4),10), digits = 3)


## ----model 5 Prediction, message=FALSE, warning=FALSE----------------------------------------------------------------

#model_5 = lm(log(adj_gross) ~ PG13 + score + PG13 * score + log(adj_budget) + log(adj_budget) * PG13, data = df)

BUDGET = 50000000

score_range = seq(0, 10, by = 0.001)
score_str = toString(round(mean(df$score),3))

df_predict_r = data.frame(
  score = score_range,
  PG13 = rep(factor(0), times = length(score_range)),
  adj_budget = rep(c(BUDGET), times = length(score_range))
)

df_predict_r$log_gross = predict(model_5, df_predict_r)
df_predict_r$gross = exp(df_predict_r$log_gross)

df_predict_pg13 = data.frame(
  score = score_range,
  PG13 = rep(factor(1), times = length(score_range)),
  adj_budget = rep(c(BUDGET), times = length(score_range))
)

df_predict_pg13$log_gross = predict(model_5, df_predict_pg13)
df_predict_pg13$gross = exp(df_predict_pg13$log_gross)

r_prediction = df_predict_r[df_predict_r$score == score_str, "gross"]
pg13_prediction = round(df_predict_pg13[df_predict_pg13$score == score_str, "gross"],2)
pg13_improve_est = round(pg13_prediction - r_prediction,2)

gross_plot <- ggplot() +
      geom_point(data=df_predict_r, aes(x=score, y=gross, colour=COLOR_R)) + 
      geom_point(data=df_predict_pg13, aes(x=score, y=gross, colour=COLOR_PG13)) + 
      ggtitle("Estimated Revenue vs Score\n") +
      xlab("Score") + ylab("Revenue (USD)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_hue(labels = c("R", "PG13")) +
      theme(legend.position = c(0.2, 0.85),
            legend.title = element_blank()) +
      annotate(geom="vline", xintercept = mean(df$score), y = (max(df_predict_pg13$gross)/2), linetype="dashed") +
      annotate(geom="text", x = mean(df$score), y = (max(df_predict_pg13$gross)), angle=90, vjust=-.5, hjust=1, label="Sample Mean IMDb Score") +
      annotate(geom="hline", yintercept = BUDGET, x = 0, linetype="dashed", color="darkgreen") +
      annotate(geom="text", x = 0, y = BUDGET,  vjust=-.5, hjust=0, label="Budget", color="darkgreen") 


gross_diff_plot <- ggplot() +
      geom_smooth(aes(x=df_predict_pg13$score, y=df_predict_pg13$gross - df_predict_r$gross), fill="darkgreen",
        colour="darkgreen", size=1) +
      ggtitle("Estimated Revenue Gain for\nchanging R film to PG-13") +
      xlab("Score") + ylab("PG-13 Revenue Gain (USD)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      annotate(geom="vline", xintercept = mean(df$score), y = ((max(df_predict_pg13$gross)-max(df_predict_r$gross))/2), linetype="dashed") +
      annotate(geom="text", x = mean(df$score), y = (max(df_predict_pg13$gross)-max(df_predict_r$gross)), angle=90, vjust=-.5, hjust=1, label="Sample Mean IMDb Score")

ggarrange(gross_plot, gross_diff_plot,
                        nrow = 1, ncol = 2)


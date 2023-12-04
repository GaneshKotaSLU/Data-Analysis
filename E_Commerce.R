install.packages("vioplot")
install.packages("GGally")
library(vioplot)
library(ggplot2)
library(GGally)
ds.obj <- read.csv(file.choose())
View(ds.obj)



##################################### Data Cleaning ##########################################

##### Step 1: Removing any Null values like 'NA' or 'NAN' #################
##### Step 2: If removing the null values are not encouraged, then impute them using mean, median, or mode. ###########

nrow(ds.obj)                                ###### Displaying total number of rows in the data
null_values <- sum(is.na(ds.obj))           ###### Displaying the total number of null values in the data
print(null_values)

remving_null_rows <- na.omit(ds.obj)        ##### Removing the null values from the dataframe

nrow(remving_null_rows)                     #### Displaying the total records after omitting the null values


removing_null_cols <- ds.obj[,colSums(is.na(ds.obj)) == 0]. #### Filteting the columns without any NULL values
nrow(removing_null_cols)


View(ds.obj)

##### Step 3: Removing any leading or trailing whitespaces ################


ds.obj <- ds.obj %>%
  filter_all(all_vars(trimws(as.character(.))!="")).   #### trimws() = removes the leading or trailing whitespaces


print(nrow(ds.obj))
View(ds.obj) 


##### Step 4: Removing any duplicates ##################


ds.obj <- distinct(ds.obj)       #### Finding the unique values to remove any duplicates 
print(nrow(ds.obj)).             #### Displaying the number of observations after removing the duplicates
View(ds.obj)

##### Step 5: Handle the outliers - Detect outliers using visualizations or statistical methods. ###########
##### Remove the outliers, transform the outliers, or cap the outliers  ###########

ds.obj_numeric <- select(ds.obj,8,9,10). #### Slecting the dataframe only with numeric values to figure out the outliers 

cat("Total no. of obersvations are:", nrow(ds.obj_numeric) * ncol(ds.obj_numeric))
View(ds.obj_numeric) 


              ############# Detecting the outliers using the IQR rule ##############

              detect_outliers <- function(ds_column){
                Q1 <- quantile(ds_column,0.25)
                Q3 <- quantile(ds_column, 0.75)
                
                IQR <- Q3 - Q1
                
                lower_bound <- Q1 - 1.5 * IQR
                upper_bound <- Q3 + 1.5 * IQR
                
                cat("\nThe lower and upper boundaries for Confidence Intervals are: \n", lower_bound,upper_bound)
                
                outliers_values <- ds_column[ds_column < lower_bound | ds_column > upper_bound].   #### Filters the values from the column that is lower or upper than the respective limits
                outlier_count <- length(outliers_values)
                
                return(list(count = outlier_count, values = outliers_values))
              }


# Apply the function to each column in your dataset
outliers_info <- lapply(ds.obj_numeric, detect_outliers)

# Get the counts and outlier values for each column
outlier_counts <- sapply(outliers_info, function(x) x$count)
outlier_values <- lapply(outliers_info, function(x) x$values)

# Total count of outliers across all columns
total_outlier_count <- sum(outlier_counts)

# Display counts and values of outliers for each column
print(outlier_counts)
print(outlier_values)

# Display the total count of outliers across all columns
print(total_outlier_count)

##### Identify the non-numeric values #####
non_numeric <- sapply(ds.obj, function(x) any(!is.numeric(x)))

print(names(non_numeric))

str(ds.obj)

ds.obj$PCS <- as.numeric(ds.obj$PCS)
ds.obj$RATE <- as.numeric(ds.obj$RATE)
ds.obj$GROSS.AMT <- as.numeric(ds.obj$GROSS.AMT)

str(ds.obj)

print(sum(is.na(ds.obj$PCS)))

print(sum(is.na(ds.obj$RATE)))

print(sum(is.na(ds.obj$GROSS.AMT)))

nrow(ds.obj) 

ds.obj <- na.omit(ds.obj)
nrow(ds.obj)

###################### Visualizations for outliers ################

            ##### Box plots using the regular function #######

            boxplot(ds.obj$PCS, main = "bot plots for the variable: PCS")
            boxplot(ds.obj$RATE, main = "bot plots for the variable: PCS")
            boxplot(ds.obj$GROSS.AMT, main = "bot plots for the variable: PCS")

            ############# Box plots using ggplot() #############
            ggplot(ds.obj, aes(x = "", y = PCS)) +
              geom_boxplot(fill = "skyblue") +
              labs(title = "Boxplot for Variable: PCS", x = "", y = "PCS values")
            
            ggplot(ds.obj, aes(x = "", y = RATE)) +
              geom_boxplot(fill = "skyblue") +
              labs(title = "Boxplot for Variable: Rate", x = "", y = "Rate values")
            
            ggplot(ds.obj, aes(x = "", y = GROSS.AMT)) +
              geom_boxplot(fill = "skyblue") +
              labs(title = "Boxplot for Variable: Gross", x = "", y = "Gross values")

            
            ##### All the boxplots in a single line of code ########
            # Reshape the data to long format using gather
            long_data <- ds.obj %>%
              gather(variable, value, PCS, RATE, GROSS.AMT)
            
            # Create boxplots for each variable in a single plot
            ggplot(long_data, aes(x = variable, y = value, fill = variable)) +
              geom_boxplot() +
              labs(title = "Boxplots for Multiple Variables", x = "Variables", y = "Values") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
            

            # Create a modified boxplot (e.g., violin plot) to show outliers
            vioplot(ds.obj$PCS, col = "blue", 
                    main = "Violin Plot with Outliers")
            
            library(ggplot2)
            
            # Assuming 'ds.obj' is your dataframe with numeric variables
            # Melt the dataframe to long format for ggplot
            library(tidyr)
            melted_data <- gather(ds.obj)
            
            # Create a violin plot for each variable
            ggplot(melted_data, aes(x = key, y = value, fill = key)) +
              geom_violin(trim = FALSE) +
              labs(title = "Violin Plots of Multiple Variables with Outliers", x = "Variables", y = "Values") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            
            # Assuming 'ds.obj' is your dataframe with numeric variables
            # Create a boxplot for each variable
            boxplot(ds.obj_numeric, outline = TRUE, col = "skyblue",
                    main = "Boxplots of Multiple Variables with Outliers")
          
          ggpairs(ds.obj_numeric).  #### Correlations between the different variables
          
          
          ?scale
          
          
          ######### Finding the outeliers using the z-scores ############
          # Assuming 'ds.obj' is your dataframe with numeric variables
          # Calculate z-scores for each numeric column in the dataframe
          z_scores <- scale(ds.obj_numeric)
          print(z_scores)
          
          mean(ds.obj_numeric$PCS)
          sd(ds.obj_numeric$PCS)
          # Define a threshold for identifying outliers (e.g., z-score > 3 or z-score < -3)
          threshold_pos <- 3
          threshold_neg <- -3
          
          # Find outliers based on the threshold
          outliers_uppper <- which(abs(z_scores) > threshold_pos, arr.ind = TRUE)
          outliers_lower <- which(abs(z_scores) < threshold_neg, arr.ind = TRUE)
          
          
          # Display the indices of rows and columns containing outliers
          nrow(outliers_lower)
          
            nrow(outliers_uppper)
##### Step 6: Standardizing the data - Ensure correct data types are there for the respective variables ########


##### Step 7: Handling the text data - Remove special characters, punctuations, or HTML tags from text fields ########



#### Step 8: Address inconsistent data - Correct misspellings or variations in categorical values - Standardize date formats, zip codes, and phone numbers... etcetera #######


#### Step 9: Feature Engineering - Creating new variables from or feartures from existing ones, converting categorical to numerical #####


#### Step 10: Observed skewed distributions if there are any - Appply transformations for these data  #####

### IN the above the NAs and the outliers  have been cleaned   ############
            
            
            
            
            
            
            ###### I haven't seen the changes made #######
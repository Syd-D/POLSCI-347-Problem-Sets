
## Collection of simple data analysis and visualization from the class:
## Politics and Society in Latin America (POLSCI 347) with prof. Edgar Franco-Vivanco 
## University of Michigan, Winter 2024



###############################################################################################
### Office Selling Problems 
###############################################################################################
read.csv("C:/Users/sydne/OneDrive/Desktop/R Files/~~~Polisci 347~~~/sales-dev_d.csv") -> Office_Selling

str(Office_Selling)

which(Office_Selling$district == "Miraflores")
which(Office_Selling$district == "Ayacucho")

Office_Selling[264,]
Office_Selling[38,]

## 1.4 Let’s use the dataset sales-dev_d.csv. This dataset contains information at district level of the logged value 
## of office price per capita during war periods in the colonial period (meanprice), and three contemporary developmental variables:
##
##        • The average educational level attained (schoolyears)
##        • The logaritmic value of equivalent household consumption (housecom)
## 
## Using this dataset, create a scatter plot of the relationship between these two developmental variables, and include a trendline.

plot(Office_Selling$schoolyears, Office_Selling$housecom, ## 
     main = "Average Educational Level vs Household Income", sub = NULL, xlab = "Years in School", ylab = "Household Consumption",
     col = (ifelse(Office_Selling$price >= -1, "gray0", "gray")),
     abline(lm(Office_Selling$housecom ~ Office_Selling$schoolyears), col = "red"))

legend("bottomright", legend=c("Price <= -3", "Price >= -3"), fill= c("gray0", "gray"), bg="white") 

sort(Office_Selling$price, decreasing = TRUE)

plot(Office_Selling$schoolyears, Office_Selling$housecom, 
     main = "Average Educational Level vs Household Income", sub = NULL, xlab = "Years in School", ylab = "Household Consumption",
     col = "black", cex = .5, 
     abline(lm(Office_Selling$housecom ~ Office_Selling$schoolyears), col = "chartreuse3", lwd = 2))

## Describe the relationship between variables in simple terms? Do you think these
## variables are a good measurement of development? (10%).2

###############################################################################################
### Mita Problems  

read.csv("C:/Users/sydne/OneDrive/Desktop/R Files/~~~Polisci 347~~~/mita_geography.csv") -> Mita_Data

str(Mita_Data)

## 1.2 The dataset mita_geography.csv contains some geographical data in places around the Mita assignment at locality level. The variables are:
##    ● Locality ID: A numerical id to identify each locality
##    ● Mita: A binary indicator of mita assignment (1 if inside the mita and 0 otherwise)
##    ● Elevation: Elevation in meters
##    ● Slope: A measure of terrain ruggedness
##
## For both Elevation and Slope, calculate the mean for provinces outside the mita border and the mean for provinces inside the mita. 
## Based on these results, what can you tell about the comparability across these places? (10%) (Hint: Here, you
## are replicating some elements of Table 1).

subset(Mita_Data, mita==1) -> Inside_Mita ## Inside the mita
subset(Mita_Data, mita==0) -> Outside_Mita ## outside the mita

## Elevation means:
mean(Inside_Mita$Elevation)  ## 4042.055 
mean(Outside_Mita$Elevation) ## 4018.429
4042.055-4018.429            ## 23.626
## The elevation of the studies localities is very consistant across the mita boundary, 
## with locales outside of the mita boundary only 23.626 meters lower in elevation than those within the mita boundary, on average.

with(Mita_Data, t.test(mita, Elevation)) ## t-Test to see if the elevation diff across the boundary is statistically significant. 
## With a p-value < 2.2e-16, it is not statistically significant. 

## t test is finding out how likely it is that the differences in the means for 
## two groups (being measured for the same variable) are just by chance, 
## not an accurate representation of the population being studied.
### Smaller the p value the more far fetched the null hypothesis is -- reject null if p value is under 0.05

## Slope means:
mean(Inside_Mita$Slope)  ## 5.541806
mean(Outside_Mita$Slope) ## 7.206142
7.206142-5.541806        ## 1.664336
## The difference in terrain ruggedness between localities outside of the mita and inside of the mita as measeured by "Slope" is around 1.66,
## with localities outside of the mita boundary having greater terrain ruggedness on average.

with(Mita_Data, t.test(mita, Slope)) ## t-Test to see if the slope diff across the boundary is statistically significant. 
## With a p-value < 2.2e-16, it is not statistically significant. 


###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###  Problem Set #2
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

# Assignment: Make two timeseries plots showing GDP per capita for MX and VN over the timeframe covered by the data

read.csv("C:/Users/sydne/OneDrive/Desktop/R Files/~~~Polisci 347~~~/gdp-capita-lac.csv") -> ProbSet2_Data

    str(ProbSet2_Data)

##############
### Mexico ###
##############  
    
data.frame(subset(ProbSet2_Data, country=="Mexico")) -> Mexico_GDPdata
    
dev.off() ### set up plotting space
par(mfrow=c(1,2))
    
for(i in 1){
    plot.ts(Mexico_GDPdata$gdp_per_cap, plot.type = c("single"), ### plot columns 2:7 (excludes dates) on a single time series 
            xlab= "Year", ## make labels 
            ylab= "GDP per Capita in USD",
            main = "MX GDP per Capita by Year",
            axes = FALSE, ## I do want axes
            col = c("darkgreen"), ## Apply colors to each line in the plot, assigned based on order in the dataframe
            lwd = 3
            )
    axis(1, at=seq(from=1, to=length(Mexico_GDPdata$year), by=5), 
            labels=seq(from=Mexico_GDPdata$year[1], to=Mexico_GDPdata$year[length(Mexico_GDPdata$year)], by=5), 
            las=2, cex.axis = 0.8) ## create bottom axis
    
    label_seq <- seq(from=0, to=max(Mexico_GDPdata$gdp_per_cap), by=1000) ### use sequence function to create a sequence of numerical labels for the y axis 
    axis(2, at=label_seq, labels=label_seq, las=2, cex.axis = 0.8) ### plug sequence into the axis funtion to create y axis 
}
#################
### Venezuela ###
#################
    
data.frame(subset(ProbSet2_Data, country=="Venezuela")) -> VN_GDPdata
   str(VN_GDPdata) 
  
  # plot(VN_GDPdata$year, VN_GDPdata$gdp_per_cap, 
  #     main = "VN GDP per Capita by Year", sub = NULL, xlab = "Year", ylab = "GDP Per Capita",
  #     col = "black", cex = .5) 
 
for(i in 1){     
   plot.ts(VN_GDPdata$gdp_per_cap, plot.type = c("single"), ### plot columns 2:7 (excludes dates) on a single time series 
           xlab= "Year", ## make labels 
           ylab= "GDP per Capita in USD",
           main = "VN GDP per Capita by Year",
           axes = FALSE, ## I do want axes
           col = c("orange"), ## Apply colors to each line in the plot, assigned based on order in the dataframe
           lwd = 3
   )
   axis(1, at=seq(from=1, to=length(VN_GDPdata$year), by=5), 
        labels=seq(from=VN_GDPdata$year[1], to=VN_GDPdata$year[length(VN_GDPdata$year)], by=5), 
        las=2, cex.axis = 0.8) ## create bottom axis
   
   label_seq <- seq(from=0, to=max(VN_GDPdata$gdp_per_cap), by=1000) ### use sequence function to create a sequence of numerical labels for the y axis 
   axis(2, at=label_seq, labels=label_seq, las=2, cex.axis = 0.8) ### plug sequence into the axis funtion to create y axis 
}   



###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###  Problem Set #3
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
   
## 1.1.2 Then, using the dataset, create a histogram of skin color and race for each country. Describe your results. (10%)
   
read.csv("C:/Users/sydne/OneDrive/Documents/Winter 2024/POLSCI 347/lapop-race_s.csv") -> LAPOP
   
   str(LAPOP)
   
   data.frame(LAPOP) -> LAPOP_df 
   
   LAPOP_df[c(2,4)] -> Country_Race_df ### new df with just country and race column
   
   c(unique(Country_Race_df$country)) -> County_Vector ## List of countries
   ## "Mexico"    "Guatemala" "Colombia"  "Brazil"  
   c(unique(Country_Race_df$etid)) -> Race_Vector ## List of races
   ## "White" "Mestizo" "Don't Know" "Indigenous" "No Response" "Mulatto" "Black" "Other" "Asian"
   
   data.frame(country = County_Vector, ## New empty df
              "White" = NA,
              "Mestizo" = NA,
              "Don't Know" = NA,
              "Indigenous" = NA,
              "No Response" = NA,
              "Mulatto" = NA, 
              "Black" = NA,
              "Other" = NA,
              "Asian" = NA) -> Plot_df
   
   for (h in 1:4) { ## For loop to load data into df
     subset(Country_Race_df, country == County_Vector[h]) -> country_placeholder ## Subset by country
     
     for (i in 1:length(Race_Vector)) {
       length(which(country_placeholder[,2]==Race_Vector[i])) -> Plot_df[h,(i+1)] ##  count how may respondents reported themselves as each race
       ## finds the length of a vector that includes every entry that is tied to a certain race
     }}
   
   Plot_df -> Filled_df ## save point 
   
   Filled_df
   
   ### Plotting
   
   as.data.frame(t(Filled_df)) -> flipped_df ## flip the df's columns and rows using transform function 
   
   dev.off() ## plotting set up
   par(mfrow=c(1,1), mar=c(6,4,4,9), xpd=TRUE)
   
   barplot(as.matrix(flipped_df[2:10,]), names.arg = flipped_df[1,],  ### use as.matrix and t functions to reformat the now full dataset. Also select [2:50,3:8] to avoid the first row (its just the blank space count) and select only the frequency columns
           beside=FALSE, col=c("#ff595e","#ff924c","#ffca3a","#c5ca30","#8ac926","#52a675","#1982c4","#4267ac","#6a4c93"),  
           xlab=" ", ylab="Number of Responses", main="Self Reported Racial ID by Country", 
           cex.names = 1, cex.lab=1.25, cex.axis = .7, axes = TRUE, las = 1, space = .5)
   
   legend("topright", legend=Race_Vector, fill= c("#ff595e","#ff924c","#ffca3a","#c5ca30","#8ac926","#52a675","#1982c4","#4267ac","#6a4c93"), bg="white", inset=c(-0.3,0)) ### create legend on the right side of the plotting space using a vector of colors and respective names for the event types 
   
   #######################################################################################################################################################################################
   ##### Redo with skin color 
   
   LAPOP_df[,c(2,6)] -> Country_Color_df
   
   c(unique(Country_Color_df$colorr)) -> Color_Vector
   
   data.frame(country = County_Vector,
              "color 1" = NA,
              "color 2" = NA,
              "color 3" = NA,
              "color 4" = NA,
              "color 5" = NA,
              "color 6" = NA, 
              "color 7" = NA,
              "color 8" = NA,
              "color 9" = NA,
              "color 10" = NA,
              "color 11" = NA) -> ColorPlot_df
   
   
   for (h in 1:4) {
     
     subset(Country_Color_df, country == County_Vector[h]) -> country_placeholder
     
     for(i in 1:11){
       length(which(country_placeholder[,2]==Color_Vector[i])) -> ColorPlot_df[h,(i+1)]
     }}
   
   ColorPlot_df -> Filled_Color_df
   
   ### Plotting
   
   as.data.frame(t(Filled_Color_df)) -> flipped_color_df
   
   dev.off() ## plotting set up
   par(mfrow=c(1,1), mar=c(6,4,4,7), xpd=TRUE)
   
   barplot(as.matrix(flipped_color_df[2:12,]), names.arg = flipped_color_df[1,],  ### use as.matrix and t functions to reformat the now full dataset. Also select [2:50,3:8] to avoid the first row (its just the blank space count) and select only the frequency columns
           beside=FALSE, col=c("#fff6f7","#ffd5d6","#f7bead","#f0c694","#c69473","#9c6b42","#845239","#6b4131","#4b3122", "#422110", "#312121"),  
           xlab=" ", ylab="Number of Responses", main="Skin Color by Country", 
           cex.names = 1, cex.lab=1.25, cex.axis = .7, axes = TRUE, las = 1, space = .5)
   
   legend("topright", legend=c(1:11), fill= c("#fff6f7","#ffd5d6","#f7bead","#f0c694","#c69473","#9c6b42","#845239","#6b4131","#4b3122", "#422110", "#312121"), bg="white", inset=c(-0.1,0)) ### create legend on the right side of the plotting space using a vector of colors and respective names for the event types 
   
   
   
###########################################################################################################
## For Brazil, create a table by ethnicity (*etid*) including the average income and the percentage of respondents by category. 
   
   subset(LAPOP_df, country=="Brazil") -> Brazil_Subset
   
   Brazil_Ethnicity_Income <- data.frame(Ethnicity = unique(Brazil_Subset$etid), ### Creating blank dataframe
                                         Average_Income = NA,
                                         Percent_of_Respondants = NA
   ) 
   
   which(Brazil_Subset$etid=="White") -> White_locations ## These lines are just me deciding how I want to set up the code in the for loop
   na.omit(Brazil_Subset[White_locations,7]) -> white_incomes
   mean(white_incomes)
   
   unique(Brazil_Ethnicity_Income$Ethnicity) -> ethnicities ### "White" "Mulatto" "Black" "Asian" "Indigenous""Don't Know" "No Response"
   
   for(i in 1:length(ethnicities)){ ### creating data for Average_Income column
     
     which(Brazil_Subset$etid==ethnicities[i]) -> Race_locations
     na.omit(Brazil_Subset[Race_locations,7]) -> race_incomes
     
     print(mean(race_incomes))
     
     mean(race_incomes) -> Brazil_Ethnicity_Income[i,2]
   }
   
   
   length(na.omit(Brazil_Subset$etid)) -> Number_of_Total_Respondants ## Total respondents # setup -- 1500
   
   for(i in 1:length(ethnicities)){ ## data for % of respondents column
     
     length(which(Brazil_Subset$etid==ethnicities[i])) -> Race_Respondants
     
     (Race_Respondants/Number_of_Total_Respondants)*100 -> Brazil_Ethnicity_Income[i,3]
   }
   
   Brazil_Ethnicity_Income ## Final filled out df
   
   sum(Brazil_Ethnicity_Income$Percent_of_Respondants) ### Checking that % column adds up to 100
   
   which(Brazil_Subset$etid=="No Response") ### Only one respondent entry is tagged as "no response" so we know that it is negligable
   Brazil_Subset[1319,] ### this person also did not report and income 
   
   write.csv(Brazil_Ethnicity_Income, file = "Brazil_Ethnicity_Income.csv") ### export to csv to use in assignment report 
   





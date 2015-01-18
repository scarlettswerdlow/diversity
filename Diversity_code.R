###########################################################################
#                                                                         #
#                     Racial diversity in Chicago                         #
#                   Coded in R by Scarlett Swerdlow                       #
#                        For the Chicago Sun-Times                        #
#                       scarlett.swerdlow@gmail.com                       #
#                                                                         #
###########################################################################

# Load and merge relevant data

Race <- read.csv("Race.csv" )
Geo <- read.csv("Geography.csv")
Diversity <- merge(Race, Geo, by.x = "Community_area", by.y = "Community.Area")


# Calculate race shares, diversity scores, and other stats by community area

Race_cols = c(4:8)
for (col in Race_cols) {
  Var_name = paste( names(Diversity)[col], "share", sep="_" )
  Diversity[Var_name] <- Diversity[col]/Diversity[3]
}

Diversity["Score"] <- with( Diversity, 
                            (1 - (Asian_share^2 + Black_share^2 +
                                   Latino_share^2 + White_share^2 + 
                                   Other_share^2))*100 )

Diversity["College_or_higher_share"] <- Diversity[11]/Diversity[9]
Diversity["Below_poverty_share"] <- Diversity[13]/Diversity[12]
Diversity["Index_crime_rate"] <- (Diversity[14]/Diversity[3])*10000


# Count number of community areas in each racial share decile by race

Race_share_cols = c(19:23)
decile <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", 
             "70-80", "80-90", "90-100")

for (col in Race_share_cols) {
  counts <- rep(0, 10)
  j = 10
  for ( i in 1:length(counts) ) {  
    counts[i] <- length( Diversity[ ,col ][ 
      Diversity[col] <= j/100 & 
        Diversity[col] > (j-10)/100 ] )
    j = j + 10
  }
  d <- data.frame(decile, counts)
  DF_name = paste( names(Diversity)[col], "counts", sep="_" )
  write.csv(d, file = paste(DF_name, "csv", sep = "."))
}


# Compare predominantly black neighborhoods to rest of Chicago

black <- c()
other <- c()

for ( i in 1:length(Diversity[,1]) ) {
  if ( Diversity[i, 20] >= .9 ) {
    black <- c( black, Diversity[i,1] )
  }
  else if  ( Diversity[i, 20] < .9 ) {
    other <- c( other, Diversity[i,1] )
  }
}

Black_data <- subset( Diversity, Diversity[,1] %in% black )
Black_data["Education_weight"] <- Black_data[,9]/sum(Black_data[,9])
Black_data["Poverty_weight"] <- Black_data[,12]/sum(Black_data[,12])
Black_data["Pop_weight"] <- Black_data[,3]/sum(Black_data[,3])
Black_college_mean <- weighted.mean( Black_data[,25], Black_data[,28] )
Black_poverty_mean <- weighted.mean( Black_data[,26], Black_data[,29] )
Black_crime_mean <- weighted.mean( Black_data[,27], Black_data[,30] )

Other_data <- subset( Diversity, Diversity[,1] %in% other )
Other_data["Education_weight"] <- Other_data[,9]/sum(Other_data[,9])
Other_data["Poverty_weight"] <- Other_data[,12]/sum(Other_data[,12])
Other_data["Pop_weight"] <- Other_data[,3]/sum(Other_data[,3])
Other_college_mean <- weighted.mean( Other_data[,25], Other_data[,28] )
Other_poverty_mean <- weighted.mean( Other_data[,26], Other_data[,29] )
Other_crime_mean <- weighted.mean( Other_data[,27], Other_data[,30] )

Var <- c("College", "Poverty", "Crime")
Black_rates <- c(Black_college_mean, Black_poverty_mean, Black_crime_mean)
Other_rates <- c(Other_college_mean, Other_poverty_mean, Other_crime_mean)
d <- data.frame(Var, Black_rates, Other_rates)
write.csv(d, file = "Outcomes.csv")


# Export data for Google Fusions table and map

d <- subset( Diversity, select=c(1, 2, 16:27) )
write.csv(d, file = "Diversity_map_data.csv")

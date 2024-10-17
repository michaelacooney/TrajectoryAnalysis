# Set this to your own working directory! Don't use the same as mine!
setwd("~/Documents/Advanced GIS/Movement analysis/TrajectoryAnalysis_Data_and_Code")
#setwd("C:/Urska/Teaching/StAndrews/Year4/GG4328_202324/Part2_MovementAnalysis/2_Labs/Week1_TrajectoryAnalysis")

# Only run this if this is the first time you have used these packages, this will install packages
install.packages('adehabitatLT')
install.packages('lubridate')
install.packages('sf')
install.packages('ggplot2')
install.packages('grid')
install.packages('gridExtra')
install.packages('circular')
install.packages('move')
install.packages('devtools')
devtools::install_github("16EAGLE/moveVis") # install from github, after you have installed and loaded devtools

# Run this if you have already installed the packages 
library(adehabitatLT) # for analysing trajectories
library(lubridate) # for processing time
library(sf) # for dealing with spatial data
library(ggplot2) # for making nice figures
library(grid) # for creating grids in figures
library(gridExtra) # for creating grids in figures
library(circular) # for circular statistics of turning angles and heading
library(move) # another package for movement
library(devtools) # needed to get moveVis from github
library(moveVis) # package for animation of movement




#######GPS data quality and removing NA values#######
# Read data
ducks <- read.csv('Ducks_ForLab.csv')
head(ducks)
# Unique individuals
individuals <- unique(ducks$individual.local.identifier)
individuals

# How many satellites are visible - check min and max of the attribute
min(ducks$gps.satellite.count)

max(ducks$gps.satellite.count)
# Remove points with less than 4 visible satellites
# step 1, find them
badPoints <- which(ducks$gps.satellite.count<4)
# step 2, remove them from our data
ducks <- ducks[-badPoints,]

# Check which rows have NA values in coordinates
NAPoints <- which(is.na(ducks$location.lat) | is.na(ducks$location.long))

#if there were NA points I wanted to get rid of 
#ducks <- ducks[-NAPoints,]

# Let's check what format the timestamp attribute is in
head(ducks)

# First we need to convert timestamp from factor to datetime
ducks$tstamp<-strptime(as.character(ducks$timestamp), "%Y-%m-%d %H:%M:%S")

# Then we separate timestamp into individual components
ducks$date <- as.Date(ducks$tstamp)
ducks$year <- year(ducks$tstamp)
ducks$month <- month(ducks$tstamp)
ducks$day <- day(ducks$tstamp)
ducks$hour <- hour(ducks$tstamp)
ducks$min <- minute(ducks$tstamp)
ducks$sec <- second(ducks$tstamp)
head(ducks)

# What years do we have?
years <- unique(ducks$year)

#Create a birdyear id for so that trajectories of different years can be separated if we so wish.
ducks$idyear<-paste0(ducks$individual.local.identifier, year(ducks$date))

# Get individuals by year
individualYear <- unique(ducks$idyear)
individualYear

# Convert timestamp to POSIX format
ducks$POSIX <- as.POSIXct(ducks$timestamp, format="%Y-%m-%d %H:%M:%S")
head(ducks)

# Are any times duplicated?
duplics <- which(duplicated(ducks$POSIX)==TRUE)

# Yes, we have a few points like this, so let's remove them.
ducks <- ducks[-duplics,]

# Convert the ducks data frame into an sf object by specifying which columns are coordinates
# Also we need to specify coordinate system, which in our case is WGS1984 (EPSG:4326 - for EPSG codes we just need the number))
ducksSF <- st_as_sf(ducks, coords=c('location.long','location.lat'), crs=4326)
ducksSF

#######Preparing geographic coordinates#########
# Convert the ducks data frame into an sf object by specifying which columns are coordinates
# Also we need to specify coordinate system, which in our case is WGS1984 (EPSG:4326 - for EPSG codes we just need the number))
ducksSF <- st_as_sf(ducks, coords=c('location.long','location.lat'), crs=4326)
ducksSF

# Project data
# Define new projection
crsnew <- st_crs("ESRI:102014")

# Transform data into the new coordinate system
ducksSF_proj <- st_transform(ducksSF,crs=crsnew)
head(ducksSF_proj)

# Read X an Y from geometry column as separate coordinates
ducksSF_proj$X <- st_coordinates(ducksSF_proj)[,1]  
ducksSF_proj$Y <- st_coordinates(ducksSF_proj)[,2]

# Drop geometry column
ducksSF_proj <- st_drop_geometry(ducksSF_proj)
head(ducksSF_proj)

# First turn the SF into a data frame
ducks_cleanedDF <- as.data.frame(ducksSF_proj)

# Now save this into a csv file, giving the name of the projection in the filename
write.csv(ducks_cleanedDF, 'ducks_CleanData_projected_ESRI102014.csv')


######Trajectory analysis - movement parameters######
# Create a set of trajectories, one per each individual
ducks_ltraj <- as.ltraj(xy=ducksSF_proj[,c('X','Y')],  # spatial Coordinates
                        date = ducksSF_proj$POSIX,      # timestamp in POSIX format, including date/time
                        id = ducksSF_proj$individual.local.identifier)                # individual IDs

# Let's plot these trajectories to see if it worked
plot(ducks_ltraj)

# Let's check the third duck's trajectory (note the double brackets!)
head(ducks_ltraj[[3]])
# We can also plot this one trajectory
plot(ducks_ltraj[3])

# Let's focus on two ducks for this analysis, duck 1 (the long-distance traveller) and duck 3 (sedentary).
duck1 <- ducks_ltraj[[1]]
duck3 <- ducks_ltraj[[3]]







#######3.1. Time, distance and speed#####

# Building a ggplot histogram for time difference

# Before we plot anything we need to remove the last row of the trajectory, which has NA values (this will mess up the plots otherwise)

# Check how this looks like:
tail(duck3)

# And now let's remove that row.
nrows <- nrow(duck3) # find number of rows for this trajectory
duck3 <- duck3[-nrows,] # remove the last row, i.e. row no. nrows
# Check that it is gone
tail(duck3)

# First let's just make a plot and say that we want its geometry to be a  histogram
ggplot(duck3, aes(x=dt))+geom_histogram()

# Let's improve this by specifying a better bin width, maybe 1000 (this is, a bin every 1000 seconds)
ggplot(duck3, aes(x=dt))+geom_histogram(binwidth=1000)

# Now let's change the background theme and add some titles
ggplot(duck3, aes(x=dt))+geom_histogram(binwidth=1000)+
  theme_bw()+xlab("Segment duration (s)")+ylab("Count")+ggtitle("Segment duration for duck 3")

# Plot the segment length (dist) directly with an adjustment to a smaller bin width
ggplot(duck3, aes(x=dist))+geom_histogram(binwidth=100)+
  theme_bw()+xlab("Segment length (m)")+ylab("Count")+ggtitle("Segment length for duck 3")

# Calculate speed in m/S
duck3$speedMS <- duck3$dist/duck3$dt

# Convert to km/h
duck3$speedKmH <- duck3$speedMS * 3.6

# Plot of speed in km/h
ggplot(duck3, aes(x=speedKmH))+geom_histogram()+
  theme_bw()+xlab("Speed (km/h)")+ylab("Count")+ggtitle("Speed for duck 3")

# Plot the segment duration 
duration_plot <- ggplot(duck3, aes(x=dt))+geom_histogram(binwidth=1000)+
  theme_bw()+xlab("Segment duration (s)")+ylab("Count")+ggtitle("Segment duration for duck 3")

# Plot the segment length (dist) directly with an adjustment to a smaller bin width
length_plot <- ggplot(duck3, aes(x=dist))+geom_histogram(binwidth=100)+
  theme_bw()+xlab("Segment length (m)")+ylab("Count")+ggtitle("Segment length for duck 3")

# Plot of speed in km/h
speed_plot <- ggplot(duck3, aes(x=speedKmH))+geom_histogram()+
  theme_bw()+xlab("Speed (km/h)")+ylab("Count")+ggtitle("Speed for duck 3")

# Export into figure:
png(filename="Duck3_Duration_Length_Speed.png")
# Put the three plots in a gridded display with 3 rows and 1 column
grid.newpage()
pushViewport(viewport(layout=grid.layout(3,1)))
print(duration_plot, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(length_plot, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(speed_plot, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
#for some reason this is not exporting a png to my mac 

# Let's explore segment lenghts with a box plot
ggplot(duck1, aes(x=dt))+geom_boxplot()

# Clearly there are some very high outliers, up to 10000000 seconds (= 166666 hours). We can remove everything above 50000s
duck1 <- duck1[-which(duck1$dt>50000),]

# How did this change the data? 
ggplot(duck1, aes(x=dt))+geom_boxplot()

# Step 1: Calculate mean values of duration, lenght and speed for duck 1
# Remember, data for duck 1 are accessible as the first element of the ducks_ltraj list:
ducks_ltraj[[1]]
# This is what we will use to calculate statistics

# First we need to assign the data to a generic variable we will use in the for loop
thisduck <- ducks_ltraj[[1]]

# We also need to calculate speed, because we haven't done that for all ducks yet
thisduck$speedKmH <- (thisduck$dist/thisduck$dt)*3.6

# Then we need to get rid of all NA values in the respective columns, using the two concepts you already know, is.na and | operator
dtNAs <- which(is.na(thisduck$dt)==TRUE | is.na(thisduck$dist)==TRUE | is.na(thisduck$speedKmH)==TRUE)
# We remove rows with NAs
thisduck <- thisduck[-dtNAs,]

# Finally we calculate statistics for this duck
dt_mean <- mean(thisduck$dt)
dt_std <- sd(thisduck$dt)
dist_mean <- mean(thisduck$dist)
dist_std <- sd(thisduck$dist)
speedKmH_mean <- mean(thisduck$speedKmH)
speedKmH_std <- sd(thisduck$speedKmH)

# Step 2: Initialise data frame for results: we want one that will have as many rows as there are individual ducks, plus columns for ID and all the statistics (7 columns in total).

# First we create a data frame with no values (NAs) of the right size:
results <- data.frame(matrix(NA, ncol = 7, nrow = length(individuals))) 

# Then we add names to columns
columnNames <- c("ID","dt_mean","dt_std","dist_mean","dist_std","speedKmH_mean","speedKmH_std")
names(results) <- columnNames

# Check how this looks like:
results

# Before writing the loop we can also populate the ID column directly, this can be done using the function id from adehabitatLT
# Check what this does:
id(ducks_ltraj)

# Now assign this to results:
results$ID <- id(ducks_ltraj)

# Check if it worked
results

# Step 3: For loop to calculate all the statistics

for (i in 1:length(individuals)) {
  
  # Here I have copied the calculation code from step 1, but edited to include the index i
  
  # First we need to assign the data to a generic variable we will use in the for loop
  thisduck <- ducks_ltraj[[i]] # note how this is the same as above, except I replaced 1 with the index i
  
  # We also need to calculate speed, because we haven't done that for all ducks yet
  thisduck$speedKmH <- (thisduck$dist/thisduck$dt)*3.6
  
  # Then we need to get rid of all NA values in the respective columns, using the two concepts you already know, is.na and | operator
  dtNAs <- which(is.na(thisduck$dt)==TRUE | is.na(thisduck$dist)==TRUE | is.na(thisduck$speedKmH)==TRUE)
  # We remove rows with NAs
  thisduck <- thisduck[-dtNAs,]
  
  # Finally we calculate statistics for this duck, but unlike above, we now put these into the results data frame, in row i
  results$dt_mean[i] <- mean(thisduck$dt)
  results$dt_std[i] <- sd(thisduck$dt)
  results$dist_mean[i] <- mean(thisduck$dist)
  results$dist_std[i] <- sd(thisduck$dist)
  results$speedKmH_mean[i] <- mean(thisduck$speedKmH)
  results$speedKmH_std[i] <- sd(thisduck$speedKmH)
  
} # end for (always a good idea to show where the loop ends)

# Let's see what we got:
results






############3.2. Net Squared Displacement########
# We don't have to initialise the results data frame here, because we already have it. But we will add two columns to it, like this:
results$R2n_mean <- NA
results$R2n_std <- NA
# Check what happened
results

# Now let's use a for loop to populate these two columns. The loop is very similar to the previous one:

for (i in 1:length(individuals)) {
  
  # Assign data to the generic variable
  thisduck <- ducks_ltraj[[i]] 
  
  # Then we need to get rid of all NA values in the respective column
  R2nNAs <- which(is.na(thisduck$R2n)==TRUE)
  
  # Here comes the new If statement: if there are some NAs, we remove them, if not, we don't do anything.
  if (length(R2nNAs)>0) {
    thisduck <- thisduck[-R2nNAs,]
  }
  
  # Finally we calculate statistics for this duck, but unlike above, we now put these into the results data frame, in row i
  results$R2n_mean[i] <- mean(thisduck$R2n)
  results$R2n_std[i] <- sd(thisduck$R2n)
  
} # end for 

# Check what happened
results

# Duck 1, migratory behaviour
ggplot(ducks_ltraj[[1]], aes(x=date, y=R2n)) + geom_point() + 
  theme_bw()+xlab("Date")+ylab("R2n")+ggtitle("Net square displacement over time for duck 1")

# Duck 3, sedentary behaviour
ggplot(ducks_ltraj[[3]], aes(x=date, y=R2n)) + geom_point() + 
  theme_bw()+xlab("Date")+ylab("R2n")+ggtitle("Net square displacement over time for duck 1")



#####3.3. Analysis of turning angles and heading######
# Take one duck
thisduck <- ducks_ltraj[[1]]

# Convert relative angle from radians into degrees
thisduck$turningAngle <- thisduck$rel.angle * 180 / pi
# Check it
head(thisduck)


# Build a circular plot
angleplot <- ggplot(thisduck, aes(x=turningAngle)) + geom_histogram(binwidth=2)+coord_polar(start = pi)+
  theme_bw()+scale_x_continuous(limits = c(-180,180),breaks=c(-180,-90,0,90,180))+xlab("Turning Angle")+ylab("Count")+
  ggtitle("Turning angle for duck 1")

# Export as a figure
png(filename="Duck1_TurningAngle.png")
print(angleplot)
# Remove NAs 
turnNAs <- which(is.na(thisduck$turningAngle)==TRUE)

# Here comes the new If statement: if there are some NAs, we remove them, if not, we don't do anything.
if (length(turnNAs)>0) {
  thisduck <- thisduck[-turnNAs,]
}

# Calculate statistics - because angles are circular, we need to use circular mean and circular std, from package circular
turningAngle_mean <- mean(circular(thisduck$turningAngle))
turningAngle_std <- sd(circular(thisduck$turningAngle))



# We don't have to initialise the results data frame here, because we already have it. But we will add two columns to it, like this:
results$turningAngle_mean <- NA
results$turningAngle_std <- NA
# Check what happened
results
# For loop for all the ducks
for (i in 1:length(individuals)) {
  
  # Take one duck
  thisduck <- ducks_ltraj[[i]]
  
  # Convert relative angle from radians into degrees
  thisduck$turningAngle <- thisduck$rel.angle * 180 / pi
  
  # Build a circular plot
  angleplot <- ggplot(thisduck, aes(x=turningAngle)) + geom_histogram(binwidth=2)+coord_polar(start = pi)+
    theme_bw()+scale_x_continuous(limits = c(-180,180),breaks=c(-180,-90,0,90,180))+xlab("Turning Angle")+ylab("Count")+
    ggtitle(paste("Turning angle for duck ",i, sep=""))
  
  # Export as a figure
  png(filename=paste("Duck",i,"_TurningAngle.png",sep=""))
  print(angleplot)
  dev.off()
  
  # Remove NAs 
  turnNAs <- which(is.na(thisduck$turningAngle)==TRUE)
  
  # Here comes the new If statement: if there are some NAs, we remove them, if not, we don't do anything.
  if (length(turnNAs)>0) {
    thisduck <- thisduck[-turnNAs,]
  }
  
  # Calculate statistics - because angles are circular, we need to use circular mean and circular std.
  # This means that we first transform the vector into a circular object and then calculate mean/std.
  results$turningAngle_mean[i] <- mean(circular(thisduck$turningAngle))
  results$turningAngle_std[i] <- sd(circular(thisduck$turningAngle))
  
} # end for
# Check results
results

# Take a duck
thisduck <- ducks_ltraj[[2]]

# Check to see in which columns we have the x and y coordinates
head(thisduck)

# Ok, they are called x and y. 

# So now we will take a loop that will go through this trajectory. We need to know the number of points in the trajectory,
# we can just check the length of one of the coordinates.
nPoints <- length(thisduck$x)

# Also we need to add a new field for the heading to thisduck data frame
thisduck$heading <- NA

# For loop through the trajectory, but not to the end, we skip the final point!

for (i in 1:(nPoints-1)) {
  
  # Let's read the coordinates from two consecutive points, i and i+1
  x1 <- thisduck$x[i]
  x2 <- thisduck$x[i+1]
  y1 <- thisduck$y[i]
  y2 <- thisduck$y[i+1]
  
  # And calculate the heading as arctan of the two values, plus convert to degrees
  thisduck$heading[i] <- atan2((x2-x1),(y2-y1)) * 180 / pi
  
}

# Build a circular plot
angleplot <- ggplot(thisduck, aes(x=heading)) + geom_histogram(binwidth=2)+coord_polar(start = pi)+
  theme_bw()+scale_x_continuous(limits = c(-180,180),breaks=c(-180,-90,0,90,180))+
  xlab("Heading")+ylab("Count")+ggtitle("Heading for duck 2")

# Export as a figure
png(filename="Duck2_Heading.png")
print(angleplot)
# Remove NAs - there will be at least one, for the last point through which we didn't loop
headingNAs <- which(is.na(thisduck$heading)==TRUE)

# If there are some NAs, we remove them, if not, we don't do anything.
if (length(headingNAs)>0) {
  thisduck <- thisduck[-headingNAs,]
}

# Calculate statistics - because angles are circular, we need to use circular mean and circular std, from package circular
heading_mean <- mean(circular(thisduck$heading))
heading_std <- sd(circular(thisduck$heading))

# We don't have to initialise the results data frame here, because we already have it. But we will add two columns to it, like this:
results$heading_mean <- NA
results$heading_std <- NA
# Check what happened
results


# Here start the nested loops

# External For loop for all the ducks - note the index is now called j!
for (j in 1:length(individuals)) {
  
  # Take a duck
  thisduck <- ducks_ltraj[[j]]
  
  # So now we will take a loop that will go through this trajecotry. We need to know the number of points in the   trajectory,
  # we can just check the length of one of the coordinates.
  nPoints <- length(thisduck$x)
  
  # Also we need to add a new field for the heading to thisduck data frame
  thisduck$heading <- NA
  
  # Internal for loop through the trajectory, but not to the end, we skip the final point!
  # This loop has index i
  for (i in 1:(nPoints-1)) {
    
    # Let's read the coordinates from two consecutive points, i and i+1
    x1 <- thisduck$x[i]
    x2 <- thisduck$x[i+1]
    y1 <- thisduck$y[i]
    y2 <- thisduck$y[i+1]
    
    # And calculate the heading as arctan of the two values, plus convert to degrees
    thisduck$heading[i] <- atan2((x2-x1),(y2-y1)) * 180 / pi
    
  } # end for i
  
  # Build a circular plot
  angleplot <- ggplot(thisduck, aes(x=heading)) + geom_histogram(binwidth=2)+coord_polar(start = pi)+
    theme_bw()+scale_x_continuous(limits = c(-180,180),breaks=c(-180,-90,0,90,180))+
    xlab("Heading")+ylab("Count")+ggtitle(paste("Heading for duck ",j,sep=""))
  
  # Export as a figure
  png(filename=paste("Duck",j,"_Heading.png",sep=""))
  print(angleplot)
  dev.off()
  
  # Remove NAs - there will be at least one, for the last point through which we didn't loop
  headingNAs <- which(is.na(thisduck$heading)==TRUE)
  
  # If there are some NAs, we remove them, if not, we don't do anything.
  if (length(headingNAs)>0) {
    thisduck <- thisduck[-headingNAs,]
  }
  
  # Calculate statistics - because angles are circular, we need to use circular mean and circular std
  results$heading_mean[j] <- mean(circular(thisduck$heading))
  results$heading_std[j] <- sd(circular(thisduck$heading))
  
} # end for j
# Check results
results




#####Radius of gyration####
# Pick a duck
thisduck <- ducks_ltraj[[3]]

# Find number of points in this trajectory
noPoints <- length(thisduck$x)

# Calculate the mean centre of all locations
# This is done by summing up all x's and dividing by number of points to get the x coordinate of the centre.
# And the same for y coordinate.
centreX <- sum(thisduck$x)/noPoints
centreY <- sum(thisduck$y)/noPoints

# Distance of each point to (centreX, centreY)
thisduck$distTocentre <- sqrt((centreX-thisduck$x)^2+(centreY-thisduck$y)^2)

# Check result
head(thisduck)

# Calculate radius of gyration
gyration <- sqrt(sum(thisduck$distTocentre)/noPoints)

# Prepare the field in the results data frame
results$gyration <- NA

# For loop across all ducks

for (i in 1:length(individuals)) {
  
  # Pick a duck
  thisduck <- ducks_ltraj[[i]]
  
  # Find number of points in this trajectory
  noPoints <- length(thisduck$x)
  
  # Calculate the mean centre of all locations
  centreX <- sum(thisduck$x)/noPoints
  centreY <- sum(thisduck$y)/noPoints
  
  # Distance of each point to (centreX, centreY)
  thisduck$distTocentre <- sqrt((centreX-thisduck$x)^2+(centreY-thisduck$y)^2)
  
  # Calculate radius of gyration and insert it into the results data frame
  results$gyration[i] <- sqrt(sum(thisduck$distTocentre)/noPoints)
  
}

# Check results
results

# This is the last thing we will add to results, so let's also export these into a file.
write.csv(results, "Ducks_results.csv", row.names=FALSE)





####Animation of movement#######
# Get trajectory for duck 3 and add animal identifier
duck3 <- ducks_ltraj[[3]]
duck3$ID <- "180063OT+ (JC79702)"

# make it an sf object, so we can re-project back to Long/Lat
# We are currently in the European Lambert Conformal Projection, which we called crsnew earlier.
# So we need to specify this and then reproject to long/lat
duck3_sf <- st_as_sf(duck3, coords=c('x','y'), crs=crsnew)

# Re-project into WGS1984 (EPSG:4326)
duck3_LongLat <- st_transform(duck3_sf,crs=4326)

# Separate coordinates into two columns
# Read X an Y from geometry column as separate coordinates
duck3_LongLat$Long <- st_coordinates(duck3_LongLat)[,1]  
duck3_LongLat$Lat <- st_coordinates(duck3_LongLat)[,2]

# Drop geometry column
duck3_LongLat <- st_drop_geometry(duck3_LongLat)
head(duck3_LongLat)
# change into a data frame
duck3_df <- as.data.frame(duck3_LongLat)

# Let's select a shorter time frame for animation
mindate <- min(duck3_df$date)
maxdate <- max(duck3_df$date)
# We have over a year of data in this trajetory, let's take one month of data
# i.e. 30 days, therefore 30 x 24 x 60 x 60 seconds
duck3_anim <- duck3_df[which(duck3_df$date>=mindate & duck3_df$date<(mindate+(30*24*60*60))),]

# Create a move object
duck3_move <- move(x=duck3_anim$Long, y=duck3_anim$Lat, 
                   time=as.POSIXct(duck3_anim$date, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                   proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                   data=duck3_anim, animal=duck3_anim$ID)

# Align data to a uniform time scale, let;s say every 60 min
m <- align_move(duck3_move, res = 60, unit = "mins")
# # create spatial frames with a OpenStreetMap map
frames <- frames_spatial(m, map_service = "osm", map_type = "streets_de") %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()
# Check one of the frames, for example frame 10
frames[[10]] 
# Build animation
animate_frames(frames, out_file = "moveVis.gif")


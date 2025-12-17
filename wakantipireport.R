###report for Wakan Tipi bumble bee surveys

library(dplyr)
library(vegan)
library(fossil)
library(pivottabler)
library(janitor)
library(viridis)
library(bipartite)
library(ggplot2)
library(ellipsis)
library(tidyr)


####Data all bumble bee data 2022 to 2024 and survey effort
#######################################
bees=read.csv("atlas_wakan_tipi_observations.csv")


effort=read.csv("abundanceperevent.csv")
##speciesmatrix
#####################################
matrix <- create.matrix(bees, tax.name = "vernacularName",
                              locality = "eventID",
                        time.col=NULL,
                        time=NULL,
                              abund.col = "individualCount",
                              abund = TRUE)

##species accumulation
#########################################
sp1=specaccum(matrix, method = "exact", permutations = 100,
              conditioned =TRUE, gamma = "jack1",  w = NULL)

sppaccplott=plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Collection", ylab = "Bumble bee richness +/- 95% CI")

#table by year

year <- create.matrix(bees, tax.name = "vernacularName",
                      locality = "year",
                      time.col=NULL,
                      time=NULL,
                      abund.col = "individualCount",
                      abund = TRUE )

pt <- PivotTable$new()
pt$addData(bees)
pt$addColumnDataGroups("year")
pt$addRowDataGroups("vernacularName")
pt$defineCalculation(calculationName="individualCount", summariseExpression="sum(individualCount)")
pt$renderPivot()


#### relative abundance
####################################################

relabund=bees %>%
  tabyl(year, vernacularName) %>%
  adorn_totals("col") %>% 
  adorn_percentages() 

relabund=adorn_rounding(relabund, digits = 2, rounding = "half to even")

###transpose
transprelabund=relabund[-1] %>% t() %>% as.data.frame() %>% setNames(relabund[,1])

#####format table outside of R with relative abudance



##bee abundance over time
############################################
fm1=lm(abundance ~ year, data = effort)
fm1
summary(fm1)
plot(fm1)

fm2=aov(sqrt(abundance) ~ year, data = effort)
summary(fm2)
plot(fm2)

boxplot(effort$abundance ~ effort$year,xlab="Year", ylab = "Average abundance per collection" )

# Add data points
mylevels <- levels(effort$abundance)
levelProportions <- summary(effort$abundance)/nrow(data)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- data[data$names==thislevel, "value"]
  
  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 


###plants visited by bees....graphs with relative abundance of bees on flowers. 
###heat color with bb species richness





###proportion of visits to native and non-native plants. compare ritter across years.



##interaction plots....use bipartite package
  #########################################################

###getting data into matrix of plant, species, abudnance
  
beenetwork=bees
networktable=   beenetwork%>%
      group_by(associatedTaxa, specificEpithet) %>%
    summarize(abundance = sum(individualCount))  

##going from long to wide
plotnetwork=networktable %>% pivot_wider(., names_from = specificEpithet, values_from = abundance) %>% replace(is.na(.),0)
#remove obs with no flowers
plotnetwork=subset(plotnetwork, associatedTaxa !="")
###making plot 
plotweb(plotnetwork)

##wont make plot with column of plant labels, forcing to integer, removing column
plotnetworkinteger <- as.matrix(sapply(plotnetwork, as.integer))

###making plot ...works but doesn't have plant names
plotweb(plotnetworkinteger)




        
####extra.....row names for plants
row.names(plotnetworkinteger)<-c("Agastache foeniculum","Asclepias incarnata",
                                 "Asclepias verticillata","Centaurea stoebe",
                                 "Chamaecrista fasciculata","Cirsium discolor",
                                 "Dalea purpurea","Eutrochium maculatum",
                                 "Eutrochium purpureum","Helianthus tuberosus",
                                 "Impatiens capensis","Lobelia siphilitica",
                                 "Lythrum salicaria","Monarda fistulosa",
                                 "Nepeta cataria","Pycnanthemum virginianum",
                                 "Securigera varia","Silphium perfoliatum",
                                 "Solidago canadensis","Verbena hastata",
                                 "Verbena stricta","Veronicastrum virginicum")




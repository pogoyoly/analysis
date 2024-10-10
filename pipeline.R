library(LGrafEU)
library(raster)
library(terra)
library(poll4pop)
#############
# step 1 create map
############
#mean field size 1-3 HA
#field sd 1-3 HA
#cover of arable land 50-90%
# % arabal land considered as perm grassland 0-5
# seminal natural habitat 1 - arable area randomly divided between forest and grassland

calcGras<- 5
forest <- 5
arable <- 100 - calcGras - forest

perlin<-generate_perlin_noise(200,200,1,2,3,0.01,TRUE,cat_method = "land_percentage", percetange= (100 - forest))
#raster::plot(r)
test<-establish_by_place_conquer(potential_space= perlin,
                                 cell_size=10,
                                 includsion_value = 1,
                                 mean_field_size = 500,
                                 sd_field_size = 25,
                                 distribution = "norm",
                                 mean_shape_index = 1,
                                 sd_shape_index = 0.3,
                                 percent = ( 100 - (calcGras / (100 - forest)) * 100 ),
                                 assign_farmers = TRUE,
                                 assign_mode = 2,
                                 mean_fields_per_farm = 3,
                                 sd_fields_per_farm = 3)



#raster::plot(test$map)


field_map<-test$map
raster::values(field_map) <- ifelse(raster::values(field_map) > 0, 1, NA)
#raster::plot(field_map,add = TRUE)
polygons <- raster::rasterToPolygons(test$map, n=8,fun=function(x){x > 0}, na.rm=TRUE, digits=12, dissolve=TRUE)
#raster::plot(polygons, add = TRUE, border = "black", lwd = 1)


#############
# step 2 distribute crops (needs to be done each year)
############

crops_matrix <- data.frame(crop = c("winter wheat", "winter barley", "summer barley", "maize" ,"oilseed", "peas", "sugar beet", "potatoe", "alfalfa", "lay", "flower strips"),
                           percentage = c(0.34, 0.21, 0.16, 0.06, 0.15, 0.0, 0.05, 0.03, 0.0, 0.0, 0),
                           index = c(1,2,3,4,5,6,7,8,9,10,11))  # Desired percentages
field_map<-distrubution_by_percent(test,crops_matrix)
field_map_with_crops<-plot_by_crop(field_map,method = 2)


#############
# step 4 add the field edges and the edge effect map as well as the forest and calcarous grasland
############
#edge width 0-10
edge <- 4
field_map_dist<-raster::disaggregate(field_map_with_crops, fact=10)
r <- raster::raster(raster::extent(field_map_dist), resolution = 1)
lines <- as(polygons, "SpatialLines")
distance_buffer <- raster::buffer(lines, width = edge)
rasterized_lines <- raster::rasterize(distance_buffer, r,field=1)
#raster::plot(rasterized_lines)
rasterized_lines[is.na(rasterized_lines[])] <- 0
#plot(rasterized_lines)
#raster::values(rasterized_lines) <- ifelse(raster::values(rasterized_lines) == 1, 10, 0)

edge_adder <- function(x, y) {
  y[x == 1] <- 12  # If x equals 1, set y to 11
  return(y)        # Return the modified y values
}

# Apply the function using overlay
field_map_dist_w_edge <- overlay(rasterized_lines, field_map_dist, fun = edge_adder)


#edge effect map
edge_effect <- 25
edge_effect <- edge + edge_effect
s <- sf::st_as_sf(lines)
distance_buffer_effect <- sf::st_buffer(s, dist = edge_effect)
rasterized_lines_effect <- raster::rasterize(distance_buffer_effect, r,field=1)
rasterized_lines_effect[is.na(rasterized_lines_effect[])] <- 0
#plot(rasterized_lines_effect)
#raster::values(rasterized_lines_effect) <- ifelse(raster::values(rasterized_lines_effect) == 1, 1, 0)


#add the calcerous and the grassland to the maps
forcalcsum <- function(x, y) {
  z <- x * -2  + y * - 1
  z <- z + 15
  return(z)        # Return the modified y values
}

calcr<-plot_by_arable_land(test, method = 2)
perlin_dis<-raster::disaggregate(perlin, fact=10)
calcr<-raster::disaggregate(calcr, fact=10)
extent(perlin_dis)<-extent(calcr)
for_and_calc <- overlay(calcr,perlin_dis, fun = forcalcsum)
for_and_calc[for_and_calc < 13] <- 0

full_stack<-stack(for_and_calc,field_map_dist_w_edge)
full_maping <- function(stack) {
  x<-stack[[1]]
  y<-stack[[2]]
  if(x > 0){
    j <-x
  }
  else{
    j <- y
  }
  return(j)        # Return the modified y values
}
full_map <- calc(full_stack, full_maping)

#plot(for_and_calc)


#############
# step 5 setup the price/yield/cost metrixes
############
yield_dt_per_hecatre<-c(74.3,68.2,50.0,96.5,35.8,52.2,797.3,438.5,0,0,0,0,0,0)
yield_dt_per_cell<-yield_dt_per_hecatre / 10000
price_per_dt<-c(27.81,22.74,28.95,27.03,56.64,32.53,5.58,24.32,0,0,0,0,0,0)

nutrient_removal_kg_per_dt_N<-c(2.11,1.65,1.38,1.38,3.35, -4.5, 0.18, 0.35 ,0 ,0,0,0,0,0)

nutrient_removal_kg_per_dt_PO <-c(0.8,0.8,0.8,0.8,1.8,1.0,0.1,0.14,0,0,0,0,0,0)

nutrient_removal_kg_per_dt_K<-c(0.55,0.6,0.6,0.5,1.0,1.0,0.25,0.6,0,0,0,0,0,0)

fertilizer_cost_per_kg<-c(2.22,1.18,1.38,0)

yield_dependant_cost_per_dt<-c(4.04,4.04,4.04,5.67,25.96,26.47,0,1.29,0,0,0,0,0,0,0)

other_variable_costs_per_hectare<-c(632.4,658.1,572.2,744.1,624.7,577.2,1339,2197.9,0,0,0,0,0,0,0)
other_variable_costs_per_cell<-other_variable_costs_per_hectare/10000
government_payments<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)


#############
# step 6 run run the polinator model
############
#plot(field_map_dist_w_edge)
reindex_for_poll_model = c(30,30,30,32,33,37,42,38,37,10,0,11,4,22)  # Desired percentages
reindex_raster<-function(cell_val){
  if(cell_val == 0){
    val<- 0
  }
  else{
    val <- reindex_for_poll_model[cell_val]
  }
  return(val)
}
poll_raster <- calc(full_map, reindex_raster)

#plot(poll_raster)

r_grassmargins <- full_map
r_grassmargins[full_map > 0] <- 0

r_flowermargins <- full_map
r_flowermargins[full_map > 0] <- 0

new_param<-poll4pop::parameters

nf<-computeFloralNesting(landuseMap=full_map, edgesMap=r_grassmargins, unitEdges = "sqm", widthEdges=1,
                         landuseCodes, bees=c("GroundNestingBumblebees", "GroundNestingSolitaryBees"), num_floral=3,
                         florNestInfo=new_param$florNestInfo, codeEdges=c(5), cell.size = 1,paramList=new_param)

#plot(nf$nest$GroundNestingBumblebees)

poll<-runpoll_3seasons(M_poll0 = numeric(0), firstyear=TRUE, firstyearfactor = c(1, 1),
                       bees = c("GroundNestingBumblebees", "GroundNestingSolitaryBees"), cell.size = 1, paramList=new_param, nest=nf$nest,
                       floral=nf$floral, cutoff = 0.99, loc_managed)

nf2<-computeFloralNesting(landuseMap=full_map, edgesMap=r_grassmargins, unitEdges = "sqm", widthEdges=1,
                          landuseCodes, bees=c("GroundNestingBumblebees", "GroundNestingSolitaryBees"), num_floral=3,
                          florNestInfo=new_param$florNestInfo, codeEdges=c(5), cell.size = 1,paramList=new_param)


poll2<-runpoll_3seasons(M_poll0 = poll$M_poll, firstyear=FALSE, firstyearfactor = c(1, 1),
                        bees = c("GroundNestingBumblebees", "GroundNestingSolitaryBees"), cell.size = 1, paramList=new_param, nest=nf2$nest,
                        floral=nf2$floral, cutoff = 0.99, loc_managed)



service_stack <- stack(poll2$flowvis$GroundNestingBumblebees[[2]],
                       poll2$floral$GroundNestingBumblebees[[2]],full_map)

devider <- function(stack) {
  val1<-stack[[1]]
  val2<-stack[[2]]
  val3<-stack[[3]]
 if(val3 == 5){
  j<- val1
  }
  else{
    j <- 0
  }

  return(j)
}

# Apply the function using overlay
pollination_service <- calc(service_stack, devider)

#plot(pollination_service)

values_greater_than_zero <- pollination_service[pollination_service > 0]


# Calculate the mean of values greater than 0
max_visitation <- max(values_greater_than_zero, na.rm = TRUE)




#############
# step 7 run calculate_gross_margin_grid_cell so it calculates gross margin on each cell and return a corresponding matrix
############

 # Final economic model for each grid cell
 calculate_gross_margin_grid_cell <- function(cell_value,edge_value,edge,visitation,max_visitation,yield_dt_per_cell,price_per_dt,nutrient_removal_kg_per_dt_N,
                                              nutrient_removal_kg_per_dt_PO, nutrient_removal_kg_per_dt_K,fertilizer_cost_per_kg,
                                              yield_dependant_cost_per_dt, other_variable_costs_per_cell, government_payments) {


   yields <- yield_dt_per_cell[cell_value]
   #correct for yield depression on edges
   if(edge_value == 1){
     yields <- yields * 0.952
   }
   #correct for pollination
   if(cell_value == 5 ){
     yields <- adjust_yield_for_pollination(yields, 0.21, visitation, max_visitation)
   }

   price <- price_per_dt[cell_value]
   nutrient_removal_N <- nutrient_removal_kg_per_dt_N[cell_value]
   nutrient_removal_PO <- nutrient_removal_kg_per_dt_PO[cell_value]
   nutrient_removal_K <- nutrient_removal_kg_per_dt_K[cell_value]
   fertilizer_cost_per_kg
   yield_cost <- yield_dependant_cost_per_dt[cell_value]
   other_cost <- other_variable_costs_per_cell[cell_value]
   gov_payment <- government_payments[cell_value]


   # Calculate revenue
   revenue <- calculate_revenue(yields, price, gov_payment)

   # fertlizer cost + adjusted for nitrogen fixation
   fert_amounts<-calculate_fertilizer_amount(yields, nutrient_removal_N,
                                             nutrient_removal_PO, nutrient_removal_K,edge_value,edge)

   # Total fertilizer costs
   fert_cost <- calculate_total_fertilizer_costs(fert_amounts, fertilizer_cost_per_kg)

   # Yield-dependent costs
   yield_costs <- calculate_yield_costs(yields, yield_cost)

   # Other variable costs
   other_costs <- calculate_other_variable_costs(other_cost)

   # Gross margin
   gross_margin <- calculate_gross_margin(revenue, fert_cost, yield_costs, other_costs)

   return(gross_margin)
 }

 apply_function_to_raster <- function(raster_stack) {

   cell_value <- raster_stack[[1]]
   edge_value <- raster_stack[[2]]
   visitation <- raster_stack[[3]]


   if (is.na(cell_value) || cell_value ==0) {
     return(NA)  # Handle NA values
   }
   return(calculate_gross_margin_grid_cell(cell_value,edge_value,edge,visitation,max_visitation,
                                           yield_dt_per_cell, price_per_dt,
                                           nutrient_removal_kg_per_dt_N, nutrient_removal_kg_per_dt_PO,
                                           nutrient_removal_kg_per_dt_K, fertilizer_cost_per_kg,
                                           yield_dependant_cost_per_dt, other_variable_costs_per_cell,
                                           government_payments))
 }

 # Use calc to apply the function across the raster
 s<-raster::stack(full_map,rasterized_lines_effect,pollination_service)
 output_raster <- calc(s, apply_function_to_raster)
 #raster::plot(pollination_service)


 raster_sum <- cellStats(output_raster, stat = 'sum')
 raster_sum/ (400 * (arable/ 100)) #400 hectares in the landscape multiplied by the percentage of arable land
#############
# step 7 arrange a full list of matrices of intrest in a dataframe
############

#total edge length
#OSR yiled per landscape
#gross margin per landscape
#OSR yeild per hecater
#gross margin per hectare
#forest cover
#calcerous grassland cover
#field size mean/sd
#field shape mean/sd
#edge size
# contribution of insect polination
# contribution of insect polination to gross margin
# gerneral bee indexes from poll4pop

#########################
# all functions for the econ model
########################

# Input: Revenue, fertilizer costs, yield-dependent costs, other costs for each grid cell
calculate_gross_margin <- function(revenue, fert_cost, yield_cost, other_cost) {
  # Total variable costs
  total_variable_costs <- fert_cost + yield_cost + other_cost

  # Gross margin
  gross_margin <- revenue - total_variable_costs
  return(gross_margin)
}


# Input: Yields, sales prices, gov payments, crop shares
calculate_revenue <- function(yields, sales_prices, gov_payments) {
  # Calculate revenue for each crop
  revenue_crops <- (yields * sales_prices + gov_payments)

  # Total revenue by summing crop-specific revenues
  total_revenue <- sum(revenue_crops)
  return(total_revenue)
}


# Input: Expected yields, nutrient removal, edge factor, crop shares
calculate_fertilizer_amount <- function(yields, nutrient_removal_N,
                                        nutrient_removal_PO, nutrient_removal_K,edge_factor,edge ) {

  edge_coef <- 1
  if(edge_factor == 1 && edge > 5){
    edge_coef <- 1.052
  }
  if(edge_factor == 1 && edge < 5){
    edge_coef <- 1.052
  }


  # Calculate fertilizer amount for each crop and nutrient
  fert_amounts_N <- yields * nutrient_removal_N * edge_coef
  fert_amounts_PO <- yields * nutrient_removal_PO * edge_coef
  fert_amounts_K <- yields * nutrient_removal_K * edge_coef

  fert_amounts <- c(fert_amounts_N,fert_amounts_PO,fert_amounts_K)

  return(fert_amounts)
}


# Input: Fertilizer costs, nitrogen fixation yields, nitrogen surplus rates, crop shares
adjust_fertilizer_amounts_nitrogen <- function(fert_amounts_nitrogen,yields, nitrogen_surplus_rate) {
  # Adjust fertilizer costs by subtracting nitrogen fixation
  adjusted_fert_costs <- fert_amounts_nitrogen - (yields * nitrogen_surplus_rate)
  return(adjusted_fert_costs)
}


# Input: Fertilizer amounts, nutrient prices
calculate_total_fertilizer_costs <- function(fert_amounts, nutrient_prices) {
  # Total fertilizer costs
  total_fertilizer_costs <- fert_amounts[1] * nutrient_prices[1] +fert_amounts[2] * nutrient_prices[2] +fert_amounts[3] * nutrient_prices[3]
  return(total_fertilizer_costs)
}


# Input: Yields, cost per yield unit
calculate_yield_costs <- function(yields, cost_per_yield) {
  # Yield-dependent costs
  yield_costs <- yields * cost_per_yield
  return(yield_costs)
}


# Input: Crop shares, cost per hectare
calculate_other_variable_costs <- function(cost_per_area) {
  # Other variable costs
  other_costs <- sum(cost_per_area)
  return(other_costs)
}


# Final economic model for each grid cell
calculate_gross_margin_grid_cell <- function(yields, sales_prices, gov_payments,
                                             nutrient_removal,nutrient_prices, cost_per_yield,
                                             cost_per_area,edge_factor) {
  # Calculate revenue
  revenue <- calculate_revenue(yields, sales_prices, gov_payments)

  # fertlizer cost + adjusted for nitrogen fixation
  fert_amounts<-calculate_fertilizer_amount(yields, nutrient_removal, edge_factor)

  # Total fertilizer costs
  fert_cost <- calculate_total_fertilizer_costs(fert_amounts, nutrient_prices)

  # Yield-dependent costs
  yield_cost <- calculate_yield_costs(yields, cost_per_yield)

  # Other variable costs
  other_cost <- calculate_other_variable_costs(cost_per_area)

  # Gross margin
  gross_margin <- calculate_gross_margin(revenue, fert_cost, yield_cost, other_cost)
  return(gross_margin)
}






#########################
# yield model
########################

#this equations is not reproducable from the paper
calculate_yield <- function(site_quality, coefficients) {
  # coefficients: a vector of length 2 (intercept, slope)
  intercept <- coefficients[1]
  slope <- coefficients[2]

  # Calculate yield based on the site quality
  yields <- intercept + slope * site_quality
  return(yields)
}


adjust_yield_for_pollination <- function(yield, sensitivity, visitation, max_visitation) {
  # Adjust yield for pollination effect
  adjusted_yield <- (1 - sensitivity) * yield - yield * sensitivity * ((visitation - max_visitation) / max_visitation)
  return(adjusted_yield)
}

adjust_yield_for_pollination(yield_dt_per_cell[5], 0.21, 0.0 ,0.4146886)

adjust_yield_for_edge_effects <- function(yield, edge_factor) {

  #0.952 for cells within a 25 meter radius of cell
  # Adjust yield for edge effects
  adjusted_yield <- yield * edge_factor
  return(adjusted_yield)
}


#since calculation of yields with coefficients was not reproducable
#the yields were taken from
#https://www.destatis.de/EN/Themes/Economic-Sectors-Enterprises/Agriculture-Forestry-Fisheries/Field-Crops-Grassland/Tables/field-crops-and-grassland-comparison.html#61776
#https://www.destatis.de/EN/Themes/Economic-Sectors-Enterprises/Agriculture-Forestry-Fisheries/Fruit-Vegetables-Horticulture/Tables/3-2-holdings-agruciltural-area-yield-harvest-volume.html
#and site quality was assumed to be homogenous for all site
#
# data from 2023 of dt per hectare
#
# winter wheat 74.3
# winter barley 68.2
# summer barley 50.0
# maize 96.5
# ########silage maze 421.3
# autumn oilseed rape 35.8
# peas  52.2
# sugar beet 797.3
# potato 438.5
# alfalfa
# set aside
# flowering strips
#
yield_dt_per_hecatre<-c(74.3,68.2,50.0,96.5,35.8,52.2,797.3,438.5,0,0)
#
#historical commodety prices taken from
#https://www.stmelf.bayern.de/idb/default.html
#
#
# data from 2021-2023 of euro per deciton
#
# winter wheat 27.81
# winter barley 22.74
# summer barley 28.95
# maize 27.03
# autumn oilseed rape 56.64
# peas  32.53
# sugar beet 5.58
# potato 24.32
# alfalfa
# set aside
# flowering strips
#
price_per_dt<-c(27.81,22.74,28.95,27.03,56.64,32.53,5.58,24.32,0,0)
#
#nutrient removal
#https://www.stmelf.bayern.de/idb/default.html
#               kg/dt	       N      P2O5    K2O
# winter wheat               2.11   0.8     0.55
# winter barley              1.65   0.8     0.6
# summer barley              1.38   0.8     0.6
# maize                      1.38   0.8     0.5
# autumn oilseed rape        3.35   1.8     1.0
# peas                       -4.5    1.0     1.0
# sugar beet                 0.18   0.1     0.25
# potato                     0.35   0.14    0.6
# alfalfa
# set aside
# flowering strips
#
nutrient_removal_kg_per_dt_N<-c(2.11,1.65,1.38,1.38,3.35, -4.5, 0.18, 0.35 ,0 ,0)
nutrient_removal_kg_per_dt_PO <-c(0.8,0.8,0.8,0.8,1.8,1.0,0.1,0.14,0,0)
nutrient_removal_kg_per_dt_K<-c(0.55,0.6,0.6,0.5,1.0,1.0,0.25,0.6,0,0)

#fertlizer cost euro/kg
#
# N     2.22
# P2O5	1.18
# K2O   1.38

fertilizer_cost_per_kg<-c(2.22,1.18,1.38)


# yield dependant costs per dt
#
# winter wheat   4.04
# winter barley  4.04
# summer barley  4.04
# maize          5.67
# oilseed rape   25.96
# peas           26.47
# sugar beet     0
# potato         1.29
# alfalfa
# set aside
# flowering strips

yield_dependant_cost_per_dt<-c(4.04,4.04,4.04,5.67,25.96,26.47,0,1.29,0,0,0)


# other variable costs per hectar (seeds pestisides machinery insurance)
#
# winter wheat   102.4 178.7 316.4 34.9  632.4
# winter barley  122.0 193.3 309.8 33.0  658.1
# summer barley  116.1 123.2 304.4 28.5  572.2
# maize          216.6 128.0 344.0 55.5  744.1
# oilseed rape   69.3  147.7 336.3 71.4  624.7
# peas           118.3 100.9 301.7 56.3  577.2
# sugar beet     298.2 452.9 491.5 96.4  1339
# potato         973.0 491.7 569.3 163.9 2197.9
# alfalfa
# set aside
# flowering strips

other_variable_costs_per_hectare<-c(632.4,658.1,572.2,744.1,624.7,577.2,1339,2197.9,0,0,0)

#share of crops in germany
#https://www.destatis.de/EN/Themes/Economic-Sectors-Enterprises/Agriculture-Forestry-Fisheries/Field-Crops-Grassland/Tables/field-crops-and-grassland-comparison.html#61772
#5,775.9 + 289.3 + 441.9 + 1,091.9 = 7599
#wheat 0.34
#barley 0.21
#maize 0.06
#rape 0.15
#potatoe 0.03
#sugar beet 0.05

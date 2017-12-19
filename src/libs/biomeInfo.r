
	#1	"Tropical & Subtropical Moist Broadleaf Forests", 
#2	"Tropical & Subtropical Dry Broadleaf Forests", #
#3	"Tropical & Subtropical Coniferous Forests", 
#4	"Temperate Broadleaf & Mixed Forests",
#5	"Temperate Conifer Forests", 
#6	"Boreal Forests/Taiga",
#7	"Tropical & Subtropical Grasslands Savannas & Shrublands", 
#8	"Temperate Grasslands, Savannas & Shrublands",
#9	"Flooded Grasslands & Savannas", 
#10	"Montane Grasslands & Shrublands",
#11	"Tundra", 
#12	"Mediterranean Forests, Woodlands & Scrub",
#13	"Deserts & Xeric Shrublands",
#14	"Mangroves")

biomes = list("global" = NULL,
			  "tropical\nwet forest" = c(1, 3),
			  "tropical\ndry forest" = 2,
			  "tropical\nsavanna/\ngrassland" = c(7, 9),
			  "mediterranean\nforest/woodland\nand scrub" = 12,
			  "temperate\nforest and\nwoodland" = c(4, 8, 5),
			  "boreal\nforests" = 6,
			  "shrublands" = c(10, 11, 13))
			  
biomesCols = c("global" = "black",
			   "tropical\nwet forest" = "blue",
			   "tropical\ndry forest" = "#459900",
			   "tropical\nsavanna/\ngrassland" = "red",
			   "mediterranean\nforest/woodland\nand scrub" = "magenta",
			   "temperate\nforest and\nwoodland" = "green",
			   "boreal\nforests" = "cyan",
			   "shrublands" = "orange")



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

biomes = list("Global" = NULL,
              "Tropical\nwet forest" = c(1, 3),
              "Tropical\ndry forest" = 2,
              "Tropical\nsavanna/\ngrassland" = c(7, 9),
              "Mediterranean\nforest/woodland\nand scrub" = 12,
              "Temperate\nforest and\nwoodland" = c(4, 8, 5),
              "Boreal\nforests" = 6,
              "Shrublands" = c(10, 11, 13))
			  
biomesCols = c("Global" = "black",
	       "Tropical\nwet forest" = "blue",
               "Tropical\ndry forest" = "#459900",
               "Tropical\nsavanna/\ngrassland" = "red",
               "Mediterranean\nforest/woodland\nand scrub" = "magenta",
               "Temperate\nforest and\nwoodland" = "green",
               "Boreal\nforests" = "cyan",
               "Shrublands" = "orange")


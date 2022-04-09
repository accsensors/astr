## code to prepare `create_color_palettes` dataset goes here

jtcp <- c("#FF6633", #orange
          "#0099CC", #teal3
          "#33CC99", #teal1
          "#FF3366", #pink2
          "#330099", #purple
          "#CC0066", #pink1
          "#339999", #teal2
          "#FF9900") #gold


jvcp <- c("#CC689A", #purple1
          "#0099CC", #blue
          "#FF3366", #pink
          "#33CC99", #green
          "#FF6633", #orange
          "#FFCC00", #yellow
          "#CC0066", #maroon
          "#175E46", #green2
          "#339999",
          "#FF9900",
          "#330099",
          "#70AFC4",
          "#664d99",
          "#CC6633",
          "#801832",
          "#FFE380")









#scale_fill_manual(values = jtcp)
# To use for line and point colors, add
#scale_colour_manual(values=jvcp)

usethis::use_data(jtcp, jvcp, overwrite = TRUE)

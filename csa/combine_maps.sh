cd ~/temp-sentiment/res

mkdir conv
cd conv

#Crop out all whitespace
convert ../map_wbgt.png -trim +repage map_wbgt.png
convert ../map_wbgt_income.png -trim +repage map_wbgt_income.png
convert ../map_wbgt_black.png -trim +repage map_wbgt_black.png

#Add some margins back (but narrower)
convert map_wbgt.png -gravity center -background white -extent $(identify -format '%[fx:W+50]x%[fx:H+50]' map_wbgt.png) map_wbgt.png
convert map_wbgt_income.png -gravity center -background white -extent $(identify -format '%[fx:W+50]x%[fx:H+50]' map_wbgt_income.png) map_wbgt_income.png
convert map_wbgt_black.png -gravity center -background white -extent $(identify -format '%[fx:W+50]x%[fx:H+50]' map_wbgt_black.png) map_wbgt_black.png

#Add labels
convert map_wbgt.png -gravity NorthWest -pointsize 200 -font "Arial-Bold" -annotate +50+50 'A' map_wbgt.png
convert map_wbgt_income.png -gravity NorthWest -pointsize 200 -font "Arial-Bold" -annotate +50+50 'B' map_wbgt_income.png
convert map_wbgt_black.png -gravity NorthWest -pointsize 200 -font "Arial-Bold" -annotate +50+50 'C' map_wbgt_black.png

#Stack
convert -append map_wbgt.png map_wbgt_income.png map_wbgt_black.png ../map_combined.png

cd ..
rm -rf conv

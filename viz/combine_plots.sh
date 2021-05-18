cd ~/temp-sentiment/res

mkdir conv
cd conv

#Add labels
convert ../wbgt-income.png -gravity NorthWest -pointsize 125 -font "Arial-Bold" -annotate +25+25 'a' wbgt-income.png
convert ../wbgt-race_q.png -gravity NorthWest -pointsize 125 -font "Arial-Bold" -annotate +25+25 'b' wbgt-race_q.png

#Stack
convert +append wbgt-income.png wbgt-race_q.png ../wbgt_combined.png

cd ..
rm -rf conv

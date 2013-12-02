CLIENTS=5
ENTRIES=10
UPDATES=10
TIME=3

rm out.txt
#for i in `seq 1 10`;
#do
	#echo "running with $i writeops"
	erl -compile client
	erl -run opty start $CLIENTS $ENTRIES $UPDATES $TIME -run c q >> out.txt
	#sed -i '2,4d' out.txt
	#sed -i '12,12d' out.txt
#done
cat out.txt

rm Population*.o


(
for i in 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48
do
  echo Population${i}.hs
  ghc -c Population${i}.hs
done
)  >>times.txt 2>&1 &

(
for i in 0 5 8 11 14 17 20 23 26 29 32 35 38 41 44 47
do
  echo Population${i}.hs
  ghc -c Population${i}.hs
done
)  >>times.txt 2>&1 &


for i in 1 2 4 7 10 13 16 19 22 25 28 31 34 37 40 43 46 49
do
  echo Population${i}.hs
  ghc -c Population${i}.hs
done >>times.txt 2>&1

ghc -o succ --make succ.hs >>times.txt 2>&1


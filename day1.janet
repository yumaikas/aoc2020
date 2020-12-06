(def datastr (slurp "day1-data.txt"))
(def lines (string/split "\n" datastr))
(def nums (map (fn [x] (scan-number (string/trim x))) lines))

(defn part1 [nums] 
  (var ans 0)
  (each n nums (each m nums 
      (if (= (+ n m ) 2020) 
        (do 
          (set ans (* n m ))
          (break))
      )
    )
  )
  ans
)


(defn part2 [nums]
  (var ans 0)
  (each n nums (each m nums (each o nums
        (if (= (+ n m o) 2020) 
          (do 
             (set ans (* n m o))
             (break)
          )
        )
      )
    )
  )
  ans
)

(pp (part1 nums))
(pp (part2 nums))

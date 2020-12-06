(def datastr (slurp "day5-data.txt"))
(def tickets (string/split "\n" datastr))


(def ticket-spec 
  (peg/compile '{
      :row (capture (between 7 7 (choice "F" "B")))
      :seat (capture (between 3 3 (choice "L" "R")))
      :main (sequence :row :seat)
 })
)

# Parse binary numbers, using cmp1 to represent a 1 binary number
(defn bin-parse [str cmp1] 
  (var val 0)
  (var mult 1)
  (each c (reverse str)
    (def ch (string/from-bytes c))
    (if 
      (= ch cmp1) (+= val mult)
    )
    (*= mult 2)
  )
  val
)

(defn row-of [str] (bin-parse str "B"))
(defn seat-of [str] (bin-parse str "R"))

(var high-id 0)
(var low-id 10_000)
(var ids @[])

# Parse each ticket and turn it into a seat-id
(each t tickets
  (def mat (peg/match ticket-spec t))
  (def row (row-of (mat 0)))
  (def col (seat-of (mat 1)))
  (def seat-id (+ (* row 8) col))
  (set high-id (max high-id seat-id))
  (set low-id (min low-id seat-id))
  (array/push ids seat-id)
)
# Sort them, so we can find gaps
(sort ids)

# Find the gap in the sequence
(var prev-id (- low-id 1))
(each i ids 
  (if (> (- i prev-id) 1)
    (do
      (set prev-id (- i 1))
      (break)
    )
  )
  (set prev-id i)
)

# Answer to part 1
(pp high-id)
# Answer to part 2
(pp prev-id)


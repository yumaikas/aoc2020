(def day3 (file/open "day3-data.txt" :r))
(def datastr (file/read day3 :all))

(defn not-blank [l] (not (= l "")))
(def lines (filter not-blank (string/split "\n" datastr)))

(def width (-> 0 (lines) (length)))

(def slope @{:dx 3 :dy 1})

(defn obj-at [lines loc] 
  (def ch (string/from-bytes ((lines (loc :y)) (loc :x))))
  (cond 
    (= ch ".") :OPEN
    (= ch "#") :TREE
    true (error "!!!")
  )
)


(defn count-trees [lines slope] 
  (var loc @{:x 0 :y 0})
  (def num-rows (length lines))
  (var num-trees 0)

  (while (> num-rows (loc :y))
    (if (= (obj-at lines loc) :TREE) (++ num-trees))
    # Increment y position
    (put loc :y (+ (loc :y) (slope :dy)))
    (put loc :x (mod (+ (loc :x) (slope :dx)) width))
  )
  num-trees
)

(pp (count-trees lines slope))

(defn <slope> [dx dy] @{:dx dx :dy dy})

(def slopes
  @[
    (<slope> 1 1)
    (<slope> 3 1)
    (<slope> 5 1)
    (<slope> 7 1)
    (<slope> 1 2)
])

(var res 1)

(each sl slopes (*= res (count-trees lines sl)))

(pp res)

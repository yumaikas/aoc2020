(def datastr (slurp "day6-data.txt"))
(def qgroups (string/split "\r\n\r\n" datastr))

(defn tbl-default [tbl key fallback] 
  (def val (tbl key))
  (default val fallback)
  val
)

(defn part1 [qgroups] 
  (var ans-sum 0)
  (each g qgroups
    (def yes-ans @{})
    (each p (string/split "\n" g)
      (each ans (string/trim p)
        (def answer (string/from-bytes ans))
        (put yes-ans answer 1)
      )
    )
    (+= ans-sum (sum (values yes-ans)))
  )
  ans-sum
)

(defn part2 [qgroups] 
  (var ans-sum 0)
  (each g qgroups
    (def yes-ans @{})
    (var group-cnt 0)
    (each p (string/split "\n" g)
      (each ans (string/trim p)
        (def answer (string/from-bytes ans))
        (put yes-ans answer (+ 1 (tbl-default yes-ans answer 0)))
      )
      (++ group-cnt)
    )
    (var match-ans 0)
    (each cnt (values yes-ans) 
      (if (= cnt group-cnt) 
        (++ match-ans)
      )
    )

    (+= ans-sum match-ans)
  )
  ans-sum
)

(pp (part1 qgroups))
(pp (part2 qgroups))

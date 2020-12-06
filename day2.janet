(def day2 (file/open "day2-data.txt" :r))
(def datastr (file/read day2 :all))
(def lines (string/split "\n" datastr))

(def password-spec '{
	:digit (range "09")
	:letterspec (sequence (capture (range "az") :letter) ":")
	:password (some (range "az"))

	:main (sequence 
            (capture (some :digit) :low)
            "-" (capture (some :digit) :high) 
            " " :letterspec " " 
            (capture :password :password) 
	)
})


(defn part-1 [spec lines] 
  (var sum 0)
  (each l lines
    (if (= l "") (break 0))
    (def spec (peg/match spec l))
    (def least (scan-number (spec 0)))
    (def most (scan-number (spec 1)))
    (def ch (spec 2))
    (def pw (spec 3))

    (def cnt (length (string/find-all ch pw)))

    # (pp @[least cnt most (<= least cnt most) (if (<= least cnt most) 1 0)])
    (+= sum (if (<= least cnt most) 1 0))
  )
  sum
)

# (pp (part-1 password-spec lines))

(defn part-2 [spec lines] 
  (var sum 0)
  (each l lines
    (if (= l "") (break 0))
    (def spec (peg/match spec l))
    (def ch1 (- (scan-number (spec 0)) 1))
    (def ch2 (- (scan-number (spec 1)) 1))
    (def ch (spec 2))
    (def pw (spec 3))

    (var num_matches 0)
    (if (= ch (string/slice pw ch1 (+ ch1 1))) (+= num_matches 1))
    (if (= ch (string/slice pw ch2 (+ ch2 1))) (+= num_matches 1))


    # (pp @[least cnt most (<= least cnt most) (if (<= least cnt most) 1 0)])
    (if (= num_matches 1) (+= sum 1))
  )
  sum
)

(pp (part-1 password-spec lines))
(pp (part-2 password-spec lines))


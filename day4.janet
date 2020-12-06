(def day4 (file/open "day4-data.txt" :r))
(def datastr (:read day4 :all))
(def entries (string/split "\n\n" datastr))

(def required-elems 
  @[ "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn ppass [pass] 
  (if (= nil pass) (break))
  (each re required-elems
    (pp (string re "->" (pass re)))
  )
)


(def passport-spec-light 
  (peg/compile ~{
      :dnl "\n\n"
      :nl "\n"

      :val (capture (some (choice "#" (range "09" "AZ" "az"))))
      ## Field specs
      :hgt (sequence (capture "hgt") ":" :val)
      :byr (sequence (capture "byr") ":" :val)
      :iyr (sequence (capture "iyr") ":" :val)
      :eyr (sequence (capture "eyr") ":" :val)
      :hcl (sequence (capture "hcl") ":" :val)
      :ecl (sequence (capture "ecl") ":" :val)
      :pid (sequence (capture "pid") ":" :val)
      :cid (sequence (capture "cid") ":" :val)

      :field (choice :hgt :byr :iyr :eyr :hcl :ecl :pid :cid)
      :main (some (sequence :field (any (choice " " :nl))))
  })
)

(defn part1 [entries] 
  (var pports @[])
  (each entry entries 
    (def mat (peg/match passport-spec-light entry))
    (cond
      (= mat nil) ()
      true (array/push pports (table ;mat))
    )
  )
  (var valid-pports 0)
  (each pass pports 
    (var exist-keys 0)
    (each re required-elems
      (if (pass re) (++ exist-keys))
    )
    (if (= exist-keys 7) (++ valid-pports))
  )
  valid-pports
)

(defn parseHeight [n t] 
  (def num (scan-number n))
  (cond 
    (and (= t "cm") (<= 150 num 193)) @{:type :CM :n num}
    (and (= t "in") (<= 59 num 76)) @{:type :IN :n num}
  )
)

(defn parse-byr [s] 
  (def num (scan-number s))
  (if (<= 1920 num 2002) num nil)
)

(defn parse-iyr [s] 
  (def num (scan-number s))
  (if (<= 2010 num 2020) num nil)
)

(defn parse-eyr [s] 
  (def num (scan-number s))
  (if (<= 2020 num 2030) num nil)
)

(def passport-spec-detailed
  (peg/compile ~{
      :dnl "\n\n"
      :nl "\n"

      ## Field specs
      :hgt-val (cmt (sequence (capture (some (range "09"))) (capture (choice "cm" "in"))) ,parseHeight)
      :hgt (sequence (capture "hgt") ":" :hgt-val)

      :byr-val (cmt (capture (between 4 4 (range "09"))) ,parse-byr)
      :byr (sequence (capture "byr") ":" :byr-val)

      :iyr-val (cmt (capture (between 4 4 (range "09"))) ,parse-iyr)
      :iyr (sequence (capture "iyr") ":" :iyr-val)

      :eyr-val (cmt (capture (between 4 4 (range "09"))) ,parse-eyr)
      :eyr (sequence (capture "eyr") ":" :eyr-val)

      :hcl-val (capture (sequence "#" (between 6 6 (range "09" "af"))))
      :hcl (sequence (capture "hcl") ":" :hcl-val)

      :ecl-val (capture (choice "amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
      :ecl (sequence (capture "ecl") ":" :ecl-val)

      :pid-val (capture (between 9 9 (range "09")))
      :pid (sequence (capture "pid") ":" :pid-val)


      :cid-val (capture (some (choice (range "az" "AZ" "09") "#")))
      :cid (sequence (capture "cid") ":" :cid-val)


      :field (choice :hgt :byr :iyr :eyr :hcl :ecl :pid :cid)
      :main (some (sequence :field (any (choice " " :nl))))
   })
)

(defn part2 [entries] 
  (var pports @[])
  (each entry entries 
    (def mat (peg/match passport-spec-detailed entry))
    (cond
      (= mat nil) ()
      true (array/push pports (table ;mat))
    )
  )
  (var valid-pports 0)
  (each pass pports 
    (var exist-keys 0)
    (each re required-elems
      (if (pass re) (++ exist-keys))
    )
    (if (= exist-keys 7) (++ valid-pports))
  )
  valid-pports
)

(pp (part1 entries))
(pp (part2 entries))




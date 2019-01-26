(ns structured-data)


(defn do-a-thing [x]
  (let [b (+ x x)]
    (Math/pow b b)))


(defn spiff [xs]
  (+ (get xs 0) (get xs 2)))


(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [[x y z]]
  (+ x z))


(defn point [x y]
  [x y])


(defn rectangle [bottom-left top-right]
  [bottom-left top-right])


(defn width [[[x1 y1] [x2 y2]]]
    (- x2 x1))


(defn height [[[x1 y1] [x2 y2]]]
    (- y2 y1))


(defn square? [[[x1 y1] [x2 y2]]]
    (= (- x2 x1) (- y2 y1)))


(defn area [[[x1 y1] [x2 y2]]]
    (* (- x2 x1) (- y2 y1)))


(defn contains-point? [ [[x1 y1] [x2 y2]] [px py] ]
    (and (<= x1 px x2) (<= y1 py y2)))


(defn contains-rectangle? [outer [bottom-left top-right]]
  (and (contains-point? outer bottom-left)
       (contains-point? outer top-right)))


(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (< 1 (author-count book)))


(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))


(defn alive? [author]
  (not (:death-year author)))


(defn element-lengths [xs]
  (map count xs))


(defn second-elements [xs]
  (map second xs))


(defn titles [books]
  (map :title books))


(defn monotonic? [xs]
  (or (apply <= xs) (apply >= xs)))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [xs x]
  (if (contains? xs x)
    (disj xs x)
    (conj xs x)))


(defn contains-duplicates? [xs]
  (not (= (count (set xs)) (count xs))))


(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))


(defn has-author? [book author]
  (contains? (:authors book) author))


(defn authors [books]
  (set (apply clojure.set/union
    (map :authors books))))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        date-string (if birth-year (str " (" birth-year " - " death-year ")") "")]
    (str name date-string)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (str (:title book) ", written by "
    (authors->string (:authors book))))


(defn books->string [books]
  (let [books-count (count books)
        books-string (cond
                       (= 0 books-count) "No books."
                       (= 1 books-count) "1 book. "
                       :else (str books-count " books. "))]
    (apply str books-string (interpose " " (map book->string books)))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (first (seq (filter (fn [author] (= (:name author) name)) authors))))


(defn living-authors [authors]
  (filter alive? authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%

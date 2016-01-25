(ns transmorphic.utils)

(defn add-points [point & points]
  (reduce (fn [p1 p2]
            (let [{x1 :x y1 :y} p1
                  {x2 :x y2 :y} p2]
              {:x (+ x1 x2) :y (+ y1 y2)} )) point points))

(defn eucl-distance [a {bx :x by :y}]
  (let [{:keys [x y]} (add-points a {:x (- bx) :y (- by)})]
    (+ (* x x) (* y y))))

(defn delta [{x1 :x y1 :y} {x2 :x y2 :y}]
  (add-points {:x x1 :y y1} 
              {:x (- x2) :y (- y2)}))

(defn contains-rect? [[origin-a extent-a]
                      [origin-b extent-b]]
  (and ; can the origin of a allow for a containment?
       (< (origin-a :x) (origin-b :x)) 
       (< (origin-a :y) (origin-b :y)) 
       ; if so, can the extent of a contain all of b
       ; when expanded form the origin? 
       (let [expand-a (add-points origin-a extent-a)
             expand-b (add-points origin-b extent-b)]
         (and (> (expand-a :x) (expand-b :x)) 
              (> (expand-a :y) (expand-b :y))))))

(defn bounds [morph]
  [(-> morph :props :position) (-> morph :props :extent)])

(defn bottom-right [morph]
  (-> morph :props :extent))

(defn top-right [morph]
  (let [extent (-> morph :props :extent)]
    (add-points {:x (extent :x) :y 0} extent)))

(defn bottom-left [morph]
  (let [extent (-> morph :props :extent)]
    (add-points {:x 0 :y (extent :y)} extent)))

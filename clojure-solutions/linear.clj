(defn geometric-vector? [vector] (and
                                   (vector? vector)
                                   (every? number? vector)))
(defn matrix? [matrix] (and
                         (vector? matrix)
                         (every? vector? matrix)
                         (every? #(every? number? %) matrix)))
(defn tensor? [tensor] (or (number? tensor)
                           (and (every? vector? tensor)
                                (every? #(== (count (first tensor)) (count %)) tensor))
                           (and (vector? tensor)
                                (tensor? (first tensor)))))
(defn form [vector] (cond
                      (vector? vector) (cons (count vector) (form (first vector)))
                      (number? vector) (list)))
(defn compatible? [& tensors] (let [forms (mapv form tensors)
                                    max (nth forms (.indexOf (mapv count forms) (apply max (mapv count forms))))]
                                (cond (= max ()) true
                                      :else (every? #(= (cond
                                                          (not (== 0 (count %))) (take-last (count %) max)
                                                          :else ())
                                                        %) forms))))
(defn geometric-equal? [& elements] (or (every? number? elements)
                                        (and (every? vector? elements)
                                             (every? #(== (count %) (count (first elements))) elements)
                                             (or (every? #(= (type (first %)) (type (ffirst elements))) elements)
                                                 (every? #(and (number? (first %)) (number? (ffirst elements))) elements))
                                             (every? #(apply geometric-equal? %) elements))))

(defn broadcast [& tensors] (let [forms (mapv form tensors)
                                  max-tensor (nth tensors (.indexOf (mapv count forms) (apply max (mapv count forms))))]
                              (letfn [(broadcast-bin [tensor1 tensor2]
                                        (let [form1 (form tensor1) form2 (form tensor2)]
                                          (cond
                                            (< (count form1) (count form2))
                                            (broadcast-bin
                                              (vec (repeat (nth form2 (- (count form2) (count form1) 1)) tensor1)) tensor2)
                                            (>= (count form1) (count form2)) tensor1)))]
                                (mapv #(broadcast-bin % max-tensor) tensors))))
(defn ident [& elements] (mapv identity elements))

(defn v*s ([vector & scalars]  {:pre [(and (geometric-vector? vector) (every? number? scalars))]
                                ;:post [(and (geometric-vector? %) (== (count %) (count vector)))]
                                }
           (let [sc (reduce * scalars)]
             (mapv #(* % sc) vector))))
(defn m*s ([matrix & scalars] {:pre [(and (matrix? matrix) (every? number? scalars))]
                               ;:post [(and (matrix? %) (geometric-equal? % matrix))]
                               }
           (let [sc (reduce * scalars)]
             (mapv #(v*s % sc) matrix))))

(defn awesome-func [f type-check good? change-operands]
  (fn operation [& operands]
    {:pre [(and (mapv type-check operands) (apply good? (vec operands)))]
     ;:post [(and (mapv type-check (vector %)) (good? % (first operands)))]
     }
    (let [changed-operands (apply change-operands operands)]
      (cond
        (every? number? changed-operands) (apply f changed-operands)
        (every? vector? changed-operands) (apply mapv operation changed-operands)))))

(defn v [f] (awesome-func f geometric-vector? geometric-equal? ident))
(defn m [f] (awesome-func f matrix? geometric-equal? ident))
(defn hb [f] (awesome-func f tensor? compatible? broadcast))

(def v+ (v +))
(def v- (v -))
(def v* (v *))
(def vd (v /))

(def m+ (m +))
(def m- (m -))
(def m* (m *))
(def md (m /))

(def hb+ (hb +))
(def hb- (hb -))
(def hb* (hb *))
(def hbd (hb /))

(defn m*v [matrix vector] {:pre [(and (matrix? matrix) (geometric-vector? vector)
                                      (== (count (first matrix)) (count vector)))]
                           ;:post [(and (geometric-vector? %) (== (count matrix) (count %)))]
                           }
  (mapv #(reduce + (mapv *' % vector)) matrix))
(defn v*m [vector matrix] {:pre [(and (matrix? matrix) (geometric-vector? vector)
                                      (== (count matrix) (count vector)))]
                           ;:post [(and (geometric-vector? %) (== (count (first matrix)) (count %)))]
                           }
  (apply v+ (mapv v*s matrix vector)))
(defn transpose [matrix] {:pre [(or (matrix? matrix) (geometric-vector? matrix?))]
                          ;:post [(or (matrix? %) (vector %))]
                          }
  ;(apply mapv vector matrix)
  (mapv #(mapv (fn [row] (nth (nth matrix row) %))
               (mapv identity (range 0 (count matrix))))
        (mapv identity (range 0 (count (first matrix))))))
(defn m*m [& matrices] (letfn [(m*m-bin [m1 m2] {:pre [(and
                                                         (matrix? m1)
                                                         (matrix? m2)
                                                         (== (count (first m1)) (count m2)))]
                                                 ;:post [(and
                                                 ;         (matrix? %)
                                                 ;         (== (count %) (count m1))
                                                 ;         (== (count (first %)) (count (first m2))))]
                                                 }
                                 (mapv #(v*m % m2) m1))]
                         (reduce m*m-bin matrices)))
(defn scalar [& vectors] {:pre [(and (every? geometric-vector? vectors)
                                     (every? #(== (count %) (count (first vectors))) vectors))]
                          ;:post [(number? %)]
                          } (apply + (apply v* vectors)))
(defn vect [& vectors] {:pre [(and (every? geometric-vector? vectors)
                                   (every? #(== (count %) (count (first vectors))) vectors))]
                        ;:post [(and (geometric-vector? %) (== (count %) (count (first vectors))))]
                        }
  (letfn [(bin-vect [v1 v2] (vector (- (* (nth v1 1) (nth v2 2)) (* (nth v1 2) (nth v2 1)))
                                    (- (* (nth v1 2) (nth v2 0)) (* (nth v1 0) (nth v2 2)))
                                    (- (* (nth v1 0) (nth v2 1)) (* (nth v1 1) (nth v2 0)))))]
    (reduce bin-vect vectors)))

(defn d
  ([x] (d 1 x))
  ([x & ys] (/ x (double (apply * ys)))))

(defn func [f] (fn [& args] (fn [vars] (apply f (mapv #(% vars) args)))))

(defn constant [value] (constantly value))
(defn variable [name] (fn [vars] (get vars name)))

(def add (func +))
(def subtract (func -))
(def multiply (func *))
(def divide (func d))
(def exp (func #(Math/exp %)))
(def negate (func -))
(def sumexp (fn [& args] (apply add (mapv exp args))))
(def softmax (fn [& args] (divide (exp (first args)) (apply sumexp args))))

(def operands {'+ add
               '- subtract
               '* multiply
               '/ divide
               'negate negate
               'sumexp sumexp
               'softmax softmax})

(defn parse-tokens [input _operands cnst _variable]
  (cond
    (list? input) (apply (get _operands (first input)) (rest (mapv #(parse-tokens % _operands cnst _variable) input)))
    (number? input) (cnst input)
    (symbol? input) (_variable (name input))))

(defn parseFunction [input] (parse-tokens (read-string input) operands constant variable))



;-----------------------------

(defn evaluate [this map] (._evaluate this map))
(defn toString [this] (._toString this))
(defn toStringInfix [this] (._toStringInfix this))
(defn diff [this name] (._diff this name))

(defn diff-f [diff-func diff-name args] (diff-func args (mapv #(diff % diff-name) args)))
(defn build-str [token args] (str "(" token " " (clojure.string/join " " (mapv #(toString %) args)) ")"))
(defn build-infix [token args] (str "(" (toStringInfix (first args)) " " token " " (toStringInfix (second args)) ")"))
(defn calc-f [func args vars] (apply func (mapv #(evaluate % vars) args)))
(defn calc-bitwise [func args vars] (java.lang.Double/longBitsToDouble
                                      (apply func (mapv #(java.lang.Double/doubleToLongBits %) (mapv #(evaluate % vars) args)))))


(definterface IElement
  (^Number _evaluate [map])
  (^String _toString [])
  (^String _toStringInfix [])
  (^user.IElement _diff [args]))

(deftype ConstantC [value]
  IElement
  (_evaluate [this _] value)
  (_toString [this] (str value))
  (_toStringInfix [this] (str value))
  (_diff [this _] (ConstantC. 0)))

(def ZERO (ConstantC. 0))
(def ONE  (ConstantC. 1))

(deftype VariableC [var-name]
  IElement
  (_evaluate [this map] (get map (clojure.string/lower-case (subs var-name 0 1))))
  (_toString [this] (name var-name))
  (_toStringInfix [this] (name var-name))
  (_diff [this diff-name] (if (= diff-name var-name)
                            ONE
                            ZERO)))

(deftype AddC [args]
  IElement
  (_evaluate [this vars] (calc-f + args vars))
  (_toString [this] (build-str "+" args))
  (_toStringInfix [this] (build-infix "+" args))
  (_diff [this diff-name] (diff-f (fn [_ diff-args] (AddC. diff-args)) diff-name args)))

(deftype SubtractC [args]
  IElement
  (_evaluate [this vars] (calc-f - args vars))
  (_toString [this] (build-str "-" args))
  (_toStringInfix [this] (build-infix "-" args))
  (_diff [this diff-name] (diff-f (fn [_ diff-args] (SubtractC. diff-args)) diff-name args)))


(deftype MultiplyC [args]
  IElement
  (_evaluate [this vars] (calc-f * args vars))
  (_toString [this] (build-str "*" args))
  (_toStringInfix [this] (build-infix "*" args))
  (_diff [this diff-name] (letfn [(diff-func [args diff-args]
                                    (AddC. (mapv #(MultiplyC. (assoc (vec args) %2 %1)) diff-args (range (count args)))))]
                            (diff-f diff-func diff-name args))))

(deftype DivideC [args]
  IElement
  (_evaluate [this vars] (calc-f d args vars))
  (_toString [this] (build-str "/" args))
  (_toStringInfix [this] (build-infix "/" args))
  (_diff [this diff-name] (letfn [(diff-func [args diff-args]
                                    (DivideC.
                                      [(SubtractC.
                                         (mapv #(MultiplyC. (assoc (vec args) %2 %1)) diff-args (range (count args))))
                                       (MultiplyC. (mapv #(MultiplyC. [% %]) (if (== 1 (count args))
                                                                               args
                                                                               (vec (rest args)))))]))]
                            (diff-f diff-func diff-name args))))

(deftype NegateC [args]
  IElement
  (_evaluate [this vars] (calc-f - args vars))
  (_toString [this] (build-str "negate" args))
  (_toStringInfix [this] (str "negate(" (toStringInfix (first args)) ")"))
  (_diff [this diff-name] (diff-f (fn [_ diff-args] (NegateC. diff-args)) diff-name args)))

(deftype ExpC [args]
  IElement
  (_evaluate [this vars] (calc-f #(Math/exp %) args vars))
  (_toString [this] (build-str "exp" args))
  (_diff [this diff-name] (letfn [(diff-func [args diff-args]
                                    (MultiplyC. [(ExpC. args) (first diff-args)]))]
                            (diff-f diff-func diff-name args))))

(deftype SumexpC [args]
  IElement
  (_evaluate [this map] (apply (fn [& args] (apply + (mapv #(Math/exp %) args))) (mapv #(evaluate % map) args)))
  (_toString [this] (build-str "sumexp" args))
  (_diff [this diff-name] (letfn [(diff-func [args diff-args]
                                    (AddC. (mapv #(MultiplyC. [(ExpC. [%1]) %2]) args diff-args)))]
                            (diff-f diff-func diff-name args))))

(deftype SoftmaxC [args]
  IElement
  (_evaluate [this map] (apply (fn [& args] (/ (Math/exp (first args)) (apply + (mapv #(Math/exp %) args))))
                               (mapv #(evaluate % map) args)))
  (_toString [this] (build-str "softmax" args))
  (_diff [this diff-name] (letfn [(diff-func [args diff-args]
                                    (let [sumexp (SumexpC. args)
                                          sumexp-diff (AddC. (mapv #(MultiplyC. [%1 %2]) (mapv #(ExpC. [%]) args) diff-args))]
                                      (DivideC.
                                        [(SubtractC.
                                           [(MultiplyC. [(first diff-args) (ExpC. [(first args)]) sumexp])
                                            (MultiplyC. [(ExpC. [(first args)]) sumexp-diff])])
                                         (MultiplyC. [sumexp sumexp])])))]
                            (diff-f diff-func diff-name args))))

(deftype BitAndC [args]
  IElement
  (_evaluate [this vars] (calc-f bit-and (mapv #(java.lang.Double/doubleToLongBits %) args) vars))
  (_toString [this] (build-str "&" args))
  (_toStringInfix [this] (build-infix "&" args)))

(deftype BitAndC [args]
  IElement
  (_evaluate [this vars] (calc-bitwise bit-and args vars))
  (_toString [this] (build-str "&" args))
  (_toStringInfix [this] (build-infix "&" args)))

(deftype BitOrC [args]
  IElement
  (_evaluate [this vars] (calc-bitwise bit-or args vars))
  (_toString [this] (build-str "|" args))
  (_toStringInfix [this] (build-infix "|" args)))

(deftype BitXorC [args]
  IElement
  (_evaluate [this vars] (calc-bitwise bit-xor args vars))
  (_toString [this] (build-str "^" args))
  (_toStringInfix [this] (build-infix "^" args)))

(deftype BitImplC [args]
  IElement
  (_evaluate [this vars] (calc-bitwise (fn [x y] (bit-or (bit-not x) y)) args vars))
  (_toString [this] (build-str "=>" args))
  (_toStringInfix [this] (build-infix "=>" args)))

(deftype BitIffC [args]
  IElement
  (_evaluate [this vars] (calc-bitwise (fn [x y] (bit-or (bit-and x y) (bit-and (bit-not x) (bit-not y)))) args vars))
  (_toString [this] (build-str "<=>" args))
  (_toStringInfix [this] (build-infix "<=>" args)))



(defn  Constant [value] (ConstantC. value))
(defn   Variable [name] (VariableC. name))
(defn      Add [& args] (AddC. args))
(defn Subtract [& args] (SubtractC. args))
(defn Multiply [& args] (MultiplyC. args))
(defn   Divide [& args] (DivideC. args))
(defn   Negate [& args] (NegateC. args))
(defn      Exp [& args] (ExpC. args))
(defn   Sumexp [& args] (SumexpC. args))
(defn  Softmax [& args] (SoftmaxC. args))
(defn   BitAnd [& args] (BitAndC. args))
(defn    BitOr [& args] (BitOrC. args))
(defn   BitXor [& args] (BitXorC. args))
(defn  BitImpl [& args] (BitImplC. args))
(defn   BitIff [& args] (BitIffC. args))


(def operands-object {'+           Add
                      '-           Subtract
                      '*           Multiply
                      '/           Divide
                      'negate      Negate
                      'sumexp      Sumexp
                      'softmax     Softmax
                      '&           BitAnd
                      '|           BitOr
                      (symbol "^") BitXor
                      '=>          BitImpl
                      '<=>         BitIff})

(defn parseObject [input] (parse-tokens (read-string input) operands-object Constant Variable))


;-----------------------------------

(load-file "parser.clj")

(defn sign [s tail]
  (if (= \- s)
    (cons s tail)
    tail))

(defn float-part [head tail]
  (if (= tail nil)
    head
    (concat head tail)))

(defn fold-left [coll]
  (if (== (count coll) 1)
    (first coll)
    (reduce (fn [x y] ((get operands-object (symbol (str (first y)))) x (second y))) coll)))
(defn fold-right [coll]
  (if (== (count coll) 1)
    (first coll)
    (second (reduce (fn [x y] (vector (if (vector? y) (first y) y)
                                      ((get operands-object (symbol (str (first x)))) (if (vector? y) (second y) y)
                                       (second x)))) (reverse coll)))))
(defn fold-unary [[operations obj]]
  (let [rev-ops (reverse operations)]
    (letfn [(fold [ops obj]
              (if (empty? ops)
                obj
                (fold (rest ops) ((get operands-object (symbol (str (first ops)))) obj))))] (fold rev-ops obj))))

(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(def *digit (+char "0123456789"))
(def *negate (+str (+seq (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e"))))
(def *impl (+str (+seq (+char "=") (+char ">"))))
(def *iff (+str (+seq (+char "<") (+char "=") (+char ">"))))
(def *var (+str (+plus (+char "XYZxyz"))))
(def *number (+map read-string (+str (+seqf sign
                                            (+opt (+char "-")) (+seqf float-part (+plus *digit)
                                                                      (+opt (+seqf cons (+char ".") (+plus *digit))))))))

(defn *level-parse-bin [fold next p] (+map fold (+seqf cons *ws (next) (+star (+seq *ws p *ws (next))))))
(defn *level-parse-unary [next p] (+seqf fold-unary (+seq (+star (+seqn 0 *ws p)) *ws (next))))

(declare parseInfix)
(defn *operand [] (+or
                    (+map Constant (+seqn 0 *ws *number))
                    (+map Variable (+seqn 0 *ws *var))
                    (+seqn 1 *ws (+char "(") *ws (delay parseInfix) *ws (+char ")"))))
(defn *signed [] (*level-parse-unary *operand *negate))
(defn *term [] (*level-parse-bin fold-left *signed (+char "*/")))
(defn *sum [] (*level-parse-bin fold-left *term (+char "+-")))
(defn *bitwise-and [] (*level-parse-bin fold-left *sum (+char "&")))
(defn *bitwise-or [] (*level-parse-bin fold-left *bitwise-and (+char "|")))
(defn *bitwise-xor [] (*level-parse-bin fold-left *bitwise-or (+char "^")))
(defn *bitwise-impl [] (*level-parse-bin fold-right *bitwise-xor *impl))
(defn *bitwise-iff [] (*level-parse-bin fold-left *bitwise-impl *iff))

(def parseInfix (*bitwise-iff))

(defn parseObjectInfix [str] (:value (parseInfix str)))

(require '[clojure.contrib.string :as str])

(defn strip-comments [s]
  (str/join "" (str/split #";.*\n?" s)))

(defn tokenize [s]
  (filter #(not (= "" %)) 
          (map #(str/trim %) 
               (str/partition #"\(| |\)|\"[^\"]*\"" 
                              (strip-comments s)))))

; Try to cast string first as integer, then as float
(defn parse-num [s]
  (try (Integer/parseInt s)
       (catch NumberFormatException nfe 
         (try (Float/parseFloat s) 
              (catch NumberFormatException nfe s)))))

(defn process-numeric-tokens [tokens]
  (map #(parse-num %) tokens))

(defn balanced? [tokens]
  (loop [i 0, nop 0, ncp 0]
    (if (>= i (count tokens))
      (= nop ncp)
      (cond 
        (= (nth tokens i) "(") (recur (inc i) (inc nop) ncp)          
        (= (nth tokens i) ")") (recur (inc i) nop (inc ncp))                  
        :default (recur (inc i) nop ncp)))))
          
; Assumes an implicit first paren, then finds enclosing group for it
; For instance: a ( b c ( d e ) ) f ) g (
; would yield:  a ( b c ( d e ) ) f )
(defn find-balanced-group [tokens]
  (loop [nop 1, i 0, acc ()] ; nop: nb. of opening parens
    (if (zero? nop)
      acc ; return
      (let [token (nth tokens i)]
        (cond
          (= "(" token)
            (recur (inc nop) (inc i) (concat acc (list token)))
          (= ")" token)
            (recur (dec nop) (inc i) (concat acc (list token)))
          :default
            (recur nop (inc i) (concat acc (list token))))))))

;(println (find-balanced-group (tokenize "a ( b c ( d e ) ) f ) g (")))

(defn old-build-syntax-tree [tokens]
  (loop [i 0, node ()]
    (if (>= (inc i) (count tokens))
      node ; return current node
      (let [token (nth tokens i)]
        (cond
          (= "(" token) ; create and visit a new node level, and add it to current node
            (let [token-group (find-balanced-group (drop (inc i) tokens))]
              (recur (+ i (count token-group)) 
                     (concat node (list (old-build-syntax-tree token-group)))))
          (= ")" token) ; just continue
            (recur (inc i) node)
          :default ; add token to current node
            (recur (inc i) (concat node (list token))))))))

(defn atom? [elem]
  (not (seq? elem)))

(defn count-tokens [node]
  (loop [i 0, counter 2] ; count opening and closing parentheses
    (if (>= i (count node))
      counter
      (let [elem (nth node i)]
        (if (atom? elem)
          (recur (inc i) (inc counter))
          (recur (inc i) (+ counter (count-tokens elem))))))))
                       
(defn build-syntax-tree-debug [tokens]
  (loop [i 0, node ()]
    (if (>= i (count tokens))
      node
      (let [token (nth tokens i)]
;        (println "curr token:" token " i:" i " tokens:" tokens)
        (cond
           (= "(" token) ; create and visit a new node level, and add it to current node
             (do
;               (println "creating new node from:" (drop (inc i) tokens))
               (let [sub-node (build-syntax-tree-debug (drop (inc i) tokens))]
                 (recur (+ i (count-tokens sub-node)) (concat node (list sub-node)))))
           (= ")" token) 
             (do
;               (println "returning:" node)
               node)
           :default ; add token to current node
             (do
;               (println "found: " token " cur node:" (concat node (list token)))
               (recur (inc i) (concat node (list token)))))))))

(defn build-syntax-tree [tokens]
  (loop [i 0, node ()]
    (if (>= i (count tokens))
      node
      (let [token (nth tokens i)]
        (cond
           (= "(" token) ; create and visit a new node level, 
                         ; and add it to current node
             (let [sub-node (build-syntax-tree (drop (inc i) tokens))]
               (recur (+ i (count-tokens sub-node)) 
                      (concat node (list sub-node))))
           (= ")" token) ; return node
             node
           :default ; add token to current node
             (recur (inc i) (concat node (list token))))))))
  
                                
; symbol-table: { var-symbol -> ["var", var-node],
;                 fn-symbol  -> ["function", params, fn-nodes] }, 
;                                 where fn-nodes is a list of function 
;                                 expressions, or "statements"
(defn extract-symbol-table-debug [node]
  (loop [i 0, symbol-table {}]
    (if (>= i (count node))
      symbol-table 
      (let [elem (nth node i)]
        (if (atom? elem)
          (do
;            (println "atom:" elem)
            (cond 
              (= elem "setq")
                (do
                  (assert (zero? i))
                  (assert (= (count node) 3))
                  (recur (inc i) 
                         (assoc symbol-table (nth node 1) 
                                ["var" (nth node 2)])))
              (= elem "defun")
                (do
                  (assert (zero? i))
;                  (assert (= (count node) 4))                  
                  (recur (inc i) 
                         (assoc symbol-table (nth node 1) 
                                ["function" (nth node 2) (drop 3 node)])))
              :default
                (recur (inc i) symbol-table)))
          (do
;            (println "visiting node:" elem)
            (recur (inc i) (merge symbol-table (extract-symbol-table-debug elem)))))))))


; symbol-table: { var-symbol -> ["var" var-node],
;                 fn-symbol  -> ["function" params fn-nodes] }, 
;                                 where fn-nodes is a list of function 
;                                 expressions, or "statements"
(defn extract-symbol-table [node]
  (loop [i 0, symbol-table {}]
    (if (>= i (count node))
      symbol-table 
      (let [elem (nth node i)]
        (if (atom? elem)
          (cond 
            (= elem "setq")
              (do
                (assert (zero? i))
                (assert (= (count node) 3))
                (recur (inc i) 
                       (assoc symbol-table (nth node 1) 
                              ["var" (nth node 2)])))
            (= elem "defun")
              (do
                (assert (zero? i))
                (recur (inc i) 
                       (assoc symbol-table (nth node 1) 
                              ["function" (nth node 2) (drop 3 node)])))
            :default
              (recur (inc i) symbol-table))
          (recur (inc i) (merge symbol-table (extract-symbol-table elem))))))))

; function-node: ((..p1.. (..p2..)..) (..))
; param-map: {p1 (..), p2 (..), ..}
; Returns a function-node in which all instances of params found in 
; param-map have been replaced by their actual values
(defn get-function-instance [fn-nodes param-map]
  (loop [i 0, fn-instance ()]
    (if (>= i (count fn-nodes))
      fn-instance
      (let [elem (nth fn-nodes i)]
        (if (atom? elem)
          (if (contains? param-map elem) ; match found: replace param by param-node
            (recur (inc i) (concat fn-instance (list (get param-map elem))))
            (recur (inc i) (concat fn-instance (list elem))))
          (recur (inc i) (concat fn-instance (list (get-function-instance elem param-map)))))))))

(declare execute-node)

; function: symbol
; params: list
; symbol-table: map
(defn execute-function-debug [function params symbol-table]
  (cond 
    (contains? symbol-table function) ; user-defined function
      (let [fn-cell (get symbol-table function)]
        (let [fn-params (nth fn-cell 1)
              fn-nodes (nth fn-cell 2)]
          (let [fn-instance (get-function-instance fn-nodes (zipmap fn-params params))]
;            (let [result (execute-node fn-instance symbol-table)]
            ; multi-statements emulation
            (doseq [fn-inst-node (take (dec (count fn-instance)) fn-instance)]
              (execute-node fn-inst-node symbol-table))
            (let [result (execute-node (last fn-instance) symbol-table)]                         
;              (println "exec fn =" function ", node =" fn-instance " , result =" result)
              result))))
    (= function "+")
      (do
;        (print "exec fn =" function ", params =" params)
        (let [result (apply + params)]
;            (println " , result =" result)
            result))
    (= function "-")
      (do
;        (print "exec fn =" function ", params =" params)
        (let [result (apply - params)]
;            (println " , result =" result)
            result))
    (= function "print")
      (do
;        (println "exec fn =" function ", params ="  params)
        (if (= (:dtype (first params)) java.lang.String)
          (println (str/replace-str "\"" "" (first params))) ; remove enclosing doublequotes
          (println (first params))))
    (= function "list")
      (do
 ;       (println "creating list:" params)
        params)
    (= function "count")
      (do
;        (println "exec fn =" function ", params ="  params " , result =" (count (first params)))
        (count (first params)))
    (= function "car")
      (do
        (first (first params)))
    (= function "cdr")
      (do
;        (println "cdr:" params (rest (first params)))
        (rest (first params)))
    (= function "=")
      (do
;        (println "=:" params (:dtype (first params)) (:dtype (second params)))
        (= (first params) (second params)))
    (= function "if")
      (do
        (if (execute-node (first params) symbol-table)
          (execute-node (second params) symbol-table)
          (execute-node (nth params 2) symbol-table)))
    (= function "progn")
      (do ; multi-statements emulation
        (doseq [node (take (dec (count params)) params)]
          (execute-node node symbol-table))        
        (execute-node (last params) symbol-table))      
    :default
      (if function
        (cons function params)
        ())
    ))

; function: symbol
; params: list
; symbol-table: map
(defn execute-function [function params symbol-table]
  (cond 
    (contains? symbol-table function) ; user-defined function
      (let [fn-cell (get symbol-table function)]
        (let [fn-params (nth fn-cell 1)
              fn-nodes (nth fn-cell 2)]
          (let [fn-instance (get-function-instance fn-nodes (zipmap fn-params params))]
            (doseq [fn-inst-node (take (dec (count fn-instance)) fn-instance)]
              (execute-node fn-inst-node symbol-table))
            (execute-node (last fn-instance) symbol-table))))
    (= function "+")
      (apply + params)
    (= function "-")
      (apply - params)
    (= function "print")
      (if (= (:dtype (first params)) java.lang.String)
          (println (str/replace-str "\"" "" (first params))) ; remove enclosing doublequotes
          (println (first params)))
    (= function "list")
      params
    (= function "count")
      (count (first params))
    (= function "car")
      (first (first params))
    (= function "cdr")
      (rest (first params))
    (= function "=")
      (= (first params) (second params))
    (= function "if")
      (if (execute-node (first params) symbol-table)
        (execute-node (second params) symbol-table)
        (execute-node (nth params 2) symbol-table))
    (= function "progn")
      (do ; multi-statements emulation
        (doseq [node (take (dec (count params)) params)]
          (execute-node node symbol-table))        
        (execute-node (last params) symbol-table))
    :default
      (if function
        (cons function params)
        ())
    ))

(defn execute-node [node symbol-table]
  (if (atom? node)
    ; it's an atom
    (cond
      (contains? #{"true" "t"} node) true
      (contains? #{"false" "f" "nil"} node) false
      (contains? symbol-table node) ; try to replace atom by corresponding var-node, if it exists
        (execute-node (second (get symbol-table node)) symbol-table)
      :default node)
    ; it's a node 
    (cond 
       (= (first node) "if") ; "if" node
         (do
           (assert (= (count node) 4))
           (execute-function "if" (rest node) symbol-table))
       (or (= (first node) "setq") (= (first node) "defun")) ; dont execute those
         nil
       :default
         (loop [i 0, function nil, params nil] ; not if
           (if (>= i (count node))
             (execute-function function params symbol-table)
             (let [elem (nth node i)]
               (if (zero? i)
                 (recur (inc i) elem ()) ; set 1rst elem as function
                 (let [exec-node (execute-node elem symbol-table)]
                     (recur (inc i) function 
                            (concat params (list exec-node))))))))))) ; accum params

(defn execute-program [input-str]
  (let [tokens (process-numeric-tokens (tokenize input-str))]
    (if (not (balanced? tokens))
      (println "Unbalanced parentheses problem")      
      (let [syntax-tree (build-syntax-tree tokens)]
        (let [symbol-table (extract-symbol-table syntax-tree)]
          (doseq [node syntax-tree]
            (execute-node node symbol-table)))))))

(execute-program (slurp (first *command-line-args*)))


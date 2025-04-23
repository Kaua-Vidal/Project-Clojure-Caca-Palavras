(ns caca-palavras.core
  (:require [clojure.string :as str]))

(defn grade-vazia [lin col]
  (vec (repeat lin (vec (repeat col \-)))))

(defn pode-inserir? [grade palavra [lin col] dir]
  (let [[dx dy] (case dir :h [0 1] :v [1 0])]
    (every?
     (fn [[i ch]]
       (let [x (+ lin (* i dx))
             y (+ col (* i dy))
             atual (get-in grade [x y])]
         (or (= atual \-) (= atual ch))))
     (map-indexed vector palavra))))

(defn inserir [grade palavra [lin col] dir]
  (let [[dx dy] (case dir :h [0 1] :v [1 0])]
    (reduce (fn [g [i ch]]
              (assoc-in g [(+ lin (* i dx)) (+ col (* i dy))] ch))
            grade
            (map-indexed vector palavra))))

(defn posicao-valida [lin col tam dir]
  [(rand-int (inc (if (= dir :v) (- lin tam) (dec lin))))
   (rand-int (inc (if (= dir :h) (- col tam) (dec col))))])

(defn inserir-palavras [grade palavras]
  (reduce
   (fn [g palavra]
     (loop [tentativas 0]
       (if (> tentativas 100)
         g ; desiste após 100 tentativas
         (let [dir (rand-nth [:h :v])
               pos (posicao-valida (count g) (count (first g)) (count palavra) dir)]
           (if (pode-inserir? g palavra pos dir)
             (inserir g palavra pos dir)
             (recur (inc tentativas)))))))
   grade palavras))

(defn imprimir [grade]
  (doseq [linha grade]
    (println (apply str linha))))

(defn buscar [grade palavra]
  (let [lin (count grade)
        col (count (first grade))
        dirs [:h :v]
        posicoes (for [l (range lin) c (range col) d dirs] [l c d])]
    (some (fn [[l c d]]
            (let [[dx dy] (case d :h [0 1] :v [1 0])
                  coords (map #(vector (+ l (* % dx)) (+ c (* % dy))) (range (count palavra)))
                  letras (map #(get-in grade %) coords)]
              (when (= (seq palavra) letras)
                {:palavra palavra :posicao [l c] :direcao d})))
          posicoes)))

(defn -main []
  (println "Digite as palavras para o caca-palavras (separadas por espaco):")
  (let [entrada (read-line)
        palavras (str/split (str/upper-case entrada) #"\s+")
        grade (-> (grade-vazia 10 10)
                  (inserir-palavras palavras))]
    (println "\n🧩 Grade:\n")
    (imprimir grade)
    (println "\n🔍 Resultados:\n")
    (doseq [p palavras]
      (if-let [res (buscar grade p)]
        (println (:palavra res) "→ posicao:" (:posicao res) ", direcao:" (:direcao res))
        (println p "→ ❌ nao encontrada")))))

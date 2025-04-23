(ns caca-palavras.core
  (:require [clojure.string :as str]))

(def direcoes
  {:h  [0 1]     ; →
   :v  [1 0]     ; ↓
   :d1 [1 1]     ; ↘
   :d2 [1 -1]    ; ↙
   :d3 [-1 1]    ; ↗
   :d4 [-1 -1]}) ; ↖

(defn grade-vazia [lin col]
  (vec (repeat lin (vec (repeat col \-)))))

(defn posicao-valida [lin col tam [dx dy]]
  (let [lin-max (if (neg? dx) (- lin tam) (inc (- lin (* dx (dec tam)))))
        col-max (if (neg? dy) (- col tam) (inc (- col (* dy (dec tam)))))]
    [(rand-int (max 1 lin-max)) (rand-int (max 1 col-max))]))

(defn pode-inserir? [grade palavra [x y] [dx dy]]
  (every?
   (fn [[i ch]]
     (let [nx (+ x (* i dx))
           ny (+ y (* i dy))
           atual (get-in grade [nx ny])]
       (or (= atual \-) (= atual ch))))
   (map-indexed vector palavra)))

(defn inserir [grade palavra [x y] [dx dy]]
  (reduce (fn [g [i ch]]
            (assoc-in g [(+ x (* i dx)) (+ y (* i dy))] ch))
          grade
          (map-indexed vector palavra)))

(defn inserir-palavras [grade palavras]
  (reduce
   (fn [g p]
     (loop [tentativas 0]
       (if (> tentativas 100)
         g
         (let [dir (rand-nth (keys direcoes))
               delta (direcoes dir)
               pos (posicao-valida (count g) (count (first g)) (count p) delta)]
           (if (pode-inserir? g p pos delta)
             (inserir g p pos delta)
             (recur (inc tentativas)))))))
   grade palavras))

(defn preencher-com-letras [grade]
  (let [letras (map char (range (int \A) (inc (int \Z))))]
    (vec (map #(vec (map (fn [c] (if (= c \-) (rand-nth letras) c)) %)) grade))))

(defn imprimir [grade negrito-set]
  (doseq [i (range (count grade))]
    (doseq [j (range (count (first grade)))]
      (let [ch (get-in grade [i j])
            pos [i j]
            estilo (if (contains? negrito-set pos)
                     (str "\u001B[1m\u001B[31m" ch "\u001B[0m") ; vermelho e negrito
                     (str ch))]
        (print estilo)))
    (println)))

(defn buscar [grade palavra]
  (let [palavra-str (str palavra)
        letras (seq palavra-str)
        lin (count grade)
        col (count (first grade))]
    (some (fn [[[dx dy] dir]]
            (some (fn [x]
                    (some (fn [y]
                            (let [coords (map #(vector (+ x (* % dx)) (+ y (* % dy))) (range (count letras)))
                                  chars (map #(get-in grade %) coords)]
                              (when (= letras chars)
                                {:palavra palavra-str :posicao [x y] :direcao dir :coords coords})))
                          (range col)))
                  (range lin)))
          (map (fn [[d v]] [v d]) direcoes))))

(defn -main []
  (println "Digite palavras para o caca-palavras (separadas por espaco):")
  (let [entrada (read-line)
        palavras (map str (str/split (str/upper-case entrada) #"\s+"))
        base-grade (grade-vazia 12 12)
        grade-com-palavras (inserir-palavras base-grade palavras)
        grade-final (preencher-com-letras grade-com-palavras)
        resultados (map #(buscar grade-final %) palavras)
        coords-encontradas (set (mapcat :coords (filter some? resultados)))]
    (println "\n📘 Grade:\n")
    (imprimir grade-final coords-encontradas)
    (println "\n🔍 Resultados:\n")
    (doseq [r resultados]
      (if r
        (println (:palavra r) "→ posicao:" (:posicao r) ", direcao:" (:direcao r))
        (println (:palavra r) "→ nao encontrada")))))

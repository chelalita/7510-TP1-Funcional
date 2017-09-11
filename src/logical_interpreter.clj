(ns logical-interpreter)
(use '[clojure.string])

(defn remove-last-two 
  "remueve los ultimos 2 caracteres de una cadena de strings"
  [str] (.substring (java.lang.String. str) 0 (- (count str) 2)))
(defn in? [coll elm] (some #(= elm %) coll))
(defn true?
  "dado dos valores devuelve su valor logico, toma a nil como false"
 [val1 val2] (cond (and val1 val2) true :else false))
(defn order-database [x]
  "ordena la base ingresada por texto, deja todas las lineas en formato query"
  (split (remove-last-two  (subs x 2)) #"\.\n\t") )
(defn facts? [x] (boolean (re-find #" :- " x)))
(defn rules? [x] (cond (= (boolean (re-find #" :- " x)) true) false :else true))
(defn params-query
"obtiene los parametros dentro de una query"
 [x] (split (subs x (+( index-of x "(") 1) (index-of x ")")) #", "))
(defn replace-strings
"remplaza todas las ocurrencias de la clave por la del valor"
 [text [k v]] (replace text (str k) (str v)))
(defn set-params [fact param] 
  "Setea los parametros de la query en todos los facts genericos"
  (reduce replace-strings fact (zipmap (params-query fact) param)))
(defn hashear-facts [text]
  "Dado un coleccion de facts, devuelve un hash :fact definicion"
  {(subs text 0 (index-of text " :- ")) (split (subs text (+(index-of text " :- ")4))#"\), ")})
(defn agregar-parentesis [k] (cond (ends-with? k ")") k  :else (str k ")")))
(defn query-consistente? [x] (and (includes? x (and "(" ")")) (< (+(index-of x "(") 1) (index-of x ")"))))

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (def base-ordenada (order-database database))
  (def facts (filter facts? base-ordenada))
  (def rules (filter rules? base-ordenada))
  (cond (not(reduce true? (map query-consistente? base-ordenada)))nil (not(query-consistente? query)) nil
  (in? rules query) true
  :else (do 
  (def facts-with-params  (for [f facts] (set-params f (params-query query))))
  (def hashfacts(reduce merge (map hashear-facts facts-with-params)))
  (def values-parentesis (for [[k v] hashfacts] (map agregar-parentesis v)))
  (def hash-facts-complete (zipmap (keys hashfacts) values-parentesis))
  (def rules-query (get (into {} (filter #(-> % key (= query)) hash-facts-complete)) query))
  (cond (empty? rules-query) false :else  (if(in? (for [rul rules-query ]  (in? rules rul))nil) false true))))
  )
  
(ns copas_database_test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def copas-database "
\tlibertadores-15(river).
\tlibertadores-16(atl-nacional).
\tcopa-argentina-15(boca).
\ttorneo-argentina-15(boca).
\tclasico(river, boca).
\tclasico(boca, river).
\tpais-argentina(river).
\tpais-argentina(boca).
\tlibertadores-86(river).
\ttorneo-86(river).
\tintercontinental-86(river).
\ttriple-corona-86(X) :- libertadores-86(X), torneo-86(X), intercontinental-86(X).
\tcampeon-argentina-15(X) :- copa-argentina-15(X), torneo-argentina-15(X).
\tclasico-pais-argentina(X, Y) :- pais-argentina(X), pais-argentina(Y), clasico(X, Y), clasico(Y, X).
")

(deftest copa-database-prueba
  (testing "river campeon libertadores 16 debe ser falso"
    (is (= (evaluate-query copas-database "libertadores-16(river)")
           false)))
  (testing "river campeon libertadores 15 debe ser verdadero"
    (is (= (evaluate-query copas-database "libertadores-15(river)")
           true)))
  (testing "sanlorenzo clasico arsenal debe ser falso"
    (is (= (evaluate-query copas-database "clasico(sanlorenzo, arsenal)")
           false)))
  (testing "river tricampeon-86 debe ser verdadero"
    (is (= (evaluate-query copas-database "triple-corona-86(river)")
           true)))
  (testing "boca campeon argentina- 16 debe ser falso "
    (is (= (evaluate-query copas-database "campeon-argentina-16(boca)")
           false)))

  (testing "boca campeon argentina 15 debe ser true"
    (is (= (evaluate-query copas-database "campeon-argentina-15(boca)")
           true)))
  (testing "mal sintaxis debe ser nil"
    (is (= (evaluate-query copas-database "clasico-pais-argentina(river, boca")
           nil)))
  (testing "sin parametros debe ser nil"
    (is (= (evaluate-query copas-database "clasico-pais-argentina()")
           nil)))
  )

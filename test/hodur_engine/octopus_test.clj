(ns hodur-engine.octopus-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]
            [midje.sweet :refer :all]
            [matcher-combinators.midje :refer [match throws-match]]
            [matcher-combinators.matchers :as mt]
            [datascript.core :as d]
            [hodur-engine.core :as engine.core]
            [hodur-engine.octopus-extensions :as engine.octopus]))

(deftest expandir-com-metados-dominio

  (facts "funcoes auxiliares"
         (let [meta-db (engine.core/init-path "test/schemas/octopus/octopus.edn")]

           (fact "fields-que-referenciam-enums"
                 (engine.octopus/fields-que-referenciam-enums meta-db)
                 => (match #{[23 :enum/teste 22 30] [23 :enum/teste 22 29]}))

           (fact "extra-data-about-enums"
                 (engine.octopus/extra-data-about-enums meta-db)
                 => (match '({:db/id 22, :model.attr/dominio :enum/teste}
                             {:db/id 30, :model.enum/dominio :enum/teste}
                             {:db/id 29, :model.enum/dominio :enum/teste})))))

  (facts "com init-schema"

         (let [meta-db (engine.octopus/init-schema
                         '[^{:datomic/tag                true
                             :model.attr/apenas-runtime? false}

                           default

                           ^:interface
                           Person
                           [^String name]

                           Employee
                           [^String name
                            ^{:datomic/type          :db.type/tuple
                              :datomic/tupleType     :db/long
                              :model.attr/persisted? true} tupla
                            ^{:type             String
                              :doc              "The very employee number of this employee"
                              :datomic/unique   :db.unique/identity
                              :datomic/fulltext false}
                            number
                            ^Float salary
                            ^Integer age
                            ^DateTime start-date
                            ^Employee supervisor
                            ^{:type        Employee
                              :cardinality [0 n]
                              :doc         "Has documentation"
                              :deprecation "But also deprecation"}
                            co-workers
                            ^{:datomic/type               :db.type/keyword
                              :model.attr/apenas-runtime? true}
                            keyword-type
                            ^{:datomic/type :db.type/uri}
                            uri-type
                            ^{:datomic/type :db.type/double}
                            double-type
                            ^{:datomic/type :db.type/bigdec
                              :deprecation  "This is deprecated"}
                            bigdec-type
                            ^EmploymentType employment-type
                            ^SearchResult last-search-results]

                           ^{:union true}
                           SearchResult
                           [Employee Person EmploymentType]

                           ^{:enum               true
                             :model.attr/dominio :enum/teste}
                           EmploymentType
                           [FULL_TIME
                            ^{:doc "Documented enum"}
                            PART_TIME]])]

           (fact "o metadados de dominio continua presente no tipo"
                 (d/pull @meta-db [:model.attr/dominio] [:type/name "EmploymentType"])
                 => {:model.attr/dominio :enum/teste})

           (fact "o metadados de dominio foi incluido no atributo que referncia enums"
                 (d/pull @meta-db [:field/name :model.attr/dominio] 22)
                 => {:field/name "employment-type", :model.attr/dominio :enum/teste})

           (facts
             (fact "o metadados de dominio foi incluido nos enums"
                 (d/pull @meta-db [:field/name :model.enum/dominio] 29)
                 => {:field/name "FULL_TIME", :model.enum/dominio :enum/teste})

             (fact "o metadados de dominio foi incluido nos enums"
                   (d/pull @meta-db [:field/name :model.enum/dominio] 30)
                   => {:field/name "PART_TIME", :model.enum/dominio :enum/teste}))))

  (facts "com init-path"

         (let [meta-db (engine.octopus/init-path "test/schemas/octopus/octopus.edn")]

           (fact "o metadados de dominio continua presente no tipo"
                 (d/pull @meta-db [:model.attr/dominio] [:type/name "EmploymentType"])
                 => {:model.attr/dominio :enum/teste})

           (fact "o metadados de dominio foi incluido no atributo que referncia enums"
                 (d/pull @meta-db [:field/name :model.attr/dominio] 22)
                 => {:field/name "employment-type", :model.attr/dominio :enum/teste})

           (facts
             (fact "o metadados de dominio foi incluido nos enums"
                   (d/pull @meta-db [:field/name :model.enum/dominio] 29)
                   => {:field/name "FULL_TIME", :model.enum/dominio :enum/teste})

             (fact "o metadados de dominio foi incluido nos enums"
                   (d/pull @meta-db [:field/name :model.enum/dominio] 30)
                   => {:field/name "PART_TIME", :model.enum/dominio :enum/teste})))))

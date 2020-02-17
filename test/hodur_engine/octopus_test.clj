(ns hodur-engine.octopus-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]
            [midje.sweet :refer :all]
            [matcher-combinators.midje :refer [match throws-match]]
            [matcher-combinators.matchers :as mt]
            [datascript.core :as d]
            [datascript.query-v3 :as q]
            [hodur-engine.core :as engine.core]
            [hodur-engine.octopus :as engine.octopus]))

(deftest funcoes-auxiliares

  (facts "funcoes auxiliares"
         (let [meta-db (engine.core/init-path "test/schemas/octopus/octopus.edn")]

           (fact "fields-que-referenciam-enums"
                 (engine.octopus/fields-que-referenciam-enums meta-db)
                 => (match #{[29 :enum/employment-type 28 38]
                             [31 :enum/estado-workflow.employee 30 40]
                             [10 :enum/estado-workflow.person 9 39]
                             [29 :enum/employment-type 28 37]
                             [13 :enum/estados-workflow 12 43]
                             [13 :enum/estados-workflow 12 41]}))

           (fact "tipos-que-referenciam-enums"
                 (d/pull-many @meta-db [:type/name]
                              (select [ALL FIRST] (engine.octopus/fields-que-referenciam-enums meta-db)))
                 => (match [{:type/name "EmploymentType"}
                            {:type/name "Estado-Workflow-Employee"}
                            {:type/name "Estado-Workflow-Person"}
                            {:type/name "EmploymentType"}
                            {:type/name "Estados-Workflow"}
                            {:type/name "Estados-Workflow"}]))

           (fact "fields "
                 (d/pull-many @meta-db [:field/name]
                              (select [ALL (nthpath 2)] (engine.octopus/fields-que-referenciam-enums meta-db)))
                 => (match [{:field/name "employment-type"}
                            {:field/name "status-employee"}
                            {:field/name "status-person"}
                            {:field/name "employment-type"}
                            {:field/name "status"}
                            {:field/name "status"}]))

           (fact "enums "
                 (d/pull-many @meta-db [:field/name]
                              (select [ALL LAST] (engine.octopus/fields-que-referenciam-enums meta-db)))
                 => (match [{:field/name "PART_TIME"}
                            {:field/name "ACEITO_EMPLOYEE"}
                            {:field/name "ACEITO_PERSON"}
                            {:field/name "FULL_TIME"}
                            {:field/name "Estado-Workflow.Person"}
                            {:field/name "Estado-Workflow.Employee"}]))

           (facts "extra-data-about-enums 1"
                  (let [extra-data-about-enums (engine.octopus/extra-data-about-enums meta-db)]
                    (fact
                      extra-data-about-enums
                      => (match '({:db/id 12, :model.attr/dominio :enum/estados-workflow}
                                  {:db/id 43, :model.enum/dominio :enum/estados-workflow}
                                  {:db/id 28, :model.attr/dominio :enum/employment-type}
                                  {:db/id 37, :model.enum/dominio :enum/employment-type}
                                  {:db/id 30, :model.attr/dominio :enum/estado-workflow.employee}
                                  {:db/id 40, :model.enum/dominio :enum/estado-workflow.employee}
                                  {:db/id 38, :model.enum/dominio :enum/employment-type}
                                  {:db/id 41, :model.enum/dominio :enum/estados-workflow}
                                  {:db/id 9, :model.attr/dominio :enum/estado-workflow.person}
                                  {:db/id 39, :model.enum/dominio :enum/estado-workflow.person})))

                    (fact "verificar de uma forma mais human-readable"
                          (set (map
                                 (fn [dic-as-vec]
                                   (reduce #(conj %1 (apply hash-map %2)) {} dic-as-vec))
                                 (transform [ALL FIRST] (fn [[k v]]
                                                          [k v
                                                           :field/name
                                                           (:field/name (d/pull @meta-db [:field/name] v))])
                                            extra-data-about-enums)))
                          => #{{:db/id 39, :field/name "ACEITO_PERSON", :model.enum/dominio :enum/estado-workflow.person}
                               {:db/id 40, :field/name "ACEITO_EMPLOYEE", :model.enum/dominio :enum/estado-workflow.employee}
                               #_{:db/id 39, :field/name "ACEITO_PERSON", :model.enum/dominio :enum/estados-workflow}
                               #_{:db/id 40, :field/name "ACEITO_EMPLOYEE", :model.enum/dominio :enum/estados-workflow}
                               {:db/id 41, :field/name "Estado-Workflow.Employee", :model.enum/dominio :enum/estados-workflow}
                               {:db/id 43, :field/name "Estado-Workflow.Person", :model.enum/dominio :enum/estados-workflow}

                               {:db/id 38, :field/name "PART_TIME", :model.enum/dominio :enum/employment-type}
                               {:db/id 37, :field/name "FULL_TIME", :model.enum/dominio :enum/employment-type}

                               {:db/id 28, :field/name "employment-type", :model.attr/dominio :enum/employment-type}
                               {:db/id 30, :field/name "status-employee", :model.attr/dominio :enum/estado-workflow.employee}
                               {:db/id 9, :field/name "status-person", :model.attr/dominio :enum/estado-workflow.person}
                               {:db/id 12, :field/name "status", :model.attr/dominio :enum/estados-workflow}}))))))


(deftest expandir-com-metados-dominio

  (facts "com init-schema"

         (let [meta-db (engine.octopus/init-schema
                         '[^{:datomic/tag                true
                             :model.attr/apenas-runtime? false}
                           default

                           ^:interface
                           Person
                           [^String name
                            ^Estado-Workflow-Person status-person]

                           ^{:interface true}
                           Workflow
                           [^{:type                                 Estados-Workflow, :cardinality 1, :optional false
                              :model.attr/persiste-estado-workflow? true
                              :doc                                  "Estado da máquina de estados associada à entidade"} status
                            ^{:type String, :cardinality 1, :optional false,
                              :doc  "Identificador do workflow - namespace completo"} ident]

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
                            ^Estado-Workflow-Employee status-employee
                            ^SearchResult last-search-results]

                           ^{:union true}
                           SearchResult
                           [Employee Person EmploymentType]

                           ^{:enum               true
                             :model.attr/dominio :enum/employment-type}
                           EmploymentType
                           [FULL_TIME
                            ^{:doc "Documented enum"}
                            PART_TIME]

                           ^{:enum               true
                             :model.attr/dominio :enum/estado-workflow.person}
                           Estado-Workflow-Person
                           [ACEITO_PERSON]

                           ^{:enum               true
                             :model.attr/dominio :enum/estado-workflow.employee}
                           Estado-Workflow-Employee
                           [ACEITO_EMPLOYEE]

                           ^{:enum               true
                             :union              true
                             :model.attr/dominio :enum/estados-workflow}
                           Estados-Workflow
                           [Estado-Workflow.Employee Estado-Workflow.Person]])]

           #_{{:db/id 39, :field/name "ACEITO_PERSON", :model.enum/dominio :enum/estado-workflow.person}
              {:db/id 40, :field/name "ACEITO_EMPLOYEE", :model.enum/dominio :enum/estado-workflow.employee}
              #_{:db/id 39, :field/name "ACEITO_PERSON", :model.enum/dominio :enum/estados-workflow}
              #_{:db/id 40, :field/name "ACEITO_EMPLOYEE", :model.enum/dominio :enum/estados-workflow}
              {:db/id 41, :field/name "Estado-Workflow.Employee", :model.enum/dominio :enum/estados-workflow}
              {:db/id 43, :field/name "Estado-Workflow.Person", :model.enum/dominio :enum/estados-workflow}

              {:db/id 38, :field/name "PART_TIME", :model.enum/dominio :enum/employment-type}
              {:db/id 37, :field/name "FULL_TIME", :model.enum/dominio :enum/employment-type}

              {:db/id 28, :field/name "employment-type", :model.attr/dominio :enum/employment-type}
              {:db/id 30, :field/name "status-employee", :model.attr/dominio :enum/estado-workflow.employee}
              {:db/id 9, :field/name "status-person", :model.attr/dominio :enum/estado-workflow.person}
              {:db/id 12, :field/name "status", :model.attr/dominio :enum/estados-workflow}}

           (fact "o metadados de dominio continua presente no tipo"
                 (d/pull @meta-db [:model.attr/dominio] [:type/name "EmploymentType"])
                 => {:model.attr/dominio :enum/employment-type})

           (fact "o metadados de dominio foi incluido no atributo que referncia enums"
                 (d/pull-many @meta-db [:field/name :model.attr/dominio] [9 12 28 30])
                 => [{:field/name "status-person", :model.attr/dominio :enum/estado-workflow.person}
                     {:field/name "status", :model.attr/dominio :enum/estados-workflow}
                     {:field/name "employment-type", :model.attr/dominio :enum/employment-type}
                     {:field/name "status-employee", :model.attr/dominio :enum/estado-workflow.employee}])

           (fact "o metadados de dominio foi incluido nos enums"
                 (d/pull-many @meta-db [:field/name :model.enum/dominio] [37 38 39 40])
                 => [{:field/name "FULL_TIME", :model.enum/dominio [:enum/employment-type]}
                     {:field/name "PART_TIME", :model.enum/dominio [:enum/employment-type]}
                     {:field/name "ACEITO_PERSON", :model.enum/dominio [:enum/estado-workflow.person]}
                     {:field/name "ACEITO_EMPLOYEE", :model.enum/dominio [:enum/estado-workflow.employee]}])))


  (facts "com init-path"

         (let [meta-db (engine.octopus/init-path "test/schemas/octopus/octopus.edn")]

           (fact "o metadados de dominio continua presente no tipo"
                 (d/pull @meta-db [:model.attr/dominio] [:type/name "EmploymentType"])
                 => {:model.attr/dominio :enum/employment-type})

           (fact "o metadados de dominio foi incluido no atributo que referncia enums"
                 (d/pull-many @meta-db [:field/name :model.attr/dominio] [9 12 28 30])
                 => [{:field/name "status-person", :model.attr/dominio :enum/estado-workflow.person}
                     {:field/name "status", :model.attr/dominio :enum/estados-workflow}
                     {:field/name "employment-type", :model.attr/dominio :enum/employment-type}
                     {:field/name "status-employee", :model.attr/dominio :enum/estado-workflow.employee}])

           (fact "o metadados de dominio foi incluido nos enums"
                 (d/pull-many @meta-db [:field/name :model.enum/dominio] [37 38 39 40])
                 => [{:field/name "FULL_TIME", :model.enum/dominio [:enum/employment-type]}
                     {:field/name "PART_TIME", :model.enum/dominio [:enum/employment-type]}
                     {:field/name "ACEITO_PERSON", :model.enum/dominio [:enum/estado-workflow.person]}
                     {:field/name "ACEITO_EMPLOYEE", :model.enum/dominio [:enum/estado-workflow.employee]}]))))

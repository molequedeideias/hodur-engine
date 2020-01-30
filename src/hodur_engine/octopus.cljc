(ns hodur-engine.octopus
  (:require #?@(:clj
                [[clojure.edn :as edn]
                 [clojure.java.io :as io]])
            [clojure.set :refer [difference union intersection]]
            [clojure.string :as string]
            [datascript.core :as d]
            [datascript.query-v3 :as q]
            [meander.strategy.epsilon :as r]
            [hodur-engine.core :as engine]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserir certos metada-dados no modelo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tipos-que-são-enums
  [meta-db]
  (d/q '[:find [(pull ?e [*]) ...]
         :where
         [?e :type/enum true]]
       @meta-db))

(defn fields-que-referenciam-enums
  [meta-db]
  (d/q '[:find ?type ?dominio ?field ?enum

         :where

         [?type :type/enum true]
         [?type :model.attr/dominio ?dominio]
         [?field :field/type ?type]
         [?enum :field/parent ?type]]

       @meta-db))

(defn extra-data-about-enums
  [meta-db]
  (->> meta-db
       fields-que-referenciam-enums
       ((r/top-down
          (r/attempt
            (r/rewrite
              [?type ?dominio ?field ?enum] [{:db/id              ?field
                                              :model.attr/dominio ?dominio}
                                             {:db/id              ?enum
                                              :model.enum/dominio ?dominio}]))))
       (reduce into [])
       distinct))

(defn init-schema
  [source-schema & others]
  (let [meta-db (apply engine/init-schema (into [source-schema] others))]
    (d/transact meta-db (extra-data-about-enums meta-db))
    meta-db))

#?(:clj
   (defn init-path
     [path & others]
     (let [meta-db (apply engine/init-path (into [path] others))]

       (d/transact meta-db (extra-data-about-enums meta-db))
       meta-db)))

#_(let [meta-db (init-schema
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
                     PART_TIME]])
        sorted (topological-sort meta-db {:direction {:type->field-children  :rtl
                                                      :field->param-children :rtl
                                                      :type->field-return    :rtl
                                                      :type->param-return    :rtl
                                                      :interface->implements :rtl
                                                      :union->type           :rtl}})]
    (println sorted)
    (clojure.pprint/pprint
      (d/pull-many @meta-db '[:field/name :type/name :param/name] sorted)))

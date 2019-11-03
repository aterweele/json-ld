(ns json-ld.context
  "Implementation of JSON-LD context processing
  algorithms (https://www.w3.org/TR/json-ld-api/#context-processing-algorithms)."
  (:refer-clojure :exclude [keyword?])
  (:require [better-cond.core :as engelberg]
            [clojure.set :as set]
            [clojure.string :as str]
            [extensible-cond.core :as ex]))

;;; helpers that may be moved to other namespaces

(ex/defclause ::loop
  "rhs is a bindings vector per `clojure.core/loop`. Wrap successive
  clauses in a `loop`, allowing them to use `recur` to come back to
  this point."
  [_ bindings* child]
  `(loop ~bindings* ~child))

;; TODO currently not used
(ex/defclause ::let-when
  ;; TODO could implement many bindings
  "rhs is a binding vector per `clojure.core/when-let`. Performs the
  binding if test is logically true. Proceeds with successive
  clauses."
  [_ binding child]
  `(if-let ~binding ~child ~child))

(defn iri?
  [iri]
  (RuntimeException. "TODO implement"))

(defn absolute-iri?
  [iri]
  (RuntimeException. "TODO implement"))

(defn relative-iri?
  [iri]
  (RuntimeException. "TODO implement"))

(defn compact-iri?
  [iri]
  (RuntimeException. "TODO implement"))

(defn compact-iri-prefix
  [iri]
  (RuntimeException. "TODO implement"))

(defn compact-iri-suffix
  [iri]
  (RuntimeException. "TODO implement"))

(defn blank-node-identifier?
  [s]
  (RuntimeException. "TODO implement"))

(defn resolve-against
  ;; TODO: is "path" an accurate description?
  [base-iri path]
  (RuntimeException. "TODO implement"))

(defn dereference [iri]
  ;; The failure mode of `dereference` is described in (5.2.5)
  (RuntimeException. "TODO implement"))

(defn keyword?
  [s]
  ;; also, maybe do this as a set of the JSON-LD reserved keywords.
  (RuntimeException. "TODO implement"))

(defn expand-iri
  ;; https://w3c.github.io/json-ld-api/#iri-expansion

  ;; "The algorithm takes two required and four optional input
  ;; variables. The required inputs are an active context and a value
  ;; to be expanded. The optional inputs are two flags, document
  ;; relative and vocab, that specifying whether value can be
  ;; interpreted as a relative IRI reference against the document's
  ;; base IRI or the active context's vocabulary mapping,
  ;; respectively, and a local context and a map defined to be used
  ;; when this algorithm is used during Context Processing. If not
  ;; passed, the two flags are set to false and local context and
  ;; defined are initialized to null."
  ([active-context value] (expand-iri active-context value nil))
  ([active-context
    value
    {:keys [document-relative? vocab? local-context defined]}]
   (RuntimeException. "TODO implement")))

(declare create-term-definition)

;; TODO/NB: the `active-context` in `contextualize*` and
;; `create-term-definition` is described by the second paragraph in
;; https://w3c.github.io/json-ld-api/#context-processing-algorithm, so
;; it has keys like `:term-definitions`, `:base-iri`, &c. The
;; `local-context` should be a map that comes from the JSON-decoding,
;; i.e. it looks like
;; https://w3c.github.io/json-ld-syntax/#the-context.

(defn contextualize
  ;; "This algorithm specifies how a new active context is updated
  ;; with a local context. The algorithm takes two required and three
  ;; optional input variables. The required inputs are an active
  ;; context and a local context. The optional inputs are an array
  ;; remote contexts, defaulting to a new empty array, which is used
  ;; to detect cyclical context inclusions, override protected,
  ;; defaulting to false, which is used to allow changes to protected
  ;; terms, and propagate, defaulting to true to mark term definitions
  ;; associated with non-propagated contexts."
  ([active-context local-context]
   (contextualize active-context local-context nil))
  ([active-context
    local-context
    {:keys [remote-contexts override-protected propagate processing-mode]
     :or {remote-contexts #{}
          override-protected false
          propagate true}
     ::keys [max-remote-contexts]}]
   (ex/cond
     ::ex/let [
               ;; "(1) Initialize result to the result of cloning
               ;; active context."
               result active-context
               ;; "(2) If local context is an object containing the
               ;; member @propagate, its value MUST be boolean true or
               ;; false, set propagate to that value."
               propagate (when-let [[_ propagate]
                                    (and (associative? local-context)
                                         (find local-context "@propagate"))]
                           (when-not (boolean? propagate)
                             (throw (ex-info "illegal propagate value"
                                             {:propagate propagate})))
                           propagate)
               ;; "(3) If propagate is false, and result does not have
               ;; a previous context, set previous context in result
               ;; to active context."
               result (or (and (false? propagate)
                               (not (contains? result :previous-context))
                               (assoc result :previous-context active-context))
                          result)
               ;; "(4) If local context is not an array, set it to an
               ;; array containing only local context."
               local-context (or (and (not (seq? local-context))
                                      [local-context])
                                 local-context)]
     ;; "(5) For each item context in local context:"
     ::loop [local-context local-context
             result result]
     ;; XXX this is out-of-order which might be a point for a more
     ;; magical ::loop-for.

     ;; "(6) Return result."
     (empty? local-context) result
     ;; (5) continued
     ::ex/let [[context & local-context] local-context]
     ;; "(5.1) If context is null:"
     (nil? context) (ex/cond
                      ;; "(5.1.1) If override protected is false and
                      ;; active context contains any protected term
                      ;; definitions, an invalid context nullification
                      ;; has been detected and processing is aborted."
                      ::ex/do (when (and (false? override-protected)
                                         (->> active-context
                                              :term-definitions
                                              (some :protected?)))
                                (throw (ex-info "invalid context nullification")))
                      ;; "(5.1.2) Otherwise, set result to a
                      ;; newly-initialized active context, setting
                      ;; previous context in result to the previous
                      ;; value of result if propagate is
                      ;; false. Continue with the next context. In
                      ;; JSON-LD 1.0, the base IRI was given a default
                      ;; value here; this is now described
                      ;; conditionally in § 9. The Application
                      ;; Programming Interface."
                      ::ex/let [result' {}
                                result' (or (and (false? propagate)
                                                 (assoc result' :previous-context result))
                                            result')
                                result result']
                      ::ex/else (recur local-context result))
     ;; "(5.2) If context is a string,"
     (string? context) (ex/cond
                         ;; "(5.2.1) Set context to the result of
                         ;; resolving value against the base IRI which
                         ;; is established as specified in section 5.1
                         ;; Establishing a Base URI of [RFC3986]. Only
                         ;; the basic algorithm in section 5.2 of
                         ;; [RFC3986] is used; neither Syntax-Based
                         ;; Normalization nor Scheme-Based
                         ;; Normalization are performed. Characters
                         ;; additionally allowed in IRI references are
                         ;; treated in the same way that unreserved
                         ;; characters are treated in URI references,
                         ;; per section 6.5 of [RFC3987]."

                         ;; TODO: `value` is not "bound" in the spec
                         ::ex/let [value context
                                   context (resolve-against
                                            (:base-iri active-context)
                                            value)]
                         ;; "(5.2.2) If the number of entries in the
                         ;; remote contexts array exceeds a processor
                         ;; defined limit, a context overflow error
                         ;; has been detected and processing is
                         ;; aborted; otherwise, add context to remote
                         ;; contexts."
                         ::ex/do (when (< max-remote-contexts
                                          (count remote-contexts))
                                   (throw (ex-info "context overflow"
                                                   {:max max-remote-contexts
                                                    :remote-contexts remote-contexts})))
                         ;; "(5.2.3) If context was previously
                         ;; dereferenced, then the processor MUST NOT
                         ;; do a further dereference, and context is
                         ;; set to the previously established internal
                         ;; representation."
                         #_TODO
                         ;; "(5.2.4) Otherwise, dereference context
                         ;; using the LoadDocumentCallback, passing
                         ;; context for url, and
                         ;; http://www.w3.org/ns/json-ld#context for
                         ;; profile and for requestProfile."
                         ::ex/let [context (dereference context {:profile "http://www.w3.org/ns/json-ld#context"
                                                                 :requestProfile "http://www.w3.org/ns/json-ld#context"})
                                   ;; (5.2.5) in the helper function

                                   ;; "(5.2.6) Set result to the
                                   ;; result of recursively calling
                                   ;; this algorithm, passing result
                                   ;; for active context, context for
                                   ;; local context, and a copy of
                                   ;; remote contexts."
                                   result (contextualize
                                           result context
                                           {:remote-contexts remote-contexts})]
                         ;; "(5.2.7) Continue with the next context."
                         (recur local-context result))
     ;; "(5.3) If context is not a map, an invalid local context error
     ;; has been detected and processing is aborted."
     ::ex/do (when-not (map? context)
               (throw (ex-info "invalid local context (unexpected type)"
                               {:context context})))
     ;; "(5.4) Otherwise, context is a context definition. (5.5) If
     ;; context has an @version entry:"
     ::ex/do (when-let [[_ value] (find context "@version")]
               ;; "(5.5.1) If the associated value is not 1.1, an
               ;; invalid @version value has been detected, and
               ;; processing is aborted."
               (when-not (= value 1.1)
                 ;; yes, really, this floating-point number. See
                 ;; @version under
                 ;; https://w3c.github.io/json-ld-syntax/#syntax-tokens-and-keywords.
                 (throw (ex-info "invalid @version value"
                                 {:value value})))
               ;; "(5.5.2) If processing mode is set to json-ld-1.0, a
               ;; processing mode conflict error has been detected and
               ;; processing is aborted."
               (when (= processing-mode :json-ld-1.0)
                 (throw (ex-info "processing mode conflict"
                                 {:processing-mode processing-mode
                                  :unsupported-feature "@version"
                                  "@version" value}))))
     ;; "(5.6) If context has an @import entry:"
     ::ex/let [context
               (merge
                context
                (ex/cond
                  ::ex/when-let [[_ value] (find context "@import")]
                  ;; "(5.6.1) If processing mode is json-ld-1.0, an
                  ;; invalid context entry error has been detected and
                  ;; processing is aborted."
                  ::ex/do (when (= processing-mode :json-ld-1.0)
                            (throw (ex-info "invalid context entry"
                                            {:processing-mode processing-mode
                                             :unsupported-feature "@import"})))
                  ;; crazy idea: sugar these sort of asserts?

                  ;; "(5.6.2) Otherwise, if its value is not a string,
                  ;; an invalid @import value error has been detected
                  ;; and processing is aborted."
                  ::ex/do (when-not (string? value)
                            (throw (ex-info "invalid @import value"
                                            {"@import" value})))
                  ;; "(5.6.3) Set import to the result of resolving
                  ;; the value of @import against the base IRI which
                  ;; is established as specified in section 5.1
                  ;; Establishing a Base URI of [RFC3986]. Only the
                  ;; basic algorithm in section 5.2 of [RFC3986] is
                  ;; used; neither Syntax-Based Normalization nor
                  ;; Scheme-Based Normalization are
                  ;; performed. Characters additionally allowed in IRI
                  ;; references are treated in the same way that
                  ;; unreserved characters are treated in URI
                  ;; references, per section 6.5 of [RFC3987].
                  ::ex/let [import (resolve-against
                                    (:base-iri active-context) value)]
                  ;; TODO: 5.6.4-5

                  ;; "(5.6.6) If the dereferenced document has no
                  ;; top-level map with an @context entry, or if the
                  ;; value of @context is not a context definition
                  ;; (i.e., it is not an map), an invalid remote
                  ;; context [TODO error] has been detected and
                  ;; processing is aborted; otherwise, set import
                  ;; context to the value of that entry."
                  ::ex/do (when-not (map? (get import "@context"))
                            (throw (ex-info "invalid remote context"
                                            {:remote-context
                                             (get import "@context")})))
                  ::ex/let [import-context (get import "@context")]
                  ;; "(5.6.7) If import context has a @import entry,
                  ;; an invalid context entry error has been detected
                  ;; and processing is aborted."
                  ::ex/do (when-let [entry (find import-context "@import")]
                            (throw (ex-info "invalid context entry"
                                            {:entry entry})))
                  ;; "(5.6.8) Set context to the result of merging
                  ;; context into import context, replacing common
                  ;; entries with those from context."
                  ::ex/else import-context ; see `merge` above
                  ))]
     ;; "(5.7) If context has an @base entry and remote contexts is
     ;; empty, i.e., the currently being processed context is not a
     ;; remote context:"
     ::ex/let [result
               (merge
                result
                (ex/cond
                  ::ex/when (empty? remote-contexts)
                  ;; "(5.7.1) Initialize value to the value associated
                  ;; with the @base entry."
                  ::ex/when-let [[_ value] (get context "@base")]
                  ;; "(5.7.2) If value is null, remove the base IRI of
                  ;; result."
                  (nil? value) {:base-iri nil}
                  ;; "(5.7.3) Otherwise, if value is an absolute IRI,
                  ;; the base IRI of result is set to value."
                  (absolute-iri? value) {:base-iri value}
                  ;; "(5.7.4) Otherwise, if value is a relative IRI
                  ;; and the base IRI of result is not null, set the
                  ;; base IRI of result to the result of resolving
                  ;; value against the current base IRI of result."
                  (and (relative-iri? value)
                       (some-> result :base-iri))
                  {:base-iri (resolve-against (:base-iri result) value)}
                  ;; "(5.7.5) Otherwise, an invalid base IRI error has
                  ;; been detected and processing is aborted."
                  ::ex/else (throw (ex-info "invalid base IRI"
                                            {:base-iri value}))))]
     ;; "(5.8) If context has an @vocab entry:"
     ::ex/let [result
               (merge
                result
                (ex/cond
                  ;; "(5.8.1) Initialize value to the value associated
                  ;; with the @vocab entry."
                  ::ex/when-let [[_ value] (find context "@vocab")]
                  ;; "(5.8.2) If value is null, remove any vocabulary
                  ;; mapping from result."
                  (nil? value) {:vocabulary-mapping nil}
                  ;; "(5.8.3) Otherwise, if value is an IRI or blank
                  ;; node identifier, the vocabulary mapping of result
                  ;; is set to the result of using the IRI Expansion
                  ;; algorithm, passing result as the active context,
                  ;; value, true for vocab, and true for document
                  ;; relative. . If it is not an IRI, or a blank node
                  ;; identifier, an invalid vocab mapping error has
                  ;; been detected and processing is aborted."
                  ((some-fn iri? blank-node-identifier?) value)
                  (expand-iri result value
                              {:vocab? true :document-relative? true})
                  ::ex/else (throw (ex-info "invalid vocab mapping"
                                            {:invalid-vocab value}))))]
     ;; "(5.9) If context has an @language entry:"
     ::ex/let [result
               (merge
                result
                (ex/cond
                  ;; "(5.9.1) Initialize value to the value associated
                  ;; with the @language entry."
                  ::ex/when-let [[_ value] (get context "@langauge")]
                  ;; "(5.9.2) If value is null, remove any default
                  ;; language from result."
                  (nil? value) {:default-language nil}
                  ;; "(5.9.3) Otherwise, if value is string, the
                  ;; default language of result is set to value. If it
                  ;; is not a string, an invalid default language
                  ;; error has been detected and processing is
                  ;; aborted. If value is not well-formed according to
                  ;; section 2.2.9 of [BCP47], processors SHOULD issue
                  ;; a warning. Processors MAY normalize language tags
                  ;; to lower case."
                  (string? value) {:default-language value} ; TODO
                                                            ; issue
                                                            ; warning
                                                            ; and
                                                            ; normalize
                  ::ex/else (throw (ex-info "invalid default language"
                                            {:invalid-language value}))))]
     ;; "(5.10) If context has an @direction entry:"
     ::ex/let [result
               (merge
                result
                (ex/cond
                  ;; "(5.10.2) Initialize value to the value
                  ;; associated with the @direction entry."
                  ::ex/when-let [[_ value] (find context "@direction")]
                  ;; "(5.10.1) If processing mode is json-ld-1.0, an
                  ;; invalid context entry error has been detected and
                  ;; processing is aborted."
                  ::ex/do (when (= processing-mode :json-ld-1.0)
                            (throw (ex-info "invalid context entry"
                                            {:processing-mode processing-mode
                                             :unsupported-feature "@direction"})))
                  ;; "(5.10.3) If value is null, remove any base
                  ;; direction from result."
                  (nil? value) {:base-direction nil}
                  ;; "(5.10.4) Otherwise, if value is string, the base
                  ;; direction of result is set to value. If it is not
                  ;; null, "ltr", or "rtl", an invalid base direction
                  ;; error has been detected and processing is
                  ;; aborted."
                  ::ex/do (when-not (and (string? value)
                                         (contains? #{"ltr" "rtl"} value))
                            (throw (ex-info "invalid base direction"
                                            {:invalid-base-direction value })))
                  ::ex/else {:base-direction value}))]
     ;; "(5.11) If context has an @propagate entry:"
     ::ex/do (ex/cond
               ::ex/when-let [[_ value] (get context "@propagate")]
               ;; "(5.11.1)" If processing mode is json-ld-1.0, an
               ;; invalid context entry error has been detected and
               ;; processing is aborted.
               (= processing-mode :json-ld-1.0)
               (throw (ex-info "invalid context entry"
                               {:processing-mode processing-mode
                                :unsupported-feature "@propagate"}))
               ;; "(5.11.2) Otherwise, if its value is not boolean
               ;; true or false, an invalid @propagate value error has
               ;; been detected and processing is aborted."
               (not (boolean? value))
               (throw (ex-info "invalid @propagate value"
                               {"@propagate" value}))
               ;; "(5.11.3) Otherwise, previous context was determined
               ;; before, and no further processing is necessary."
               ;; TODO: is there anything to do here? Set `result`?
               ;; stop processing?
               )
     ;; "(5.12) Create a map defined to keep track of whether or not a
     ;; term has already been defined or is currently being defined
     ;; during recursion."
     ::ex/let [defined {}]
     ;; "(5.13) For each key-value pair in context where key is not
     ;; @base, @direction, @import, @language, @propagate, @protected,
     ;; @version, or @vocab, invoke the Create Term Definition
     ;; algorithm, passing result for active context, context for
     ;; local context, key, defined, the value of the @protected entry
     ;; from context, if any, for protected, and propagate ."
     ::ex/else (recur
                local-context
                (:context
                 (reduce
                  (fn [{result :context :keys [defined]} [_ v]]
                    (create-term-definition result context v defined
                                            ;; TODO many other params
                                            ))
                  {:context result :defined defined}
                  (apply dissoc context
                         #{"@base" "@direction" "@import" "@language"
                           "@propagate" "@protected" "@version" "@vocab"})))))))

(defn create-term-definition
  "https://www.w3.org/TR/json-ld-api/#h3_create-term-definition"
  ;; "(§ 6.2) Create Term Definition – This algorithm is called from
  ;; the Context Processing algorithm to create a term definition in
  ;; the active context for a term being processed in a local
  ;; context."

  ;; returns: a new active context, a new `defined` (recursion),
  ;; [others?]. I think the right structure to return might be
  ;; {:context ... :defined ...}.
  [active-context local-context term defined]
  (engelberg/cond
    ;; "(1) If defined contains the key term and the associated value
    ;; is true (indicating that the term definition has already been
    ;; created), return. Otherwise, if the value is false, a cyclic
    ;; IRI mapping error has been detected and processing is aborted."
    (contains? defined term) (if (defined term)
                               ;; TODO right thing to return?
                               {:context active-context :defined defined}
                               (throw (ex-info "cyclic IRI mapping"
                                               { ;; TODO show the cycle
                                                :defined defined
                                                :collided-term term})))
    ;; "(2) Set the value associated with defined's term key to
    ;; false. This indicates that the term definition is now being
    ;; created but is not yet complete."
    :let [defined (assoc defined term false)]
    ;; "(3) Since keywords cannot be overridden, term must not be a
    ;; keyword. Otherwise, a keyword redefinition error has been
    ;; detected and processing is aborted."
    (keyword? term) (throw (ex-info "keyword redefinition"
                                    {:keyword term}))
    :let [ ;; "(4) Remove any existing term definition for term in
          ;; active context." TODO could pull in `dissoc-in` here.
          active-context (update active-context
                                 :term-definitions
                                 #(dissoc % term))
          ;; "(5) Initialize value to a copy of the value associated
          ;; with the key term in local context."
          value (get local-context term)]
    ;; "(6) If value is null or value is a JSON object containing the
    ;; key-value pair @id-null, set the term definition in active
    ;; context to null, set the value associated with defined's key
    ;; term to true, and return."
    (or (nil? value)
        (and (associative? value)
             (= ["@id" nil] (find value "@id"))))
    {:context (assoc-in active-context [:term-definitions term] nil)
     :defined (assoc defined term true)}
    ;; "(7) Otherwise, if value is a string, convert it to a JSON
    ;; object consisting of a single member whose key is @id and whose
    ;; value is value."
    :let [value (if (string? value)
                  {"@id" value}
                  value)]
    ;; "(8) Otherwise, value must be a JSON object, if not, an invalid
    ;; term definition error has been detected and processing is
    ;; aborted."
    :do (when-not (associative? value)
          (throw (ex-info "invalid term definition: unexpected type"
                          {:term value})))
    :let [ ;; "(9) Create a new term definition, definition."
          definition {}
          ;; "(10) If value contains the key @type: (10.1) Initialize
          ;; type to the value associated with the @type key, which
          ;; must be a string. Otherwise, an invalid type mapping
          ;; error has been detected and processing is aborted."
          definition (if-let [[_ type] (find value "@type")]
                       ;; "(10.2) Set type to the result of using the
                       ;; IRI Expansion algorithm, passing active
                       ;; context, type for value, true for vocab,
                       ;; false for document relative, local context,
                       ;; and defined. If the expanded type is neither
                       ;; @id, nor @vocab, nor an absolute IRI, an
                       ;; invalid type mapping error has been detected
                       ;; and processing is aborted. (10.3) Set the
                       ;; type mapping for definition to type."

                       ;; (note that the intermediate `type` was
                       ;; omitted.)
                       (assoc definition
                              :type-mapping
                              (expand-iri active-context type
                                          {:vocab? true
                                           :local-context local-context
                                           :defined defined}))
                       definition)]
    ;; "(11) If value contains the key @reverse:"
    (contains? value "@reverse")
    (let [reverse-value (get value "@reverse")
          ;; "(11.1) If value contains an @id, member, an invalid
          ;; reverse property error has been detected and processing
          ;; is aborted."
          _ (when (contains? value "@id")
              (throw (ex-info "invalid reverse property"
                              {:value value})))
          ;; "(11.2) If the value associated with the @reverse key is
          ;; not a string, an invalid IRI mapping error has been
          ;; detected and processing is aborted."
          _ (when-not (string? reverse-value)
              (throw (ex-info "invalid IRI mappping"
                              {:illegal-reverse reverse-value})))
          ;; "(11.3) Otherwise, set the IRI mapping of definition to
          ;; the result of using the IRI Expansion algorithm, passing
          ;; active context, the value associated with the @reverse
          ;; key for value, true for vocab, false for document
          ;; relative, local context, and defined. If the result is
          ;; neither an absolute IRI nor a blank node identifier,
          ;; i.e., it contains no colon (:), an invalid IRI mapping
          ;; error has been detected and processing is aborted."
          iri (expand-iri active-context reverse-value
                          {:vocab? true
                           :local-context local-context
                           :defined defined})
          _ (when-not ((some-fn absolute-iri? blank-node-identifier?)
                       iri)
              (throw (ex-info "invalid IRI mapping"
                              {:iri iri})))
          definition (assoc definition :iri-mapping iri)
          ;; "(11.4) If value contains an @container member, set the
          ;; container mapping of definition to its value; if its
          ;; value is neither @set, nor @index, nor null, an invalid
          ;; reverse property error has been detected (reverse
          ;; properties only support set- and index-containers) and
          ;; processing is aborted."
          definition (merge
                      definition
                      (when-let [[_ container-mapping] (find value "@container")]
                        (when-not (contains? #{"@set" "@index" nil}
                                             container-mapping)
                          (throw (ex-info "invalid reverse property"
                                          {:invalid-container container-mapping})))
                        {:container-mapping container-mapping}))
          ;; "(11.5) Set the reverse property flag of definition to
          ;; true."
          definition (assoc definition :reverse-property? true)]
      ;; "(11.6) Set the term definition of term in active context to
      ;; definition and the value associated with defined's key term
      ;; to true and return."
      {:context (assoc-in active-context [:term-definitions term] definition)
       :defined (assoc defined term true)})
    :let [ ;; "(12) Set the reverse property flag of definition to false."
          definition (assoc definition :reverse-property? false)
          ;; "(13) If value contains the key @id and its value does
          ;; not equal term:"
          definition (merge
                      definition
                      (when-let [[_ id] (find value "@id")]
                        (when-not (= id term)
                          ;; "(13.1) If the value associated with the
                          ;; @id key is not a string, an invalid IRI
                          ;; mapping error has been detected and
                          ;; processing is aborted."
                          (when-not (string? id)
                            (throw (ex-info "invalid IRI mapping"
                                            {:iri id})))
                          ;; "(13.2) Otherwise, set the IRI mapping of
                          ;; definition to the result of using the IRI
                          ;; Expansion algorithm, passing active
                          ;; context, the value associated with the
                          ;; @id key for value, true for vocab, false
                          ;; for document relative, local context, and
                          ;; defined. If the resulting IRI mapping is
                          ;; neither a keyword, nor an absolute IRI,
                          ;; nor a blank node identifier, an invalid
                          ;; IRI mapping error has been detected and
                          ;; processing is aborted; if it equals
                          ;; @context, an invalid keyword alias error
                          ;; has been detected and processing is
                          ;; aborted."
                          (let [iri  (expand-iri active-context id
                                                 {:vocab? true
                                                  :local-context local-context
                                                  :defined defined})]
                            (when-not ((some-fn absolute-iri?
                                                absolute-iri?
                                                blank-node-identifier?)
                                       iri)
                              (throw (ex-info "invalid IRI mapping"
                                              {:iri iri})))
                            (when (= iri "@context")
                              (throw (ex-info "invalid keyword alias"
                                              {:iri iri})))
                            {:iri-mapping iri}))))]
    ;; (14). Very ugly. This section of the algorithm heavily assumes
    ;; mutability, whereas I have to track what could be modified with
    ;; `returns`. Each branch of the `engelberg/cond` yields an
    ;; updated `returns`.
    :let [{:keys [active-context defined definition]}
          (engelberg/cond
            :let [returns {:active-context active-context
                           :defined defined
                           :definition definition}]
            ;; "(14) Otherwise if the term contains a colon (:):"
            ;; (Note that this is more naturally expressed backwards)
            (not (str/includes? term ":")) returns
            ;; "(14.1) If term is a compact IRI with a prefix that is
            ;; a key in local context a dependency has been found. Use
            ;; this algorithm recursively passing active context,
            ;; local context, the prefix as term, and defined."
            :let [{:as returns
                   :keys [active-context defined definition]}
                  (merge returns
                         (when (and (compact-iri? term)
                                    (contains? local-context (compact-iri-prefix term)))
                           (set/rename-keys (create-term-definition active-context local-context)
                                            {:context :active-context})))]
            ;; "(14.2) If term's prefix has a term definition in
            ;; active context, set the IRI mapping of definition to
            ;; the result of concatenating the value associated with
            ;; the prefix's IRI mapping and the term's suffix."
            (get-in active-context [:term-definitions (compact-iri-prefix term)])
            (assoc-in returns
                      [:definition :iri-mapping]
                      (str (get-in active-context [:term-definitions (compact-iri-prefix term)])
                           (compact-iri-suffix term)))
            ;; "(14.3) Otherwise, term is an absolute IRI or blank
            ;; node identifier. Set the IRI mapping of definition to
            ;; term."
            (assoc-in returns [:definition :iri-mapping term]))]
    ;; "(15) Otherwise, if active context has a vocabulary mapping,
    ;; the IRI mapping of definition is set to the result of
    ;; concatenating the value associated with the vocabulary mapping
    ;; and term. If it does not have a vocabulary mapping, an invalid
    ;; IRI mapping error been detected and processing is aborted.
    :let [definition (if-let [mapping (get active-context :vocabulary-mapping)]
                       (assoc definition :iri-mapping (str mapping term))
                       (throw (ex-info "invalid IRI mapping"
                                       {})))]
    :let [definition (merge
                      definition
                      ;; "(16) If value contains the key @container:
                      ;; (16.1) Initialize container to the value
                      ;; associated with the @container key, which
                      ;; must be either @list, @set, @index, or
                      ;; @language. Otherwise, an invalid container
                      ;; mapping error has been detected and
                      ;; processing is aborted."
                      (when-let [[_ container] (find value "@container")]
                        (when-not (contains?
                                   #{"@list" "@set" "@index" "@language"}
                                   container)
                          (throw (ex-info "invalid container mapping"
                                          :container container)))
                        ;; "(16.2) Set the container mapping of
                        ;; definition to container."
                        {:container-mapping container}))
          definition (merge
                      definition
                      ;; "(17) If value contains the key @language and
                      ;; does not contain the key @type:"
                      (when (and (contains? value "@language")
                                 (not (contains? value "@type")))
                        ;; "(17.1) Initialize language to the value
                        ;; associated with the @language key, which
                        ;; must be either null or a string. Otherwise,
                        ;; an invalid language mapping error has been
                        ;; detected and processing is aborted."
                        (let [language (get value "@language")
                              _ (when-not (or (nil? language)
                                              (string? language))
                                  (throw
                                   (ex-info "invalid language mapping"
                                            {:language-mapping language})))
                              ;; "(17.2) If language is a string set
                              ;; it to lowercased language. Set the
                              ;; language mapping of definition to
                              ;; language."
                              language (some-> language str/lower-case)]
                          {:language-mapping language})))]
    ;; "(18) Set the term definition of term in active context to
    ;; definition and set the value associated with defined's key term
    ;; to true."
    {:context (assoc-in active-context [:term-definitions term] definition)
     :defined (assoc defined term true)}))

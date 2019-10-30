(ns json-ld.context
  "Implementation of JSON-LD context processing
  algorithms (https://www.w3.org/TR/json-ld-api/#context-processing-algorithms)."
  (:refer-clojure :exclude [keyword?])
  (:require [better-cond.core :as engelberg]
            [clojure.set :as set]
            [clojure.string :as str]))

;;; helpers that may be moved to other namespaces

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
  ;; The failure mode of `dereference` is described in (3.2.3):

  ;; "(3.2.3) ...If context cannot be dereferenced, a loading remote
  ;; context failed error has been detected and processing is
  ;; aborted. If the dereferenced document has no top-level JSON
  ;; object with an @context member, an invalid remote context has
  ;; been detected and processing is aborted; ..."
  (RuntimeException. "TODO implement"))

(defn keyword?
  [s]
  ;; also, maybe do this as a set of the JSON-LD reserved keywords.
  (RuntimeException. "TODO implement"))

(defn expand-iri
  ([active-context type] (expand-iri active-context type nil))
  ([active-context
    type
    {:keys [document-relative? vocab? local-context defined]}]
   (RuntimeException. "TODO implement")))

(declare create-term-definition)

;; TODO/NB: the `active-context` in `contextualize*` and
;; `create-term-definition` is described by
;; https://www.w3.org/TR/json-ld-api/#dfn-term-definition, so it has
;; keys like `:term-definitions`, `:base-iri`, &c. The `local-context`
;; should be a map that comes from the JSON-decoding, i.e. it looks
;; like https://www.w3.org/TR/2014/REC-json-ld-20140116/#the-context.

(defn- contextualize*
  ;; TODO bad name. Impl of 6.1 Context Processing Algorithm.

  ;; "This algorithm specifies how a new active context is updated
  ;; with a local context. The algorithm takes three input variables:
  ;; an active context, a local context, and an array remote contexts
  ;; which is used to detect cyclical context inclusions. If remote
  ;; contexts is not passed, it is initialized to an empty array."

  ;; Note that `document-iri` is also passed (the algorithm assumes it
  ;; to be available).
  [{:as active-context
    :keys [base-iri]}
   local-context
   document-iri
   remote-contexts]

  ;; "(2) If local context is not an array, set it to an array
  ;; containing only local context."
  (let [local-contexts (if (sequential? local-context)
                         ;; TODO: check if this properly handles when
                         ;; local-context is nil. I think it does, as
                         ;; nil and [] should be handled differently
                         ;; by my reading of the spec.
                         local-context
                         [local-context])]
    (loop [;; "(1) Initialize result to the result of cloning active
           ;; context."
           result active-context
           local-contexts local-contexts]
      ;; "(3) For each item context in local context:"
      (if-let [[context & rest] (seq local-contexts)]
        (condp #(%1 %2) context
          ;; "(3.1) If context is null, set result to a
          ;; newly-initialized active context and continue with the
          ;; next context. The base IRI of the active context is set
          ;; to the IRI of the currently being processed document
          ;; (which might be different from the currently being
          ;; processed context), if available; otherwise to null. If
          ;; set, the base option of a JSON-LD API Implementation
          ;; overrides the base IRI."
          nil? (recur {:base-iri document-iri} rest)
          ;; "(3.2) If context is a string, ..."
          string?
          ;; "(3.2.1) Set context to the result of resolving value
          ;; [sic, should be context] against the base IRI which is
          ;; established as specified in section 5.1 Establishing a
          ;; Base URI of [RFC3986]. Only the basic algorithm in
          ;; section 5.2 of [RFC3986] is used; neither Syntax-Based
          ;; Normalization nor Scheme-Based Normalization are
          ;; performed. Characters additionally allowed in IRI
          ;; references are treated in the same way that unreserved
          ;; characters are treated in URI references, per section 6.5
          ;; of [RFC3987].
          (let [context (resolve-against base-iri context)]
            ;; "(3.2.2) If context is in the remote contexts array, a
            ;; recursive context inclusion error has been detected and
            ;; processing is aborted; otherwise, add context to remote
            ;; contexts."
            (when (contains? remote-contexts context)
              ;; TODO consider a craaaazy `deferror` macro which would
              ;; define a function that works pretty much like
              ;; `ex-info`.
              (throw
               (ex-info
                "A cycle in remote context inclusions has been detected."
                ;; TODO show the actual cycle
                {:remote-contexts remote-contexts
                 :added-twice context
                 :ref "https://www.w3.org/TR/json-ld-api/#idl-def-JsonLdErrorCode.recursive-context-inclusion"})))
            (as-> context %
              ;; "(3.2.3) Dereference context. ... set context to the
              ;; value of that member."
              dereference
              ;; "(3.2.4) Set result to the result of recursively
              ;; calling this algorithm, passing result for active
              ;; context, context for local context, and remote
              ;; contexts."
              (contextualize* result
                              %
                              document-iri
                              (conj remote-contexts context))
              ;; "(3.2.5) Continue with the next context."
              (recur % rest)))
          ;; "(3.3) If context is not a JSON object, an invalid local
          ;; context error has been detected and processing is
          ;; aborted."
          (complement map?)
          (throw (ex-info "invalid local context: unexpected type"
                          {:invalid-local-context context}))
          ;; context is a map
          (constantly true)
          ;; TODO: take this `(merge ...)` and do 3.7-8
          (merge
           result
           ;; "(3.4) If context has an @base key and remote contexts
           ;; is empty, i.e., the currently being processed context is
           ;; not a remote context:"
           (when-let [[_ value] (and (empty? remote-contexts)
                                     (find context "@base"))]
             ;; "(3.4.1) Initialize value to the value associated with
             ;; the @base key." (see above)
             (let [{:keys [base-iri]} result]
               {:base-iri
                (cond
                  ;; "(3.4.2) If value is null, remove the base IRI of
                  ;; result."
                  (nil? value) nil
                  ;; "(3.4.3) Otherwise, if value is an absolute IRI,
                  ;; the base IRI of result is set to value."
                  (absolute-iri? value) value
                  ;; "(3.4.4) Otherwise, if value is a relative IRI and
                  ;; the base IRI of result is not null, set the base
                  ;; IRI of result to the result of resolving value
                  ;; against the current base IRI of result."
                  (and (relative-iri? value) (some? base-iri))
                  (resolve-against base-iri value)
                  ;; "(3.4.5) Otherwise, an invalid base IRI error has
                  ;; been detected and processing is aborted."
                  :else (throw (ex-info "invalid base IRI"
                                        {:base-iri value})))}))
           ;; "(3.5) If context has an @vocab key:"
           (when-let [[_ value] (find context "@vocab")]
             ;; "(3.5.1) Initialize value to the value associated with
             ;; the @vocab key." (see above)
             {:vocab
              (cond
                ;; "(3.5.2) If value is null, remove any vocabulary
                ;; mapping from result."
                (nil? value) nil
                ;; "(3.5.3) Otherwise, if value is an absolute IRI
                ;; or blank node identifier, the vocabulary mapping
                ;; of result is set to value.
                ((some-fn absolute-iri? blank-node-identifier?) value) value
                ;; "...If it is not an absolute IRI or blank node
                ;; identifier, an invalid vocab mapping error has
                ;; been detected and processing is aborted."
                :else (throw (ex-info "invalid vocab mapping"
                                      {:vocab value})))})
           ;; "(3.6) If context has an @language key:"
           (when-let [[_ value] (find context "@language")]
             ;; "(3.6.1) Initialize value to the value associated with
             ;; the @language key." (see above)
             {:language
              (cond
                ;; "(3.6.2) If value is null, remove any default
                ;; language from result."
                (nil? value) nil
                ;; "(3.6.3) Otherwise, if value is string, the
                ;; default language of result is set to lowercased
                ;; value."
                (string? value) (str/lower-case value)
                ;; "...If it is not a string, an invalid default
                ;; language error has been detected and processing
                ;; is aborted."
                :else (throw (ex-info
                              "invalid default language: unexpected type"
                              {:language value})))})))
        ;; "(4) Return result."
        result))))

(defn contextualize
  ([active-context local-context]
   (contextualize active-context local-context
                  ;; per alg 6.1 step 3.1, this could be a default
                  ;; value identifying this json-ld impl. TODO
                  ;; consider it.
                  nil))
  ([active-context local-context document-iri]
   (contextualize* active-context local-context document-iri #{})))

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

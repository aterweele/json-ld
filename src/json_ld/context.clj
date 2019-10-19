(ns json-ld.context
  "Implementation of JSON-LD context processing
  algorithms (https://www.w3.org/TR/json-ld-api/#context-processing-algorithms)."
  (:require [clojure.string :as str]))

;;; helpers that may be moved to other namespaces

(defn absolute-iri?
  [iri]
  (RuntimeException. "TODO implement"))

(defn relative-iri?
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

(declare create-term-definition)

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
  ;; [others?] 
  [active-context local-context term defined]

  ;; "(1) If defined contains the key term..."
  (if-let [[_ term-defined] (find defined term)]
    ;; "...and the associated value is true (indicating that the term
    ;; definition has already been created), return.
    (if term-defined
      ;; TODO return what?!
      'todo
      ;; Otherwise, if the value is false, a cyclic IRI mapping error
      ;; has been detected and processing is aborted."
      (throw (ex-info "cyclic IRI mapping"
                      {;; TODO show the cycle
                       })))))

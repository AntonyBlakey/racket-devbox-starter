#lang racket
(require (for-syntax syntax/parse))

#;(define-grammar ldw::model
    [
     (<model> ("model" <fqn> (opt "extends" <fqn>) "{" (or <definition> <deletion> <update>) ... "}"))
     (<fqn> #:noskip (<identifier> ("::" <identifier>) ...))
     (identifier #:token (<identifier-start> <identifier-next> ...))
     (<identifier-start> #:token (or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9)))
     (<identifier-next> #:token (or <identifier-start> #\_))
     ])

(define-syntax define-model
  (λ (stx)
    (define (parse-type stx)
      (syntax-case stx (integer string boolean product sum opt seq set map enum)
        ; Core structural types
        [(product (name type) ...)
         #`(product-type
            (list #,@(map (λ (name type) #`(product-type-member '#,name #,(parse-type type)))
                          (syntax->list #'(name ...))
                          (syntax->list #'(type ...)))))]
        [(sum name ...) #'(sum-type (list (type-reference 'name) ...))]
        ; Generic types
        [(opt value-type) #`(opt-type #,(parse-type #'value-type))]
        [(seq value-type) #`(seq-type #,(parse-type #'value-type))]
        [(set value-type) #`(set-type #,(parse-type #'value-type))]
        [(map key-type value-type) #`(map-type #,(parse-type #'key-type) #,(parse-type #'value-type))]
        ; Primitive types
        [(enum s ...) #'(enum-type (list s ...))]
        [integer #'(integer-type)]
        [string #'(string-type)]
        [boolean #'(boolean-type)]
        ; Type references
        [name #'(type-reference 'name)]))
    (define (parse-definition stx)
      (syntax-case stx ()
        [(name type) #`(definition 'name #,(parse-type #'type))]))
    (define (parse-operation stx)
      (syntax-case stx (+ -)
        ; Deletions
        [(- name) #'(type-deletion 'name)]
        [(- name s) (string? (syntax->datum #'s)) #'(enum-member-deletion 'name s)]
        [(- name field-name-or-name) #'(member-deletion 'name 'field-name-or-name)]
        ; Additions
        [(+ name (field-name type)) #`(product-addition 'name (product-type-member 'field-name #,(parse-type #'type)))]
        [(+ name s) (string? (syntax->datum #'s)) #'(enum-addition 'name s)]
        [(+ name member-name) #'(sum-addition 'name (type-reference 'member-name))]))
    (syntax-parse stx
      [(_ name
          (~optional (~seq (~literal extends) (~describe #:role "predecessor model" "model name" super:id)) #:defaults ([super #'#f]))
          (~optional (~seq (~literal types) (type-definition ...)))
          (~optional (~seq (~literal modifications) (type-modification ...))))
       #`(define name (model 'name 'super (list #,@(map parse-definition (syntax->list #'(type-definition ...)))
                                                #,@(map parse-operation (syntax->list #'(type-modification ...))))))])))

(struct model (name extends operations) #:transparent)

(struct operation (name) #:transparent)

(struct definition operation (type) #:transparent)

(struct deletion operation () #:transparent)
(struct type-deletion deletion () #:transparent)
(struct member-deletion deletion (member-name) #:transparent)

(struct enum-member-deletion deletion (string) #:transparent)

(struct addition operation () #:transparent)
(struct product-addition addition (product-type-member) #:transparent)
(struct sum-addition addition (member-typename) #:transparent)
(struct enum-addition addition (string) #:transparent)

(struct type () #:transparent)

(struct product-type type (members) #:transparent)
(struct product-type-member (member-name type) #:transparent)
(struct sum-type type (member-typenames) #:transparent)

(struct opt-type type (value-type) #:transparent)
(struct seq-type type (value-type) #:transparent)
(struct set-type type (value-type) #:transparent)
(struct map-type type (key-type value-type) #:transparent)

(struct enum-type type (strings) #:transparent)
(struct integer-type type () #:transparent)
(struct string-type type () #:transparent)
(struct boolean-type type () #:transparent)
(struct type-reference type (name) #:transparent)

(define-model ldw::model::parsed
  extends ldw::model::base
  types (
     [<prod> (product (f1 string) (f2 boolean))]
     [<sum> (sum <int-set> <prod>)]
     [<enum> (enum "a" "b" "c")]
     [<int-set> (set integer)]
     [<string-bool-map> (map string boolean)]
     [<opt-prod> (opt <prod>)]
     [<seq-sum> (seq (sum <string-bool-map> <opt-prod>))])
  modifications (
     [- <seq-sum>]
     [- <prod> f1]
     [- <sum> <prod>]
     [- <enum> "b"]
     [+ <prod> (f3 integer)]
     [+ <sum> <opt-prod>]
     [+ <enum> "d"]))

(pretty-print ldw::model::parsed)
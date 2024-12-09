#lang racket

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
    (define (map-operations stx)
      (define (op stx)
        (define (td stx)
          (syntax-case stx (integer string boolean product sum opt seq set map enum)
            [(product (name type) ...)
             #`(product-type (list
                              #,@(for/list ([n (syntax->list #'(name ...))]
                                            [t (syntax->list #'(type ...))])
                                   #`(product-type-member '#,n #,(td t)))))]
            [(sum name ...) #'(sum-type (list (type-reference 'name) ...))]
            [(opt value-type) #`(opt-type #,(td #'value-type))]
            [(seq value-type) #`(seq-type #,(td #'value-type))]
            [(set value-type) #`(set-type #,(td #'value-type))]
            [(map key-type value-type) #`(map-type #,(td #'key-type) #,(td #'value-type))]
            [(enum s ...) #'(enum-type (list s ...))]
            [integer #'(integer-type)]
            [string #'(string-type)]
            [boolean #'(boolean-type)]
            [name #'(type-reference 'name)]))
        (syntax-case stx (+ -)
          [(- name) #'(type-deletion 'name)]
          [(- name s) (string? (syntax->datum #'s)) #'(enum-member-deletion 'name s)]
          [(- name field-name-or-name) #'(member-deletion 'name 'field-name-or-name)]
          [(+ name (field-name type)) #`(product-addition 'name (product-type-member 'field-name #,(td #'type)))]
          [(+ name s) (string? (syntax->datum #'s)) #'(enum-addition 'name s)]
          [(+ name member-name) #'(sum-addition 'name (type-reference 'member-name))]
          [(name type) #`(definition 'name #,(td #'type))]))
      (map op (syntax->list stx)))
    (syntax-case stx ()
      [(_ name operations) #`(model 'name #f (list #,@(map-operations #'operations)))]
      [(_ name super operations) #`(model 'name 'super (list #,@(map-operations #'operations)))])))

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

(pretty-print
 (define-model ldw::model::parsed ldw::model::base
   [
    (<prod> (product (f1 string) (f2 boolean)))
    (<sum> (sum <int-set> <prod>))
    (<enum> (enum "a" "b" "c"))
    (<int-set> (set integer))
    (<string-bool-map> (map string boolean))
    (<opt-prod> (opt <prod>))
    (<seq-sum> (seq (sum <string-bool-map> <opt-prod>)))

    ; (+ <name> (<field-name> string))

    ; Modification
    ;  (- <name>)
    ;  (- <product-or-sum-name> <field-name-or-name>)
    ;  (- <enum-name> string)
    ;  (+ <product-name> (<field-name> type))
    ;  (+ <sum-name> <name>)
    ;  (+ <enum-name> string)
    ]))

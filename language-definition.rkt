#lang racket
(require json-parsing)

(struct element () #:transparent)

(struct parser element (name lexer) #:transparent)
(struct peg-parser parser (atomic? [element #:mutable]) #:transparent)
(struct pratt-parser parser ([operators #:mutable] [primaries #:mutable]) #:transparent)

(struct sequence element (child-elements) #:transparent)
(struct alternatives element (child-elements) #:transparent)

(struct optional element (child-element) #:transparent)
(struct zero-or-more element (child-element) #:transparent)
(struct one-or-more element (child-element) #:transparent)

(struct text element (text) #:transparent)
(struct range element (from to) #:transparent)

(struct language (name root parsers versions) #:transparent)

(define (json->language json)
  (match-define (hash 'name name 'versions versions 'root_item root-item 'sections sections #:open)
    (json->sjson json))

  (define parsers
    (for*/hash ([section sections]
                [topic (hash-ref section 'topics)]
                #:do [(define lexer (hash-ref topic 'lexical_context #f))]
                [item (hash-ref topic 'items)])
      (match-define (hash-table (type (hash 'item (hash 'name name #:rest definition))))
        item)
      (values name
              (vector type definition
                      (match type
                        ['Precedence (pratt-parser name lexer '() '())]
                        [(or 'Struct 'Enum 'Separated 'Repeated) (peg-parser name lexer #f '())]
                        [(or 'Trivia 'Keyword 'Token 'Fragment) (peg-parser name lexer #t '())])))))

  (define root-parser
    (vector-ref (hash-ref parsers root-item) 2))
    
  (define (parser-from-reference hash)
    (vector-ref (hash-ref parsers (hash-ref hash 'reference)) 2))

  (define built-parsers
    (hash-map parsers
              (match-λ** [(_name (vector type definition parser))
                          (match type
                            ['Precedence (void)]
                            ['Struct (void)]
                            ['Enum
                             (set-peg-parser-element!
                              parser
                              (alternatives (for/vector ([variant (hash-ref definition 'variants)])
                                              (parser-from-reference variant))))]
                            ['Separated (void)]
                            ['Repeated (void)]
                            ['Trivia (void)]
                            ['Keyword (void)]
                            ['Token (void)]
                            ['Fragment (void)])
                          parser])))

  (language name root-parser built-parsers (list->vector versions)))

;; TODO: do this by printing the language definition explicitly
(parameterize ([print-graph #t])
  (pretty-write (json->language (file->string "language-definition.json"))))
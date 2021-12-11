(define-module (define-struct transformer)
  #:use-module (define-struct spec)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs lists)
  #:export (transformer))

(define (byte-getter-form type vec k e)
  (byte-accessor-form type vec k #t #f e))
(define (byte-setter-form type vec k n e)
  (byte-accessor-form type vec k #f n e))
(define (byte-accessor-form type vec k getter? n e)
  (let ((end
          (if (or (eq? type 'u8)
                  (eq? type 's8))
            '()
            (list
              `(endianness ,e)))))
    (cons* (symbol-append 'bytevector- type (if getter? '-ref '-set!))
           vec k (if getter?
                   end
                   (cons n end)))))

(define (def-mutator name field spec end xport?)
  (let* ((name
           (symbol-append name '. field '!))
         (form
           `(define (,name vec val)
              ,(byte-setter-form (typeof spec field) 'vec
                                 (offsetof spec field) 'val end))))
    (if xport?
      `(begin ,form (export ,name))
      form)))

(define (def-accessor name field spec end xport?)
  (let* ((accessor-name
           (symbol-append name '. field))
         (form
           `(define ,accessor-name
              (case-lambda
                ((vec)
                 ,(byte-getter-form (typeof spec field) 'vec
                                    (offsetof spec field) end))
                ((vec val)
                 (let ((vec (bytevector-copy vec)))
                   (,(symbol-append name '. field '!) vec val)
                   vec))))))
    (if xport?
      `(begin ,form (export ,accessor-name))
      form)))

(define (def-membership name spec xport?)
  (let* ((membership-name
           (symbol-append name '?))
         (form
           `(define (,membership-name vec)
              (and (bytevector? vec)
                   (= (bytevector-length vec)
                      ,(sizeof spec))))))
    (if xport?
      `(begin ,form (export ,membership-name))
      form)))

;; (define-struct foo ((x u32) (y u32)))

;; (make-foo (x 4) (y 7)) =>
;; (let ((vec (make-bytevector 8 0)))
;;   (foo.x! vec 4)
;;   (foo.y! vec 7)
;;   vec)

;; (def-constructor 'foo foo-spec 'little #f) =>
;; (define-syntax make-foo
;;   (lambda (syntax)
;;     (datum->syntax syntax)
;;       (match (syntax->datum syntax)
;;         ((_ . inits)
;;           `(let ((vec (make-bytevector 8 0)))
;;              ,@(map (lambda (init)
;;                       (match init ((field val)
;;                         (list (symbol-append 'foo '. field '!) 'vec val))))
;;                     inits)
;;              vec)))))

(define (def-constructor name spec xport?)
  (let* ((constructor-name
           (symbol-append 'make- name))
         (form
           (list 'define-syntax constructor-name
             (list 'lambda '(syntax)
               (list 'datum->syntax 'syntax
                 (list 'match '(syntax->datum syntax)
                   (list '(_ . inits)
                     (list 'quasiquote
                       (list 'let `((vec (make-bytevector ,(sizeof spec) 0)))
                         (list 'unquote-splicing
                           `(map (lambda (init)
                                   (match init ((field val)
                                     (list (symbol-append ',name '. field '!)
                                           'vec val))))
                                 inits))
                         'vec)))))))))
    (if xport?
      `(begin ,form (export ,constructor-name))
      form)))

(define transformer
  (lambda (syntax)
    (datum->syntax syntax
      (match (syntax->datum syntax)
        ((_ name membs . args)
         (let-keywords args #f ((export? #f)
                                (endianness (native-endianness)))
           (let ((spec (membs->spec membs)))
             (cons* 'begin
               (def-constructor name spec export?)
               (def-membership name spec export?)
               (fold-left
                 (lambda (a field)
                   (cons* (def-mutator name field spec endianness export?)
                          (def-accessor name field spec endianness export?)
                          a))
                 '()
                 (fields spec))))))))))

(define-module (define-struct)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:export (define-struct))

(define (type->size type)
  (match type
    ('u8 1)
    ('s8 1)
    ('u16 2)
    ('s16 2)
    ('u32 4)
    ('s32 4)
    ('u64 8)
    ('s64 8)
    ('ieee-single 4)
    ('ieee-double 8)))

;; membs:
;; ((name type) (name type))
;; spec:
;; (size (name type offset) (name type offset))

;; Return a new spec containing all members of spec a and spec b.
;; If union?, then merge as a union, rather than as a struct.
(define (merge-spec a union? b)
  (cons ((if union? max +) (car a) (car b))
    (append
      (cdr a)
      (map (lambda (elt)
             (match elt ((name type offset)
               (list name type (if union? offset
                                 (+ offset (car a)))))))
           (cdr b)))))

(define* (membs->spec membs #:key (spec '(0)) (union? #f))
  (if (null? membs)
    spec
    (membs->spec (cdr membs)
      #:union? union?
      #:spec
        (merge-spec spec union?
          (match (car membs)
            (('union . sub-membs)
              (membs->spec sub-membs #:union? #t))
            (('struct . sub-membs)
              (membs->spec sub-membs #:union? #f))
            ((name type)
              (list (type->size type)
                    (list name type 0))))))))

(define (offsetof spec field)
  (let loop ((spec (cdr spec)))
    (match (car spec) ((name type offset)
      (if (eq? name field)
        offset
        (loop (cdr spec)))))))

(define (typeof spec field)
  (let loop ((spec (cdr spec)))
    (match (car spec) ((name type offset)
      (if (eq? name field)
        type
        (loop (cdr spec)))))))

(define (fields spec)
  (map (lambda (entry)
         (match entry ((name type offset)
           name)))
       (cdr spec)))

(define (sizeof spec)
  (car spec))

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

(define (def-accessor name field spec end xport?)
  (let* ((name
           (symbol-append name '. field))
         (form
           `(define ,name
              (case-lambda
                ((d)
                 ,(byte-getter-form (typeof spec field) 'd
                                    (offsetof spec field) end))
                ((d val)
                 (let ((d (bytevector-copy d)))
                   ,(byte-setter-form (typeof spec field) 'd
                                      (offsetof spec field) 'val end)
                   d))))))
    (if xport?
      `(begin ,form (export ,name))
      form)))

(define (def-membership name spec xport?)
  (let* ((name
           (symbol-append name '?))
         (form
           `(define (,name s)
              (and (bytevector? s)
                   (= (bytevector-length s)
                      ,(sizeof spec))))))
    (if xport?
      `(begin ,form (export ,name))
      form)))

(define (def-constructor name spec end xport?)
  (let* ((name
           (symbol-append 'make- name))
         (form
           `(define ,(cons name (fields spec))
              (let ((s (make-bytevector ,(sizeof spec) 0)))
                ,@(map (lambda (field)
                        (byte-setter-form (typeof spec field) 's
                                          (offsetof spec field) field end))
                      (fields spec))
                s))))
    (if xport?
      `(begin ,form (export ,name))
      form)))

(define-syntax define-struct
  (lambda (syntax)
    (datum->syntax syntax
      (match (syntax->datum syntax)
        ((_ name membs . args)
         (let-keywords args #f ((export? #f)
                                (endianness (native-endianness)))
           (let ((spec (membs->spec membs)))
             (cons* 'begin
               (def-constructor name spec endianness export?)
               (def-membership name spec export?)
               (map (lambda (field)
                      (def-accessor name field spec endianness export?))
                    (fields spec))))))))))

(define-module (define-struct spec)
  #:use-module (ice-9 match)
  #:export (membs->spec offsetof typeof sizeof fields))

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

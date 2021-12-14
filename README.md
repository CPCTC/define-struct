## Define-struct
#### Yet another bytevector library

This library provides `define-struct`, which defines functions that operate on mytevects, interpreting thier contents in a specific way.

The seperate `Example` sections in the API reference below should be treated as one big example.

The library is imported with:

```scheme
(use-modules (define-struct))
```

#### define-struct
```text
[syntax]
  define-struct *name* *initializer-list*
                       [#:export?=#f]
                       [#:endianness=(native-endianness)]
    name := a symbol, not evaluated
    initializer-list := (*initializer* ...)
    initializer := (*field* *type*)
                 | (union . *initializer-list*)
                 | (struct . *initializer-list*)
    field := a symbol, not evaluated
    value := a type specifier, not evaluated. See below.

Defines syntax and procedures that access bytevectors, interpreting them as the type defined with *initializer-list*.

Every `(*field* *type*)` in *initializer-list* defines a field of the structure.
The field is refered to by the symbol *field*, and has type *type*. *type* must be one of the following symbols:
`u8`, `s8`, `u16`, `s16`, `u32`, `s32`, `u64`, `s64`, `ieee-single`, or `ieee-double`.

The struct stores values with the endianness specified by *endianness*, by default the native endianness. If given, this should be the symbol `little` or `big`.

Anonymous structs and unions are created with the second and third form of *initializer*.

The definitions made by this form are documented below. If *export?*, then they are exported from the current module.
```

Example:

```scheme
(define-struct person
  ((id s32)
   (salary ieee-single)))

(define-struct two-bytes
  ((union
     (both u16)
     (struct
       (low u8)
       (high u8))))
  #:endianness little)
```

#### sizeof-\<name>
```text
[variable]
  sizeof-<name>

An integer representing the size of a <name>, in bytes. Equivalent to the return value of (bytevector-length (make-<name>)).
```

Example:

```scheme
sizeof-person => 8

sizeof-two-bytes => ;; guess
```

#### \<name>?
```text
[procedure]
  <name>? *obj* => result
    obj := an object
    result := an object

Returns a truthy value if and only if *obj* is a bytevector of size *sizeof-<name>*.
```

Example:

```scheme
(person? (make-person)) => #t

(two-bytes?
  (make-bytevector 2 0)) => #t

(two-bytes?
  (make-bytevector 3 0)) => #f
```

#### make-\<name>
```text
[syntax]
  make-<name> *field-spec* ... => struct
    field-spec := (*field* *val*)
    field := a symbol, not evaluated
    val := a form
    struct := a bytevector

Create a new bytevector that satisfies <name>?, assigning each *field* of the bytevector to the corresponding *val*. Unspecified fields are filled with zero bytes.
```

Example:

```scheme
(make-two-bytes) => #vu8(0 0)

(make-two-bytes (both 4)) => #vu8(4 0)
```

#### \<name>.\<field>
```text
[procedure]
  <name>.<field> *vec* [*val*] => result
  vec := a bytevector
  val := an object
  result := an object, see below

If only one argument given, then retrieve the corresponding <field> from the bytevector *vec*, interpreting it as a <name>.

If two arguments are given, then return a new bytevector that is identical to *vec*, except whose field <field> is set to *val*. The original bytevector is unchanged.
```

Example:

```scheme
(let ((tb
        (make-two-bytes (both 1))))
  (two-bytes.low tb) => 1
  (two-bytes.both tb #xFFFF) => #vu8(255 255)
  (two-bytes.high tb) => 0
  )
```

#### \<name>.\<field>!
```text
[procedure]
  <name>.<field>! *vec* *val* => unspecified
  vec := a bytevector
  val := an object

Exactly like the 2-argument version of <name>.<field>, except that instead of returning a new bytevector, *vec* is modified. The return value is unspecified.
```

Examples:

```scheme
(let ((tb
        (make-two-bytes (high 255) (low 255))))
  (two-bytes.both! tb 0)
  tb) => #vu8(0 0)
```

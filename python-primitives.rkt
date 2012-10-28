#lang plai-typed

(require "python-core-syntax.rkt" "python-monad.rkt" "python-lib.rkt" "python-interp.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

#|
(define (class (obj : CVal)) : (PM CVal)
  (type-case CVal obj
    [VUndefined () (interp-error "local used before being set")]
    [VNone () (get-global "none-type")]
    [VTrue () (get-global "bool-type")]
    [VFalse () (get-global "bool-type")]
    [VNum (n) (get-global "num-type")]
    [VStr (s) (get-global "str-type")]
    [VBox (v) (interp-error "Boxes don't have a class")]
    [VObj (dict class) (get-box (list class))]
    [VPrimF (id) (get-global "func-type")]
    [VPrimMap (m) (interp-error "prim maps don't have a class")]
    [VClosure (e a v b) (get-global "func-type")]
    [VTuple (l) (get-global "tuple-type")]))

(define (dict (obj : CVal)) : (PM CVal)
  (type-case CVal obj
    [VObj (dict class) (get-box (list dict))]
    [else (m-return (VNone))]))

(define (super (c : CVal)) : (PM CVal)
  (m-do ([c-c (class c)]
         [class-type (get-global "class-type")]
         [(if (eq? c-c class-type)
              (pm-catch-error
               (local-lookup c (VStr "__super__"))
               (lambda (error)
                 (get-global "obj-type")))
              (interp-error "only classes have superclasses"))])))

(define (prim-dict-add (c-dict : CVal) (key : CVal) (val : CVal)) : (PM CVal)
  (type-case CVal c-dict
    [VPrimMap (m)
              (m-return (VPrimMap (hash-set m key val)))]
    [else (interp-error "prim-dict-add expects a prim-dict")]))

(define (prim-dict-lookup (c-dict : CVal) (key : CVal)) : (PM CVal)
  (type-case CVal c-dict
    [VPrimMap (m)
              (type-case (optionof CVal) (hash-ref m key)
                [some (v) (m-return v)]
                [none () (interp-error
                          (string-append "key not found: "
                                         (to-string key)))])]
    [else (interp-error
           (string-append "prim-dict-lookup wants a dict: "
                          (to-string key)))]))

(define (local-lookup (obj : CVal) (key : CVal)) : (PM CVal)
  (m-do ([c-dict (dict obj)]
         [(prim-dict-lookup c-dict key)])))

(define (class-has-member? (args : (listof CVal))) : (PM CVal)
  (pm-catch-error (m-do ([(class-lookup args)])
                        (VTrue))
                  (lambda (x)
                    (m-return (VFalse)))))

(define (class-lookup (args : (listof CVal))) : (PM CVal)
  (m-do
   ([obj-type (get-global "obj-type")]
    [partial (get-global "partial-apply")]
    [(local [(define object (first args))
             (define name (first (rest args)))
             (define (iter c)
               (m-do ([c-dict (dict c)]
                      [res (pm-catch-error
                            (local-lookup c object)
                            (lambda (error)
                              (if (eq? c obj-type)
                                  (pm-error error)
                                  (m-bind (super c)
                                          iter))))]
                      [(if (or (VClosure? res)
                               (VPrimF? res))
                           (apply-func partial
                                       (list res object)
                                       (VTuple empty))
                           (m-return res))])))]
            (let ([object (first args)]
                  [name (first (rest args))])
              (m-bind (class object)
                      iter)))])))

(define (obj-lookup (args : (listof CVal))) : (PM CVal)
  (let ([obj (first args)]
        [name (first (rest args))])
    (cond
     [(equal? name (VStr "__dict__")) (dict obj)]
     [(equal? name (VStr "__class__")) (class obj)]
     [else (pm-catch-error
            (local-lookup obj name)
            (lambda (error)
              (class-lookup (list obj name))))])))

(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VNone () "None"]
    [VTuple (l) (let ([vals (reverse (map pretty l))])
                  (cond
                   [(empty? vals) "()"]
                   [(empty? (rest vals)) (string-append "("
                                                        (string-append (first vals)
                                                                       ",)"))]
                   [else (string-append
                          "("
                          (string-append
                           (foldl (lambda (c t)
                                    (string-append c
                                                   (string-append ", "
                                                                  t)))
                                  (first vals)
                                  (rest vals))
                           ")"))]))]
    [else (error 'prim (string-append
                        "Can't print this yet: "
                        (to-string arg)))]))

(define get-globals
  (pm-lookup-store -2))

(define (set-globals (v : CVal))
  (type-case CVal v
    [VPrimMap (m) (pm-add-store -2 v)]
    [else (interp-error "globals must be a prim dict")]))

(define (get-global (arg : string))
  (m-do ([d get-globals]
         [(prim-dict-lookup d (VStr arg))])))

(define (add-global (name : string) (val : CVal))
  (m-do ([d get-globals]
         [new-d (prim-dict-add d (VStr name) val)]
         [(set-globals new-d)])))

(define (print args)
  (m-do ([(m-return (display (pretty (first args))))]
         [(m-return (display "\n"))])
        (VNone)))

(define (equal args)
  (if (equal? (first args)
              (first (rest args)))
      (m-return (VTrue))
      (m-return (VFalse))))

(define (add args)
  (m-return (VNum
             (+ (VNum-n (first args))
                (VNum-n (first (rest args)))))))

(define (get-box (args : (listof CVal))) : (PM CVal)
  (let ([box (first args)])
    (pm-lookup-store (VBox-v box))))

(define (set-box args)
  (let ([box (first args)]
        [val (first (rest args))])
    (pm-add-store (VBox-v box) val)))

(define (tuple-append args)
  (m-return
   (foldr (lambda (t l)
            (append (VTuple-l t)
                    l))
          empty
          args)))

(define (python-prim op) : ((listof CVal) -> (PM CVal))
  (case op
    [(print) print]
    [(equal) equal]
    [(Add) add]
    [(get-box) get-box]
    [(set-box) set-box]
    [(class-has-member?) class-has-member?]
    [(class-lookup) class-lookup]
    [(tuple-append) tuple-append]))

|#
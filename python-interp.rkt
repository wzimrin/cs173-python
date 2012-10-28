#lang plai-typed

(require "python-core-syntax.rkt" 
         "python-monad.rkt"
         "python-lib.rkt"
         (typed-in racket/base [display : (string -> void)]))

;;note: interp and primitives need to be mutually recursive -- primops
;;need to lookup and apply underscore members (ie + calls __plus__),
;;and applying a function requires m-interp.  Since racket doesn't
;;allow mutually recursive modules, I had to move python-primitives
;;into python-interp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Primitives          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
   (VTuple
    (foldr (lambda (t l)
             (append (VTuple-l t)
                     l))
           empty
           args))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            interp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (take n lst)
  (cond
   [(or (empty? lst)
        (= n 0)) empty]
   [else (cons (first lst)
               (take (- n 1)
                     (rest lst)))]))

(define (drop n lst)
  (cond [(empty? lst) empty]
        [(= n 0) lst]
        [else (drop (- n 1) (rest lst))]))

(define (apply-func func args varargs)
  (let ([args (append args
                      (VTuple-l varargs))])
    (type-case CVal func
      [VClosure
       (c-env off-args off-vararg body)
       (let ([named-args (take (length off-args) args)]
             [varargs (drop (length off-args) args)])
         (if (or (< (length args)
                    (length off-args))
                 (and (not (empty? varargs))
                      (none? off-vararg)))
             (interp-error "Application failed with arity mismatch")
             (m-do ([new-env
                     (m-foldl
                      (lambda (pair env)
                        (local [(define-values (name val) pair)]
                               (m-do ([loc (add-new-loc val)])
                                     (hash-set env name loc))))
                      (type-case (optionof symbol) off-vararg
                        [none () (m-return c-env)]
                        [some (name)
                              (m-do ([loc (add-new-loc (VTuple varargs))])
                                    (hash-set c-env name loc))])
                      (map2 (lambda (x y)
                              (values x y))
                            off-args named-args))]
                    [(pm-catch-return (m-do ([(m-interp body
                                                        new-env)])
                                            (VNone))
                                      m-return)]))))]
      [VPrimF (id) ((python-prim id) args)]
      [else (interp-error (string-append "Applied a non-function: "
                                         (to-string func)))])))

(define (m-interp expr env) : (PM CVal)
  (type-case CExp expr
    [CUndefined () (m-return (VUndefined))]
    [CNone () (m-return (VNone))]
    [CTrue () (m-return (VTrue))]
    [CFalse () (m-return (VFalse))]
    [CNum (n) (m-return (VNum n))]
    [CStr (s) (m-return (VStr s))]
    [CBox (v) (m-do ([val (m-interp v env)]
                     [loc (add-new-loc val)])
                    (VBox loc))]
    [CObj (d c)
          (m-do ([dict (m-interp d env)]
                 [class (m-interp c env)])
                (VObj dict class))]
    [CPrimMap (vals)
              (m-do ([contents
                      (m-map (lambda (pair)
                               (local [(define-values (key val) pair)]
                                      (m-do ([key (m-interp key env)]
                                             [val (m-interp val env)])
                                            (values key val))))
                             vals)])
                    (VPrimMap
                     (hash contents)))]
    [CTuple (l)
            (m-do ([contents (m-map (lambda (v)
                                      (m-interp v env))
                                    l)])
                  (VTuple contents))]

    [CId (x)
         (type-case (optionof Location) (hash-ref env x)
           [some (l)
                 (m-do ([store pm-get-store]
                        [(let ([v (type-case (optionof CVal) (hash-ref store l)
                                    [some (v) v]
                                    [none () (error 'interp
                                                    (string-append
                                                     "can't find loc for var: "
                                                     (to-string x)))])])
                           (if (VUndefined? v)
                               (interp-error (string-append "local used before it was defined: "
                                                            (to-string x)))
                               (m-return v)))]))]
           [none () (interp-error (string-append "Unbound identifier: "
                                                 (to-string x)))])]
    [CSet! (id v)
           (m-do ([v (m-interp v env)]
                  [(type-case (optionof Location) (hash-ref env id)
                     [some (l) (pm-add-store l v)]
                     [none () (error 'interp (string-append "variable never bound:"
                                                            (to-string id)))])]))]
    [CLet (x bind body)
          (m-do ([val (m-interp bind env)]
                 [loc (add-new-loc val)]
                 [(m-interp body (hash-set env x loc))]))]
    [CAddGlobal (id bind)
                (m-do ([bind (m-interp bind env)]
                       [(add-global (symbol->string id) bind)]))]
    [CSeq (e1 e2)
          (m-do ([(m-interp e1 env)]
                 [(m-interp e2 env)]))]

    [CFunc (args vararg body)
           (m-return (VClosure env args vararg body))]
    [CApp (func args varargs)
          (m-do ([func (m-interp func env)]
                 [args (m-map (lambda (arg) (m-interp arg env)) args)]
                 [varargs (m-interp varargs env)]
                 [(apply-func func args varargs)]))]
    [CReturn (v)
             (m-do ([v (m-interp v env)]
                    [(pm-return v)]))]
    [CPrimF (id) (m-return (VPrimF id))]
    [CIf (test t e)
         (m-do ([test-v (m-interp test env)]
                [(if (equal? test-v (VFalse))
                     (m-interp e env)
                     (m-interp t env))]))]
    [CError (val)
            (m-bind (m-interp val env) pm-error)]
    ;;[else (error 'm-interp (string-append "not implemented: "
    ;;                                      (to-string expr)))]
    ))

(define (interp expr)
  (local [(define-values (store res)
            ((m-interp expr (hash (list)))
             empty-store))]
         (type-case (ROption CVal) res
           [RValue (v) v]
           [RReturn (v) (error 'interp "returned when not in function context")]
           [RError (v) (error 'interp (to-string v))])))



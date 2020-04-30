;; script: fennel
(global lume (require :lume))
(global *t* 0)
(global *x* 96)
(global *y* 24)

(macros
 {
  :incf (fn [a ?n]
           "Shortcut for increment. Increments a by n"
           `(set ,a (+ ,a (or ,?n 1))))

  :decf (fn [a ?n]
           "Shortcut for decrement Decrements a by n"
           `(set ,a (- ,a (or ,?n 1))))

  :decg (fn [a ?n]
            "Shortcut for global decrement. Decrements a by n"
            `(global ,a (- ,a (or ,?n 1))))

  :incg (fn [a ?n]
            "Shortcut for global increment. Increments a by n"
            `(global ,a (+ ,a (or ,?n 1))))
  })

(fn inc [x] (+ x 1))
(fn dec [x] (- x 1))

(fn oscilator [framecount period]
    "sine oscilator of frequency period given framecount"
    (math.sin (/ (* 2 math.pi framecount) period)))

(fn osc-min-max [framecount period min max]
    "sine oscilator between across a range from min to max"
    (+ min
       (/ (- max min) 2)
       (* (/ (- max min) 2)
          (oscilator framecount period))))

(fn tbl-osc [arr framecount period]
    "sine oscilator across the elements in an array"
    (let [l (length arr)]
      (. arr (lume.round (osc-min-max framecount period 1 l)))))

(fn stepper [n t period]
    "steps through the numbers 0 to n exclusive across the time period of time series t"
    (// (% t period)
        (/ period n)))

(fn simple-stepper [t period min max step]
    "simple-stepper returns a number based upon a minimum,
 a maximum and steps in a series"
    (+ (* step
          (stepper (/ (- (+ step max) min) step) t period)) min))

(fn tbl-stepper [xs t period]
    "tbl-stepper returns the items in a"
    (. xs (inc (stepper (length xs) t period))))

(fn button-handler []
    (if (btn 0) (decg *y*))
    (if (btn 1) (incg *y*))
    (if (btn 2) (decg *x*))
    (if (btn 3) (incg *x*)))

(global s {:x 50 :y 50})

(global TIC
        (fn tic []

            (tset s :x (stepper 240 *t* 600))
            (tset s :y (osc-min-max *t* 120 30 120))


            (button-handler)

            (cls 12)

            (spr (tbl-osc [1 3 5] *t* 60)
                 72 24 14 3 0 0 2 2)

            (spr (tbl-stepper [3 5 3 1] *t* 60)
                 124 24 14 3 0 0 2 2)

            ;; (spr 0
            ;;      (. s :x) (. s :y)
            ;;      0)


            (lume.each [0 1 2 3]
                       (fn [n]
                           (pix (+ n (. s :x)) (+ n (. s :y)) n)))

            (print (lume.round (osc-min-max *t* 120 1 3)) 20 20)
            (print (stepper 5 *t* 120) 89 89)
            (print (simple-stepper *t* 120 1 9 2) 39 39)
            (print (tbl-stepper ["this" "is" "a" "call"] *t* 180) 66 80)

            (incg *t*)))

;; <TILES>
;; 000:0099990009cf99909ccc99999cc9999999999999999999990999999000999900
;; 001:efffffffff222222f8888888f8222222f8fffffff8ff0ffff8ff0ffff8ff0fff
;; 002:fffffeee2222ffee88880fee22280feefff80fff0ff80f0f0ff80f0f0ff80f0f
;; 003:efffffffff222222f8888888f8222222f8fffffff8fffffff8ff0ffff8ff0fff
;; 004:fffffeee2222ffee88880fee22280feefff80ffffff80f0f0ff80f0f0ff80f0f
;; 005:efffffffff222222f8888888f8222222f8fffffff8fffffff8fffffff8ff0fff
;; 006:fffffeee2222ffee88880fee22280feefff80ffffff80f0ffff80f0f0ff80f0f
;; 017:f8fffffff8888888f888f888f8888ffff8888888f2222222ff000fffefffffef
;; 018:fff800ff88880ffef8880fee88880fee88880fee2222ffee000ffeeeffffeeee
;; 019:f8fffffff8888888f888f888f8888ffff8888888f2222222ff000fffefffffef
;; 020:fff800ff88880ffef8880fee88880fee88880fee2222ffee000ffeeeffffeeee
;; 021:f8fffffff8888888f8888888f888fffff8888888f2222222ff000fffefffffef
;; 022:fff800ff88880ffe88880feef8880fee88880fee2222ffee000ffeeeffffeeee
;; </TILES>

;; <WAVES>
;; 000:00000000ffffffff00000000ffffffff
;; 001:0123456789abcdeffedcba9876543210
;; 002:0123456789abcdef0123456789abcdef
;; </WAVES>

;; <SFX>
;; 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
;; </SFX>

;; <PALETTE>
;; 000:140c1c44243430346d4e4a4e854c30346524d04648757161597dced27d2c8595a16daa2cd2aa996dc2cadad45edeeed6
;; </PALETTE>

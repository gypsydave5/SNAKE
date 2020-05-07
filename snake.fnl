;; script: fennel
(global lume (require :lume))
(global utils (require :utils))

(global *cell-size* 8)
(global *game-width* (/ 240 *cell-size*))
(global *game-height* (- (/ 136 *cell-size*) 1))

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

(local empty? utils.empty?)
(local rest utils.rest)
(local inc utils.inc)
(local dec utils.dec)
(local rest utils.rest)
(local butlast utils.butlast)
(local take utils.take)
(local first lume.first)
(local last utils.last)
(local apply utils.apply)
(local second utils.second)
(local partition utils.partition)

(fn make-snake [cells direction]
    {:cells cells :direction direction :next-direction direction})

(fn cell [x y]
    [x y])

(fn debug-cell [c r]
    (print (. c 1) 0 r)
    (print (. c 2) 20 r))

(fn wrap-cell [cell]
    "game field is a torus"
    [(% (. cell 1) *game-width*)
     (% (. cell 2) *game-height*)])

(fn sum-cell [c1 c2]
    (cell (+ (. c1 1)
             (. c2 1))
          (+ (. c1 2)
             (. c2 2))))

(fn sub-cell [c1 c2]
    (cell (- (. c1 1)
             (. c2 1))
          (- (. c1 2)
             (. c2 2))))

(fn random-cell []
    [(lume.round (lume.random (dec *game-width*)))
     (lume.round (lume.random (dec *game-height*)))])

(fn cell-equal? [c1 c2]
    (and (= (. c1 1)
            (. c2 1))
         (= (. c1 2)
            (. c2 2))))

(global *directions*
        {
         :up [0 1]
         :down [0 -1]
         :left [-1 0]
         :right [1 0]
         })

(fn opposite-dir? [d1 d2]
    (let [s (sum-cell d1 d2)]
      (and (= (. s 1) 0)
           (= (. s 2) 0))))

(fn change-direction [snake dir]
    (if (not (opposite-dir? dir (. snake :direction)))
        (tset snake :next-direction dir)))

(fn snake-head [snake]
    (first (. snake :cells)))

(fn snake-body [snake]
    (rest (. snake :cells)))

(fn grow [snake]
    (let [head (snake-head snake)]
      {
       :cells (lume.concat [(sum-cell (snake-head snake) (. snake :next-direction))]
                           (. snake :cells))
       :direction (. snake :next-direction)
       :next-direction (. snake :next-direction)
       }))

(fn move [snake]
    (let [s  (grow snake)]
      {
       :cells (butlast (. s :cells))
       :direction (. s :direction)
       :next-direction (. s :next-direction)
       }))

(fn next-food [snake-head food]
    (let [eaten (lume.reject food (partial cell-equal? snake-head))]
      (if (empty? eaten)
          [(random-cell)]
          eaten)))

(fn next-snake [snake food]
    (let [s (grow snake)]
      (if (lume.any food (lambda [f] (cell-equal? f (snake-head s))))
          s
          (move snake))))

(fn out-of-bounds? [cell]
    (or (not (< -1 (. cell 1) *game-width*))
        (not (< -1 (. cell 2) *game-height*))))

(fn next-game [game]
    (let [ns (next-snake (. game :snake) (. game :food))]
      {
       :snake ns
       :food (next-food (snake-head ns) (. game :food))
       :over (or
              (out-of-bounds? (snake-head ns))
              (lume.any (snake-body ns)
                        (partial cell-equal? (snake-head ns))))
       :score (inc (. game :score))
       }))

(fn new-game []
    {
     :snake (make-snake [[5 7] [5 6] [5 5]] (. *directions* :up))
     :food [(random-cell)]
     :over false
     :score 0
     })

(fn render-sprite [sprite-index cell]
    (spr sprite-index
         (* *cell-size* (. cell 1))
         (* *cell-size* (inc (. cell 2)))))

(local render-snakecell
       (partial render-sprite 0))

(local render-food (partial render-sprite 1))

(fn status-bar [score]
    (rect 0 0 240 *cell-size* 0)
    (print (.. "SCORE: " score)  0 0 4))



(fn direction-of-segment [triple]
    (let [v1 (sub-cell (first triple) (. triple 2))
             v2 (sub-cell (. triple 3) (. triple 2))]
      (match [v1 v2]
             [[1 0] [-1 0]] :horizontal
             [[-1 0] [1 0]] :horizontal
             [[0 1] [0 -1]] :vertical
             [[0 -1] [0 1]] :vertical
             [[0 1] [-1 0]] :down-left
             [[-1 0] [0 1]] :down-left
             [[1 0] [0 1]] :down-right
             [[0 1] [1 0]] :down-right
             [[0 -1] [-1 0]] :up-left
             [[-1 0] [0 -1]] :up-left
             [[1 0] [0 -1]] :up-right
             [[0 -1] [1 0]] :up-right)))

(fn cell-x [c]
    (* *cell-size* (. c 1)))

(fn cell-y [c]
    (* *cell-size* (inc (. c 2))))

(fn render-body [triple]
    (let [direction (direction-of-segment triple)
                    c (. triple 2)]
      (match direction
             :horizontal (spr 17 (cell-x c) (cell-y c) 7 1 0 0)
             :vertical (spr 17 (cell-x c) (cell-y c) 7 1 0 1)
             :down-left (spr 18 (cell-x c) (cell-y c) 7 1 0 0)
             :down-right (spr 18 (cell-x c) (cell-y c) 7 1 1 0)
             :up-left (spr 18 (cell-x c) (cell-y c) 7 1 2 0)
             :up-right (spr 18 (cell-x c) (cell-y c) 7 1 3 0)
             _ (render-sprite 0 c))))

(fn render-head [headparts]
    (let [rot (match (apply sub-cell headparts)
                [0 -1] 0
                [1 0] 1
                [0 1] 2
                [-1 0] 3)
              c (first headparts)]
      (spr 16 (cell-x c) (cell-y c) 1 1 0 rot)))

(fn render-dead-head [headparts]
    (let [rot (match (apply sub-cell headparts)
                [0 -1] 0
                [1 0] 1
                [0 1] 2
                [-1 0] 3)
              c (first headparts)]
      (spr 20 (cell-x c) (cell-y c) 1 1 0 rot)))

(fn render-tail [tail]
    (let [c (last tail)
            v (sub-cell (first tail) c)]
      (match v
             [-1 0] (spr 19 (cell-x c) (cell-y c) 7 1 0 0)
             [1 0] (spr 19 (cell-x c) (cell-y c) 7 1 0 2)
             [0 1] (spr 19 (cell-x c) (cell-y c) 7 1 0 3)
             [0 -1] (spr 19 (cell-x c) (cell-y c) 7 1 0 1))))

(fn render-snake [snake]
    (let [cells (. snake :cells)]
      (lume.each (butlast (butlast (partition 3 1 cells)))
                 render-body)
      (render-tail (lume.slice cells (dec (length cells)) (length cells)) )
      (render-head (utils.take 2 cells))))

(fn render-dead-snake [snake]
    (let [cells (. snake :cells)]
      (lume.each (butlast (butlast (partition 3 1 cells)))
                 render-body)
      (render-tail (lume.slice cells (dec (length cells)) (length cells)))
      (render-dead-head (rest (utils.take 3 cells)))))

(fn render-game [game]
    (status-bar (. game :score))
    (lume.each (. game :food)
               render-food)
    (if (. game :over)
        (render-dead-snake (. game :snake))
        (render-snake (. game :snake))))

(global *game* (new-game))
(global *t* 0)
(global *speed* 10)

(fn button-handler [game]
    (when (btn)
      (change-direction (. game :snake)
                        (if (btn 0) (. *directions* :down)
                            (btn 1) (. *directions* :up)
                            (btn 2) (. *directions* :left)
                            (btn 3) (. *directions* :right)
                            (. game :snake :next-direction)))))

(fn debug []
    ;; (print (. *game* :snake :next-direction 1) 0 20)
    ;; (print (. *game* :snake :next-direction 2) 10 20)
    ;; (print (. *game* :snake :direction 1) 0 30)
    ;; (print (. *game* :snake :direction 2) 10 30)
    (print  (. (snake-head (. *game* :snake)) 1) 0 40)
    (print  (. (snake-head (. *game* :snake)) 2) 20 40)

    (lume.each (. *game* :food) (lambda [fc]
                                  (print (. fc 1) 0 60)
                                  (print (. fc 2) 20 60)))
    ;; (print (cell-equal? (snake-head (. *game* :snake)) (. *game* :food 1)) 0 80)
    (print *t* 0 0))

(fn play-game []
    (cls 7)

    (button-handler *game*)

    (when (= 0 (% *t* *speed*))
      (global *game* (next-game *game*)))

    (render-game *game*)

    (incg *t*))

(fn game-over []
    (print "GAME OVER" 50 50)
    (print "PRESS Z TO PLAY AGAIN" 50 60)
    (if (btn 4) (global *game* (new-game))))

(global TIC (fn tic []
                (if (. *game* :over)
                    (game-over)
                    (play-game))))

;; <TILES>
;; 000:77666677765c666765cc66666555666665666666666666667666666777666677
;; 001:7777677777776777777666777322766732223267322322277223222777772277
;; 016:7772777777726777776266777662266776655567766666677666666776c66c67
;; 017:7777777766666666666666666666666666666666666666666666666677777777
;; 018:7777777766666777666666776666666766666667666666676666666776666667
;; 019:7777777766667777666667776666666766666677666667776666777777777777
;; 020:222c222222222222762222677626226776262667766626677666266776666667
;; 032:0666666006666660066666600666666006666660066666600666666006666660
;; </TILES>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>


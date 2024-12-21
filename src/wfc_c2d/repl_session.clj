(ns wfc-c2d.repl-session
  (:require
   [clojure2d.core :as c2d]
   [clojure2d.pixels :as pix]
   [fastmath.core :as m]
   [fastmath.vector :as v]
   [lambdaisland.data-printers :as printers]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(printers/register-print fastmath.vector.Vec4 "vec4" vec)
(printers/register-pprint fastmath.vector.Vec4 "vec4" vec)

(defn to-vec4 [m]
  (apply v/vec4 m))

(defn size [o]
  [(c2d/width o) (c2d/height o)])

(defn area
  ([n]
   (for [^long x (range n)
         ^long y (range n)
         :when (and (<= 0 x) (<= 0 y))]
     [x y]))
  ([w h]
   (for [^long x (range w)
         ^long y (range h)
         :when (and (<= 0 x) (<= 0 y))]
     [x y]))
  ([^long x ^long y w h]
   (for [^long xx (range w)
         ^long yy (range h)
         :let [x (+ x xx)
               y (+ y yy)]
         :when (and (<= 0 x) (<= 0 y))]
     [x y]))
  ([x y w h {:keys [max-x max-y ^long pad]
             :or {max-x Long/MAX_VALUE
                  max-y Long/MAX_VALUE
                  pad 0}}]
   (let [x (long x)
         y (long y)
         w (long w)
         h (long h)]
     (for [x (range (- x pad) (+ x w pad))
           y (range (- y pad) (+ y h pad))
           :when (and (<= 0 x max-x) (<= 0 y max-y))]
       [x y]))))

(defn win->cnv [window canvas ^long x ^long y]
  [(* x (double (/ (c2d/width canvas) (c2d/width window))))
   (* y (double (/ (c2d/height canvas) (c2d/height window))))])

(defn tile-draw [canvas window _ {:keys [pixels]}]
  (when (and (c2d/mouse-pressed? window))
    (let [[x y] (win->cnv window canvas (c2d/mouse-x window) (c2d/mouse-y window))]
      (if (<= 0 x (dec (c2d/width canvas)))
        (pix/set-color! pixels x y @color)
        (reset! color (pix/get-color palette 0 y)))))
  (pix/set-canvas-pixels! canvas tile)
  (pix/set-canvas-pixels! canvas tsize 0 palette)
  {:pixels pixels})

(defn set-palette! [pixels]
  (doseq [[idx color]
          (map vector
               (range)
               (concat
                [[0 0 0]
                 [255 255 255]]
                (for [a [100 200 255]
                      b [100 200 255]
                      c [100 200 255]
                      :when (and (not= a b) (not= b c) (not= a c))]
                  [a b c])))]
    (pix/set-color! pixels 0 idx color)))

(defn transpose [m]
  (apply mapv vector m))

(defn rotate [m]
  (mapv (comp vec reverse) (transpose m)))

(defn mirrorx [m]
  (mapv reverse m))

(mirrory [[1 2 3] [4 5 6]])

(defn mirrory [m]
  (vec (reverse m)))

(defn rotations [m]
  (take 4 (iterate rotate m)))

(defn variations [m]
  (mapcat
   rotations
   [m (mirrorx m) (mirrory m)]))

(defn tile-segments [tile ^long ssize]
  (distinct
   (mapcat
    variations
    (for [[^long x ^long y] (area (- (c2d/width tile) (dec ssize))
                                  (- (c2d/width tile) (dec ssize)))]
      (vec
       (for [^long dx (range ssize)]
         (vec
          (for [^long dy (range ssize)]
            (pix/get-color tile (+ x dx) (+ y dy))))))))))

(defn rand-pos [o ^long gap]
  [(rand-int (- (c2d/width o) gap))
   (rand-int (- (c2d/height o) gap))])

(defn draw-segment [output ^long x ^long y segment]
  (doseq [[^long sx ^long sy] (area ssize)]
    (pix/set-color! output
                    (+ x sx) (+ y sy)
                    (get-in segment [sy sx]))))

(def blank "Color representing no color"
  (v/vec4 1.0 1.0 1.0 255.0))

(defn can-place? [output ^long x ^long y segment]
  (reduce
   (fn [acc [^long sx ^long sy]]
     (let [c (pix/get-color output (+ x sx) (+ y sy))]
       (if (or (= c blank)
               (= c (get-in segment [sy sx])))
         acc
         (reduced false))))
   true
   (area (count (first segment)) (count segment))))

(def tsize "tile size" 8)
(def osize "output size" 64)
(def ssize "segment size" 3)

(def tscale "tile scale" 32)
(def oscale "output scale" 8)

(defonce tile (pix/pixels tsize tsize))
(def palette (pix/pixels 1 tsize))
(def color (atom (v/vec4 100 200 255 255)))

(set-palette! palette)

(c2d/show-window
 {:canvas (c2d/canvas (inc tsize) tsize)
  :w (* (inc tsize) tscale)
  :h (* tsize tscale)
  :draw-fn #'tile-draw
  :draw-state {:pixels tile}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def output (pix/pixels osize osize))

(c2d/show-window
 {:canvas (c2d/canvas osize osize)
  :w (* oscale osize)
  :h (* oscale osize)
  :draw-fn (fn [canvas _ _ _]
             (pix/set-canvas-pixels! canvas output))})

(def segments (tile-segments tile ssize))
(def positions (atom {}))
(for [[x y] (apply area (size output))]
  (pix/set-color! output x y blank))

;; Plant the seed
(swap! positions
       (fn [p]
         (let [pos (rand-pos output ssize)
               seg (rand-nth segments)
               _ (draw-segment output (first pos) (second pos) seg)
               p (assoc p pos [seg])]
           (reduce
            (fn [acc pos]
              (let [[x y] pos
                    f (filter (partial can-place? output x y))]
                (if-let [poss (get acc pos)]
                  (if (= 1 (count poss))
                    acc
                    (update acc pos (partial sequence f)))
                  (assoc acc pos (sequence f segments)))))
            p
            (area (inc (- (long (first pos)) ^long ssize))
                  (inc (- (long (second pos)) ^long ssize))
                  (+ ^long ssize (dec ^long ssize))
                  (+ ^long ssize (dec ^long ssize))
                  {:max-x (- (c2d/width output) ^long ssize)
                   :max-y (- (c2d/height output) ^long ssize)})))))

;; Watch it grow!
(while (some #(< 1 (count %)) (vals @positions))
  (swap! positions
         (fn [p]
           (let [pos (key (first (sort-by val
                                          (remove (comp #{1 0} val)
                                                  (update-vals @positions count)))))
                 ;; TODO: make sure this segment correctly overlaps with the
                 ;; possibility space of nearby pixels
                 seg (rand-nth (get p pos))
                 _ (draw-segment output (first pos) (second pos) seg)
                 p (assoc p pos [seg])]
             (reduce
              (fn [acc pos]
                (let [[x y] pos
                      f (filter (partial can-place? output x y))]
                  (if-let [poss (get acc pos)]
                    (if (= 1 (count poss))
                      acc
                      (update acc pos (partial sequence f)))
                    (assoc acc pos (sequence f segments)))))
              p
              (area (inc (- (long (first pos)) ^long ssize))
                    (inc (- (long (second pos)) ^long ssize))
                    (+ ^long ssize (dec ^long ssize))
                    (+ ^long ssize (dec ^long ssize))
                    {:max-x (- (c2d/width output) ^long ssize)
                     :max-y (- (c2d/height output) ^long ssize)}))))))

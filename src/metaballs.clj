(ns yuggoth.views.metaballs
  (:import
    [javax.swing JFrame]
    [java.awt Canvas Graphics Color]
    java.awt.image.BufferStrategy))

(set! *warn-on-reflection* true)

(def ^:const WIDTH (int 300))
(def ^:const HEIGHT (int 300))
(def ^:const THRESHOLD (double 1.005))

(defn ^:static move [{:keys [x y vx vy radius color]}]
  (let [vx (double (if (or (> x WIDTH) (neg? x)) (- vx) vx))
        vy (double (if (or (> y HEIGHT) (neg? y)) (- vy) vy))]
    {:x (double (+ x vx))
     :y (double (+ y vy))
     :vx vx
     :vy vy
     :radius radius
     :color color}))

(defn ^:static fix-color [c]
  (cond 
    (< c 0) 0
    (> c 255) 255
    :default c))

(defn ^:static color-in-range [r g b]
  (new Color (int (fix-color r)) (int (fix-color g)) (int (fix-color b))))

(defn ^:static falloff-color [c total]
  (* 2 (/ c  ((if (> total THRESHOLD) - +) THRESHOLD total))))

(defn ^:static influence 
  [{:keys [x y radius]} px py]
  (let [dx (double (- x px))
        dy (double (- y py))]
    (double (/ radius (Math/sqrt (+ (* dx dx) (* dy dy)))))))


(defn ^:static paint-square [^Graphics g ^Color color x y size]
  (doto g
    (.setColor color)
    (.fillRect x y size size)))

(defn ^:static compute-color [x y [sum red-cur green-cur blue-cur] ball]   
  (let [influence (influence ball x y)
        [r g b] (:color ball)] 
    [(+ sum influence)
     (+ red-cur (* influence r))
     (+ green-cur (* influence g))
     (+ blue-cur (* influence b))]))

(defn ^:static draw [^Canvas canvas balls]
  (let [^BufferStrategy buffer (.getBufferStrategy canvas)
        ^Graphics g            (.getDrawGraphics buffer)
        step 3]
    (try      
      (loop [x 0]
        (loop [y 0]          
          (let [[total red green blue] 
                (reduce (partial compute-color x y) [0 0 0 0] balls)]

            ;;center
            (if (>= total THRESHOLD)              
              (paint-square g (color-in-range red green blue) x y step))
            
            ;;outline
            (if (and (>= total THRESHOLD) (<= total THRESHOLD))                
              (paint-square g (color-in-range red green blue) x y step))
            
            ;;falloff
            (if (<= total THRESHOLD)
              (paint-square g 
                (color-in-range 
                  (falloff-color red total) 
                  (falloff-color green total) 
                  (falloff-color blue total))
                x y step)))            
          (if (< y HEIGHT) (recur (int (+ y step)))))
        (if (< x WIDTH) (recur (int (+ x step)))))
      
      (finally (.dispose g)))
    (if-not (.contentsLost buffer)
      (.show buffer)) ))
 
(defn metaball [_]
  {:x      (rand-int WIDTH)
   :y      (rand-int HEIGHT)
   :vx     (double (inc (rand-int 6)))
   :vy     (double (inc (rand-int 6)))
   :radius (+ 10 (rand-int 19))
   :color  [(rand-int 256) (rand-int 256) (rand-int 256)]})

(defn -main [& args]
  (let [frame  (JFrame. "Metaballs")
        canvas (Canvas.)]
     
    (doto frame
      (.setSize WIDTH HEIGHT)      
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add canvas)
      (.setVisible true))
 
    (doto canvas
      (.createBufferStrategy 2)      
      (.setVisible true)
      (.requestFocus))
         
    (loop [balls (map metaball (range 6))]
      (draw canvas balls)
      (recur (map move balls)))))
 
;(-main)
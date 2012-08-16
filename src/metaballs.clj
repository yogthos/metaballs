(ns metaballs
  (:import
    [javax.swing JFrame]
    [java.awt Canvas Graphics Color]
    java.awt.image.BufferStrategy))

(set! *warn-on-reflection* true)

(def ^:const WIDTH (int 300))
(def ^:const HEIGHT (int 300))
(def ^:const MIN-THRESHOLD (double 1))
(def ^:const MAX-THRESHOLD (double 1.1))

(defn ^:static move [{:keys [x y vx vy radius color]}]
  (let [new-vx (double (if (or (> x WIDTH) (neg? x)) (- vx) vx))
        new-vy (double (if (or (> y HEIGHT) (neg? y)) (- vy) vy))]
    {:x (double (+ x new-vx))
     :y (double (+ y new-vy))
     :vx new-vx
     :vy new-vy
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
  (* 2 (/ c  (if (> total MAX-THRESHOLD) 
               (- MAX-THRESHOLD total) 
               (+ MIN-THRESHOLD total)))))

(defn ^:static get-influence 
  [{:keys [x y radius]} px py]
  (let [dx (double (- x px))
        dy (double (- y py))
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (if (>= dist 0) (double (/ radius dist)) 0)))

(defn ^:static paint-square [^Graphics g ^Color color x y size]
  (.setColor g color)
  (.fillRect g x y size size))

(defn ^:static compute-color [x y [sum red-cur green-cur blue-cur] ball]   
  (let [influence (get-influence ball x y)
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
      (doto g
        (.setColor Color/BLACK)
        (.fillRect 0 0 WIDTH HEIGHT))

      (loop [x 0]
        (loop [y 0]          
          (let [[total red green blue] 
                (reduce (partial compute-color x y) [0 0 0 0] balls)]

            ;;center
            (if (>= total MIN-THRESHOLD)              
              (paint-square g (color-in-range red green blue) x y step))
            
            ;;outline
            (if (and (>= total MIN-THRESHOLD) (<= total MAX-THRESHOLD))                
              (paint-square g (color-in-range red green blue) x y step))
            
            ;;falloff
            (if (<= total MAX-THRESHOLD)
              (paint-square g 
                (color-in-range 
                  (falloff-color red total) 
                  (falloff-color green total) 
                  (falloff-color blue total))
                x y step)))            
          (if (< y HEIGHT)              
            (recur (int (+ y step)))))
        (if (< x WIDTH)           
          (recur (int (+ x step)))))
      
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
 
(-main)
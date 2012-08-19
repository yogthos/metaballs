(ns metaballs
  (:import
    [javax.swing JFrame]
    [java.awt Canvas Graphics Color]
    java.awt.image.BufferStrategy))

(def  SIZE 300)

(defn dv [p v]
  (if (or (> p SIZE) (neg? p)) (- v) v))

(defn move [{:keys [x y vx vy radius color]}]
  (let [vx (dv x vx)
        vy (dv y vy)]
    {:x (+ x vx)
     :y (+ y vy)
     :vx vx
     :vy vy
     :radius radius
     :color color}))

(defn color-in-range [c]
  (cond 
    (< c 0) 0
    (> c 255) 255
    :default c))

(defn color [r g b]
  (new Color (int (color-in-range r)) (int (color-in-range g)) (int (color-in-range b))))

(defn influence [{:keys [x y radius]} px py]
  (let [dx (double (- x px))
        dy (double (- y py))]
    (double (/ radius (Math/sqrt (+ (* dx dx) (* dy dy)))))))

(defn paint-square [^Graphics g ^Color color x y size]
  (doto g
    (.setColor color)
    (.fillRect x y size size)))

(defn compute-color [x y [red-cur green-cur blue-cur] ball]   
  (let [influence (influence ball x y)
        [r g b] (:color ball)] 
    [(+ red-cur (* influence r))
     (+ green-cur (* influence g))
     (+ blue-cur (* influence b))]))

(defn draw [^Canvas canvas balls]
  (let [buffer (.getBufferStrategy canvas)
        g      (.getDrawGraphics buffer)
        step   3]
    (try
      (loop [x 0]
        (loop [y 0]          
          (let [[red green blue] 
                (reduce (partial compute-color x y) [0 0 0] balls)]                        
            (paint-square g (color red green blue) x y step))            
          (if (< y (- SIZE step)) (recur (+ y step))))
        (if (< x (- SIZE step)) (recur (+ x step))))
      
      (finally (.dispose g)))
    (if-not (.contentsLost buffer)
      (.show buffer)) ))
 
(defn metaball [_]
  {:x      (rand-int SIZE)
   :y      (rand-int SIZE)
   :vx     (double (inc (rand-int 6)))
   :vy     (double (inc (rand-int 6)))
   :radius (+ 10 (rand-int 19))
   :color  [(rand-int 256) (rand-int 256) (rand-int 256)]})

(defn -main [& args]
  (let [frame  (JFrame. "Metaballs")
        canvas (Canvas.)]
     
    (doto frame
      (.setSize SIZE SIZE)      
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
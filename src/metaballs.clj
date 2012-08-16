(ns metaballs
  (:import
    [javax.swing JFrame]
    [java.awt Canvas Graphics Color]
    java.awt.image.BufferStrategy))

(def ^:const WIDTH (int 400))
(def ^:const HEIGHT (int 400))
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
  (int
    (cond 
      (< c 0) 0
      (> c 255) 255
      :default c)))

(defn ^:static color-in-range [r g b] 
  (new Color (fix-color r) (fix-color g) (fix-color b)))

(def color-in-range-memo (memoize color-in-range))

(defn ^:static falloff-color [c total]
  (* 2 (/ c  (if (> total MAX-THRESHOLD) 
               (- MAX-THRESHOLD total) 
               (+ MIN-THRESHOLD total)))))

(defn ^:static get-influence 
  [{:keys [^double x ^double y ^double radius]} ^double px ^double py]
  (let [dx (double (- x px))
        dy (double (- y py))
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (if (>= dist 0) (double (/ radius dist)) 0)))

(defn ^:static paint-square [^Graphics g ^Color color x y size]
  (.setColor g color)
  (.fillRect g x y size size))

(defn ^:static draw [^Canvas canvas balls]
  (let [^BufferStrategy buffer (.getBufferStrategy canvas)
        ^Graphics g            (.getDrawGraphics buffer)
        step 4]
    (try      
      (doto g
        (.setColor Color/BLACK)
        (.fillRect 0 0 WIDTH HEIGHT))

      (loop [x 0]
        (loop [y 0]          
          (let [[^double total red green blue] 
                (reduce  (fn [[sum red-cur green-cur blue-cur] ball] 
                           (let [influence (get-influence ball x y)
                                 [r g b] (:color ball)] 
                             [(+ sum influence)
                              (+ red-cur (* influence r))
                              (+ green-cur (* influence g))
                              (+ blue-cur (* influence b))])) 
                  [0, 0, 0, 0] balls)]

            ;;center
            (if (>= total MIN-THRESHOLD)              
              (paint-square g (color-in-range-memo red green blue) x y step))
            
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
            
          (when (< y HEIGHT)              
            (recur (int (+ y step)))))
        (when (< x WIDTH)           
          (recur (int (+ x step)))))
      
      (finally (.dispose g)))
    (if-not (.contentsLost buffer)
      (.show buffer)) ))

(defn start-renderer [^Canvas canvas balls]
  (->>
    (fn [] (draw canvas @balls) (recur))
    (new Thread)
    (.start)))
 
(defn -main [& args]
  (let [frame  (JFrame. "Metaball")
        canvas (Canvas.)
        balls (atom (map (fn [_]
                           {:x      (rand-int WIDTH)
                            :y      (rand-int HEIGHT)
                            :vx     (double (inc (rand-int 6)))
                            :vy     (double (inc (rand-int 6)))
                            :radius (+ 10 (rand-int 19))
                            :color  [(rand-int 256) (rand-int 256) (rand-int 256)]})  
                         (range 6)))]
     
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
     
    (start-renderer canvas balls)
    
    (loop []      
      (swap! balls 
        #(map move %))
      (Thread/sleep 50)
      (recur))))
 
(-main)

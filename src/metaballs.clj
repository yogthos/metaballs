(ns metaballs
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Graphics Color)))

(def *WIDTH* 400)
(def *HEIGHT* 400)
(def *MIN-THRESHOLD* (double 1))
(def *MAX-THRESHOLD* (double 1.1))

(defstruct metaball :x :y :vx :vy :radius :color)

(defn ^:static move [{x :x, y :y, vx :vx, vy :vy, radius :radius, color :color}]
  (let [new-vx (if (or (> x *WIDTH*) (neg? x))
                 (- vx) vx)
        new-vy (if (or (> y *HEIGHT*) (neg? y))
                 (- vy) vy)]
    (struct metaball (+ x new-vx), (+ y new-vy), new-vx, new-vy, radius, color)))

(defn ^:static color-in-range [& args] 
  (let [[r g b] (map #(cond 
                        (< % 0) 0
                        (> % 255) 255
                        :default (int %)) args)] 
    (new Color (int r) (int g) (int b))))

(defn ^:static falloff-color [c total]
  (let [distance (cond 
                   (> total *MAX-THRESHOLD*) (- *MAX-THRESHOLD* total) 
                   :default (+ *MIN-THRESHOLD* total))]
    (* 2 (/ c  distance))))

(defn ^:static get-influence [{x :x, y :y, radius :radius} px py]
  (let [dx (double (- x px))
        dy (double (- y py))
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (if (> dist 0) (double (/ radius dist)) 0)))

(defn ^:static paint-square [#^Graphics g #^Color color x y size]
  (.setColor g color)
  (.fillRect g x y size size))

(defn ^:static draw [#^Canvas canvas balls]
  (let [buffer  (.getBufferStrategy canvas)
        g       (.getDrawGraphics buffer)
        step 4]
    (try      
      (doto g
        (.setColor Color/BLACK)
        (.fillRect 0 0 *WIDTH* *HEIGHT*))

      (loop [x 0]
        (loop [y 0]          
          (let [[total red green blue] 
                (reduce  (fn [[sum red-cur green-cur blue-cur] ball] 
                           (let [influence (get-influence ball x y)
                                 [bred bgreen bblue] (:color ball)] 
                             [(+ sum influence)
                              (+ red-cur (* influence bred))
                              (+ green-cur (* influence bgreen))
                              (+ blue-cur (* influence bblue))])) 
                  [0, 0, 0, 0] balls)]

            ;;center
            (when (>= total *MIN-THRESHOLD*)              
              (paint-square g (color-in-range red green blue) x y step))
            
            ;;outline
            (when (and (>= total *MIN-THRESHOLD*) (<= total *MAX-THRESHOLD*))                
              (paint-square g (color-in-range red green blue) x y step))
            
            ;;falloff
            (when (<= total *MAX-THRESHOLD*)
              (paint-square g 
                (color-in-range 
                  (falloff-color red total) 
                  (falloff-color green total) 
                  (falloff-color blue total))
                x y step)))
            
          (when (< y *HEIGHT*)              
            (recur (+ y step))))
        (when (< x *WIDTH*)           
          (recur (+ x step))))
      
      (finally (.dispose g)))
    (if-not (.contentsLost buffer)
      (.show buffer)) ))

(defn start-renderer [canvas balls]
  (->>
    (fn [] (draw canvas @balls) (recur))
    (new Thread)
    (.start)))
 
(defn -main [& args]
  (let [frame  (JFrame. "Metaball")
        canvas (Canvas.)
        balls (atom (map (fn [_]
                           (struct metaball
                             (rand-int *WIDTH*)
                             (rand-int *HEIGHT*)
                             (double (inc (rand-int 6)))
                             (double (inc (rand-int 6)))
                             (+ 10 (rand-int 19))
                             [(rand-int 256) (rand-int 256) (rand-int 256)]))  
                      (range 6)))]
     
    (doto frame
      (.setSize *WIDTH* *HEIGHT*)      
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

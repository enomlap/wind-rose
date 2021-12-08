;=============================================================================
;zuo,Mar 27, 2014
;generate-png.clj
;=============================================================================
(import '(java.awt.image BufferedImage)
        '(java.awt Color BasicStroke Font)
        '(javax.imageio ImageIO IIOImage)
        '(javax.imageio.stream FileImageOutputStream)
        '(java.io ByteArrayOutputStream File)
        '(java.awt.geom Line2D))

(defmacro defs [vars definations]
  (doseq [[v d] (map list vars definations)]
         (println (format "def variable : %-15s AS: %-20s" v d));#(intern 'user v d))); python style assignation
         (intern *ns* v d)))
(defs [image-file-name Title nSpokes pngWidth pngHeight] [ "./test1.png" "Wind Rose Example(%)" 16 720 960])

(def rose-center-x (/ pngWidth 2))
(def rose-center-y (- pngHeight (/ pngWidth 2)))
(def rose-r (/ (* 0.8 pngWidth) 2))
(def notes {4 ["N" "E" "S" "W"], 8 ["N" "NE" "E" "SE" "S" "SW" "W" "NW"], 16 ["N" "NEN" "NE" "ENE" "E" "ESE" "SE" "SSE" "S" "SSW" "SW" "WSW" "W" "WNW" "NW" "NNW"]})

(def image-writer (.next (ImageIO/getImageWritersBySuffix "png")))
(def BI (BufferedImage. pngWidth pngHeight BufferedImage/TYPE_INT_ARGB)) ; Call constructure function of 'BufferedImage'
;(def BI (new BufferedImage (+ 1 pngWidth) (+ 1 pngHeight) BufferedImage/TYPE_INT_ARGB)) ; the same as 'BufferedImage.'
(def WHITE Color/WHITE); constants
(def g2d (.createGraphics BI)) ; g2d: Graphics2D object

; initialize:
(.setBackground g2d Color/WHITE)
(.setStroke g2d (BasicStroke. 1))
(.setColor g2d Color/BLACK)
;data for plotting:
;(def data [30 40 50 80 10 23 43 44 30 40 50 80 10 23 43 44])

;== read in data
(def fileall (slurp "ed.dat"))
(def lines (clojure.string/split fileall #"\r\n"))
(def Title (first lines))
;(print Title)
(def data (map #(Integer. %) (next lines)))
(def nSpokes (count (next lines)))
(def angel (map #(- 90 %) (range 0 360 (/ 360 nSpokes))))
(def ticks (get notes nSpokes))

;support functions:
(defn call-with-setting
      "s is environment setting for calling f, should b Color, or &(color linestroke fontsize fontname)"
      [s f & args]
      (let [settings (flatten (cons s []))
        fs (take (count a) [#(.setColor g2d %) #(.setStroke g2d (BasicStroke. %)) #(.setFont g2d (Font. "Arial" Font/BOLD,%))]) ]
        (doseq [[tf a]  (map list fs settings)] 
               ;                       (println "---------------------" tf a)
               (tf a)))
      (apply f args)
      (.setColor g2d Color/BLACK) (.setStroke g2d (BasicStroke. 1)) (.setFont g2d (Font. "Arial" Font/BOLD,16)))
(defn rose-rtheta-to-xy [r theta] [(+ rose-center-x (* r (Math/cos (/ (* theta 3.14) 180)))) (- rose-center-y (* r (Math/sin (/ (* theta 3.14) 180))))])
(defn draw-ab 
      [r0 theta0 r1 theta1]
      (apply #(.drawLine g2d %1 %2 %3 %4) (concat (rose-rtheta-to-xy r0 theta0) (rose-rtheta-to-xy r1 theta1))))
(defn draw-center-str
      [s cx cy]
      (let [str-len (.. g2d getFontMetrics (getStringBounds s g2d) getWidth) str-height (.. g2d getFontMetrics (getStringBounds s g2d) getHeight)]
        (.drawString g2d s (int (- cx (/ str-len 2))) (int (+ cy (/ str-height 2))))))

(defn draw-percent [d a]
      "for array d:direction and array a:angels, draw percent line"
      (let [ m (apply max data) ; max of data
        mm (-> m (/ 10) int (+ 1) (* 10)) ;a number greater a little than m
        s (-> mm (/ 20) int (+ 1) (* 5)) ;tick step ;determine max tick, and step first!  ;rule: count of ticks should [4,6], and max-tick should be a read-convience number, e.g,100,75,50,80,etc.
        mmm (* s 4)
        scalefn #(* (/ rose-r mmm) %)
        scaled-d (map scalefn d)]
        (doseq [r (range s (+ mmm s) s)]
               (let [rr (scalefn r)] (.drawOval g2d (- rose-center-x rr) (- rose-center-y rr) (* 2 rr) (* 2 rr)))
               (draw-ab (scalefn r) 45 (scalefn r) 44)
               (let [xy (rose-rtheta-to-xy (scalefn r) 43)] (call-with-setting [Color/BLACK 5 16] draw-center-str (str r) (first xy) (+ (second xy) 10))))
        (doseq [[d0 a0 d1 a1] (map list scaled-d a (take nSpokes (drop 1 (cycle scaled-d))) (take nSpokes (drop 1 (cycle a))))]
               ;(println "AB:" d0 a0 d1 a1)
               (call-with-setting [Color/BLUE 5] draw-ab d0 a0 d1 a1)
               )))

;==================== draw basic elements:
;draw a box for entire plot:
(let [x (- pngWidth 1) y (- pngHeight 1)]
  (.drawLine g2d 0 0 x 0) (.drawLine g2d 0 0 0 y) (.drawLine g2d x 0 x y) (.drawLine g2d 0 y x y))
;draw base cycle and spokes
(.drawOval g2d (- rose-center-x rose-r) (- rose-center-y rose-r) (* 2 rose-r) (* 2 rose-r))
(let [rs (repeat nSpokes rose-r),thetas angel,ts (get notes nSpokes)]
  (doseq [[r theta t] (map list rs thetas ts)]
         ;(draw-center-str t (->> (+ r 15) (* (-> theta (* 3.14) (/ 180) Math/cos)) (+ rose-center-x)) (->> (+ r 15) (* (-> theta (* 3.14) (/ 180) Math/sin)) (+ rose-center-y)))
         (let [[x y] (rose-rtheta-to-xy (+ r 15) theta)] 
           (call-with-setting [Color/BLACK 1 16] draw-center-str t x y))
         (.drawLine g2d (->> r (* (-> theta (* 3.14) (/ 180) Math/cos)) (+ rose-center-x)) (->> r (* (-> theta (* 3.14) (/ 180) Math/sin)) (+ rose-center-y)) rose-center-x rose-center-y)))

;draw legend:
;(.fillRect g2d 100 100 50 50)

;draw Title and Direction-note:
(call-with-setting [Color/BLUE 5 18] draw-center-str Title rose-center-x (- rose-center-y rose-r 40)) ;distance above plot
;;==================== draw basic elements end.
;(call-with-setting Color/BLUE draw-ab 200 30 100 60)
;(call-with-setting [Color/BLUE 5] draw-ab 200 30 100 60)

(draw-percent data angel)

; write out to png file:
(def output (FileImageOutputStream. (File. image-file-name)))
(.setOutput image-writer output)
(defn write-image [writer image]
      (let [iio-image (IIOImage. image nil nil)]
        (.write writer nil iio-image nil)))
(write-image image-writer BI)
(.close output) ;(.flush output)
(.dispose image-writer)

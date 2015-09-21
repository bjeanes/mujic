(ns mujic
  (:require [clojure.set :as set]
            [clojure.string :as str]
            #?(:cljs [reagent.core
                      :as r
                      :refer [render]]))
  #?@(:cljs [(:require-macros [mujic :refer [defintervals]])
             (:refer-clojure :exclude [atom])]))

#?(:cljs
   (do (enable-console-print!)
       (def ratom r/atom))

   :clj
   (do
     (defmacro render [& args])
     (def ratom atom)))

(declare note-series)

;; Some global state to keep all the interactive components using the same key
;; or tonic
(def tonic (ratom :C))
(def key tonic)

(defn- get-selected-key [event]
  (-> event
      .-target
      .-selectedOptions
      (aget 0)
      .-value
      keyword))

(defn- key-selector-on-change [event]
  (reset! key (get-selected-key event)))

(defn key-selector-component
  "Render a re-usable widget that selects the key or tonic for the interactive example.
   Adjusts the key for the entire page (i.e. global state)."
  []
  (letfn [(option-fn [note] [:option {:value note} (name note)])]
    (let [notes (take 12 (note-series :C))
          notes (map first notes) ; TODO handle C#/Db distinctions better
          options (map option-fn notes)]
      [:div {:style {:text-align "center"}}
       (into [:select {:value @key
                       :style {:padding "auto"}
                       :on-change key-selector-on-change}]
             options)])))

(def ordered-notes
  ;; I'm using actual flat (♭) and sharp (♯) symbols because this
  ;; project is for fun and I have working auto-complete.
  ;; Practicality be damned!
  [#{:C} #{:C♯ :D♭}
   #{:D} #{:D♯ :E♭}
   #{:E}
   #{:F} #{:F♯ :G♭}
   #{:G} #{:G♯ :A♭}
   #{:A} #{:A♯ :B♭}
   #{:B}])

(def h  1) ; half step
(def W  2) ; whole step
(def Wh 3) ; whole-and-half step

;; just a few common scales will do for now
(def scales {:major          [W W h W W W h]
             :minor/natural  [W h W W h W W]
             :minor/harmonic [W h W W h Wh h]})

(let [note-width 100
      lbls {1 "H" 2 "W" 3 "W+H"}
      height 40]

  (letfn [(g
            ([attrs grouped] (into [:g attrs] grouped))
            ([grouped] (g {} grouped)))

          (scale-step-svg-component
            [rel abs]

            (let [lbl (get lbls rel rel)
                  rel (* note-width rel)
                  abs (* note-width abs)
                  lbl-x (/ rel 2)
                  gap (* 0.1 note-width)]

              [:g {:transform (str "translate(" abs " 0)")}
               [:text {:text-anchor "middle"
                       :x lbl-x}
                lbl]
               [:line {:stroke "black"
                       :x1 gap :x2 (- rel gap)
                       :y1 0 :y2 0}]]))

          (scale-steps-svg-component
            [scale]
            (let [relative (scales @scale)               ; (2 2 1 2 2 2 1)
                  absolute (take-nths relative (range))] ; (0 2 4 5 ...)
              (g {:transform "translate(0 50)"}
                 (map (fn [rel abs]
                        [scale-step-svg-component
                         rel abs])
                      relative
                      absolute))))

          (scale-notes-svg-component []
            (let [notes (take 13 (note-series @tonic))]
              (g (map-indexed #(vector :text
                                       {:x (* note-width %1)}
                                       (name (first %2)))
                              notes))))

          (scales-sieve-svg-component
            [scale]
            [:svg {:width "100%"
                   :height height
                   :viewBox [0 0 (* 13 note-width) height]}
             [scale-notes-svg-component]
             [scale-steps-svg-component scale]])

          (scale-selector-component [scale]
            (let [name (name @scale) ; doesn't include namespace, so:
                  ns (namespace @scale)
                  val (if ns (str ns "/" name) name)]
              [:div {:style {:text-align "center"}}
               [:select {:value val
                         :on-change #(reset! scale (get-selected-key %)) }
                [:option {:value "major"} "Major"]
                [:option {:value "minor/natural"} "Natural Minor"]
                [:option {:value "minor/harmonic"} "Harmonic Minor"]]])) ]

    (defn scales-sieve-component []
      (let [scale (ratom :major)]
        (fn []
          [:div
           [key-selector-component]
           [scale-selector-component scale]
           [scales-sieve-svg-component scale]])))))

(defn take-nths
  "Takes a collection and returns the values of each interval
  from the previous value (or start). Always includes the first
  item.

  Named due to similarity with take-nth, with stable n:

      (take-nth          2  (range)) ;=> (0 2 4 6 8 10 ...)
      (take-nths (repeat 2) (range)) ;=> (0 2 4 6 8 10 ...)
  "
  [ns coll]
  (let [indices (reductions + 0 ns)]
    (map (partial nth coll) indices)))

(take-nths [3 1 1 4] [1 2 3 4 5 6 7 8 9 10])
;;=> (1 4 5 6 10)

(count (take 20 ordered-notes))           ;=> 12
(take-nths (scales :major) ordered-notes) ;!! java.jang.IndexOutOfBoundsException

(count (take 20 (cycle ordered-notes)))           ;=> 20
(take-nths (scales :major) (cycle ordered-notes)) ;=> (#{:C} #{:D} #{:E} #{:F} #{:G} #{:A} #{:B} #{:C})

;; convenience list of all valid note names
(def note-names (apply set/union ordered-notes))

(defn note-series
  "Returns an infinite sequence of chromatic notes starting
  with :C or the provided `start` note"
  ([] (note-series :C))
  ([root]
   (when-let [root (note-names root)]
     (drop-while (complement root)
                 (cycle ordered-notes)))))

(take-nths (scales :minor/natural) (note-series :A))
;;=> (#{:A} #{:B} #{:C} #{:D} #{:E} #{:F} #{:G} #{:A})

(cycle '(#{:A} #{:B} #{:C} #{:D} #{:E} #{:F} #{:G} #{:A}))
;;=> (#{:A} #{:B} #{:C} #{:D} #{:E} #{:F} #{:G} #{:A} #{:A} #{:B} #{:C} ...)
;;                                                 ^ 2x  ^

(defn scale
  "Return the sequence of notes for the specified scale and tonic"
  [scale tonic]
  (take-nths (scales scale) (note-series tonic)))

;; arguments are in that order so its amenable to currying:
(def major-scale (partial scale :major))
(major-scale :C) ;=> (#{:C} #{:D} #{:E} #{:F} #{:G} #{:A} #{:B} #{:C})

;; Just a few chords for now will do. We'll map a chord
;; name to a scale and the notes from the scale to use.
(def chords
  {:M  [:major         [1 3 5]]
   :M7 [:major         [1 3 5 7]]
   :m  [:minor/natural [1 3 5]]
   :m7 [:minor/natural [1 3 5 7]]})

(defn chord
  [chord root]
  (let [[scale-name idxs] (chords chord)
        scale (scale scale-name root)]
    (map (comp (partial nth scale) dec) ; dec so 0-indexed
         idxs)))

;; arguments are in that order so its amenable to currying:
(def minor-7th (partial chord :m7))
(minor-7th :C) ;=> (#{:C} #{:D♯ :E♭} #{:G} #{:A♯ :B♭})

(defn scale-degrees
  [tonic scale-name]
  (map-indexed #(list (inc %1) (last %2))
               (scale scale-name tonic)))

(scale-degrees :C :major)
;;=> ((1 :C) (2 :D) (3 :E) (4 :F) (5 :G) (6 :A) (7 :B) (8 :C))

(scale-degrees :C :minor/natural)
;;=> ((1 :C) (2 :D) (3 :E♭) (4 :F) (5 :G) (6 :A♭) (7 :B♭) (8 :C))

(scale-degrees :C :minor/harmonic)
;;=> ((1 :C) (2 :D) (3 :E♭) (4 :F) (5 :G) (6 :A♭) (7 :B) (8 :C))

(def scales {:major          [W W h W W W h]
             :minor/natural  [W h W W h W W]
             :minor/harmonic [W h W W h Wh h]
             :minor/melodic  [W h W W W W h]})

(scale-degrees :C :major)
;;=> ((1 :C) (2 :D) (3 :E) (4 :F) (5 :G) (6 :A) (7 :B) (8 :C))

(scale-degrees :C :minor/melodic)
;;=> ((1 :C) (2 :D) (3 :E♭) (4 :F) (5 :G) (6 :A) (7 :B) (8 :C))

(def chords
  {:M  [:major         [1 3 5]]
   :M7 [:major         [1 3 5 7]]
   :m  [:minor/natural [1 3 5]]
   :m7 [:minor/natural [1 3 5 7]]
   ;; ...
   :m6 [:minor/melodic [1 3 5 6]]})

(defn rotate
  "Moves n elements in s from the front to the back."
  [n s]
  (let [shift (mod n (count s))]
    (concat (drop shift s)
            (take shift s))))

(defn rotations
  "Returns a sequence generated by rotating finite
  sequence s repeatedly until the original order is
  encountered."
  [s]
  (take (count s)
        (iterate (partial rotate 1) s)))

(rotate 1 [:a :b :c :d])  ;=> (:b :c :d :a)
(rotations [:a :b :c :d]) ;=> ([:a :b :c :d] (:b :c :d :a) (:c :d :a :b) (:d :a :b :c))
(scales :major)           ;=> [2 2 1 2 2 2 1]
(rotations (scales :major))
;;=> ([2 2 1 2 2 2 1] (2 1 2 2 2 1 2) (1 2 2 2 1 2 2) (2 2 2 1 2 2 1) (2 2 1 2 2 1 2) (2 1 2 2 1 2 2) (1 2 2 1 2 2 2))

(def scales
  (let [scales {:major          [W W h W W W h]
                :minor/natural  [W h W W h W W]
                :minor/harmonic [W h W W h Wh h]
                :minor/melodic  [W h W W W W h]}
        modes (zipmap [:mode/ionian :mode/dorian :mode/phrygian
                       :mode/lydian :mode/mixolydian :mode/aeolian
                       :mode/locrian]
                      (rotations (scales :major)))]
    (merge scales modes)))

(def chords
  {:M     [:major         [1 3 5]]
   :M7    [:major         [1 3 5 7]]
   :m     [:minor/natural [1 3 5]]
   :m7    [:minor/natural [1 3 5 7]]
   ;; ...
   :m6    [:minor/melodic [1 3 5 6]]
   :M7#11 [:mode/lydian   [1 3 5 7 11]]})

(reductions + 0 [W W h W W W h]) ;=> (0 2 4 5 7 9 11 12)

(map (partial nth (note-series :C)) '(0 2 4 5 7 9 11 12))
;;=> (#{:C} #{:D} #{:E} #{:F} #{:G} #{:A} #{:B} #{:C})

;; Am chord
(map (partial nth (note-series :A)) [0 3 7]) ;=> (#{:A} #{:C} #{:E})

;; G♯M7
(map (partial nth (note-series :G♯)) [0 4 7 11]) ;=> (#{:G♯ :A♭} #{:C} #{:D♯ :E♭} #{:G})

#?(:clj (defmacro defintervals
          [& names]
          `(do
             (def ~'interval-names '[~@names])
             ~@(map-indexed #(when-not (= '_ %2)
                               (list 'def %2 %1))
                            names))))

(defintervals
  ;; simple
     P1
   m2   M2
   m3   M3
     P4
     TT     ; tritone
     P5
   m6   M6
   m7   M7
     P8

  ;; compound
   m9   M9
  m10   M10
     P11
      _     ; nameless?
     P12
  m13   M13
  m14   M14
     P15)

M2 ;=> 2

;; (defintervals a b _ c)
(comment
 (do
  (def interval-names '[a b _ c])
  (def a 0)
  (def b 1)
  nil
  (def c 3)))

(defn intervals-in-major-scale-component []
  (let [major (partial scale :major)
        fmt (fn
              ([t]   (str/join "/" (map name t)))
              ([t n] (str/join "/" (map #(str (name %) n)
                                        t))))]
    (fn []
      (let [scale (major @tonic)
            tri-tone (nth (note-series @tonic) 6)
            [front back] (split-at 4 scale)]
        [:div
         [key-selector-component]
         [:table
          [:thead
           [:tr
            [:th "Interval"]
            [:th (fmt (nth scale 0) 0)]
            [:th (fmt (nth scale 1))]
            [:th (fmt (nth scale 2))]
            [:th (fmt (nth scale 3))]
            [:th [:i (str "(" (fmt tri-tone) ")")]]
            [:th (fmt (nth scale 4))]
            [:th (fmt (nth scale 5))]
            [:th (fmt (nth scale 6))]
            [:th (fmt (nth scale 7) 1)]
            ]
           #_(vec (concat [:tr [:th "Interval"]]
                          (map fmt front)
                          [(fmt tri-tone)]
                          (map fmt back)))]]]))))

(defn invert* [t] (- P8 t))
(def invert (comp interval-names invert*))

(invert* m3) ;=> 9
(invert m3)  ;=> M6
(invert P4)  ;=> P5
(invert TT)  ;=> TT

(defn inversion?
  [t1 t2]
  (= t1 (invert* t2)))

(inversion? P4 P5) ;=> true
(inversion? m2 M7) ;=> true
(inversion? M7 m2) ;=> true
(inversion? m2 M2) ;=> false

(defn invert [t] (interval-names (Math/abs (- P8 t))))

(def R P1) ; reads nicer in this context

(def chords
  {:M       [R M3 P5]    ; ... previously [:major [1 3 5]]
   :m       [R m3 P5]    ; ... previously [:minor [1 3 5]]
   :m6      [R m3 P5 M6] ; ... etc

   ;;; Some 7th chords for fun
   :7       [R M3 P5 m7] ; major/minor (major triad + minor 7th)    "dominant 7th"
   :M7      [R M3 P5 M7] ; major/major (major triad + major 7th)    "major 7th"
   :m7      [R m3 P5 m7] ; minor/minor (minor triad + minor 7th)    "minor 7th"
   :m/M7    [R m3 P5 M7] ; minor/major (minor triad + major 7th)
   })

(defn chord
  [name root]
  (map (partial nth (note-series root))
       (chords name)))

(chord :m7 :C) ;=> (#{:C} #{:D♯ :E♭} #{:G} #{:A♯ :B♭})

(defn map-values
  [f m]
  (into {}
        (map (fn [[k v]] [k (f v)])
             m)))

(def interval-scales (map-values (partial reductions + 0) scales))

interval-scales ;=> {:mode/aeolian (0 2 3 5 7 8 10 12), ...}

(def named-interval-scales (map-values (partial map interval-names) interval-scales))

named-interval-scales ;=> {:mode/aeolian (P1 M2 m3 P4 P5 m6 m7 P8), ...}

(defn chord-in-scale?
  [chord scale]
  (every? (set (interval-scales scale))
          (chords chord)))

(chord-in-scale? :M :major) ;=> true
(chord-in-scale? :m :minor) ;=> false

(defn scales-for-chord
  [chord]
  (into {}
        (for [[s ints] interval-scales
              :when (chord-in-scale? chord s)]
          [s (map (comp inc #(.indexOf ints %))
                  (chords chord))])))

(scales-for-chord :M)    ;=> {:mode/ionian (1 3 5), :major (1 3 5) ...}
(scales-for-chord :m)    ;=> {:mode/aeolian (1 3 5), :minor/natural (1 3 5), ...}
(scales-for-chord :7)    ;=> {:mode/mixolydian (1 3 5 7)}
(scales-for-chord :m/M7) ;=> {:minor/melodic (1 3 5 7), :minor/harmonic (1 3 5 7)}

(def scale-chords
  (map-values scales-for-chord
              (map (fn [[k _]] [k k])
                   chords)))

scale-chords
;;=> {:M {:mode/ionian (1 3 5), ...}, :m/M7 {:minor/melodic (1 3 5 7), ...}, ...}

;; a 24-fret guitar fretboard of notes
(def guitar
  (map (comp (partial take 24) note-series)
       [:E :A :D :G :B :E]))

#?(:cljs
   ;; For every function defined in current namespace which ends in
   ;; `-component`, wire it up as a Reagent component to a DOM element of the
   ;; same ID.
   ;;
   ;; Must be at end of file because ns-interns is a macro
   (.addEventListener
    js/document
    "DOMContentLoaded"
    (fn []
      (let [intern-kv (ns-interns 'mujic)
            component-keys (filter (comp (partial re-find #"-component$")
                                      name)
                                   (keys intern-kv))]
        (doseq [k component-keys
                :let [f @(k intern-kv)
                      e (.getElementById js/document
                                         (name k))]]
          (when e
            (render [f] e)))))))

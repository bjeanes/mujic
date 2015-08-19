(ns mujic.core
  (:require [clojure.set :as set]
            [moosic.util :refer :all]))

;; Symbols: ♯ ♭ ° ♮ +

(def h  1) ; half step
(def W  2) ; whole step
(def Wh 3) ; whole-and-half step

;; Uses "reader ignores" to name scale degrees and to conveniently
;; stop auto-formatter from left-aligning everything...
(def intervals
  {;; DEGREE      MINOR    PERFECT  MAJOR        DIMINISHED AUGMENTED
   #_ first                :P1  0                           :A1  1
   #_ second      :m2  1            :M2  2       :d2  0     :A2  3
   #_ third       :m3  3            :M3  4       :d3  2     :A3  5
   #_ fourth               :P4  5                :d4  4     :A4  6
   #_ fifth                :P5  7                :d5  6     :A5  8
   #_ sixth       :m6  8            :M6  9       :d6  7     :A6  10
   #_ seventh     :m7  10           :M7  11      :d7  9     :A7  12
   #_ eighth               :P8  12               :d8  11    :A8  13
   #_ ninth       :m9  13           :M9  14      :d9  12    :A9  15
   #_ tenth       :m10 15           :M10 16      :d10 14    :A10 17
   #_ eleventh             :P11 17               :d11 16    :A11 18
   #_ twelfth              :P12 19               :d12 18    :A12 20
   #_ thirteenth  :m13 20           :M13 21      :d13 19    :A13 22
   #_ fourteenth  :m14 22           :M14 23      :d14 21    :A14 24
   #_ fifteenth            :P15 24               :d15 23    :A15 25

   ;; OTHER COMMON NAMES
   :R  0
   :TT 6})

(def interval (comp intervals keyword))

(def ordered-notes
  [#{:C} #{:C♯ :D♭}
   #{:D} #{:D♯ :E♭}
   #{:E}
   #{:F} #{:F♯ :G♭}
   #{:G} #{:G♯ :A♭}
   #{:A} #{:A♯ :B♭}
   #{:B}])

;; All valid note names
(def note-names (apply set/union ordered-notes))

(defn note-series
  "Returns an infinite sequence of chromatic notes starting
  with :C or the provided `start` note"
  ([] (note-series :C))
  ([root]
   (when-let [root (note-names root)]
     (drop-while (complement root)
                 (cycle ordered-notes)))))

(def scales
  (let [scales '{:major          [W W h W W W h]
                 :minor/natural  [W h W W h W W]
                 :minor/harmonic [W h W W h Wh h]
                 :minor/melodic  [W h W W W W h]}
        modes (zipmap [:mode/ionian :mode/dorian :mode/phrygian :mode/lydian
                       :mode/mixolydian :mode/aeolian :mode/locrian]
                      (rotations (scales :major)))]
    (merge scales modes)))

;; https://en.wikipedia.org/wiki/Chord_names_and_symbols_(popular_music)#Rules_to_decode_chord_names_and_symbols
;; (defn parse-chord [s]
;;   (let [accidentals {"#" :♯
;;                      "b" :♭
;;                      "♯" :♯
;;                      "♭" :♭
;;                      "♮" nil}
;;         chords {"M"   :Maj
;;                 "maj" :Maj}]))

;; Maps chord descriptors to a series of intervals to apply to the notes
;;
;; http://www.smithfowler.org/music/Chord_Formulas.htm (defined everything relative to major)
;;
;; TODO: expand notation
;;    major = Maj M
;;    minor = min m
;;    major 7 = Δ
;;    minor 7 = -7
;;    diminished = ° dim

;; Chord names represented as component intervals from root
;;
;; TODO: this seems thoroughly interesting https://gist.github.com/lfnoise/bcf1f3e8713c1dd1d6e8
(def chords
  '{;;; M/m Triads
    :M       [R M3 P5]       ; ...previously [:major [1 3 5]]
    :m       [R m3 P5]       ; ...previously [:minor [1 3 5]]
    :dim     [R m3 d5]

    ;; Misc
    :5       [R    P5]       ; "power chord" (optionally, would have P8)

    ;; Added chords
    :add4    [R M3 P4 P5]
    :madd4   [R m3 P4 P5]
    :6       [R M3    P5 M6]
    :m6      [R m3    P5 M6]    ; minor/major 6th (though often notated as m6)

    ;; Suspended https://en.wikipedia.org/wiki/Suspended_chord
    :sus2    [R M2 P5]
    :sus4    [R P4 P5]

   ;;; 7ths https://en.wikipedia.org/wiki/Seventh_chord
    ;; 7th chords are a major or minor triad with either a minor or major added 7th.
    ;; They are described by the qualities of both these intervals (e.g. major/minor 7),
    ;; but if both qualities are the same (e.g. major/major 7) the redundant quality is
    ;; omitted (e.g. major 7).
    :7       [R M3 P5 m7]    ; major/minor (major triad + minor 7th)    "dominant 7th"
    :M7      [R M3 P5 M7]    ; major/major (major triad + major 7th)    "major 7th"
    :m7      [R m3 P5 m7]    ; minor/minor (minor triad + minor 7th)    "minor 7th"
    :m/M7    [R m3 P5 M7]    ; minor/major (minor triad + major 7th)

                                        ; TODO: understand these names/notations
    :m/M7♭5  [R m3 d5 M7]    ; diminished major 7th
    :m7♭5    [R m3 d5 m7]    ; half diminished 7th (ø)
    :dim7    [R m3 d5 d7]    ; diminished 7th
    :+M7     [R M3 A5 M7]    ; augmented major 7th
    :+7      [R M3 A5 m7]    ; augmented(/minor) 7th
    :7♭5     [R M3 d5 m7]    ; dominant seventh flat 5

   ;;; 9ths https://en.wikipedia.org/wiki/Ninth_chord
    :9       [R M3 P5 m7 M9] ; dominant 9th (dominant 7th + major 9th)
    :M9      [R M3 P5 M7 M9]
    :m9      [R m3 P5 m7 M9]
    :m/M9    [R m3 P5 M7 M9] ; minor/major 9th (minor/major 7th + major 9th)
    :add9    [R M3 P5    M9] ; no 7th
    :69      [R M3 P5 M6 M9] ; 6/9
    :7♭9     [R M3 P5 m7 m9] ; dominant minor ninth

   ;;; 11ths https://en.wikipedia.org/wiki/Eleventh_chord
    :11 [] ; dominant 11th
    :M11 []
    :m11 []
    :♯11 []
    :M7♯11 []

   ;;; TO CONVERT / old notation:
    ;; :M11   {:major          '[1 3 5 (7) (9) 11]}
    ;; :M13   {:major          '[1 3 5 (7) (9) (11) 13]}
    ;; :#11   {:mode/lydian    [1 5 11]}
    ;; :M7#11 {:mode/lydian    [1 3 5 7 11]}

    ;; :m     {:minor/natural  [1 3 5]}
    ;; :madd4 {:minor/natural  [1 3 4 5]}
    ;; :m6    {:minor/melodic  [1 3 5 6]}
    ;; :m7    {:minor/natural  [1 3 5 7]}
    ;; :madd9 {:minor/natural  [1 3 5 9]}
    ;; :m69   {:minor/melodic  [1 3 5 6 9]}
    ;; :m9    {:minor/natural  [1 3 5 7 9]}
    ;; :m11   {:minor/natural  '[1 3 5 7 (9) 11]}
    ;; :m13   {:minor/melodic  '[1 3 5 7 (9) (11) 13]}
    ;; :m/M7  {:minor/harmonic [1 3 5 7]}
    ;; :m/M9  {:minor/harmonic [1 3 5 7 9]}
    ;; :m/M11 {:minor/harmonic '[1 3 5 7 (9) 11]}
    ;; :m/M13 {:minor/melodic  '[1 3 5 7 (9) (11) 13]}

    ;; ;; not sure how to do this with known minor scales
    ;; :m7-5 {} ; half-diminished 7th
    ;; :m7♭5 {}
    ;; :ø {}

    ;; :° {} ; 1 b3 b5
    ;; :°7 {} ; 1 b3 b5 bb7
    ;; :+ {} ; 1 3 #5

    ;; :♭5 {} ; 1 b5
    })

(defn scale
  "Return the sequence of notes for the specified scale and tonic"
  [scale tonic]
  (take-nths (eval (scales scale))
             (note-series tonic)))

;; (defn scale-degrees
;;   [tonic scale-name]
;;   (map-indexed #(list (inc %1) (last %2))
;;                (scale scale-name tonic)))

;; (defn all-scales [tonic]
;;   (into {} (map (juxt identity
;;                       (partial scale tonic))
;;                 (keys scales))))

;; (def interval-scales (map-values (comp vec (partial reductions + 0)) scales))
;; (def named-interval-scales (map-values (comp vec (partial map interval-names)) interval-scales))

(defn chord
  [name root]
  (map (comp (partial nth (note-series root))
             interval)
       (chords name)))

;; (defn chord-in-scale?
;;   [chord scale]
;;   (every? (set (interval-scales scale))
;;           (chords chord)))

;; (defn scales-for-chord
;;   [chord]
;;   (into {}
;;         (for [[s ints] interval-scales
;;               :when (chord-in-scale? chord s)]
;;           [s (map (comp inc #(.indexOf ints %))
;;                   (chords chord))])))

;; (def scale-chords (map-values scales-for-chord (map (fn [[k _]] [k k]) chords)))

;; (defn fretboard
;;   [strings]
;;   (map (comp (partial take 24) note-series) strings))

;; ;; https://en.wikipedia.org/wiki/List_of_guitar_tunings
;; ;; TODO: define relationships of "raised" or "lowered" by a pos/neg number of semitones from another standard
;; (def guitar   (map-values fretboard {:standard [:E :A :D :G :B :E]
;;                                      :drop-d   [:D :A :D :G :B :E]
;;                                      :open-a   [:E :A :C♯ :E :A :E]
;;                                      :open-b []
;;                                      :open-c []
;;                                      :open-d []
;;                                      :open-e []
;;                                      :open-f []
;;                                      :open-g []}))
;; (def ukelele  (map-values fretboard {:standard [:G :C :E :A]}))
;; (def mandolin (map-values fretboard {:standard [:G :D :E :A]}))

;; (defn invert [t] (interval-names (Math/abs (- P8 t))))

;; (defn inversion?
;;   [t1 t2]
;;   (= t1 (var-get (resolve (invert t2)))))

(defn mode? [[n _]] (= (namespace n) "mode"))

;; ;; chords for mode
;; (let [modes (into {} (filter identity interval-scales))
;;       chord-tones [1 3 5 7]]
;;   (map-values (comp first
;;                     (fn [intervals]
;;                       (let [chord (map (comp intervals dec)
;;                                        chord-tones)]
;;                         (keep (fn [[n ints]] (when (= chord ints) n))
;;                               chords))))
;;               modes))



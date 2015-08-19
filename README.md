# Mujic: Learning Music Theory with Clojure

## Preamble

I've played guitar since I was about 7 years old. Like many casual musicians, my interest and focus has waxed and waned over those years and, consequently, my skill level plateaued sometime before I was 18. Every time that I've been inspired to play a lot more, it lasts only as long as it takes for me to hit that same plateau threshold; that is, my frustration understanding how music really worked diminishes<sup>([rimshot](https://en.wikipedia.org/wiki/Diminished_triad))</sup> my interest.

The feeling of really grokking music theory has always eluded me. Every time that I have tried to learn some music theory, it just felt like a house of cards built on memorising labels and rules, each with an abundance of special cases and exceptions. Every bit of theory I managed to learn in the past would slip away just as quickly because the foundation wasn't sturdy.

I have been feeling pretty burned out at work the last few weeks, despite loving what I'm working on, so I decided to take a week off to focus on more playful and experimental projects. I've also been itching to get my fingers dirty with Clojure again.

So, I decided to try to learn music theory by using Clojure to explore the relationships between different concepts (notes, intervals, scales, modes, chords). Approaching music theory as if it were any other unknown domain that I had to model in code should help me really understand and formalise how things work and hopefully let it stick in my head.

This post serves as my logbook and worksheet for the experiment. As such, it preserves any understandings which are incorrect or incomplete and subsequent re-understandings of concepts are appended. I will retroactively edit earlier sections to make them clearer or easier to refer to later, though.

## Logbook

It seems obvious that I need some concept of the usable (Western music) notes and their order, so I'll start there:

```clojure
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
```

I've modelled the notes as a vector of sets. Each set spells out the valid identifiers for that note. At this point, I'm not sure that this model of the keys will be ideal—in particular, I may need to treat flats (♭) and sharps (♯) distinctly based on context—but it suffices for the time being.

My exploration with music theory in the past left me with the impression that scales acted as a kind of sieve to highlight specific notes from all notes and that chords acted as sieves over specific scales to further highlight specific notes. 

With that vague semblance, I pursue modelling this understanding. Scales are usually defined in terms of number of semitone steps:

```clojure
(def h  1) ; half step
(def W  2) ; whole step
(def Wh 3) ; whole-and-half step

;; just a few common scales will do for now
(def scales {:major          [W W h W W W h]
             :minor/natural  [W h W W h W W]
             :minor/harmonic [W h W W h Wh h]})
```

Now, I need a way to pick items from a list based on these steps, which I think of as relative indices. I have a sense that this might be a generically applicable operation so I'll write it generically (this type of operation may very well already exist, but I can explore that later):

```clojure
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
```

Here's how that behaves:

```clojure
(take-nths [3 1 1 4] [1 2 3 4 5 6 7 8 9 10])
;=> (1 4 5 6 10)
```

Because the scale definition includes the interval to reach the octave, I'll use `cycle` to give it an infinite number of notes for the scale to select:

```clojure
(count (take 20 ordered-notes))           ;=> 12
(take-nths (scales :major) ordered-notes) ;!! java.jang.IndexOutOfBoundsException

(count (take 20 (cycle ordered-notes)))           ;=> 20
(take-nths (scales :major) (cycle ordered-notes)) ;=> (#{:C} #{:D} #{:E} #{:F} #{:G} #{:A} #{:B} #{:C})
```

Since this needs a cycling notion of the available notes and I'll surely need it later for chords with super-octave notes (9th, 11th, 13th, etc), I'll create a function to return an infinite list of notes, starting with a specific note (**C** if not specified):

```clojure
;; convenience list of all valid note names
(require '[clojure.set :as set])
(def note-names (apply set/union ordered-notes))

(defn note-series
  "Returns an infinite sequence of chromatic notes starting
  with :C or the provided `start` note"
  ([] (note-series :C))
  ([root]
   (when-let [root (note-names root)]
     (drop-while (complement root)
                 (cycle ordered-notes)))))
```

(side-note: super valuable to put `{:user :global-vars {*print-length* 103 *print-level* 15}}` in your `~/.lein/profiles` file so that you don't accidentally print an infinite list in the REPL and have it hang!)

Likely, the "infinite sequence of notes" model is a stopgap until I can tie in the octave of the note into the representation (e.g. with [scientific pitch notation](https://en.wikipedia.org/wiki/Scientific_pitch_notation)).

OK, so now I can come back to building scales by sieving all possible notes from a root:

```clojure
(take-nths (scales :minor/natural) (note-series :A))
;=> (#{:A} #{:B} #{:C} #{:D} #{:E} #{:F} #{:G} #{:A})
```

I can anticipate that repeating scales (using `cycle`, as with the note series) which means that the root note, which is in both first and 8th position, will appear twice in a row:

```clojure
(cycle '(#{:A} #{:B} #{:C} #{:D} #{:E} #{:F} #{:G} #{:A}))
;=> (#{:A} #{:B} #{:C} #{:D} #{:E} #{:F} #{:G} #{:A} #{:A} #{:B} #{:C} ...)
                                               ; ^ 2x  ^
```

I'll tackle that later when it comes up, though. 

Here is my new `scale` function:

```clojure
(defn scale
  "Return the sequence of notes for the specified scale and tonic"
  [scale tonic]
  (take-nths (scales scale) (note-series tonic)))
  
; arguments are in that order so its amenable to currying:
(def major-scale (partial scale :major))
(major-scale :C) ;=> (#{:C} #{:D} #{:E} #{:F} #{:G} #{:A} #{:B} #{:C})
```

As I mentioned above, I've always thought of chords as a "sieve" over their scale. In some contexts, this is obvious. The major triad is [defined as **1 3 5**](http://www.smithfowler.org/music/Chord_Formulas.htm) of the major scale, for example.

Interestingly, a minor triad is typically in terms of the major scale too. It is [represented as **1 ♭3 5**](http://www.smithfowler.org/music/Chord_Formulas.htm). This is one of those things that seemed like an exception and special case in my past—just more memorisation. However, playing around in code let me discover that it's also **1 3 5** of a minor scale. This property allows me to use these intervals as 1-based indexes of the scale and feels like a beautiful musical property—one I'll likely remember:

```clojure
;; Just a few chords for now will do. We'll map a chord 
;; name to a scale and the notes from the scale to use.
(def chords
  {:M  [:major         [1 3 5]]
   :M7 [:major         [1 3 5 7]]
   :m  [:minor/natural [1 3 5]]
   :m7 [:minor/natural [1 3 5 7]]})
```

This exploration helped throw some light on the relationship between scales and chords, which is exactly the point.

To get the notes in a chord, I'll define a new function which takes a chord identifier and a root note:

```clojure
(defn chord
  [chord root]
  (let [[scale-name idxs] (chords chord)
        scale (scale scale-name root)]
    (map (comp (partial nth scale) dec) ; dec so 0-indexed
         idxs)))
         
; arguments are in that order so its amenable to currying:
(def minor-7th (partial chord :m7))
(minor-7th :C) ;=> (#{:C} #{:D♯ :E♭} #{:G} #{:A♯ :B♭})
```

I defined a few dozen chords like this in terms of the scales whose structures I had defined earlier. Eventually, the process of converting typical notation (**1 ♭3 5**) into my notation (**1 3 5** in natural minor) to be quite difficult for some chords. I kept having to mentally convert the typical notation into notes for a **C** chord then pattern match a **C** scale that had all those notes. This is mentally taxing, because I haven't defined my scales in those terms yet. 

I'll explore two such chords which drove the need to introduce a new scale and to explore [modes](https://en.wikibooks.org/wiki/Music_Theory/Modes). Later, I'll go back and re-model chords to avoid this mental taxation.

The **m6** (minor sixth) chord is typically defined in terms of the major scale as **1 ♭3 5 6**. If I compare the major scale intervals with the two minors that I have defined, I can see that both of the defined minors flatten the **6th**. The **6** in terms of either minor scale would be incorrect, and I'd have a different chord:

```clojure
(defn scale-degrees
  [tonic scale-name]
  (map-indexed #(list (inc %1) (last %2))
               (scale scale-name tonic)))

(scale-degrees :C :major)
;=> ((1 :C) (2 :D) (3 :E) (4 :F) (5 :G) (6 :A) (7 :B) (8 :C))

(scale-degrees :C :minor/natural)
;=> ((1 :C) (2 :D) (3 :E♭) (4 :F) (5 :G) (6 :A♭) (7 :B♭) (8 :C))

(scale-degrees :C :minor/harmonic)
;=> ((1 :C) (2 :D) (3 :E♭) (4 :F) (5 :G) (6 :A♭) (7 :B) (8 :C))
```

**1 ♭3 5 6** from the major scale would be **C E♭ G A** but both minor scales have an **A♭** instead. Luckily, there is a minor scale, the [melodic minor](https://en.wikipedia.org/wiki/Minor_scale#Melodic_minor_scale), which has an **A**, so I'll add that to my defined scales:

```clojure
(def scales {:major          [W W h W W W h]
             :minor/natural  [W h W W h W W]
             :minor/harmonic [W h W W h Wh h]
             :minor/melodic  [W h W W W W h]})
```

The melodic minor is a bit special because, in melodies, it's only played in ascending order. When descending, the natural minor is used instead, though it can be referred to as the descending melodic minor scale. For our purposes of chord construction, this doesn't seem relevant, so I'll conveniently just treat it as a normal scale. Here it is, compared to the major:

```clojure
(scale-degrees :C :major)
;=> ((1 :C) (2 :D) (3 :E) (4 :F) (5 :G) (6 :A) (7 :B) (8 :C))

(scale-degrees :C :minor/melodic)
:=> ((1 :C) (2 :D) (3 :E♭) (4 :F) (5 :G) (6 :A) (7 :B) (8 :C))
```

Note the 6th note is natural (♮ not ♭ or ♯). After all that work, I can finally add the **m6** chord to the chord definitions:

```clojure
(def chords
  {:M  [:major         [1 3 5]]
   :M7 [:major         [1 3 5 7]]
   :m  [:minor/natural [1 3 5]]
   :m7 [:minor/natural [1 3 5 7]]
   ; ...
   :m6 [:minor/melodic [1 3 5 6]]})
```

The next chord that I struggled with is the **M7#11**. The 11th degree of a [diatonic scale](https://en.wikipedia.org/wiki/Diatonic_scale) (7-note scales) is the same note as the 4th, just an octave higher. So to tackle a **M7#11** chord, I need a scale which has a **♯4** in it. I sat down at the piano and played a major scale with a sharpened 4th to see how it sounded. It was nice! A combination of my foggy memory of [modes](https://en.wikibooks.org/wiki/Music_Theory/Modes) and some Wikipedia spelunking led me to the [lydian mode](https://en.wikipedia.org/wiki/Lydian_mode), which is a major scale with a **♯4**. Perfect.

It turns out that modes can be thought of as "rotations" of an existing scale. That is, rotating the C major scale (**C D E F G A B**) a degree gives a new scale with the same notes (**D E F G A B C**). Typically, when modes are mentioned, the modes based on the 7 degrees of a major scale are implied, though they can be based on other scales, [such as the melodic minor scale](https://en.wikipedia.org/wiki/Jazz_scale#Modes_of_the_melodic_minor_scale) or [harmonic minor scale](http://docs.solfege.org/3.22/C/scales/har.html). Since this definition seems important and interesting, instead of writing out all the modes as I have done for other scales, it seems relevant and important to encode this relationship between modes and scales in code. 

After a few iterations, I came up with some generic functions (prior implementations likely exist):

```clojure
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
```

They work like so:

```clojure
(rotate 1 [:a :b :c :d])  ;=> (:b :c :d :a)
(rotations [:a :b :c :d]) ;=> ([:a :b :c :d] (:b :c :d :a) (:c :d :a :b) (:d :a :b :c))
(scales :major)           ;=> [2 2 1 2 2 2 1]
(rotations (scales :major))
;=> ([2 2 1 2 2 2 1] (2 1 2 2 2 1 2) (1 2 2 2 1 2 2) (2 2 2 1 2 2 1) (2 2 1 2 2 1 2) (2 1 2 2 1 2 2) (1 2 2 1 2 2 2))
```

I'm going to add these modes to the `scales` list in a way that reflects this relationship:

```clojure
(def scales 
  (let [scales {:major          [W W h W W W h]
                :minor/natural  [W h W W h W W]
                :minor/harmonic [W h W W h Wh h]
                :minor/melodic  [W h W W W W h]}
        modes (zipmap [:mode/ionian :mode/dorian :mode/phrygian :mode/lydian
                       :mode/mixolydian :mode/aeolian :mode/locrian]
                      (rotations (scales :major)))]
    (merge scales modes)))
```

Astute readers might realise that the `:minor/natural` and `:mode/aeolian` actually have the same associated definition now. The natural minor is the 6th degree mode of the major scale. It's rewarding to have read about that and then see it accidentally fall out of the model I've created. That relationship might actually stick in my head now.

All of that work was so that I could encode the **M7#11** chord, so I'll do that finally:

```clojure
(def chords
  {:M     [:major         [1 3 5]]
   :M7    [:major         [1 3 5 7]]
   :m     [:minor/natural [1 3 5]]
   :m7    [:minor/natural [1 3 5 7]]
   ; ...
   :m6    [:minor/melodic [1 3 5 6]]
   :M7#11 [:mode/lydian   [1 3 5 7 11]]})
```

At this point, it has become apparent to me that defining chords in terms of their scales taught me a lot about the relationship between chords and scales. However, it doesn't help me compare chords mentally because I have to think too hard about the scales (which I only know on paper). If I can factor out the indirection so that all chords are defined in the same absolute space, it would be an improvement. 

I could define everything in terms of the major scale, as is often done, but I really like thinking about the intervals as indexes in a space of notes. Defining them in terms of one specific scale means a lot of complexity around dealing with sharps and flats and other accidentals. Instead, I can define both chords and scales in terms of absolute distances from a starting note.

I've found the representation used up until now to be extremely helpful so as I move on, I'll be thinking about how I can later *generate* those previous representations when needed. 

Writing the chords out as pitch intervals (i.e. number of half steps) instead of as scale degrees will allow easy comparison of chords based on different scales. Intervals describe the distance between two pitches (in terms of physics, they are ratios between the frequencies of each pitch). This means we could use it to describe all scales in terms of the intervals between each note and its tonic note or the previous note. We can also describe all chords in the same fashion. This seems like a useful internal representation of these concepts, from which the prior representations can be derived anyway.

In fact, my scales are already defined in this way, though I am representing them relatively instead of absolutely and indirectly converting them *to* an absolute representation (inside `take-nths`):

```clojure
(reductions + 0 [W W h W W W h]) ;=> (0 2 4 5 7 9 11 12)
```

I then pluck those absolute half step counts from a chromatic series of notes to get the final scale:

```clojure
(map (partial nth (note-series :C)) '(0 2 4 5 7 9 11 12))
;=> (#{:C} #{:D} #{:E} #{:F} #{:G} #{:A} #{:B} #{:C})
```

This strategy can work for chords too!

```clojure
;; Am chord
(map (partial nth (note-series :A)) [0 3 7]) ;=> (#{:A} #{:C} #{:E})

;; G♯M7
(map (partial nth (note-series :G♯)) [0 4 7 11]) ;=> (#{:G♯ :A♭} #{:C} #{:D♯ :E♭} #{:G})
```

Next, I want to write out a chord definition list in this fashion and adjust my `chord` function appropriately. However, these magic numbers aren't very meaningful by themselves and they don't reveal much about the nature of music. I'd rather reference them by name so that the significance of the difference between chords is shown most effectively.

There are 12 half steps between a note and its octave note (13 if you count the 1:1 interval between a note and itself). These intervals are named by their **degree** (first/unison, second, ..., eighth/octave) and their **quality** (major, minor, and perfect). Some degrees are perfect, while some degrees have both a major and a minor quality. This doesn't make immediate sense and seems arbitrary. 

Here are the two octaves worth of intervals (note that the second octave just repeats the pattern of qualities) for us to think about. 

*Simple* intervals are those within a single octave:

| ½ Steps     |  0  | 1  | 2  | 3  | 4  | 5   | 6  | 7   | 8  | 9  | 10 | 11 | 12  |
|------------:|:---:|:--:|:--:|:--:|:--:|:---:|:--:|:---:|:--:|:--:|:--:|:--:|:---:|
| **Degree**  | 1st |   2nd  ||   3rd  || 4th | -  | 5th |   6th  ||   7th  || 8th |
| **Quality** | P   | m  | M  | m  | M  | P   | TT | P   | m  | M  | m  | M  | P   |

*(I'll come to the **TT** interval at 6 half steps, later...)*

*Compound* intervals are those that span more than one octave:

| ½ Steps     | 12  | 13 | 14 | 15 | 16 | 17   | 18 | 19   | 20 | 21 | 22 | 23 | 24   |
|------------:|:---:|:--:|:--:|:--:|:--:|:----:|:--:|:----:|:--:|:--:|:--:|:--:|:----:|
| **Degree**  | 8th |   9th  ||  10th  || 11th | -  | 12th |  13th  ||  14th  || 15th |
| **Quality** | P   | m  | M  | m  | M  | P    | -  | P    | m  | M  | m  | M  | P    |

I'm going to define symbols for these interval names with the half step count as the value. I'm going to use a macro so that I can organize the symbols visually in a way that currently makes sense to me:

```clojure
(defmacro defintervals
  [& names]
  `(do
     (def interval-names '[~@names])
     ~@(map-indexed #(when-not (= '_ %2)
                         (list 'def %2 %1))
                      names)))

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

(def R P1)  ; convenient alias for "root" note

M2 ;=> 2
```

If you don't understand macros, don't worry. See below to see how the macro is *expanded* by the compiler, so you can understand what I could have written instead:

``` clojure
(macroexpand '(defintervals a b _ c)) 
;=> (do (def moosic.core/interval-names (quote [a b _ c])) (def a 0) (def b 1) nil (def c 3))
```

Written more idiomatically, it's equivalent to:

``` clojure
(def interval-names '[a b _ c]))
(def a 0)
(def b 1)
; skip _
(def c 3)
```

Between the prior text and the following text, I spent about a day trying to really grok intervals. The idea of the number of halfsteps between two notes is obvious to understand, but the significance of the names applied to them is not. It seemed to me like more learn-by-rote and memorising special cases than I am comfortable with. Most explanatinos for the names were tautological without prior knowledge—"a major interval becomes a minor interval when in verted" doesn't actualy reveal what it means to be major or minor in the first place!

I tried to think about these intervals in terms of the scales I had already represented in code, and came up with what seemed like a rule of thumb and a use mnemonic:

> Minor intervals are those which only appear in some minor scale. Of the remaining, the major intervals are those which appear in the major scale but only in some or no minor scale. Perfect intervals are those of the remaining which always appear in both.

Unfortunately, after thinking about it some more, this didn't fit. The **minor second** interval is not in any common minor scale. There isn't even a scale I can find which has just  **♭2 ♭3 ♭6 ♭7** (though one might still exist). Furthermore, the interval between **perfect fourth** and **perfect fifth** isn't in either the major scale or a minor scale. It doesn't even have a **degree** or **quality**! What is going on‽

Luckily, I finally stumbled on [an explanation](http://music.stackexchange.com/a/30413/21702) that provided insight instead of yielding even more questions.

The intervals from the root in an *ascending* major scale form the major and perfect intervals. The intervals from the octave in a *descending* major scale form the minor and perfect intervals. Perfect intervals are in both and the majors/minors are in the ascending/descending, respectively. At the heart of this is the concept of [inversions](https://en.wikipedia.org/wiki/Interval_(music)#Inversion). In this context, it's raising the lowest or lowering the highest notes in an interval (e.g. **C→F** to **F→C**). An inverted **M7** interval (**C→B**) is a **m2** (**B→C**). The perfect intervals remain perfect (a **P5** becomes a **P4** and vice versa while the **P1** becomes **P8** and vice versa). That middle tone (6 half steps) which has no degree in the table above is called the [Tritone](https://en.wikipedia.org/wiki/Tritone). It's special because it is directly in the middle of the chromatic 12 steps, which means its inversion is the same interval (**C→F♯** is the same number of half steps as **F♯→C**). It also doesn't appear (relative to the tonic or octave) in either the ascending or descending major scale, like the rest.

Here's an example in terms of the **C Major** scale:

|     Interval | C<sub>1</sub> |  D |  E |  F | *(F♯)* |  G |  A |  B |  C<sub>2</sub> |
|-------------:|:-------------:|:--:|:--:|:--:|:------:|:--:|:--:|:--:|:--------------:|
| **from C<sub>1</sub>** | P1  | M2 | M3 | P4 | *(TT)* | P5 | M6 | M7 | P8             |
|   **to C<sub>2</sub>** | P8  | m7 | m6 | P5 | *(TT)* | P4 | m3 | m2 | P1             |

_(Note: the **F♯** tritone is obviously not in the scale but including it here is helpful to see the structural symmetry and to cover all the interval types.)_

So that's cool. I feel like I've unlocked a nice, consistent, explainable property of intervals—exactly the kind of thing I personally need to learn and remember concepts.

I feel like I should come back to some code now, so I'm going to try to see if I understand the concept of an interval inversion:

```clojure
(defn invert [t] (interval-names (- P8 t)))

(invert m3) ;=> M6
(invert P4) ;=> P5
(invert TT) ;=> TT

(defn inversion? 
  [t1 t2]
  (= t1 (var-get (resolve (invert t2)))))
    
(inversion? P4 P5) ;=> true
(inversion? m2 M7) ;=> true
(inversion? M7 m2) ;=> true
(inversion? m2 M2) ;=> false
```

That seems right. According to [this article](http://www.thecipher.com/inversions-intervals_2.html), compound intervals invert differently. That is, they simply traspose down an octave (e.g. M9→M2), which means a compound inversion is not symmetrical. So, a very minor adjustment is needed:

```clojure
(defn invert [t] (interval-names (Math/abs (- P8 t))))
```

From my reading about intervals, I've determined that there are multiple names for the numeric intervals and taht the correct name to use is entirely dependent on context. Nonetheless, I'll leave this as is and move on.

All of this dancing around interval names was so that my chords could be defined with named intervals, so that's next:

```clojure
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
```

I also need to adjust the `chord` function to use the new representation. Fortunately, it gets even simpler:

```clojure
(defn chord
  [name root]
  (map (partial nth (note-series root))
       (chords name)))
       
(chord :m7 :C) ;=> (#{:C} #{:D♯ :E♭} #{:G} #{:A♯ :B♭})
```

Now, I'm curious if I can get back to my scale-based representation of chords. I should be able to filter the scales by ones that include the notes of the chord, then return the indexes of those notes in the scale, along with the scale name.

First, I'll need to convert the scales to absolute intervals:

```clojure
(defn map-values
  [f m]
  (into {}
        (map (fn [[k v]] [k (f v)])
             m)))

(def interval-scales (map-values (partial reductions + 0) scales))

interval-scales ;=> {:mode/aeolian (0 2 3 5 7 8 10 12), ...}

(def named-interval-scales (map-values (partial map interval-names) interval-scales))

named-interval-scales ;=> {:mode/aeolian (P1 M2 m3 P4 P5 m6 m7 P8), ...}
```

Then, I need to filter the scales by whether or not it includes the chord's intervals:

```clojure
(defn chord-in-scale?
  [chord scale]
  (every? (set (interval-scales scale)) 
          (chords chord)))

(chord-in-scale? :M :major) ;=> true
(chord-in-scale? :m :minor) ;=> false
```

Neat! This function acts poorly if given arguments without definitions, but I'm not worrying about that level of correctness right now, because I expect a level of churn with all this code as my understanding of the concepts evolves.

Now, let's find scale representations for a chord!

```clojure
(defn scales-for-chord
  [chord]
  (into {}
    (for [[s ints] interval-scales 
          :when (chord-in-scale? chord s)]
      [s (map (comp inc #(.indexOf ints %)) 
              (chords chord))])))
              
(scales-for-chord :M) ;=> {:mode/ionian (1 3 5), :major (1 3 5) ...}
(scales-for-chord :m) ;=> {:mode/aeolian (1 3 5), :minor/natural (1 3 5), ...}
(scales-for-chord :7) ;=> {:mode/mixolydian (1 3 5 7)}
(scales-for-chord :m/M7) ;=> {:minor/melodic (1 3 5 7), :minor/harmonic (1 3 5 7)}
```

Let's get all the matching scales for all our chords!

```clojure
(def scale-chords (map-values scales-for-chord (map (fn [[k _]] [k k]) chords)))

scale-chords ;=> {:M {:mode/ionian (1 3 5), ...}, :m/M7 {:minor/melodic (1 3 5 7), ...}, ...}
```

## Future

* generate chord charts dynamically by applying notes to the fretboard with constraints (how wide can a hand stretch, how many fingers exist, which strings can be muted, etc)
   
    ```clojure
    ;; a 24-fret guitar fretboard of notes
    (def guitar
      (map (comp (partial take 24) note-series)
           [:E :A :D :G :B :E]))
    ```
* interactive visualisations (circle of fifths)
* representing chords
	* as intervals as applied to all known scales
	* as absolute intervals (e.g. dim is R m3 d5 - i.e. intervals from root)
	* as relative intervals (e.g. dim is R m3 m3 — i.e. intervals between component tones)
* representing scales
    * as semitone/tone
    * as intervals from root
* "diff" a chord (e.g. diff minor to Major? "♭3")
* "diff" a scale
* generate modal melodies based on a sequence of chords
* interface with Java or JS (if ClojureScript) MIDI/sound libraries to hear intervals and chords
* interval/chord ear training
* typing tutor style exercise that asks for intervals and listens to an instrument pluck the relative interval. Goal here is to learn the positions of intervals on the fretboard, I think.
* Edit this to an executable "literate Clojure" file so snippets are executable and remain correct.
	* or a Gorilla REPL worksheet! Especially if the "plot" renderings are pluggable!
* Whatever feels like fun!
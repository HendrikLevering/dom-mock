(ns de.levering-it.dom-mock.model.data
  (:require [clojure.string :as string]
            [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]))

(def elements #{"a"
                "abbr"
                "area"
                "article"
                "aside"
                "audio"
                "b"
                "bdi"
                "blockquote"
                "br"
                "button"
                "canvas"
                "cite"
                "code"
                "col"
                "colgroup"
                "data"
                "datalist"
                "dd"
                "del"
                "details"
                "dialog"
                "div"
                "dl"
                "dt"
                "em"
                "embed"
                "fieldset"
                "figure"
                "footer"
                "form"
                "h1"
                "h2"
                "h3"
                "h4"
                "h5"
                "h6"
                "header"
                "hgroup"
                "hr"
                "i"
                "iframe"
                "img"
                "input"
                "ins"
                "itemprop"
                "kbd"
                "label"
                "legend"
                "li"
                "link"
                "main"
                "mark"
                "math"
                "menu"
                "meter"
                "nav"
                "noscript"
                "object"
                "ol"
                "optgroup"
                "output"
                "p"
                "picture"
                "pre"
                "progress"
                "q"
                "ruby"
                "s"
                "samp"
                "script"
                "section"
                "select"
                "slot"
                "small"
                "span"
                "strong"
                "style"
                "sub"
                "summary"
                "sup"
                "table"
                "tbody"
                "td"
                "template"
                "textarea"
                "th"
                "thead"
                "time"
                "tr"
                "u"
                "ul"
                "var"
                "video"
                "wbr"})





(def ^:const SVG-NS "http://www.w3.org/2000/svg")
(def ^:const XLINK-NS "http://www.w3.org/1999/xlink")


;; Lang attribute - valid language tag
(s/def ::lang (s/and string? #(re-matches #"^[a-zA-Z]{2,3}(-[a-zA-Z]{2,3})?$" %)))

;; Translate - only yes/no values
(s/def ::translate #{"yes" "no"})

;; Inert - boolean attribute
(s/def ::inert boolean?)

;; Class - space-separated string or sequence of strings
(s/def ::class (s/or :string-list (s/and string? #(re-matches #"^[a-zA-Z0-9_-]+(\s[a-zA-Z0-9_-]+)*$" %))
                     :sequence (s/coll-of string?)))

;; ID - unique identifier string
(s/def ::id (s/and string? #(re-matches #"^[A-Za-z][A-Za-z0-9_:.-]*$" %)))

;; Enterkeyhint - specific values for virtual keyboard enter key
(s/def ::enterkeyhint #{"enter" "done" "go" "next" "previous" "search" "send"})

;; Style - either string or map of style properties
(s/def ::style (s/or :string string?
                     :map (s/map-of keyword? string?)))

;; Inputmode - specific values for virtual keyboard type
(s/def ::inputmode #{"none" "text" "decimal" "numeric" "tel" "search" "email" "url"})

;; Dir - text direction
(s/def ::dir #{"ltr" "rtl" "auto"})

;; Draggable - boolean attribute
(s/def ::draggable boolean?)

;; Popover - specific values for popover behavior
(s/def ::popover #{"auto" "manual"})

;; Title - string tooltip
(s/def ::title string?)

;; Contenteditable - boolean or specific values
(s/def ::contenteditable (s/or :boolean boolean?
                               :string #{"true" "false"}))

;; Accesskey - single character or space-separated list
(s/def ::accesskey (s/and string? #(re-matches #"^[a-zA-Z0-9](\s[a-zA-Z0-9])*$" %)))

;; Hidden - boolean or specific values
(s/def ::hidden (s/or :boolean boolean?
                      :string #{"hidden" "until-found"}))

;; Data attributes - any string with data- prefix
(s/def ::data-attr (s/and string? #(string/starts-with? % "data-")))
(s/def ::data-value string?)
(s/def ::data-attributes (s/map-of ::data-attr ::data-value))

;; Spellcheck - boolean or specific values
(s/def ::spellcheck (s/or :boolean boolean?
                          :string #{"true" "false"}))

;; Tabindex - integer within reasonable range
(s/def ::tabindex (s/int-in -32768 32767))

;; Combined global attributes spec
(s/def ::global-attributes
  (s/keys :opt-un [::lang ::translate ::inert ::class ::id ::enterkeyhint
                   ::style ::inputmode ::dir ::draggable ::popover ::title
                   ::contenteditable ::accesskey ::hidden ::data-attributes
                   ::spellcheck ::tabindex]))

(def global-attr-gen
  (gen/let [attr (gen/elements [:lang :translate :inert :class :id :enterkeyhint
                                :style :inputmode :dir :draggable :popover :title
                                :contenteditable :accesskey :hidden :spellcheck :tabindex])
            value (gen/one-of
                   [(case attr
                      :lang (gen/fmap #(str (first %) "-" (second %))
                                      (gen/tuple (gen/elements ["en" "de" "fr"])
                                                 (gen/elements ["US" "DE" "FR"])))
                      :translate (gen/elements ["yes" "no"])
                      :inert gen/boolean
                      :class (gen/one-of [(gen/elements ["header" "main" "footer"])
                                          (gen/vector (gen/elements ["btn" "primary" "active"]) 1 3)])
                      :id (gen/fmap #(str "id-" %) gen/string-alphanumeric)
                      :enterkeyhint (gen/elements ["enter" "done" "go" "next" "previous" "search" "send"])
                      :style (gen/hash-map :color (gen/elements ["red" "blue" "green"]))
                      :inputmode (gen/elements ["none" "text" "decimal" "numeric"])
                      :dir (gen/elements ["ltr" "rtl" "auto"])
                      :draggable gen/boolean
                      :popover (gen/elements ["auto" "manual"])
                      :title gen/string-ascii
                      :contenteditable (gen/one-of [gen/boolean (gen/elements ["true" "false"])])
                      :accesskey (gen/elements ["a" "b" "c"])
                      :hidden (gen/one-of [gen/boolean (gen/elements ["hidden" "until-found"])])
                      :spellcheck (gen/one-of [gen/boolean (gen/elements ["true" "false"])])
                      :tabindex (gen/choose -32768 32767))])]
    [attr value]))

(comment
  (s/valid? ::global-attributes {:lang "en-US"
                                              :translate "yes"
                                              :class ["header" "main"]
                                              :id "id-123"
                                              :style {:color "blue"}
                                              :dir "ltr"
                                              :draggable true
                                              :title "Sample Title"
                                              :tabindex 1})

    (s/explain ::global-attributes {:lang "invalid-format"
                                   :translate "invalid"
                                   :tabindex 99999})

  (gen/sample (gen/one-of [gen/boolean (gen/elements ["true" "false"])]))
  (gen/sample global-attr-gen 5)
  )
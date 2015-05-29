(ns guidemo.core
  (:gen-class)
  (:use [date-clj])
  (:require [seesaw.core :as seesaw]
            [seesaw.dev :as dev]
            ))

(defn count-non-numeric [num]
  (count (filter #(not (number? (read-string (str %)))) (flatten (partition-all 1 num)))))

(defn count-spaces [num]
  (count (filter #(= \space %) (flatten (partition-all 1 num)))))

(defn count-decimals [num]
  (count (filter #(= \. %) (flatten (partition-all 1 num)))))

(defn valid-number? [num]
  (and (zero? (count-spaces num)) (zero? (count-non-numeric num))))

(defn valid-price? [num]
  (or
    (and (zero? (count-decimals num)) (valid-number? num))
    (and (= (count-decimals num) 1) (<= (count-non-numeric num) 1) )))

(defn format-currency [num]
  (if (valid-price? num)
    (str \$ (format "%.2f" (double (read-string num))))))

(defn output-date [date]
  (.format (java.text.SimpleDateFormat. "MM/dd/yyyy") date))

(defn change-case  [x]
  (clojure.string/lower-case x))

(def main-frame)
(def invalid-num-error "Please enter a valid number over 1!")
(def blank-error "Please fill out all data fields!")

(defn read-in [file-name]
  (binding [*read-eval* false]
    (read-string (slurp file-name))))

(def movies-coll (read-in "movies.clj"))
(def rentals-coll (read-in "rentals.clj"))

(def movies-table
  (seesaw/table :id :movies-table
    :model [
      :columns [ {:key :title :text "Movie Title"}
                 {:key :price :text "Price"}
                 {:key :qty :text "Quantity"} ]
      :rows movies-coll ] ))


(def rented-table
  (seesaw/table :id :rented-table
    :model [
      :columns [ {:key :title :text "Movie Title"}
                 {:key :renter :text "Renter"}
                 {:key :due :text "Due Date"}]
      :rows rentals-coll ]))


(defn get-all-movies [start]
  (cond
    (nil? (seesaw.table/value-at movies-table start)) '()
    :else (cons (seesaw.table/value-at movies-table start) (get-all-movies (inc start))) ))

(defn get-all-rentals [start]
  (cond
    (nil? (seesaw.table/value-at rented-table start)) '()
    :else (cons (seesaw.table/value-at rented-table start) (get-all-rentals (inc start))) ))

(defn write-out [table-name]
  (if (= table-name "movies")
    (spit "movies.clj" (prn-str (get-all-movies 0)))
    (spit "rentals.clj" (prn-str (get-all-rentals 0))) ))


(defn display-info [movie]
  (let [movies (get-all-movies 0)
        title (:title movie)
        price (:price movie)
        qty (:qty movie)
        id (:id-num movie)]
    (if (empty? title)
      (seesaw/alert "Unable to find movie in database")
      (seesaw/alert (str "Title: " title  "\nPrice: " price  "\nQuantity: " qty "\nID: " id)) )))

(defn traverse-list
  "Searches for specific movie based on search-type given"
  [coll search-type x]
  (let [movie (first coll)
        title (:title movie)
        price (:price movie)
        qty (:qty movie)]
  (cond
    (empty? coll) {:title "" :price "" :qty "" :id-num ""}
    (= (change-case ((keyword search-type) movie)) (change-case x)) movie
    :else (traverse-list (rest coll) search-type x))))


(def id-max (atom 0))

(defn initialize-id
  "Gets current max id in movies coll and initializes id-max to that value"
  [id-max]
  (let [rows (dec (seesaw.table/row-count movies-table))
        value (:id-num (seesaw.table/value-at movies-table rows))]
    (reset! id-max value)))

(defn set-id []
  (swap! id-max inc))

(defn get-movie
  "Gets movie at the specified index from available movies table"
  [index]
  (seesaw.table/value-at movies-table index))

(defn get-rental [index]
  (seesaw.table/value-at rented-table index))

(defn movie-index
  "Gets the index of selected movie in available movies table"
  [title index]
  (let [current (:title (get-movie index))]
    (cond
      (= current title) index
      :else (movie-index title (inc index)))))


(defn add-action
  "Adds movie in table and writes results to file"
  [rows t p q]
  (do
    (seesaw.table/insert-at! movies-table rows {:title t :price (format-currency p) :qty (read-string q) :id-num (set-id)})
    (write-out "movies")))

(defn add-fn [movie]
  (let [title (seesaw/text (seesaw/select (seesaw/to-root movie) [:#title]))
        price (seesaw/text (seesaw/select (seesaw/to-root movie) [:#price]))
        qty (seesaw/text (seesaw/select (seesaw/to-root movie) [:#qty]))
        rows (seesaw.table/row-count movies-table)]
  (cond
    (or (empty? title) (empty? price) (empty? qty)) (seesaw/alert blank-error)
    (or (not (valid-number? qty)) (not (valid-price? price))) (seesaw/alert invalid-num-error)
    :else (add-action rows title price qty) )))

(defn add-dialog [e]
  (seesaw/dialog :id :dialog :visible? true :width 340 :height 250 :title "Add New Movie" :content
    (seesaw/form-panel
      :items
         [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
         [(seesaw/label :text "Please fill out the following fields to add movie:   " :halign :center :valign :top)]
         [(seesaw/grid-panel
           :border "Movie Information"
           :columns 2
           :items ["Title" (seesaw/text :columns 20 :halign :left :id :title)
                   "Price" (seesaw/text :columns 20 :halign :left :id :price)
                   "Quantity" (seesaw/text :columns 20 :halign :left :id :qty)]) :grid :wrap] ])
      :option-type :ok-cancel
      :success-fn add-fn ))


(defn update-qty [sel old-qty qty]
  (do
    (seesaw.table/update-at! movies-table sel {:qty (+ old-qty (read-string qty))})
    (write-out "movies") ))

(defn add-qty-fn [movie]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        old-qty (:qty (get-movie sel))
        qty (seesaw/text (seesaw/select (seesaw/to-root movie) [:#qty]))]
    (cond
      (nil? sel) (seesaw/alert "Please choose available movie! ")
      (empty? qty) (seesaw/alert blank-error)
      (valid-number? qty) (update-qty sel old-qty qty)
      :else (seesaw/alert invalid-num-error))))

(defn qty-dialog [movie]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        title (:title (get-movie sel))
        price (:price (get-movie sel))
        qty (str (:qty (get-movie sel)))]
  (seesaw/dialog :visible? true :width 330 :height 260 :title "Add copies to inventory" :content
    (seesaw/form-panel
      :items [
        [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(seesaw/label :text "Add copies of existing movie" :halign :center :valign :top)]
        [(seesaw/grid-panel
          :border "Movie Information"
          :columns 2
          :items ["Title" title
                  "Price" price
                  "Quantity" qty]) :grid :wrap]
        [(seesaw/horizontal-panel
          :items ["Copies added " (seesaw/text :columns 5 :halign :left :id :qty)]) :grid :wrap]])
      :option-type :ok-cancel
      :success-fn add-qty-fn )))


(defn remove-action [sel old-qty qty]
  (do
    (seesaw.table/update-at! movies-table sel {:qty (- old-qty (read-string qty))})
    (write-out "movies")))

(defn delete-action [sel]
  (do
    (seesaw.table/remove-at! movies-table sel))
    (write-out "movies"))

(defn delete-fn [e]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        chosen-movie (:title (get-movie sel))
        rentals (get-all-rentals 0)
        title (:title (traverse-list rentals "title" chosen-movie))]
  (do
    (cond
      (empty? title) (delete-action sel)
      :else (seesaw/alert "Copies of movie are currently rented! Cannot complete deletion"))
    (seesaw/return-from-dialog e nil)) ))

(defn remove-fn [e]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        old-qty (:qty (get-movie sel))
        title (:title (get-movie sel))
        qty (seesaw/text (seesaw/select (seesaw/to-root e) [:#qty-field])) ]
    (do
      (cond
        (empty? qty) (seesaw/alert blank-error)
        (not (valid-number? qty)) (seesaw/alert invalid-num-error)
        (> (read-string qty) old-qty) (seesaw/alert "Cannot remove more copies than current amount!")
        (not= 0 old-qty) (remove-action sel old-qty qty)
        :else (seesaw/alert "There are no more copies to remove!"))
      (seesaw/return-from-dialog e nil))))



(defn remove-dialog [e]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        title (:title (get-movie sel))
        price (:price (get-movie sel))
        qty (str (:qty (get-movie sel)))
        ]
  (seesaw/custom-dialog :visible? true :width 360 :height 230 :title "Remove Movie From Inventory" :content
    (seesaw/form-panel
      :items [
        [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(seesaw/label :text "Remove copies of current movie or delete a movie" :halign :center :valign :top)]
        [(seesaw/grid-panel
          :border "Selected Movie"
          :columns 2
          :items ["Title" title
                  "Price" price
                  "Quantity" qty]) :grid :wrap]
        [(seesaw/horizontal-panel
          :items ["Copies removed " (seesaw/text :columns 5 :halign :left :id :qty-field)
                        (seesaw/action :name "Remove Copies" :handler remove-fn)]) :grid :wrap]
        [(seesaw/horizontal-panel
          :items [(seesaw/action :name "Delete Movie" :handler delete-fn)
                        (seesaw/action :name "Cancel" :handler
                                       (fn [e] (seesaw/return-from-dialog e nil)))]) :grid :wrap]] ))))


(defn rent-action [title renter due qty rows sel]
  (do
    (seesaw.table/insert-at! rented-table rows [title renter due])
    (seesaw.table/update-at! movies-table sel {:qty (dec qty)})
    (write-out "movies")
    (write-out "rentals")))

(defn rent-fn [movie]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        title (:title (get-movie sel))
        qty (:qty (get-movie sel))
        rows (seesaw.table/row-count rented-table)
        renter (seesaw/text (seesaw/select (seesaw/to-root movie) [:#renter]))
        due-date (output-date (from-now 14 :days))]
    (cond
      (empty? renter) (seesaw/alert "Please fill out all data fields!")
      (not (zero? qty)) (rent-action title renter due-date qty rows sel)
      :else (seesaw/alert "No more copies are available for rent!") )))


(defn rent-dialog [movie]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        title (:title (get-movie sel))
        qty (str (:qty (get-movie sel)))
        price (:price (get-movie sel))
        rows (seesaw.table/row-count rented-table)
        due-date (output-date (from-now 14 :days))]
    (seesaw/dialog :visible? true :width 340 :height 300 :title "Rent Movie" :content
      (seesaw/form-panel
        :items [
        [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(seesaw/label :text "The following movie will be rented " :halign :center :valign :top)]
        [(seesaw/grid-panel
          :border "Selected Movie"
          :columns 2
          :items ["Title" title
                  "Price" price
                  "Quantity" qty
                  "Due Date" due-date]) :grid :wrap]
        [(seesaw/grid-panel
          :border "Renter Information"
          :columns 2
          :items ["Name" (seesaw/text :columns 20 :halign :left :id :renter)]) :grid :wrap] ])
        :option-type :ok-cancel
        :success-fn rent-fn )))


(defn find-fn [movie]
  (let [title-field (seesaw/text (seesaw/select (seesaw/to-root movie) [:#title]))
        id-field (seesaw/text (seesaw/select (seesaw/to-root movie) [:#id-field]))
        movies (get-all-movies 0)]
    (do
      (cond
        (not (valid-number? id-field)) (seesaw/alert "Please enter only integers for ID")
        (and (not (empty? title-field)) (empty? id-field) ) (display-info (traverse-list movies "title" title-field))
        (and (not (empty? id-field)) (empty? title-field)) (display-info (traverse-list movies "id-num" id-field))
        :else (seesaw/alert "Please fill in one of the fields!")) )))

(defn find-dialog [e]
  (seesaw/dialog  :visible? true :width 420 :height 280 :title "Find Movie Information" :content
    (seesaw/form-panel
      :items [
        [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(seesaw/label :text "Fill out one of the following fields to search for movie information" :h-text-position :center)]
        [(seesaw/grid-panel
               :border "Search by title"
               :columns 2
               :items ["Title" (seesaw/text :columns 20 :halign :left :id :title)]) :grid :wrap]
        ["OR" :grid :wrap]
        [(seesaw/grid-panel
               :border "Search by ID"
               :columns 2
               :items ["ID number" (seesaw/text :columns 20 :halign :left :id :id-field)]) :grid :wrap] ])
      :option-type :ok-cancel
      :success-fn find-fn ))


(defn return-action [rented index qty]
  (do
    (seesaw.table/remove-at! rented-table rented)
    (seesaw.table/update-at! movies-table index {:qty (inc qty)})
    (write-out "movies")
    (write-out "rentals")))

(defn return-fn [movie]
  (let [rented (seesaw/selection (seesaw/select main-frame [:#rented-table]))
        title (:title (get-rental rented))
        movies (get-all-movies 0)
        qty (:qty (traverse-list movies "title" title))]
    (cond
      (not (nil? rented)) (return-action rented (movie-index title 0) qty)
      :else (seesaw/alert "Please choose a rented movie!")) ))


(defn return-dialog [e]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#rented-table]))
        title (:title (get-rental sel))
        renter (:renter (get-rental sel))
        date (:due (get-rental sel))]
  (seesaw/dialog :visible? true :width 300 :height 200 :title "Return Movie To Inventory" :content
    (seesaw/form-panel
      :items [
        [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(seesaw/label :text "Return the movie back to inventory?" :halign :center :valign :top)]
        [(seesaw/grid-panel
          :border "Movie Information"
          :columns 2
          :items ["Title" title
                  "Renter" renter
                  "Due Date" date]) :grid :wrap]])
      :option-type :ok-cancel
      :success-fn return-fn )))

(defn update-price [sel price]
  (do
    (seesaw.table/update-at! movies-table sel {:price (format-currency price)})
    (write-out "movies")))

(defn change-fn [movie]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        price (seesaw/text (seesaw/select (seesaw/to-root movie) [:#price-field]))]
    (do
      (cond
        (nil? sel) (seesaw/alert "Please select a movie!")
        (empty? price) (seesaw/alert "Please fill out all data fields!")
        (valid-price? price) (update-price sel price)
        :else (seesaw/alert "Please enter valid numbers greater than 1")))))

(defn change-dialog [e]
  (let [sel (seesaw/selection (seesaw/select main-frame [:#movies-table]))
        title (:title (get-movie sel))
        price (:price (get-movie sel))]
  (seesaw/dialog :visible? true :width 260 :height 220 :title "Change Price of Movie" :content
    (seesaw/form-panel
      :items [
        [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        ["Enter new price for selected movie\n"]
        [(seesaw/grid-panel
          :border "Selected Movie"
          :columns 2
          :items ["Title" title
                  "Current Price  " price]) :grid :wrap]
        [(seesaw/horizontal-panel
          :items ["New Price " (seesaw/text :columns 5 :halign :left :id :price-field)]) :grid :wrap]])
      :option-type :ok-cancel
      :success-fn change-fn)))


(def add-button
  (seesaw/button
    :id :add-button
    :text "Add New Movie"
    :listen [:mouse-pressed add-dialog]
    :popup add-dialog))

(def add-qty-button
  (seesaw/button
    :text "Add Copies"
    :listen [:mouse-pressed qty-dialog]
    :popup qty-dialog))

(def remove-button
  (seesaw/button
    :id :remove-button
    :text "Remove Movie"
    :listen [:mouse-pressed remove-dialog]
    :popup remove-dialog))

(def rent-button
  (seesaw/button
    :text "Rent Movie"
    :listen [:mouse-pressed rent-dialog]
    :popup rent-dialog))

(def change-button
  (seesaw/button
    :id :change-button
    :text "Change Price"
    :listen [:mouse-pressed change-dialog]
    :popup change-dialog))

(def find-button
  (seesaw/button
    :text "Find Price/Quantity"
    :listen [:mouse-pressed find-dialog]
    :popup find-dialog))

(def return-button
  (seesaw/button
    :id :return-button
    :size [150 :by 80]
    :text "Return Movie"
    :listen [:mouse-pressed return-dialog]
    :popup return-dialog))


(def button-frame
  (seesaw/border-panel
    :items [[return-button :north]]))

(def rentals-frame
  (seesaw/form-panel
   :items [
     [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
     [(seesaw/label :text "Rented Movies" :halign :center :valign :top)]
     [[5 :by 5] :grid :wrap]
     [(seesaw/scrollable rented-table)]
     [button-frame :grid :next :gridheight 50] ]))

(def movies-frame
  (seesaw/form-panel
      :items [
        [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(seesaw/label :text "Available Movies" :halign :center :valign :top)]
        [[5 :by 5] :grid :wrap]
        [(seesaw/scrollable movies-table)]
        [(seesaw/grid-panel :columns 1
          :items [add-button add-qty-button remove-button rent-button change-button find-button]) :grid :next ]]))

(def contents-frame
  (seesaw/form-panel
    :items [
      [nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
      [movies-frame]
      [rentals-frame :grid :next]
      ["Click on a movie then click buttons to perform the action" :grid :wrap] ]))

(def main-frame
  (seesaw/frame :title "Video Inventory"
                :on-close :exit
                :resizable? false
                :content contents-frame ))

(defn -main
  [& args]
  (seesaw/show! main-frame)
  (seesaw/pack! main-frame)
  (initialize-id id-max))




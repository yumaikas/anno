(defn add-time [time time-spec]
    "Adds the numer of days/hours/minutes/seconds specified"
    (var ret-time time)
    (+= ret-time (* (get time-spec :days 0) (* 24 60 60)))
    (+= ret-time (* (get time-spec :hours 0) (* 60 60)))
    (+= ret-time (* (get time-spec :minutes 0) 60))
    (+= ret-time (* (get time-spec :seconds 0) 1))
    ret-time
)


(defn- to-midnight [time]
    (def dt (os/date time))
    (add-time time 
        { 
            :hours (- (dt :hours)) 
            :minutes (- (dt :minutes)) 
            :seconds (- (dt :seconds))
        })
)

(defn- next-weekday [given-day &opt time-from] 
    (def today (os/date (or time-from (os/time))))
    (def week-day (today :week-day))
    
    (to-midnight (add-time (os/mktime today) {:days (- (- week-day given-day))}))
)

(defn- next-month [] 
    (def today (os/date))
    (def month-day (today :month-day))
    
    # Go to the first of this month
    (def month-first (add-time (os/mktime today) {:days (- month-day)}))
    
    # Will always get us into next month
    (def day-in-next-month (os/date (add-time month-first {:days 32})))
    
    # Now go back to the first of next month
    (add-time (os/mktime day-in-next-month) {:days (- (day-in-next-month :month-day))})
)

(defn- next-year [&opt time-from]
    (def today (or time-from (os/date)))
    (def year-day (today :year-day))
    (def year-first (add-time (os/mktime today) {:days (- year-day)}))
    (def day-in-next-year (os/date (add-time year-first {:days 366})))
    (add-time (os/mktime day-in-next-year) {:days (- (day-in-next-year :year-day))})
)

(defn from-ymd [date-spec] 
    "Takes a ymd value in years, months, and days, 1-indexed (aka human friendly)"
    (os/mktime {
        :year (get date-spec :year 1970)
        :month (- (get date-spec :month 1) 1)
        :month-day (- (get date-spec :day 1) 1)
    })
)

(defn to-ymdstr [date]
    (string (date :year) "-" (+ (date :month) 1) "-" (+ (date :month-day) 1))
)

(defn parse-ymdstr [str]
    (defn parse-date [&opt year month day] 
        (from-ymd {
            :year year
            :month month
            :day day}
        )
    )
    (def ymd-pat (peg/compile ~{
        :digit (range "09")
        :sep (choice "-" "/")
        :year (cmt (capture (between 2 4 :digit)) ,scan-number)
        :month (cmt (capture (between 1 2 :digit)) ,scan-number)
        :day (cmt (capture (between 1 2 :digit)) ,scan-number)
        :main (cmt (sequence :year :sep :month :sep :day) ,parse-date)
    }))
    
    (match (peg/match ymd-pat str)
        @[dt] dt
        nil nil
        _ (error "Wasn't a YMD date!")
    )
)

(defn to-ymd [time] 
    (def t (os/date time))
    @{:year (t :year) :month (+ (t :month) 1) :day (+ (t :month-day) 1)} 
)


(defn next [time-spec &opt time-from] 
    (def today (or time-from (os/time)))
    (match (string/ascii-lower time-spec)
        "sunday" (next-weekday 0)
        "monday" (next-weekday 1)
        "tuesday" (next-weekday 2)
        "wednesday" (next-weekday 3)
        "thursday" (next-weekday 4)
        "friday" (next-weekday 5)
        "saturday" (next-weekday 6)
        "week" (next-weekday 0)
        "month" (next-month)
        "year" (next-year)
    )
)

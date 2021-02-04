(import path)
(import ./date)
(import ./plugins)
(use ./lib-anno)

# @task[Add a defer status to tasks to remove them from show up by default (but maybe show a count?)]
# @task[Multithread this as much as makese sense]
# @task[Figure out how to profile how much time is spent in a given dir]

(defn- walk-rec-dirs [path func &opt list-dir?] 
    (if (not= (string/find ".git" path) nil)
        (break)
    )
    (default list-dir? base-list-dir?)
    (each p (os/dir path)
        (def currP (path/abspath (path/join path p)))
        (def info (os/stat currP))
        (match (info :mode)
            :directory (if 
                (list-dir? currP) 
                (do 
                    (func currP)
                    (walk-rec currP func)
                )
            )
            _ false
        )
    )
)

(defn- print-rec [path]
    (walk-rec path (fn [x] (print x)))
)



(defn- scan-file [path] 
    (def to-search (slurp path))
    (def results (anno-scan to-search))
    (if (> (length results) 0) 
        (print path)
    )
    (each hit results
        (print (hit :type))
        (put hit :range nil)
        (pp (table/to-struct hit))
    )
)

(defn- scan-rec [args]
    (walk-rec (dyn :jd-folder) scan-file)
)

(defn- is-todo-valid? [todo] 
   true 
)


(defn proc-dated-entry [path hit spec] 
    (defn hit-data [key &opt dflt] 
        (get-in hit [:args :kv key] dflt)
    )
    (var date-on nil)
    (var date-ts nil)
    (if-let [
        l-date-on (hit-data (spec :datefield))
        parsed-date-on (date/parse-ymdstr l-date-on)]
        (do 
            (set date-on (os/date parsed-date-on))
            (set date-ts parsed-date-on)
        )
        (break nil)
    )
    (def saturday (date/next "Saturday"))
    (def last-saturday (date/add-time saturday :days -7))
    
    (if (<= last-saturday date-ts saturday)
    {
        :filename (path/join (splice (array/slice (path/parts path) -3 -1)))
        :desc (string/join (get-in hit [:args :pos]) "\r\n")
        (keyword (spec :prefix) "-ts") date-ts
        (keyword (spec :prefix) "-on") (date/to-ymdstr date-on)
        :pp (spec :pp) 
    })
)


(defn- review [path] 
    (def path (dyn :jd-folder))
    # ; Today I learned something
    (def til @[])
    
    # ; Closes over the til collection
    (defn proc-hit [path hit]
        (match (hit :type)
            "til" (array/push-not-nil til (proc-dated-entry path hit {
                :prefix "learned"
                :datefield "on"
                :pp (fn [self] 
                    (print (string "date: " (self :learned-on) " | " (self :desc) " | filename: " (self :filename)))
                )
            }))
        )
    )

    (walk-rec path (entry-fn proc-hit))
    
    (print-hits til 
        {:title "# Learning review"
        :if-empty "No TILs this week"})
)

(defn proc-flaggable-entry [path hit spec] 
    (defn hit-data [key &opt dflt] 
        (get-in hit [:args :kv key] dflt)
    )
    (def status (hit-data (spec :status-field)))
    
    (if ((spec :show?) status)
    {
        :id (hit-data "id")
        :project (hit-data "project" nil)
        :filename (path/join (splice (array/slice (path/parts path) -3 -1)))
        :desc (string/join (get-in hit [:args :pos]) "\r\n")
        :status status
        :pp (spec :pp) 
    })
)

(defn- scan-tasks [args] 
    (def path (dyn :jd-folder))
    (def tasks @[])
    
    (defn proc-hit [path hit]
        (match (hit :type)
            "task" (array/push-not-nil tasks (proc-flaggable-entry path hit {
                :show? (fn [status] (not= status "done"))
                :status-field "status"
                :pp (fn [self] 
                    (prin (string "id: " (self :id) " | " (self :desc)))
                    (if (self :project) 
                        (prin (string " | project: " (self :project))))
                    (prin (string " | filename: " (self :filename)))
                    (print "")
                )
            }))
        )
    )
    (walk-rec path (entry-fn proc-hit))
    (print-hits tasks 
        {:title "# Tasks"
        :if-empty "No tasks are currently incomplete"}))
(defn- scan-deferred [args] 
    (def path (dyn :jd-folder))
    (def tasks @[])
    
    (defn proc-hit [path hit]
        (match (hit :type)
            "task" (array/push-not-nil tasks (proc-flaggable-entry path hit {
                :show? (fn [status] (or (= status "deferred") (= status "defer")))
                :status-field "status"
                :pp (fn [self] 
                    (prin (string "id: " (self :id) " | " (self :desc)))
                    (if (self :project) 
                        (prin (string " | project: " (self :project))))
                    (prin (string " | filename: " (self :filename)))
                    (print "")
                )
            }))
        )
    )
    (walk-rec path (entry-fn proc-hit))
    (print-hits tasks 
        {:title "# Deferred Tasks"
        :if-empty "No tasks are currently deferred"}))

(defn- scan-projects [args]
    (def path (dyn :jd-folder))
    (def projects @[])
    
    (defn proc-hit [path hit]
        (match (hit :type)
            "project" (array/push-not-nil projects (proc-flaggable-entry path hit {
                :show? (fn [status] (not= status "done"))
                :status-field "status"
                :pp (fn [self] 
                    (prin (string "id: " (self :id) " | " (self :desc)))
                    (if (self :project) 
                        (prin (string " | project: " (self :project))))
                    (prin (string " | filename: " (self :filename)))
                    (print "")
                )
            }))
        )
    )
    (walk-rec path (entry-fn proc-hit))
    (print-hits projects 
        {:title "# Projects"
        :if-empty "You don't have any projects defined"})
)

(defn- agenda [args] 
    (def path (dyn :jd-folder))
    # ; Todos are things that should be done during a given period of time
    (def todos @[])
    # ; Appointments, which Will Not Be Missed
    (def appt @[])
    
    (defn proc-hit [path hit]
        (match (hit :type)
            "todo" (array/push-not-nil todos (proc-dated-entry path hit {
                :datefield "due"
                :prefix "due"
                :pp (fn [self] 
                    (print (string "due: " (self :due-on) " | " (self :desc) " | filename: " (self :filename)))
                )
            })) 
            "appt" (array/push-not-nil appt (proc-dated-entry path hit {
                :prefix "date-of"
                :datefield "date"
                :pp (fn [self] 
                    (print (string "date: " (self :date-of-on) " | " (self :desc) " | filename: " (self :filename)))
                )
            }))
        )
    )

    (walk-rec path (entry-fn proc-hit))
    
    (print-hits appt 
        {:title "# Appointments"
        :if-empty ""})
    (print-hits todos 
        {:title "# Todos this week"
        :if-empty ""})
)


(defn- jd-areas [args] 
    (def dir (dyn :jd-folder))
    (def area-name-pat (peg/compile ~{
        :jd-dig (between 1 2 (range "09"))
        :jd-prefix (* :jd-dig "-" :jd-dig)
        :main (any (+ (* ($) :jd-prefix) 1))
    }))
    
    (defn is-area-name? [p] 
        (def m (peg/match area-name-pat p))
        (match (peg/match area-name-pat p)
            @[0] true
            _ false))
    
    (defn print-if-area [p] 
        (def dir (array/slice (path/parts p) -2 -1))
        # ; (pp ~(,dir ,(is-area-name? (dir 0))))
        (if (is-area-name? (dir 0)) 
            (print (path/basename p))
        )
    )
    (walk-rec-dirs dir print-if-area)
)

(defn print-lines [& lines] 
    (each l lines (print l))
)

(defn- string/pad-right
  "Pad a string on the right with some spaces."
  [str n]
  (def len (length str))
  (if (>= len n)
    str
    (string str (string/repeat " " (- n len))))
)

(defn- string/pad-left [str n] 
  "Pad a string on the left with some spaces."
  (def len (length str))
  (if (>= len n)
    str
    (string (string/repeat " " (- n len)) str)))

(defn- usage [] 
    (comment 
        "Commands to add:
        decimals          - Prints out a list of the individual JD topics\n
        cap(ture)         - Capture a @todo to \"$JD_FOLDER/1 Captures\"\n
        "
    )
    
    (print "anno <subcommand> args")
    (print "")
    (print "Supported subcommands:")
    (print "")
    (def max-name-len (max (splice (map (fn [c] (length (c :name))) *commands*))))
    (each cname *command-order*
        (def c (*commands* cname))
        (print (string 
            "    " 
            (string/pad-right (c :name) (+ 2 max-name-len) )
            " - "
            (c :short-desc)
        ))
    )
)

(defn- detailed-help [args] 
    (print-lines
        "Anno(tations) is a program that scans a directory laid out according to the"
        "Johnny Decimal system."
        "It scans for annotations that look like so: @todo[Get Milk|start:2021-1-4|due:2021-1-5]"
        ""
        "Annot(tations). Copyright 2020 Andrew Owen" 
    )
)

(def- ignore-text "This file tells anno to ignore this directory")
(defn- ignore [args] 
    (cond 
        (= (length args) 1) (spit (path/join (args 0) ".anno-ignore") ignore-text)
        (= (length args) 0) (spit ".anno-ignore" ignore-text)
        true (print "Please only give one arg to the ignore subcommand!")
    )
)

(defn- dir-stats [args] 
    (def stats @{})
    (defn count [p] 
        (fn [path] 
            (put stats p (+ (get stats p 0) 1)))
    )
    (each p (os/dir ".")
        (when (and (os/is-dir? p) (base-list-dir? p))
            (pp p)
            (walk-rec p (count p))
        )
    )
    
    (each (k v) (pairs stats) 
        (print (string "Directory " k " has " v " files"))
    )
    (match (length stats)
        0 (print "No subdirectories here scanned by anno"))
)

(defn main [& args] 
    
    (add-command jd-areas      "areas"      "Prints out a list of the JD areas in $JD_FOLDER" "")
    (add-command agenda        "agenda"     "Prints out @todos and @appts" "")
    (add-command scan-tasks    "tasks"      "Prints out @tasks" "")
    (add-command scan-projects "projects"   "Prints out @projects" "")
    (add-command review        "review"     "Prints out @tils")
    (add-command ignore        "ignore"     "Tells anno to ignore the current directory")
    (add-command dir-stats     "dir-stats"  "Count all of the files that are scanned by anno. Use when anno gets slow")
    
    (comptime
        (if (os/is-file? "./plugins/init.janet")
            (do 
                (import ./plugins)
                (plugins/plug)
            )
        )
    )
    
    (add-command scan-rec      "debug-dump" "Prints out a list of all recognized annotations and their data")
    (add-command detailed-help "help"       "Shows help for a given subcommand")
    
    (def args (array/concat @[] args))
    (setdyn :jd-folder (os/getenv "JD_FOLDER" "."))
    
    # ; Pop off the script name
    (array/shift args)
    # ; Do we have any arguments left?
    (match (length args) 
        0 (do
            (print "No subcommand given")
            (usage)
            (break))
        _ ()
    )
    
    # ; Get the script subcommand
    (def subcommand (array/shift args))
    
    (defn usage- [&] 
        (print (string "Unrecognized subcommand: " subcommand))
        (usage)
    )
)

(declare-project 
    :name "anno"
    :description "Read out annotations in text files"
    :dependencies ["https://github.com/janet-lang/path.git"]
    :author "Andrew Owen <yumaikas94@gmail.com>"
    :url "https://github.com/yumaikas/anno"
)

# ; (declare-native :name "strptime" :source @["strptime.cpp"])

(declare-executable
    :name "anno"
    :entry "anno-impl.janet"
)


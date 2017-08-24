bookmarks: [
    {
        group: "H1"
        filename: '.*md'
        regexp: ///^\#{1}\s{1}(.*)$///
        labelfx: (match)->
            return "#{match[1]}"
    },{
        group: "H2"
        filename: '.*md'
        regexp: ///^\#{2}\s{1}(.*)$///
        labelfx: (match)->
            return "#{match[1]}"
    },{
        group: "H3"
        filename: '.*md'
        regexp: ///^\`r\s{1}tufte\:\:newthought\(["'](.*?)['"]\)\`$///
        labelfx: (match)->
            return "#{match[1]}"
    },{
        group: "TODO"
        filename: '.*md'
        regexp: ///\<\!\-\- TODO\: (.*?) \-\-\>///
        labelfx: (match)->
            return "#{match[1]}"
    }
]

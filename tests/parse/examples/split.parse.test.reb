; %split.parse.test.reb

[
    (
        split-test5: func [series [text!] dlm [text! char?] <local> value] [
            rule: complement charset dlm
            return parse series [collect opt some [
                keep value: across some rule | <any>
            ]]
        ]
        true
    )
    (["Hello" "bright" "world!"] = split-test5 "Hello bright world!" space)
    (["Hell" "bright" "w" "rld!"] = split-test5 "Hello bright world!" " o")
]

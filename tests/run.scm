(use gumbo test)

(test-group "parse some html"
  (test '(*TOP* (html (head) (body (h1 "Test"))))
        (html->sxml "<h1>Test</h1>"))
  (test '(*TOP* (html (head) (body (div "one" (b "two" (i "three")) "four"))))
        (html->sxml "<div>one<b>two<i>three</i></b>four</div>"))
  (test '(*TOP* (html (head) (body (h1 (@ (class "foo") (id "bar")) "Test"))))
        (html->sxml "<h1 class=\"foo\" id=\"bar\">Test</h1>"))
  (test '(*TOP* (*COMMENT* " example comment ") (html (head) (body)))
        (html->sxml "<!-- example comment -->"))
  (test '(*TOP* (html (head (template (b "test"))) (body)))
        (html->sxml "<template><b>test</b></template>")))

(test-exit)

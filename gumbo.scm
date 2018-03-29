(module gumbo

;; exports
(html->sxml)

(import chicken scheme foreign)
(use srfi-1 lolevel)

(foreign-declare "#include <gumbo.h>")


(define gumbo_parse
  (foreign-lambda (c-pointer (struct GumboOutput))
      "gumbo_parse"
    c-string))

(define TAG_UNKNOWN
  (foreign-value "GUMBO_TAG_UNKNOWN" int))

(define DOCUMENT (foreign-value "GUMBO_NODE_DOCUMENT" int))
(define ELEMENT (foreign-value "GUMBO_NODE_ELEMENT" int))
(define TEXT (foreign-value "GUMBO_NODE_TEXT" int))
(define CDATA (foreign-value "GUMBO_NODE_CDATA" int))
(define COMMENT (foreign-value "GUMBO_NODE_COMMENT" int))
(define WHITESPACE (foreign-value "GUMBO_NODE_WHITESPACE" int))
(define TEMPLATE (foreign-value "GUMBO_NODE_TEMPLATE" int))

(define node-type
  (foreign-lambda* int (((c-pointer (struct GumboNode)) node))
    "C_return(((GumboNode*)node)->type);"))

(define (node->sxml node)
  (let ((type (node-type node)))
    ((cond
      ((= type DOCUMENT) document->sxml)
      ((= type ELEMENT) element->sxml)
      ((= type TEXT) node-text)
      ((= type CDATA) cdata->sxml)
      ((= type COMMENT) comment->sxml)
      ((= type WHITESPACE) node-text)
      ((= type TEMPLATE) element->sxml)
      (else
       (abort (make-property-condition
               'exn
               'message (sprintf "Unknown node type: ~S~n"
                                 (node-type node))))))
     node)))

(define (document->sxml node)
  `(*TOP* ,@(filter-map node->sxml (document-children node))))

(define (document-children node)
  (gumbo-vector->list
   ((foreign-lambda* (c-pointer (struct GumboVector))
        (((c-pointer (struct GumboNode)) node))
      "C_return(&(((GumboNode*)node)->v.document.children));")
    node)))

(define (comment->sxml node)
  `(*COMMENT* ,(node-text node)))

(define (cdata->sxml node)
  `(*CDATA* ,(node-text node)))

(define (element-children node)
  (gumbo-vector->list
   ((foreign-lambda* (c-pointer (struct GumboVector))
        (((c-pointer (struct GumboNode)) node))
      "C_return(&(((GumboNode*)node)->v.element.children));")
    node)))

(define (element-attributes node)
  (gumbo-vector->list
   ((foreign-lambda* (c-pointer (struct GumboVector))
        (((c-pointer (struct GumboNode)) node))
      "C_return(&(((GumboNode*)node)->v.element.attributes));")
    node)))

(define gumbo-vector-length
  (foreign-lambda* unsigned-int (((c-pointer (struct GumboVector)) v))
    "unsigned int length = ((GumboVector*)v)->length;
     C_return(length);"))

(define gumbo-vector-ref
  (foreign-lambda* c-pointer
      (((c-pointer (struct GumboVector)) v)
       (unsigned-int i))
    "C_return(((GumboVector*)v)->data[i]);"))

(define gumbo-attribute-name
  (foreign-lambda* c-string
      (((c-pointer (struct GumboAttribute)) attribute))
    "C_return(((GumboAttribute*)attribute)->name);"))

(define gumbo-attribute-value
  (foreign-lambda* c-string
      (((c-pointer (struct GumboAttribute)) attribute))
    "C_return(((GumboAttribute*)attribute)->value);"))

(define (attribute->sxml a)
  (list (string->symbol (gumbo-attribute-name a))
        (gumbo-attribute-value a)))

(define (gumbo-vector->list v)
  (list-tabulate (gumbo-vector-length v)
                 (cut gumbo-vector-ref v <>)))

(define gumbo-string-piece-length
  (foreign-lambda* size_t
      (((c-pointer (struct GumboStringPiece)) p))
    "C_return(((GumboStringPiece*)p)->length);"))

(define gumbo-string-piece-data
  (foreign-lambda* c-pointer
      (((c-pointer (struct GumboStringPiece)) p))
    "C_return(((GumboStringPiece*)p)->data);"))

(define (gumbo-string-piece->string p)
  (let* ((len (gumbo-string-piece-length p))
         (str (make-string len)))
    (move-memory! (gumbo-string-piece-data p)
                  (location str)
                  len)
    str))

(define element-tag
  (foreign-lambda* int (((c-pointer (struct GumboNode)) node))
    "C_return(((GumboNode*)node)->v.element.tag);"))

(define normalized-tagname
  (foreign-lambda c-string "gumbo_normalized_tagname" int))

(define tag-from-original-text
  (foreign-lambda void "gumbo_tag_from_original_text"
    c-pointer))

(define element-original-tag-string-piece
  (foreign-lambda* (c-pointer (struct GumboStringPiece))
      (((c-pointer (struct GumboNode)) node))
    "C_return(&(((GumboNode*)node)->v.element.original_tag));"))
    
(define (element-original-tagname node)
  (let ((original (element-original-tag-string-piece node)))
    (tag-from-original-text original)
    (gumbo-string-piece->string original)))

(define (tagname node)
  (let ((tag (element-tag node)))
    (string->symbol
     (if (= TAG_UNKNOWN tag)
         (element-original-tagname node)
         (normalized-tagname tag)))))

(define node-text
  (foreign-lambda* c-string (((c-pointer (struct GumboNode)) node))
    "C_return(((GumboNode*)node)->v.text.text);"))

(define (element->sxml node)
  `(,(tagname node)
    ,@(let ((attrs (element-attributes node)))
        (if (null? attrs)
            '()
            (list (cons '@ (map attribute->sxml attrs)))))
    ,@(filter-map node->sxml (element-children node))))

(define output-document
  (foreign-lambda* (c-pointer (struct GumboNode))
      (((c-pointer (struct GumboOutput)) output))
    "C_return(((GumboOutput *)output)->document);"))

(define destroy-output!
  (foreign-lambda* void
      (((c-pointer (struct GumboOutput)) output))
    "gumbo_destroy_output(&kGumboDefaultOptions, (GumboOutput*)output);"))

(define (html->sxml src)
  (let* ((output (gumbo_parse src))
         (result (handle-exceptions
                     exn (begin (destroy-output! output)
                                (abort exn))
                     (node->sxml (output-document output)))))
    (destroy-output! output)
    result))

)

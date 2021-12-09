;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |CS 2500 Homework 7 (Full)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A [List-of X] is one of:
; - '()
; - (cons X [List-of X])
; Interpretation: A list of items, where every item is an X.
; list-template : [List-of X] -> ?
(define (list-template l)
  (cond
    [(empty? l) ...]
    [(cons? l) (... (first l) ...
                    (list-template (rest l)) ...)]))

;; Exercise 2
(define-struct tweet [text likes retweets])
(define-struct facebook [text likes])
(define-struct medium [text views])

;; A Social-Media-Post is one of:
;; - (make-tweet String Natural Natural)
;; - (make-facebook String Natural)
;; - (make-medium String Natural)
;; Interpretation: A Social-Media-Post is either a
;; - (make-tweet t l rt) where t is the text of the tweet, l is the number of likes, and rt is the
;; number of retweets
;; - (make-facebook a b) where a is the text of the post and b is the number of likes
;; - (make-medium c d) where c is the text of the story and d is the number of page views

(define ex-socialmedia-1 (make-tweet "Hello" 25 3))
(define ex-socialmedia-2 (make-facebook "Goodbye" 50))
(define ex-socialmedia-3 (make-medium "Fundies" 2500))
(define ex-socialmedia-4 (make-tweet "Class of 2020" 300 46))
(define ex-socialmedia-5 (make-facebook "Northeastern" 400))
(define ex-socialmedia-6 (make-tweet "Like this post" 0 0))
(define ex-socialmedia-7 (make-facebook "I like chips" 0))
(define ex-socialmedia-8 (make-medium "Water is good for you" 0))
(define ex-socialmedia-9 (make-tweet "My laptop is on fire" 5 103))
(define ex-socialmedia-10 (make-tweet "See ya" 102 34))
(define ex-socialmedia-11 (make-tweet "I love caffeine" 5 0))
(define ex-socialmedia-12 (make-tweet "Tiktok is addicting" 0 200))
(define ex-socialmedia-13 (make-facebook "Im the King" 202000))
(define ex-socialmedia-14 (make-tweet "bai bai" 12 4))

;; socialmedia-templ : Social-Media-Post -> ???
(define (socialmedia-templ sm)
  (cond
    [(tweet? sm) (...(tweet-text sm)...
                     (tweet-likes sm) ...
                     (tweet-retweets sm)...)]
    [(facebook? sm) (...(facebook-text sm) ...
                        (facebook-likes sm) ...
                        (facebook-views sm) ...)]
    [(medium? sm) (...(medium-text sm) ...
                      (medium-views sm)...)]))

;; List-of Social-Media-Post Examples
(define social-media-list1 (list ex-socialmedia-1 ex-socialmedia-2 ex-socialmedia-3))
(define social-media-list2 (list ex-socialmedia-4 ex-socialmedia-5 ex-socialmedia-6))
(define social-media-list3 (list ex-socialmedia-7 ex-socialmedia-8 ex-socialmedia-9))
(define social-media-list4 (list ex-socialmedia-1 ex-socialmedia-2 ex-socialmedia-3 ex-socialmedia-4
                                 ex-socialmedia-5 ex-socialmedia-6 ex-socialmedia-7 ex-socialmedia-8
                                 ex-socialmedia-9 ex-socialmedia-10))
(define social-media-list5 (list ex-socialmedia-6 ex-socialmedia-8))
(define social-media-list6 (list ex-socialmedia-1 ex-socialmedia-11))
(define social-media-list7 (list ex-socialmedia-2 ex-socialmedia-12))
(define social-media-list8 (list ex-socialmedia-7 ex-socialmedia-11 ex-socialmedia-12))
(define social-media-list9 (list ex-socialmedia-5 ex-socialmedia-7 ex-socialmedia-8))
(define social-media-list10 (list ex-socialmedia-7 ex-socialmedia-8 ex-socialmedia-9
                                  ex-socialmedia-12 ex-socialmedia-13 ex-socialmedia-14))
                                 
;; Exercise 3
;; Social-Media-Post --> Boolean
;; Takes in a Social-Media-Post and returns a boolean if the number of retweets of the post >= 1, if
;; not returns #false

(check-expect (at-least-retweet ex-socialmedia-9) #t)
(check-expect (at-least-retweet ex-socialmedia-10) #t)
(check-expect (at-least-retweet ex-socialmedia-6) #f)
(check-expect (at-least-retweet ex-socialmedia-8) #f)
 
(define (at-least-retweet rt)
  (cond
    [(tweet? rt) (if (>= (tweet-retweets rt) 1) #t #f)]
    [(facebook? rt) #f]
    [(medium? rt) #f]))

;; List-of SocialMediaPosts --> List-of SocialMediaPosts
;; Filters out the list items that return #f when at-least-retweet is ran
(check-expect (retweet-check social-media-list1) (list (make-tweet "Hello" 25 3)))
(check-expect (retweet-check social-media-list5) empty)
(check-expect (retweet-check social-media-list4)
              (list
               (make-tweet "Hello" 25 3)
               (make-tweet "Class of 2020" 300 46)
               (make-tweet "My laptop is on fire" 5 103)
               (make-tweet "See ya" 102 34)))
(define (retweet-check s)
  (filter at-least-retweet s))

;; Exercise 4
;; post-failure : List-of Social-Media-Posts -> List-of Social-Media-Posts
;;  Filters out the social media posts that return false when failure? is run

(check-expect (post-failure social-media-list1) empty)
(check-expect (post-failure social-media-list2) (list ex-socialmedia-6))
(check-expect (post-failure social-media-list4) (list ex-socialmedia-6
                                                      ex-socialmedia-7
                                                      ex-socialmedia-8))

(define (post-failure l)
  (filter failure? l))

;; failure? : Social-Media-Post -> Boolean
;; Takes a social media post and produces only the items that have never been
;; viewed, shared, liked, or retweeted

(check-expect (failure? ex-socialmedia-1) #f)
(check-expect (failure? ex-socialmedia-2) #f)
(check-expect (failure? ex-socialmedia-3) #f)
(check-expect (failure? ex-socialmedia-8) #t)
(check-expect (failure? ex-socialmedia-7) #t)
(check-expect (failure? ex-socialmedia-11) #t)
(check-expect (failure? ex-socialmedia-12) #t)

(define (failure? sm)
  (cond
    [(tweet? sm) (if (or (= 0 (tweet-likes sm)) (= 0 (tweet-retweets sm))) #t #f)]
    [(facebook? sm) (if (= 0 (facebook-likes sm)) #t #f)]
    [(medium? sm) (if (= 0 (medium-views sm)) #t #f)]))

;; Exercise 5
;; add-posts (X, Y) X Y --> Number
;; Checks the data type of the x input, and if it is a;
;; Empty returns 0,
;; Number retuns the sum of x and 7,
;; Tweet, returns the sum of tweet-likes of x, tweet-retweets of x, and 7,
;; Facebook returns the sum of facebook-likes of x and y,
;; Medium returns the sum of medium-views of x and y.

(check-expect (add-posts (make-tweet "See ya" 102 34) 108) 244)
(check-expect (add-posts ex-socialmedia-7 30) 30)
(check-expect (add-posts 20 30) 50)
(check-expect (add-posts '() 80) 0)

(define (add-posts x y)
  (cond
    [(empty? x) 0]
    [(number? x) (+ x y)]
    [(tweet? x) (+ (tweet-likes x) (tweet-retweets x) y)]
    [(facebook? x) (+ (facebook-likes x) y)]
    [(medium? x) (+ (medium-views x) y)]))

;; List-of SocialMediaPosts --> Number
;; Uses add-posts on the given list and returns the sum of all SocialMediaPost tweet-likes,
;; tweet-retweets, facebook-likes, medium-views

(check-expect (sum-of-interaction social-media-list5) 0)
(check-expect (sum-of-interaction social-media-list1) 2578)
(check-expect (sum-of-interaction social-media-list4) 3568)

(define (sum-of-interaction s)
  (foldr add-posts 0 s))

;; Exercise 6
;; crosspost : List-of Social-Media-Post -> List-of SocialMedia-Post
;; consumes a list of social media items, and produces a list that has two posts on the two other
;; platforms, allowing clients to "cross post" anything they share

(check-expect (crosspost social-media-list1) (list
                                              (make-facebook "Hello" 0)
                                              (make-medium "Hello" 0)
                                              (make-tweet "Goodbye" 0 0)
                                              (make-medium "Goodbye" 0)
                                              (make-tweet "Fundies" 0 0)
                                              (make-facebook "Fundies" 0)))                                            
(check-expect (crosspost social-media-list2) (list
                                              (make-facebook "Class of 2020" 0)
                                              (make-medium "Class of 2020" 0)
                                              (make-tweet "Northeastern" 0 0)
                                              (make-medium "Northeastern" 0)
                                              (make-facebook "Like this post" 0)
                                              (make-medium "Like this post" 0)))                                              
(check-expect (crosspost social-media-list8) (list
                                              (make-tweet "I like chips" 0 0)
                                              (make-medium "I like chips" 0)
                                              (make-facebook "I love caffeine" 0)
                                              (make-medium "I love caffeine" 0)
                                              (make-facebook "Tiktok is addicting" 0)
                                              (make-medium "Tiktok is addicting" 0)))
(check-expect (crosspost '()) empty)

(define (crosspost l)
  (cond
    [(empty? l) empty]
    [(cons? l)
     (append (create-posts (first l))
             (crosspost (rest l)))]))
             
;; create-posts : Social-Media-Post -> List-of Social-Media-Post
;; Takes a social media post and if it is a: 
;; - tweet, produces a facebook with the same text and zero likes and a medium with the same text and
;; zero page views
;; - facebook, produces a tweet with the same text (limited to 280 characters) and zero retweets and
;; a medium with the same text and zero page views
;; - medium, produces a tweet with the same text (limited to 280 characters) and zero retweets and
;; a facebook with the same text and zero likes

(check-expect (create-posts ex-socialmedia-1) (list
                                               (make-facebook "Hello" 0)
                                               (make-medium "Hello" 0)))
(check-expect (create-posts ex-socialmedia-2) (list
                                               (make-tweet "Goodbye" 0 0)
                                               (make-medium "Goodbye" 0)))
(check-expect (create-posts ex-socialmedia-3) (list
                                               (make-tweet "Fundies" 0 0)
                                               (make-facebook "Fundies" 0)))

(define (create-posts sm)
  (cond
    [(tweet? sm) (list (make-facebook (tweet-text sm) 0)
                       (make-medium (tweet-text sm) 0))]
    [(facebook? sm) (list (make-tweet (facebook-text sm) 0 0)
                          (make-medium (facebook-text sm) 0))]
    [(medium? sm) (list (make-tweet (medium-text sm) 0 0)
                        (make-facebook (medium-text sm) 0))]))

;; Exercise 7
;; append-apply-to-all [X, Y] Y X --> List
;; If the given x is a list, perform the give function y on the list x and create a singular list
;; with the result

(check-expect (append-apply-to-all string->list (list "Here")) (list #\H #\e #\r #\e))
(check-expect (append-apply-to-all string->list empty) empty)
(check-expect (append-apply-to-all string->list (list "I")) (list #\I))

(define (append-apply-to-all f l)
  (cond
    [(empty? l) empty]
    [(cons? l) (append (f (first l)) (append-apply-to-all f (rest l)))]))

;; Exercise 8
;; crosspost/v2 :  List-of Social-Media-Post -> List-of SocialMedia-Post
;; consumes a list of social media items, and produces a list that has two posts on the two other
;; platforms, allowing clients to "cross post" anything they share

(check-expect (crosspost/v2 social-media-list1) (list
                                                 (make-facebook "Hello" 0)
                                                 (make-medium "Hello" 0)
                                                 (make-tweet "Goodbye" 0 0)
                                                 (make-medium "Goodbye" 0)
                                                 (make-tweet "Fundies" 0 0)
                                                 (make-facebook "Fundies" 0)))
                                             
(check-expect (crosspost/v2 social-media-list2) (list
                                                 (make-facebook "Class of 2020" 0)
                                                 (make-medium "Class of 2020" 0)
                                                 (make-tweet "Northeastern" 0 0)
                                                 (make-medium "Northeastern" 0)
                                                 (make-facebook "Like this post" 0)
                                                 (make-medium "Like this post" 0)))
                                              
(check-expect (crosspost/v2 social-media-list8) (list
                                                 (make-tweet "I like chips" 0 0)
                                                 (make-medium "I like chips" 0)
                                                 (make-facebook "I love caffeine" 0)
                                                 (make-medium "I love caffeine" 0)
                                                 (make-facebook "Tiktok is addicting" 0)
                                                 (make-medium "Tiktok is addicting" 0)))
(define (crosspost/v2 l)
  (append-apply-to-all create-posts l))

;; Exercise 9
;; items-since-tweet: (X,Y) --> X.  List-of SocialMediaPosts and String --> List-of SocialMediaPosts
;; Takes in a list of SocialMediaPosts and returns a list of all the SocialMediaPosts that come after
;; the tweet that includes the given String

(check-expect (items-since-tweet social-media-list4 "Like this post")
              (list
               (make-tweet "Like this post" 0 0)
               (make-facebook "I like chips" 0)
               (make-medium "Water is good for you" 0)
               (make-tweet "My laptop is on fire" 5 103)
               (make-tweet "See ya" 102 34)))
(check-expect (items-since-tweet social-media-list9 "Waterrrrr") '())
(check-expect (items-since-tweet
               social-media-list6 "I love caffeine") (list (make-tweet "I love caffeine" 5 0)))
(check-expect (items-since-tweet '() "Hello World") empty)

(define (items-since-tweet sm s)
  (cond
    [(empty? sm) empty]
    [(tweet? (first sm)) (if (string=? (tweet-text (first sm)) s) sm
                             (items-since-tweet (rest sm) s))]
    [(facebook? (first sm)) (items-since-tweet (rest sm) s)]
    [(medium? (first sm)) (items-since-tweet (rest sm) s)]))

;; Exercise 10

;; List-of SocialMediaPosts --> List-of SocialMediaPosts
;; Takes in a list of SocialMediaPosts and returns a list of all SocialMediaPosts that come after
;; any facebook post in the list that has >= 10 likes, including the facebook post

(check-expect (items-since-10-likes social-media-list10)
              (list (make-facebook "Im the King" 202000) (make-tweet "bai bai" 12 4)))
(check-expect (items-since-10-likes social-media-list6) '())
(check-expect (items-since-10-likes social-media-list4) (list
                                                         (make-facebook "Goodbye" 50)
                                                         (make-medium "Fundies" 2500)
                                                         (make-tweet "Class of 2020" 300 46)
                                                         (make-facebook "Northeastern" 400)
                                                         (make-tweet "Like this post" 0 0)
                                                         (make-facebook "I like chips" 0)
                                                         (make-medium "Water is good for you" 0)
                                                         (make-tweet "My laptop is on fire" 5 103)
                                                         (make-tweet "See ya" 102 34)))
(check-expect (items-since-10-likes '()) empty)

(define (items-since-10-likes sm)
  (cond
    [(empty? sm) empty]
    [(tweet? (first sm)) (items-since-10-likes (rest sm))]
    [(facebook? (first sm)) (if (10likes? (first sm)) sm (items-since-10-likes (rest sm)))]
    [(medium? (first sm)) (items-since-10-likes (rest sm))]))

;; Helper Function
;; 10likes? : Social-Media-Post -> Boolean
;; Determines if the number of facebook likes is greater than or equal to 10

(check-expect (10likes? (make-facebook "I like sushi!" 10)) #t)
(check-expect (10likes? (make-facebook "Winnie the Pooh" 15)) #t)
(check-expect (10likes? (make-facebook "Save the Earth" 5)) #F)
(check-expect (10likes? (make-medium "Vote!!" 20)) #f)

(define (10likes? lk)
  (cond
    [(facebook? lk)(if (>= (facebook-likes lk) 10) #t #f)]
    [else #f]))

;; Exercise 11

;; suffix-from-2500 : List-of Numbers -> List-of Numbers
;; consumes a list of numbers, and produces the suffix of that list that begins from the first 2500
;; that occurs in the given list. The produced list must include the first 2500.
(check-expect (suffix-from-2500 (list 3 25 2500 54 0 0.5 108 2)) (list 2500 54 0 0.5 108 2))
(check-expect (suffix-from-2500 (list 2500 100 -39 475 9 75)) (list 2500 100 -39 475 9 75))
(check-expect (suffix-from-2500 (list 32 346 9482 0.5 2500)) (list 2500))
(check-expect (suffix-from-2500 (list 75 80 92)) empty)
(check-expect (suffix-from-2500 '()) empty)

(define (suffix-from-2500 l)
  (cond
    [(empty? l) empty]
    [(cons? l) (if (number2500? (first l)) l (suffix-from-2500 (rest l)))]))

;; Helper Function
;; number2500? : Number -> Number
;; consumes a number and determines if it's equal to 2500

(check-expect (number2500? 2500) #t)
(check-expect (number2500? 5.5) #f)
(check-expect (number2500? -10) #f)
(check-expect (number2500? 0) #f)

(define (number2500? n)
  (if (= n 2500) #t #f))

;; Exercise 12
;; [X -> Boolean] [List-of X] -> [List-of X]
;; Consumes a list of X and generates a list of only the element that satisfies the operation and
;; all of the elements that follow it
(define (items-since op l)
  (cond
    [(empty? l) empty]
    [(cons? l) (if (op (first l)) l (items-since op (rest l)))]))

;; Exercise 13
;; suffix-from-2500/v2. : X -> X   List-of Numbers -> List-of Numbers
;; consumes a list of numbers, and produces the suffix of that list that begins from the first 2500
;; that occurs in the given list. The produced list must include the first 2500.
(check-expect (suffix-from-2500/v2. (list 2500 100 -39 475 9 75)) (list 2500 100 -39 475 9 75))
(check-expect (suffix-from-2500/v2. (list 32 346 9482 0.5 2500)) (list 2500))
(check-expect (suffix-from-2500/v2. (list 75 80 92)) empty)
(define (suffix-from-2500/v2. l)
  (items-since number2500? l))

;; items-since-10-likes/v2. : X -> X   List-of SocialMediaPosts --> List-of SocialMediaPosts
;; Takes in a list of Social-Media-Post and returns a list of all Social-Media-Post that come after
;; any facebook post in the list that has >= 10 likes, including the facebook post
(check-expect (items-since-10-likes/v2. social-media-list10)
              (list (make-facebook "Im the King" 202000) (make-tweet "bai bai" 12 4)))
(check-expect (items-since-10-likes/v2. social-media-list4)
              (list
               (make-facebook "Goodbye" 50)
               (make-medium "Fundies" 2500)
               (make-tweet "Class of 2020" 300 46)
               (make-facebook "Northeastern" 400)
               (make-tweet "Like this post" 0 0)
               (make-facebook "I like chips" 0)
               (make-medium "Water is good for you" 0)
               (make-tweet "My laptop is on fire" 5 103)
               (make-tweet "See ya" 102 34)))
(check-expect (items-since-10-likes/v2. social-media-list6) '())

(define (items-since-10-likes/v2. l)
  (items-since 10likes? l))

;; item-since-tweet/v2. : (X,Y) --> X
;; List-of Social-Media-Post and String --> List-of Social-Media-Post
;; Takes in a list of SocialMediaPosts and returns a list of all the SocialMediaPosts that come after
;; the tweet that includes the given String
(check-expect (items-since-tweet/v2. social-media-list4 "Like this post")
              (list
               (make-tweet "Like this post" 0 0)
               (make-facebook "I like chips" 0)
               (make-medium "Water is good for you" 0)
               (make-tweet "My laptop is on fire" 5 103)
               (make-tweet "See ya" 102 34)))
(check-expect (items-since-tweet/v2. social-media-list9 "Waterrrrr") '())
(check-expect (items-since-tweet/v2.
               social-media-list6 "I love caffeine") (list (make-tweet "I love caffeine" 5 0)))

(define (items-since-tweet/v2. sm string)
  (local [;; same? : String -> Boolean
          ;; Determines if the given string matches the text of the tweet
          (define (same? s)
            (cond
              [(tweet? s)(if (string=? (tweet-text s) string) #t #f)]
              [else #f]))]
    (items-since same? sm)))
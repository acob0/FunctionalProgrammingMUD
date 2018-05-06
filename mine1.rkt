#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;Defining which objects are in the game
(define objects '((1 "a silver dagger")
                  (2 "a gold coin")))

;Defining different descriptions for rooms
(define descriptions '((0 "You are in a cave")
                       (1 "You are in the lobby")
                       (2 "You are in the hallway")
                       (3 "You are in a swamp")))

;Defining all the actions and various ways they can be called
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define help '(((help) help) ((what) help)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define actions `(,@look ,@quit ,@help ,@inventory))

;Defining the layout of the dungeon by defining which directions can be taken in each room
(define decisiontable `((1 ((north) 2) ,@actions)
                        (2 ((north) 3) ((south) 1),@actions)
                        (3 ((north) 4) ((east) 12) ((south) 2) ((west) 7),@actions)
                        (4 ((north) 5) ((south) 3),@actions)
                        (5 ((north) 6) ((south) 4) ((west) 11) ,@actions)
                        (6 ((south) 5) ,@actions)
                        (7 ((east) 3) ((west) 8) ,@actions)
                        (8 ((north) 9) ((east) 7) ,@actions)
                        (9 ((north) 10) ((south) 8) ,@actions)
                        (10 ((east) 11) ((south) 9) ,@actions)
                        (11 ((east) 5) ((west) 10) ,@actions)
                        (12 ((east) 13) ((west) 3) ,@actions)
                        (13 ((north) 15) ((south) 14) ((west) 12) ,@actions)
                        (14 ((west) 13) ,@actions)
                        (15 ((south) 13) ,@actions)))

;Defining a function to create a single string from a list
(define (slist->string l)
  (string-join (map symbol->string l)));                                                   Creates a string from each element of the list and joins them into one string

;Defining a function to return the possible directions for a given room
(define (get-directions id)
  (let ((record (assq id decisiontable)));                                                 Sets record to be the list starting with the id in the decision table
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)));               Removes the first element of record and if there are any numbers in that list it returns those and saves it as result
           (n (length result)));                                                           Sets n to be the length of the result list
      (cond ((= 0 n);                                                                      Checks if n is 0
             (printf "You appear to have entered a room with no exits.\n"));               #t Tells you there are no exits
            ((= 1 n);                                                                      Checks if n is 1
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))));   #t Tells you where the single exit is by getting the first element of the first element in the result list
            (else;                                                                         If n is neither 0 or 1
             (let* ((losym (map (lambda (x) (car x)) result));                             Set losym to be the first element of each list contained in result
                    (lostr (map (lambda (x) (slist->string x)) losym)));                   Set lostr to be losym converted to strings
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))));Print all the possible exits by showing each element in lostr and joining those with the string and

;Defining a function that takes a list of lists and returns the last element
;from the list starting with id
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)));                                                               Uses eq? to find the list starting with id and removes the first element, which will be the id

;Defining a function that takes a list of lists and returns the last element
;from the list starting with id, same as above but uses eqv? instead of eq?
;to find the list
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

;Defining a function to randomly return a description for a room
(define (get-response id)
  (car (assq-ref descriptions (random 3))));                                               Uses assq-ref and random to return a random description

;Defining a function to return all the possible actions that can be taken in
;a given room
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)));                                               Sets keys to be the list returned from the assq-ref function
    (map (lambda (key) (car key)) keys)));                                                 Returns the first element of each  key


;; outputs a list in the form: (0 0 0 1 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

;Defining a function that returns the index of the largest number in a list
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))));                                               Sorts the given list in descending order and stores the first element as n
    (if (zero? n);                                                                         Checks if n is 0
      #f;                                                                                  #t Return false
      (list-index (lambda (x) (eq? x n)) list-of-numbers))));                              #f Returns the index of the leftmost element of the given list that equals n

;Defining a function that returns either an id of a room if a direction is given,
;an action to take or false if what the user requested is not possible in that room
(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id));                                             Sets record to be the last elements of the list in the decisiontable starting with id
         (keylist (get-keywords id));                                                      Sets keylist to be a list of keyword obtained with the get-keywords function
         (index (index-of-largest-number (list-of-lengths keylist tokens))));              Sets index to be the index of the largest number in the list
    (if index ;                                                                            Checks whether index is false, any value that isn't #f will evaluate to true
      (cadr (list-ref record index));                                                      #t Gets the list at the given index and returns the last element of the first element
      #f)))

;Defining a function that starts the game and handles the users input
(define (startgame initial-id)
  (let loop ((id initial-id) (description #t));                                            Sets loop to be a function that takes and id and description, initially the id is the one given and description is true
    (if description;                                                                       Checks if description is true
        (printf "~a\n> " (get-response id));                                               #t Prints the room description for the given room
        (printf "> "));                                                                    #f Prints >
    (let* ((input (read-line));                                                            Sets the users input to input
           (string-tokens (string-tokenize input));                                        Creates a list of substrings from the users input
           (tokens (map string->symbol string-tokens)));                                   Creates a list of strings from the substrings
      (let ((response (lookup id tokens)));                                                Sets response to be the string that is returned from lookup
        (cond ((number? response);                                                         Check if the response is a number, if it is loop again with the response
               (loop response #t))
              ((eq? #f response);                                                          If the response is false it is not a valid instruction so let the player know and loop again
               (format #t "huh? I didn't understand that!\n")
               (loop id #f));                                                              
              ((eq? response 'look);                                                       If the response equals look, show the possible directions and loop again
               (get-directions id)
               (loop id #f))
              ((eq? response 'help);                                                       If the response is drop drop the item
               (printf "~a\n" (map (lambda (x) slist->string(car x)) (get-keywords id)))
               (loop id #f))
              ((eq? response 'quit);                                                       If the response equals quit, exit the game
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

(startgame 1)

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

;Defining the hashes the will hold objects and your inventory
(define  objectdb (make-hash ))
(define  inventorydb (make-hash ))

;Defining the function to add an object to a hash
(define (add-object  db id  object)
  (if (hash-has-key? db id);                                                               Checks if the hash contains the given key
      (let (( record (hash-ref  db id)));                                                  #t Returns the value for key in hash and binds it to the variable record
        (hash-set! db id (cons  object  record )));                                        #t Joins object and record and maps id to that in hash, overwriting any existing mapping for id
      (hash-set! db id (cons  object  empty ))));                                          #f Joins object and an empty list and maps id to that in hash, overwriting any existing mapping for id

;Defining the function to add the list of objects to a hash 
(define (add-objects  db)
  (for-each;                                                                               Applies the following annonymous function to each element of the objects list
   (lambda (r);                                                                            Create an annonymous function that takes r as its argument
     (add-object  db (first r) (second r))) objects));                                     Use the add-object function from above to add items to a hash where the first element of the list is the key and the second the value

;Defining a function to display which objects are in a given hash
(define (display-objects db id)
  (cond ((hash-has-key? db id);                                                            Check whether the hash contains the given key
    (let* (( record (hash-ref  db id));                                                    #t Returns the value for key in hash and binds it to the variable record
           (output (string-join  record " and ")));                                        #t Creates a string with the elements of record seperated by the word and, then sets this to the output variable
      (cond ((not (equal? output ""));                                                     #t Checks that the output isn't empty
        (if (eq? id 'bag);                                                                 #t #t Checks if the output equals bag
            (printf "You  are  carrying ~a.\n" output);                                    #t #t #t Print what you are carring in your bag
            (printf "You  can  see ~a.\n" output))))));                                    #t #t #f Print what there is in the room
        (#t (if (eq? id 'bag);                                                             #f If the key can't be found in the given hash and the id equals bag
            (printf "Your bag is empty.\n");                                               #f #t Print that your bag is empty
            (printf "The room is empty.\n")))));                                           #f #f Print that the room is empty

;Defining a function that either returns the id or bag
(define (check a b id)
  (cond ((equal? a b) 'bag);                                                               If a and b are equal return 'bag
        (#t id)));                                                                         Else return the id

;Defining a function that removes an item from one has and adds it to the other
(define (remove-object db id from input)
  (let*((str (string-join (cdr (string-split input))));                                    Splits the input into seperate strings, removes the first string, then joins it back into one string and stores it as str
        (newid (check from 'bag id)));                                                     Checks whether from equals 'bag and stores the result ('bag or id) as newid
    (when (hash-has-key? db newid);                                                        Checks whether the hash given contains the key created
      (let* ((record (hash-ref db newid));                                                 #t Sets record to be the value paired with the created key
                 (result (remove (lambda (x) (string-suffix-ci? str x)) record));          #t Checks whether str apears anywhere in x (case insensitive) and removes the first instance of it from record and binds that to result
                 (item (lset-difference equal? record result)));                           #t Gets all the elements that are in record but not in result and stores that as item
        (cond ((null? item);                                                               #t Checks whether the item is null
               (printf "I don't see that item in the ~a! \n" from));                       #t #t If the item is null print that it can't find the item
              (else
               (cond((eq? from 'room);                                                     #t #f Checks whether the item is from the room
                     (printf "Added ~a to your bag.\n" (first item));                      #t #f #t Tells you it added the item to your bag
                     (add-object inventorydb 'bag (first item));                           #t #f #t Adds the item to the inventory hash
                     (hash-set! db id result));                                            #t #f #t Maps id to result in the given hash, overwriting any existing record
                    (else
                     (printf "Removed ~a from your bag . \n" (first item));                #t #f #f Tells you it removed the item to your bag
                     (add-object objectdb id (first item));                                #t #f #f Adds the item to the object hash
                     (hash-set! db 'bag result)))))))));                                   #t #f #t Maps 'bag to result in the given hash, overwriting any existing record

;Defining a function to remove an item from the objectdb hash
(define (pick-item  id  input)
  (let ((item (string-join (cdr (string-split  input )))));                                Splits the input up into individual strings then removes the first string and joins it back together into one string and stores it as item
    (remove-object objectdb id 'room item )));                                             Then uses the remove-object function to remove the item from the objectdb hash and add it to the inventorydb hash

;Defining a function to add an item to your inventory
(define (put-item  id  input)
  (let ((item (string-join (cdr (string-split  input )))));                                Splits the input up into individual strings then removes the first string and joins it back together into one string and stores it as item
    (remove-object inventorydb id 'bag item )));                                           Then uses the remove-object function to remove the item from the inventorydb hash and add it to the objectdb hash

;Defining a function that calls display-objects for the inventory
(define (display-inventory)
  (display-objects  inventorydb 'bag))

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
              ((eq? response 'inventory);                                                  If the response is inventory, show the inventory and loop again
               (display-objects inventorydb 'bag)
               (loop id #f))
              ((eq? response 'pick);                                                       If the response is pick pick up the item
               (remove-object objectdb id 'bag input)
               (loop id #f))
              ((eq? response 'drop);                                                       If the response is drop drop the item
               (remove-object inventorydb id 'room input)
               (loop id #f))
              ((eq? response 'help);                                                       If the response is drop drop the item
               (printf "~a\n" (map (lambda (x) slist->string(car x)) (get-keywords id)))
               (loop id #f))
              ((eq? response 'quit);                                                       If the response equals quit, exit the game
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

(startgame 1)

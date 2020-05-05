; TODO ex 3.39
;
; I think it's still correct.

; ex 3.40
;
; 100
; 1000
; 10000
; 100000
; 1000000
;
; Only 1000000 remains.

; ex 3.41
;
; No. Concurrency is not a problem if there's only reading. Though the exchange
; procedure in the following section might be a problem.

; TODO ex 3.42

; TODO ex 3.43
;
; If run concurrently without serialization, it's possible to get wrong result.
; For example, a: 10, b: 20, c: 30. First calculate the difference between a
; and b which is -10. Before adding the difference back, swap a and c. Now a is
; 30, c is 10. Add the difference -10 to a and b. a becomes 40 and b becomes
; 10.
;
; The reason why the total amount is preserved is because even though the
; exchange is not serialized, the deposit and withdraw on each account is
; serialized. So changes of the balance will not interleave.

; ex 3.44
;
; I don't think so. The difference between exchange and transfer is that
; exchange reads the balance twice, one for calculating the difference (not
; serialized), one for setting the new balance, whereas transfer is just a
; sequence of actions on different accounts. Extra care needs to be taken to
; make exchange consistent; transfer only needs the account to be serialized.

; TODO ex 3.45

; TODO ex 3.46

; TODO ex 3.47

; TODO ex 3.48

; TODO ex 3.49

; LOADER.CTL VERSION NUMBER %2(40)
;
; THIS IS A DUMMY LOADER CONTROL FILE IN THAT IT HAS BEEN DECIDED
; TO CREATE THE NEW VERSIONS OF LOADER AND MACRO TOGETHER, THUS
; MAKING IT IMPERATIVE THAT THE USER'S AREA HAVE BOTH A
; LOADER.MAC AND A MACRO.MAC ON HIS AREA IN ORDER TO COMPILE
; EITHER.

.PLEASE LOADER CONTROL FILE SPOOLING MACRO CONTROL FILE
.QUEUE I:=MACRO/REST:1/TIME:2::/UNIQ:0
.IF (ERROR) .PLEASE SUBMISSION FAILURE FOR MACRO CONTROL FILE
.KJOB/F
 
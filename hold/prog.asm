R1      EQU     1
R2      EQU     2
R3      EQU     3
R4      EQU     4
R12     EQU     12
R13     EQU     13
R14     EQU     14
R15     EQU     15
        USING   *,10
        STM R14,R12,12(R13)
        BALR    R10,0
        LA      R3,SVA
        ST      3,8(13)
        ST      13,4(3)
        LR      13,3
        LA      4,PARMS
        LA      3,4
        L       2,0(1)
        LH      1,0(2)
        LTR     R1,R1
        BCR     2,OPEN
LOOP    CLC     2(2,2),0(4)
        BCR     4,OPEN
        LA      4,2(4)
        BCT     3,LOOP
        LA      4,PARMS
OPEN    SIO     5
PARMS   L       1,1
SVA     L       1,1
        END
   
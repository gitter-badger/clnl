(in-package #:cl-nl-test)

(defsimplecommandtest "Nothing" "" "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")
(defsimplecommandtest "Simple crt" "crt 1" "2F08B31AC06C9D5339E6B3E953C2B4B71FDB9CDE")
(defsimplecommandtest "Simple crt 2" "crt 5" "9FE588C2749CD9CE66CB0EA451EFB80476E881FB")
(defsimplecommandtest "Simple crt and fd" "crt 5 ask turtles [ fd 1 ]" "BEB43404EDC7852985A9A7FC312481785FE553A0")
(defsimplecommandtest "Simple crt and fd random" "crt 5 ask turtles [ fd random-float 1 ]" "F7AC3B3492CDFD01D1FB5BD69FAAA67E06D3A870")
;(defsimplecommandtest "Simple crt and fd random 2" "crt 30 ask turtles [ fd random-float 1 ]" "F7AC3B3492CDFD01D1FB5BD69FAAA67E06D3A870") ; we start getting floating errors, cool!

(defsimplereportertest "Random 1" "random-float 5" "4.244088516651127" "17D1BF7FF7BF2C7F3F5F7DD7CF67CFF2772CFFFC")

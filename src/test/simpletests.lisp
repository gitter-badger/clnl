(in-package #:cl-nl-test)

(defsimplecommandtest "Nothing" "" "E1DE30F072D785E0D0B59F28B0F7853E3D3E0D8B")
(defsimplecommandtest "Simple crt" "crt 1" "2F08B31AC06C9D5339E6B3E953C2B4B71FDB9CDE")
(defsimplecommandtest "Simple crt 2" "crt 5" "9FE588C2749CD9CE66CB0EA451EFB80476E881FB")
(defsimplecommandtest "Simple crt and fd" "crt 5 ask turtles [ fd 1 ]" "BEB43404EDC7852985A9A7FC312481785FE553A0")

(defsimplereportertest "Random 1" "random-float 5" "4.244088516651127" "17D1BF7FF7BF2C7F3F5F7DD7CF67CFF2772CFFFC")

readfile =: 1!:1
writefile =: 1!:2
fn =. < 'day-01.input'
data =: >0 ". each cutopen toJ readfile fn

fuelreq =: (-&2)@:<.@:(%&3)

(+/ fuelreq data) writefile (2)
exit''

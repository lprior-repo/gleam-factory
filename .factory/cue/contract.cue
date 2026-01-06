// TCR AI Contract - Machine readable schemas for runtime validation
// Usage: echo '{"s":"tw",...}' | cue vet contract.cue -d '#A' -
package tcr

// Auditor output
#A: {s:"tw",t:{n:=~"_test$",f:=~"^test/",b:!=""},r?:string} | {s:"rc"} | {s:"e",r:string}

// Implementer output
#I: {s:"ok",c:[...{f:=~"^src/",a:"+"|"~",l:>0}]&[_,...],r?:string} | {s:"bl",b:!=""} | {s:"e",r:string}

// Architect output
#R: {s:"rf",lb:>=0,la:>=0&<=lb,c:[...{t:"d"|"s"|"x"|"r"|"i",w:!=""}]&[_,...],sm?:[...{t:!="",d:!="",p:0|1|2|3}]} | {s:"nc"} | {s:"e",r:string}

// Reviewer output
#V: {s:"p",m:true,v:[...{c:!="",m:bool,e:string}]&[_,...],l?:string} | {s:"f",m:false,v:[...{c:!="",m:bool,e:string}]&[_,...],l?:string}

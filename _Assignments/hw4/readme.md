```ocaml
parse "
 let p = {ssn = 10; age <= 30}
in begin
 p.age <= 31;
p
 end";;

```

---

expected O/P

```txt
expr =
Let ("p", Record [("ssn", (false , Int 10)); ("age", (true , Int 30))],
BeginEnd [SetField (Var "p", "age", Int 31); Var "p"])
```

---

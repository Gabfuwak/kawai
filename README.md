# Fonctionnalités obligatoires

|                      | kawalexer | kawaparser | typechecker | interpreter |
| -------------------- | --------- | ---------- | ----------- | ----------- |
| Arithmétique         | OK        | OK         | OK          | OK          |
| Variables            | OK        | OK         | OK          | OK          |
| Instructions         | OK        | OK         | OK          | OK          |
| Classes et attributs | OK        | OK         | OK          | OK          |
| Méthodes             | OK        | OK         | OK          | OK          |
| Héritage             | OK        | OK         | OK          | OK          |


# Extensions  
## 1 - Print accepte tous les types implémentés    
Trivialement on ajoute juste des cas dans les match du typechecker et de l'interpreteur  
## 2 - else if avec possibilité de s'enchainer 
On permet a la grammaire d'accepter (if (...) {} else if (...) {} else if (...) {})  

On ajoute une partie "else block" recursive dans la grammaire qui permet d'ajouter autant de block else if qu'on veut:

```ocaml
instr:
[...]
| IF LPAR e=expression RPAR BEGIN instrlist=list(instr) END elseblock=option(else_block) 
  {
    match elseblock with
    | None ->    If(e, instrlist, [])
    | Some block ->  If(e, instrlist, block) 
  }
[...]
;


else_block: (*renvoie une liste d'instructions*)
| ELSE BEGIN instrlist=list(instr) END { instrlist }
| ELSE IF LPAR e=expression RPAR BEGIN instrlist=list(instr) END elseblock=option(else_block)
  { match elseblock with
    | None ->    [If(e, instrlist, [])]
    | Some block ->  [If(e, instrlist, block)] }
;
```


## 3 - Déclarations simplifiées
On modifie uniquement la grammaire. 

On crée le type typed_def
```ocaml
type typed_def =
    | VarAttr of (string * typ)
    | Meth of method_def
```

Qui aura son equivalent grammaire:
```ocaml
typed_def: 
| (*parsing de variable*) { VarAttr (id, t) }
| (*parsing de methode*)  {
	 Meth {
        method_name = id;
        code = code;
        params = params;
        locals = variables;
        return = t;
      }
    }
;
```


Une typed def est une definition d'un membre avec type, concrètement une variable, un attribut ou une méthode.

Grace a ce type, on va "tagger" ce qui est une méthode et ce qui est une variable dans les classes, et au lieu d'attendre deux listes de `var_decl` et de `method_decl`, on va attendre simplement une liste de typed_def que l'on va ensuite partitionner pour en extraire toutes les definitions de variables et toutes les definitions de méthodes:
```ocaml
class_def:
| CLASS name=IDENT parent=option(extends_clause) BEGIN members=list(typed_def)  END 
    { 
      let (attributes, methods) = List.partition(
        function VarAttr _ -> true | Meth _ -> false
      ) members in
      
      let attributes = (*unpack*) in
      let methods = (*unpack*) in

      {
        class_name = name;
        attributes = attributes;
        methods = methods;
        parent = parent;
      }
    }
;
```

Le principe est le même pour les méthodes. On peut voir les méthodes comme une liste de declarations de variables et d'instructions.

On va donc définir le type method_line:
```ocaml
type method_line =
    | MemberVar of (string * typ)
    | MemberInstr of instr
```

Qui va être utilisé dans la partie méthode de typed_def exactement comme typed_def était utilisé dans les classes:

```ocaml
typed_def: 
(*[...]*)
| t=typ id=IDENT LPAR params=separated_list(COMMA, method_param) RPAR BEGIN body=list(method_line) END
    {
      let (variables, code) = List.partition(
        function MemberVar _ -> true | MemberInstr _ -> false
      ) body in

      let variables = (*unpack*) in
      let code = (*unpack*) in

      Meth {
        method_name = id;
        code = code;
        params = params;
        locals = variables;
        return = t;
      }
    }
```
## 3.5 - Déclarations mélangées

Ma solution d'implementation pour les declaration simplifiées a aussi eu pour effet de permettre de declarer les différents elements ou on veut. La structure attendue étant maintenant une liste soit de method_line pour les méthodes, soit de typed_def pour les classes, il n'est pas un soucis d'écrire par exemple:

```java
  int compute(int x, int y) {
    // On peut mélanger déclarations et instructions
    int temp;
    this.store(x);
    int other_temp;  // On peut déclarer des variables n'importe où
    return this.add(y);
  }
```


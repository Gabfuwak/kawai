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
Note: Les declaration melangées ne marchent pas pour la main function. Je n'ai pas fait de cas particulier pour celle ci.

## 4 - Did you mean 'recursion'?
On ajoute des suggestions dans les messages d'erreur du typechecker pour aider les utilisateurs. Pour chaque type d'erreur (classe/méthode/attribut/variable inconnue), on suggère l'identificateur le plus proche en calculant la distance de Levenshtein.

L'algorithme de Levenshtein calcule le nombre minimum d'opérations (insertions, suppressions, substitutions) nécessaires pour transformer une chaîne en une autre. Par exemple, la distance entre "bark" et "barkk" est de 1 (une insertion).

On crée des fonctions spécialisées de suggestion selon le contexte :
```ocaml
let suggest_class word cenv = (* suggère une classe similaire *)
let suggest_attr_or_var word cls_name_opt tenv cenv = (* suggère un attribut/variable *)
let suggest_meth word cls_name cenv = (* suggère une méthode *)
```

La suggestion prend en compte l'héritage : si on cherche un attribut/méthode, on regarde aussi dans les classes parentes. Pour les attributs/variables, on regarde à la fois les attributs de classe ET les variables du scope actuel.

Les suggestions sont ajoutées dans tous les messages d'erreur pertinents :

- Variables non définies :
```ocaml
error("Undefined variable \"" ^ x ^ "\". Did you mean \"" ^ 
      suggest_attr_or_var x None tenv cenv ^ "\"?")
```

- Champs non trouvés :
```ocaml
error("Field '" ^ field_name ^ "' not found in class \"" ^ class_name ^ 
      "\". Did you mean \"" ^ suggest_attr_or_var field_name (Some class_name) tenv cenv ^ "\"?")
```

- Méthodes non trouvées :
```ocaml
error("Method '" ^ method_name ^ "' not found in class \"" ^ cls_name ^ 
      "\". Did you mean \"" ^ suggest_meth method_name cls_name cenv ^ "\"?")
```

- Classes inconnues :
```ocaml
error("Class \"" ^ class_name ^ "\" does not exist. Did you mean \"" ^ 
      suggest_class class_name cenv ^ "\"?")
```

Exemples d'erreurs avec suggestions :
```java
var Anmal pet;  // Error: Class "Anmal" does not exist. Did you mean "Animal"?
dog.barkk();    // Error: Method 'barkk' not found in class "Dog". Did you mean "bark"?
print(doog);    // Error: Undefined variable "doog". Did you mean "dog"?
```

Les suggestions ne sont faites que si la distance est suffisamment petite (≤3 pour les classes, ≤2 pour les méthodes/attributs) pour éviter les fausses suggestions. Cette différence de seuil s'explique par le fait qu'il y a généralement moins de classes que de méthodes/attributs, donc on peut se permettre d'être plus permissif.

Le code de test fourni permet de vérifier tous les cas d'erreurs possibles et leurs suggestions associées. Chaque erreur peut être testée individuellement en commentant les autres.

## 5 - Déclarations en série
On permet de déclarer plusieurs variables/attributs du même type sur une seule ligne :
```c
int x, y, z;          // Variables globales
int result, memory;   // Attributs de classe
int temp, partial;    // Variables locales
```

Cet ajout a été grandement simplifié par l'implementation précédemment des declarations simplifiées.

Le changement principal est la modification du type `VarAttr` pour qu'il supporte une liste de variables :
```ocaml
type typed_def =
  | VarAttr_list of (string * typ) list
  | Meth of method_def
```

On modifie ensuite la grammaire pour accepter des listes d'identifiants dans les déclarations :
```ocaml
typed_def: 
| t=typ id_list=separated_nonempty_list(COMMA, IDENT) SEMI 
    { VarAttr_list (List.map (fun id -> (id, t)) id_list) }
```

Cette modification impacte plusieurs règles qui doivent maintenant gérer des listes :
```ocaml
method_line:
| t=typ id_list=separated_nonempty_list(COMMA, IDENT) SEMI 
    { List.map (fun id -> MemberVar (id, t)) id_list }
| i=instr { [MemberInstr i] }

class_def:
| CLASS name=IDENT parent=option(extends_clause) BEGIN members=list(typed_def) END 
    { 
      (* ... *)
      let attributes = List.flatten (List.map (function 
        | VarAttr_list vars -> vars 
        | _ -> failwith "unreachable") attributes) in
      (* ... *)
    }
```

Ce changement ne nécessite pas de modifications du typechecker ou de l'interpréteur car les déclarations en série sont "dépliées" par le parser en déclarations individuelles avant d'atteindre ces composants.

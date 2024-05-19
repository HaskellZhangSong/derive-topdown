data A a b = A (a,b)

para : {A -> [a,b]}
fields : {A -> (a,b)}
parameters: {}
substitution: {P -> (a,b) -> {a1 -> a, a2 -> b}}

```haskell
data P a b = M (E a (Px b))
data E x y = L x | R y
data Px z = Px

parameters: { P -> [a,b] }
fields:     { P -> {Either a (Proxy b)} }

for P's fields 

parameters: { P -> [a,b], Either -> [x,y]}
fields:     { P -> {Either a (Proxy b)}  }
substitution: { P -> Either a (Proxy b) -> {x -> a, y -> Proxy b}}
inferred:   {  Either -> {x, y} }

如果所有的field中没有最外层是AppT ConT那么，过滤一下，应该要去掉没有type var的，如果全不存在，那么就放到inferred中，这是整个算法的basecase，然后如果最外层是ConT的AppT，且这个ConT还是个data type，这样就需要递归向下。
apply subs to P
refuse id type mapping

parameters: { P -> [a,b], Either -> [x,y]}
fields:     { P -> {a, Proxy b} }
substitution: { (P, NonRec) -> Either a (Proxy b) -> {x -> a, y -> Proxy b} }

make (Name, Rec) map into fix point

inferred { Either -> {VarT x, VarT y} }
```



P [a,b] {Either a (Proxy b)}

Either [x y] {x, y}
       {x -> a,
        y -> Proxy b} this map does not rely on the inference of Either but {x,y} does

subs  {Either a (Proxy b)} -> {a, Proxy b}
foreach {a, Proxy b}
if VarT , keep
   type family, keep
   ConT , handle

Proxy [a] {}
a -> b {}

a -> {a}
Proxy b -> {}

{a} union {} = {a}

data List a = Nil | Cons a (List a)

infer List 
List [a] {a, List a}
List recur
{a}

a -> a 

如果递归了，就移除，换成映射



data P a b = P b (Rose [] a)
data Rose k z = L z | B z (k z)

P [a, b] {b, Rose [] a}
Rose [k, z] {z, k z} 
P : [Sub P Rose {k -> [], z -> a}]
{z, k z} -> {a, ([] a)}



data List a = Nil | Cons a (List a)

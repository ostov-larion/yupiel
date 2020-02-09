%% DOM flatten.

:- module(dom,[dom/1,op(990,fx,dom)]).
:- use_module(yupiel).

:- meta_predicate 'dom'(:).
:- meta_predicate 'dom'(:,?).

dom DOM :- dom(DOM,_).
dom(Module:DOM, Sym) :-
	gensym("node#",Sym),
	findall(_chid,(member(Child,DOM.get(children)),dom(Module:Child,_chid)),Childrens),
	asserta(Module:object(DOM.put([uuid:Sym,children:Childrens]))),
	broadcast(DOM.put([uuid:Sym,children:Childrens])).
	
user:This.parent():=Parent :- object Obj, member(This.uuid,Obj.get('children')),Parent=Obj,!.
user:This.sibling():=Sibling :-
	Parent=This.parent(),
	member(_child,Parent.children),
	not(_child=This.uuid),
	Sibling = $_{uuid:_child}.
	
user:This.siblings():=Siblings :-
	findall(Sibling,Sibling=This.sibling(),Siblings).
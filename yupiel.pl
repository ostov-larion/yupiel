:- module(yupiel,[
	op(1200,xfx,'=>'),
	op(1200,xfx,'<=>'),
	op(700,xfx,'$>'),
	op(700,fx,'$:'),
	op(990,fx,component),
	op(990,fx,object),
	op(990,xfx,instanceof),
	op(1,xf,[]),
	'$>'/2,
	'$:'/1,
	'$'/1,
	instanceof/2,
	dtc_assert/1,
	is_instanceof/2,
	cache/1
]).
:- use_module(library(pairs)).
% Reactivity

:- meta_predicate broadcast(:).
:- meta_predicate '$>'(:,:).
:- dynamic user:cached_event/1.

%% +Dict1 <=> +Dict2 // is det
/*
Bi-directional Data Binding
*/
system:term_expansion(X<=>Y,(:- dict_separate(X,X1,X2),dict_separate(Y,Y1,Y2),dtc_assert(X => Y1 $> Y2),dtc_assert(Y => X1 $> X2))).

%% ?Head => +Body // is semidet
/*
DTC - Data Transformation Clause.
*/
system:term_expansion(X:<Z=>Y,(
	:-is_dict(X,_),
	listen(ID>Z,
	(
		is_dict(Z,_),
		X:<Z,
		Cache=cached_event(ID>X),
		not(Cache),
		forall(cached_event(ID>Ev),
		(
			not(X=Ev),retract(cached_event(ID>Ev)))
		),
		asserta(Cache),
		Y
	)
	)
	)
	).
system:term_expansion(X=>Y,
	(
		:-is_dict(X,_),
		listen(ID>Z,
		(
			is_dict(Z,_),
			X:<Z,
			Cache=cached_event(ID>X),
			not(Cache),
			forall(cached_event(ID>Ev),
			(
				not(X=Ev),retract(cached_event(ID>Ev)))
			),
			asserta(Cache),Y))
			;
			listen(X,Y)
	)
	).

:- meta_predicate dtc_assert(:).

dtc_assert(M:(X:<Z=>Y)) :- is_dict(X,_),listen(Z,(is_dict(Z,_),X:<Z,M:Y)),!.
dtc_assert(M:(X=>Y)) :- is_dict(X,_),listen(Z,(is_dict(Z,_),X:<Z,M:Y));listen(X,Y).

%% +From $> +To // is det
/*
RDT - Reactive Data Transformation.
*/
From $> To :- From=_:MaybeDict,not(is_dict(MaybeDict,_)), retractall(From), (To=user:Term;To=Term),!, broadcast(Term), assertz(To).
From $> To :- 
	%writeq(From$>To),nl,
	From=M1:MaybeDict,
	get_object(From,ID=Obj),!,
	%writeq(Obj),nl,
	is_dict(MaybeDict,_), 
	To=_:MaybeDict2,
	MaybeDict3=MaybeDict2.assign(Obj),
	retractall(M1:object(ID=Obj)),
	assertz(M1:object(ID=MaybeDict3)),
	broadcast(ID>MaybeDict3).

%% $: +Event // is det
/*
Broadcast a event. Just syntax sugar, not more.
*/
$:Event :- Event.clear() $> Event.

% Components

:- dynamic is_instanceof/2.
:- dynamic user:(object _).

system:term_expansion(object(Obj),[(:-gensym('obj#',ID),assertz(object(ID=Obj)))]).

system:term_expansion(component(Component),(
	:-	Computed=Component,
		assertz(component(Computed))/*,
		is_dict(Computed,Tag),
		forall(Value=Computed.get(Key),
			(Value=THIS>>Goals,Term=..[Key,THIS,THIS],assertz(Tag:Term:-Goals);true)
		)*/
		)
	).

user:This.init():=Return :- is_dict(This,Tag), component(Component), is_dict(Component,Tag),Return=This.assign(Component),!.
user:This.assign(Dict) := Return :-
	dict_pairs(This,Tag,Pairs1),
	dict_pairs(Dict,_,Pairs2),
	append(Pairs1,Pairs2,Pairs),
	deduplicate(Pairs,Set),
	dict_create(Return,Tag,Set).
	
user:This.exept(Key):=Return :-
	findall(_key-Value,(Value=This.get(_key),not(_key=Key)),_pairs),
	is_dict(This,Tag),
	dict_create(Return,Tag,_pairs).
	
user:This.tag():=Tag :- is_dict(This,Tag).

user:This.clear():=Clear :- dict_create(Clear,This.tag(),[]).
	
deduplicate(L1,L2) :- 
	keysort(L1,Sorted),
	group_pairs_by_key(Sorted,L3),
	findall(Key-Value,(member(Key-[Value|_],L3)),L2).

user:This.from(ComponentTag):=Return :- 
	component(Component), 
	is_dict(Component,ComponentTag),
	is_dict(This,ThisTag),
	assertz(user:is_instanceof(ThisTag,ComponentTag)),
	Return=This.assign(Component),!.

user:dict_separate(From,D1,D2):-
	is_dict(From,Tag),
	findall(Key-Value,(Value=From.get(Key),nonvar(Value)),L1),
	bagof(Key-Value,(Value=From.get(Key),var(Value)),L2),
	dict_create(D1,Tag,L1),
	dict_create(D2,Tag,L2).

:- meta_predicate '$'(:).
:- meta_predicate get_object(:,?).
user:goal_expansion(Obj = $Dict,get_object(Dict,_=Obj)).
$Dict :- get_object(Dict,_).
user:get_object(Dict,Obj) :- get_object(user:Dict,Obj).
get_object(Module:Dict,ID=Obj) :- Module:object(ID=Obj),Dict:<Obj.
	
Sub instanceof Super :- is_dict(Sub,SubTag),is_dict(Super,SuperTag),is_instanceof(SubTag,SuperTag).

% Other Utilities
:- meta_predicate cache(:).
cache(Module:Head):-Module:clause(Head,Body),dynamic(Module:Head),retract(Module:(Head:-Body)),assertz(Module:(Head :- Body,Module:asserta(Head:-!))).
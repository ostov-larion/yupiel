:- module(yupiel,[
	op(1200,xfx,'=>'),
	op(700,xfx,'#>'),
	op(700,fx,'$:'),
	'#>'/2,
	'$:'/1
]).

:- meta_predicate broadcast(:).
:- meta_predicate '#>'(:,:).

%% ?Head => +Body // is semidet
/*
DTC - Data Transformation Clause.
*/
system:term_expansion(X=>Y,(:- listen(X,Y))).

%% +From #> +To // is det
/*
RDT - Reactive Data Transformation
*/
From #> To :- retractall(From), (To=user:Term;To=Term),!, broadcast(Term), assertz(To).

%% $: +Event // is det
/*
Broadcast a event. Just syntax sugar, not more.
*/
$:Event :- broadcast(Event).
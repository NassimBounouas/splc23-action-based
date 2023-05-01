%% ------------------ Action EVALUATIONS -----------------------------

evalActionSeq([],[]) :- !.
evalActionSeq(ActionList,ActionSet) :-
    Seq =.. [seq,ActionList],
    transformActionSeq(Seq,ActionSet),
    call(ActionSet).
transformActionSeq(ActionSeq,NewActionSeq) :-
    term_to_atom(ActionSeq,Atom),
    remove_quotes(Atom, NewAtom),
    atom_concat(NewAtom,'.',Atom2),
    open_string(Atom2, Stream),
    read_term(Stream, NewActionSeq, []),
    close(Stream).

seq([]).
seq([Action|LActions]) :-
    call(Action),
    seq(LActions).

remove_quotes(String, Result) :-
    atom_chars(String, Chars),
    exclude(=('\''), Chars, CleanChars1),
    exclude(=('\"'), CleanChars1, CleanChars2),
    atom_chars(Result, CleanChars2).


%% ---------------- Adaptation operations  -----------------------

createProduct(OutProductRef,ProductName) :-
    gensym(ProductName,OutProductRef),
    asserta(product(OutProductRef,ProductName)),
    trace(createProduct,[OutProductRef,ProductName]).

%%ProductRef only to facilitate the use of the trace.
createObservation(ProductRef,OutRef,Criteria, Subjects) :-
    checkPrecondition(createObservation,createObservation(ProductRef,OutRef,Criteria, Subjects)),
    gensym(obs,OutRef),
    asserta(obs(OutRef,Criteria,Subjects)),
    checkRequirementOnCriteria(Criteria,OutRef),
    trace(createObservation,[OutRef,Criteria, Subjects]).


setValue(_ProductRef,OutRefToValue,RefToObs,Value) :-
    gensym(val,OutRefToValue),
    (Value=''-> Val='empty'; Val=Value),
    asserta(val(OutRefToValue,RefToObs,Val)),
    trace(setValue,[OutRefToValue,RefToObs,Val]).


addObservation(ProductRef,RefToObs) :-
   checkPrecondition(addObservation,addObservation(ProductRef,RefToObs)),!,
    asserta(obs(ProductRef,RefToObs)),
    trace(addObservation,[ProductRef,RefToObs]).

createRequirement(_ProductRef,RefToCriterion,RefToCriteria) :-
    asserta(req(RefToCriterion,RefToCriteria)),
    trace(createRequirement,[RefToCriterion,RefToCriteria]).

%% ------------------ Preconditions for Adaptation rules -----------------------------
checkPrecondition(createProduct,CreateProductCall) :-
    CreateProductCall =.. [createProduct, ProductRef,ProductName],
    \+ product(ProductRef,ProductName), !.

checkPrecondition(createObservation,CreateObservationCall) :-
    CreateObservationCall =.. [createObservation, ProductRef,_RefToObs,Criterion, Subjects],
    %% about criteria
    checkCriteria(ProductRef,Criterion,Subjects),
    %% about subjects
    checkSubjects(ProductRef,Criterion,Subjects),
    %% about requirements
    checkRequirementsOnSubjects(Criterion,Subjects),
    %% observations are distinguishable within a product (same criteria and subjects)
    checkDistinctObservations(ProductRef,RefToObs,Criterion,Subjects).
checkPrecondition(addObservation,AddObsAction) :-
    AddObsAction =.. [addObservation, ProductRef,RefToObs],
    product(ProductRef,_ProductName),
    obs(RefToObs,_Criteria,_Subjects),
    %% observations are distinguishable within a product (same criteria and subjects) 
    checkDistinctObservations(ProductRef,RefToObs,Criteria,Subjects).
checkPrecondition(createRequirement,CreateRequirementAction) :-
    CreateRequirementAction =.. [createRequirement, ProductRef,RefToCriterion,RefToCriteria],
    product(ProductRef,_ProductName),
    checkProductObservationsConformsToRequirement(ProductRef,RefToCriterion,RefToCriteria).
checkPrecondition(setValue,SetValueAction) :-
    SetValueAction =.. [setValue, _ProductRef,_OutRefToValue,_RefToObs,_Value].
checkPrecondition(ActionName,Action) :-
    write("Precondition failed on "),write(ActionName),write(" for action : "),write(Action),nl,
    !,fail.

checkCriteria(_ProductRef,[],[]).
checkCriteria(ProductRef,Criteria,_Subjects) :-
    isCriteria(Criteria),!,
    obs(ProductRef,Criteria).
checkCriteria(ProductRef,Criteria,_Subjects) :-
    write("Criteria "),printObservation(Criteria),write(" is not a criteria in product "),write(ProductRef),nl,!,fail.

checkSubjects(_ProductRef,_Criteria,[]).
checkSubjects(ProductRef,Criteria,[Subject|Subjects]) :-
    isSubject(Subject),!,
    obs(ProductRef,Subject),
    checkSubjects(ProductRef,Criteria,Subjects),!.
checkSubjects(ProductRef,_Criteria,[Subject|_Subjects]) :-
    write("Subject  "),printObservation(Subject),write(" is not a subject in product "),write(ProductRef),nl,!,fail.

%%two observations are distinguishable if they have different criteria or different subjects
%% We dont check if the criteria is the same because it depends on the value of the criteria
checkDistinctObservations(_ProductRef,_RefToObs,[],_Subjects) :- !.
%% We dont check if the subject is the same because it depends on the value of the subject
checkDistinctObservations(_ProductRef,_RefToObs,_Criterion,[]) :- !.
checkDistinctObservations(ProductRef,_RefToObs,Criterion,Subjects) :-
    findall(OtherSubjects,(obs(OtherRefToObs,Criterion,OtherSubjects), obs(ProductRef,OtherRefToObs)),ListOfSubjectList),!,
    noOneIncluded(Subjects,ListOfSubjectList).
checkDistinctObservations(_ProductRef,_RefToObs,_Criteria,_Subjects).



noOneIncluded(_Subjects,[]).
noOneIncluded(Subjects,[OtherSubjects|ListOfSubjectList]) :-
    \+ includedIn(Subjects,OtherSubjects),
    \+ includedIn(OtherSubjects,Subjects),!,
    noOneIncluded(Subjects,ListOfSubjectList).

%% List2 contains list1
includedIn(List1, List2) :-
    %% substract list2 from list1 and check if the result is  empty
    subtract(List1, List2, Rest),
    Rest = [].

checkProductObservationsConformsToRequirement(ProductRef,RefToCriterion,_RefToCriteria) :-
     findall(RefToObs,(obs(ProductRef,RefToObs),obs(RefToObs,RefToCriterion,Subjects),checkRequirementsOnSubjects(RefToCriterion,Subjects)),_ListOfObs).


%% ------------------- On Requirements
%%

:- dynamic req/2.
req(any,[]).

checkRequirementOnCriteria(RefToCriterion, RefToObs) :-
    obs(RefToObs,RefToCriterion,Subjects),
    checkRequirementsOnSubjects(RefToCriterion,Subjects).

checkRequirementsOnSubjects([],_Subjects).
checkRequirementsOnSubjects(RefToCriterion,Subjects) :-
    req(RefToCriterion,RefToCriteria),!,
    getCriteriaOnObservations(Subjects,SubjectCriteria),
    checkRequirementsOnSubjectList(RefToCriteria,SubjectCriteria).
checkRequirementsOnSubjects(_RefToCriterion,_Subjects).

checkRequirementsOnSubjectList([Criterion|RefToCriteria],SubjectCriteria):-
    member(Criterion,SubjectCriteria),!,
    checkRequirementsOnSubjectList(RefToCriteria,SubjectCriteria).
checkRequirementsOnSubjectList([],_Subjects).

%%----------Tools
%% on Observation
isCriteria(RefToObs) :-
    obs(RefToObs,[],_).

isSubject(RefToObs) :-
    \+ isCriteria(RefToObs),!,
    obs(RefToObs,_,[]).

getValue(RefToObs,Value) :-
    val(_RefToValue,RefToObs,Value).

getCriteria(Obs,Criteria) :-
    obs(Obs,Criteria,_).

getSubjects(Obs,Subjects) :-
    obs(Obs,_Criteria,Subjects).

getCriteriaByNameIn(Product,RefToCriteria,CriteriaName) :-
    stringOrAtom(CriteriaName,Atom),
    val(_RefToValue,RefToCriteria,Atom),
    obs(Product,RefToCriteria),
    obs(RefToCriteria,[],[]),!.

 stringOrAtom(CriteriaName,CriteriaName).
 stringOrAtom(CriteriaName,Atom) :-
    string_codes(CriteriaName, Codes), atom_codes(Atom, Codes).

getSubjectByValueIn(Product,RefToSubject,Criteria,SubjectValue) :-
    stringOrAtom(SubjectValue,Atom),
    val(_RefToValue,RefToSubject,Atom),
    obs(RefToSubject,Criteria,[]),
    obs(Product,RefToSubject).

getCriteriaOnObservations(Observations,Criteria) :-
    findall(RefToCriteria,(member(RefToObs,Observations),getCriteria(RefToObs,RefToCriteria)),Criteria).
%%% On Product
observations(RefToProduct,Observations) :-
    findall(RefToObs,obs(RefToProduct,RefToObs),Observations).
criteria(RefToProduct,CriteriaObservations) :-
    findall(RefToObs,(obs(RefToProduct,RefToObs),isCriteria(RefToObs)),CriteriaObservations).
subjects(RefToProduct,SubjectObservations) :-
    findall(RefToObs,(obs(RefToProduct,RefToObs),isSubject(RefToObs)),SubjectObservations).

%%% ----------------------- TRACES

assertaTrace(InProduct,OutProductRef,Function,Args,Term) :-
    trace(Function,Args,Term),
    getDate(Date),
    asserta(trace(InProduct,OutProductRef,Term,Date)).
trace(Function,Args,Term) :-
    Term=.. [Function,Args],
    writeq(Term),
    writeln('').
trace(Function,Args) :-
    trace(Function,Args,_Term).

getDate(Date) :-
    get_time(Stamp),
    format_time(atom(Date), '%Y-%m-%d-%H:%M:%S', Stamp).

history(InProductRef,OutProductRef,history(Traces)) :-
    Trace = trace(InProductRef,OutProductRef,_Term,_Date),
    findall(Trace,Trace,Traces),!.

%%------------------------ derive
%%derive('notes.csv8',deriveNotes,NP,LA).
%% ------------------ derive Product -----------------------------
%%derive('notes.csv1',deriveNotes,NP,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V|LA]), transform_actions([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V|LA],LA2).
%%derive('notes.csv2',deriveNotesBis,NP,ActionSeq),evalActionSeq(ActionSeq,ActionSet).


deriveAndDo(ProductRef,NewName,NewProduct) :-
    derive(ProductRef,NewName,NewProduct,ActionsToDo),
    assertaTrace(ProductRef,NewProduct,derive,[ProductRef,NewName,NewProduct],_NewTrace),
    evalActionSeq(ActionsToDo,ActionSet),
    assertaTrace([],NewProduct,initialActions,ActionSet,_T).


derive(null,NewName,NewProduct,[]) :-
    createProduct(NewProduct,NewName).

derive(ProductRef,NewName,NewProduct,ActionsToDo) :-
    createProduct(NewProduct,NewName),
    deriveObservations(ProductRef,NewProduct,ActionsToDo).

deriveObservations(ProductRef,NewProduct,ActionList) :-
    buildActionList(ProductRef,NewProduct,ActionList).
    %% to evaluate call : evalActionSeq(ActionList,ActionSet).

buildActionList(ProductRef,NewProduct,ActionList) :-
    observations(ProductRef,Observations),
    buildActionList4Obs(NewProduct,Observations,ActionList).

buildActionList4Obs(_NewProduct,[],[]).
buildActionList4Obs(NewProduct,OldObservations,ActionList) :-
    partitionObservations(_ProductRef,OldObservations,Criteria,Subjects,Observations),
    buildActionListByKind(NewProduct,Criteria,CriteriaList),
    buildActionListByKind(NewProduct,Subjects,SubjectList),
    buildActionListByKind(NewProduct,Observations,ObservationList),
    append([CriteriaList,SubjectList,ObservationList],ActionList).

buildActionListByKind(_NewProduct,[],[]).
buildActionListByKind(NewProduct,[Obs|Observations],ActionList) :-
    buildCreateObservation(NewProduct,Obs,Actions),
    buildActionListByKind(NewProduct,Observations,ActionSublist),
    append([Actions,ActionSublist],ActionList).

buildCreateObservation(NewProduct,Obs,[CreateObsAction,SetValueAction,AddObsAction]) :-
    createVarName(Obs,NewObs),
    createRefToCriteria(Obs,NewCriteria),
    createRefToSubjects(Obs,NewSubjects),
    %%write('NewSubjects='),writeln(NewSubjects),
    CreateObsAction =.. [createObservation,NewProduct,NewObs,NewCriteria,NewSubjects],
    getValue(Obs,Value),
    SetValueAction =.. [setValue,NewProduct,_OutValue,NewObs,Value],
    AddObsAction =.. [addObservation,NewProduct,NewObs].

createRefToCriteria(Obs,NewCriteria) :-
    getCriteria(Obs,Criteria),
    ( Criteria=[] ->
        NewCriteria = [];
        createVarName(Criteria,NewCriteria) ).

createRefToSubjects(Obs,NewSubjects) :-
    getSubjects(Obs,Subjects),
    createVarNames(Subjects,NewSubjects).


createVarNames([],[]).
createVarNames([Var|Vars],[NewVar|NewVars]) :-
    createVarName(Var,NewVar),
    createVarNames(Vars,NewVars).
createVarName(VarName,NewVarName) :-
    atom_concat('X',VarName,NewVarName).


%% ------------------ Extract -----------------------------
%%criteria('notes.csv1',Criteria),extract('notes.csv1',Criteria,template,NP,ActionsToDO,LostObservations),evalActionSeq(ActionsToDO,ActionSet).
%%criteria('notes.csv1',Criteria),subjects('notes.csv1',Subjects),append([Criteria,Subjects],Obs),extract('notes.csv1',Obs,template,NP,ActionsToDO,LostObservations),evalActionSeq(ActionsToDO,ActionSet).
%%criteria('notes.csv1',Criteria), extract('notes.csv1',Criteria,template,NP,ActionsToDO,LostObservations).
%%criteria('notes.csv1',Criteria), extract('notes.csv1',[obs15|Criteria],template,NP,ActionsToDO,LostObservations),evalActionSeq(ActionsToDO,ActionSet).
extract(ProductRef,ObservationsToKeep,NewName,NewProduct,ActionsToDO,LostSetOfObservations) :-
    partitionObservations(ProductRef,ObservationsToKeep,Criteria,Subjects,Observations),
    %%Filter subjects that do not have criteria
    filterObservations(Criteria,Subjects,    NewSubjects,    LostSubjects),
    %%Filter observations that do not have criteria
    filterObservations(Criteria,Observations,NewObservations,LostObservations),
    %%TODO : REMOVE OBSERVATIONS THAT DO NOT HAVE SUBJECTS or change but how????
    append(LostSubjects,LostObservations,LostSetOfObservations),
    createProduct(NewProduct,NewName),
    append([Criteria,NewSubjects,NewObservations],NewSetOfOldObservations),
    buildActionList4Obs(NewProduct,NewSetOfOldObservations,ActionsToDO).
%    createProductFromObservations(NewProduct,Criteria,NewSubjects,NewObservations).

extractAndDo(ProductRef,ObservationsToKeep,NewName,NewProduct):-
    extract(ProductRef,ObservationsToKeep,NewName,NewProduct,ActionsToDO,LostSetOfObservations),
    assertaTrace(ProductRef,NewProduct,extract,[ProductRef,ObservationsToKeep,NewName,NewProduct,ActionsToDO,LostSetOfObservations],_NewTrace),
    evalActionSeq(ActionsToDO,ActionSet),
    assertaTrace([],NewProduct,initialActions,ActionSet,_T).
%% ------------------ Partition -----------------------------
%%observations('notes.csv8',Observations), partitionObservations(ProductRef,Observations,Criteria,Subjects,NewObservations).
partitionObservations(_ProductRef,[],[],[],[]).
partitionObservations(ProductRef,[Obs|ObservationsToKeep],[Obs|Criteria],Subjects,Observations) :-
    isCriteria(Obs),!,
    partitionObservations(ProductRef,ObservationsToKeep,Criteria,Subjects,Observations).
partitionObservations(ProductRef,[Obs|ObservationsToKeep],Criteria,[Obs|Subjects],Observations) :-
    isSubject(Obs),!,
    partitionObservations(ProductRef,ObservationsToKeep,Criteria,Subjects,Observations).
partitionObservations(ProductRef,[Obs|ObservationsToKeep],Criteria, Subjects,[Obs|Observations]) :-
    partitionObservations(ProductRef,ObservationsToKeep,Criteria,Subjects,Observations).


%% ------------------ Filter -----------------------------
%%observations('notes.csv8',Observations), partitionObservations(ProductRef,Observations,[_,_|Criteria],Subjects,NewObservations), filterObservations(Criteria,Subjects,NewSubjects,LostSubjects), filterObservations(Criteria,Observations,NewObservations,LostObservations).
%A criteria is required
filterObservations([],Obs,[],Obs).
filterObservations(Criteria,[Obs|Observations],[Obs|NewObservations],LostObservations) :-
    getCriteria(Obs,CriteriaObs),
    member(CriteriaObs,Criteria),!,
    filterObservations(Criteria,Observations,NewObservations,LostObservations).
filterObservations(Criteria,[Obs|Observations],NewObservations,[Obs|LostObservations]) :-
    filterObservations(Criteria,Observations,NewObservations,LostObservations).
filterObservations(_Criteria,[],[],[]).

%% ------------------ PRINT -----------------------------
printProduct(ProductRef) :-
    product(ProductRef,Name),
    writeln(Name),
    printObservations(ProductRef).

printObservations(ProductRef) :-
    findall(RefToObs,obs(ProductRef,RefToObs),ObsList),!,
    printObservationList(ObsList).

printObservationList([RefToObs|ObsList]) :-
    printObservation(RefToObs),
    printObservationList(ObsList).
printObservationList([]).

printObservation(RefToObs) :-
    obs(RefToObs,Criteria,Subjects),
    write(RefToObs),
    write('- criterion : '),
    ( printValueOf(Criteria) -> true ; write(' isCriterion') ),
    write(' subjects : '),
     printValuesOf(Subjects) ,
    write(' value : '),
    printValueOf(RefToObs),
        writeln('').

printValueOf(RefToObs) :-
    getValue(RefToObs,Value),
    write(Value).

printValuesOf([RefToObs|ObsList]) :-
    printValueOf(RefToObs),
    write(','),
    printValuesOf(ObsList).
printValuesOf([]).

/* ------------------------- composite operations ------------------------- */


createAndAddCriteria(ProductRef,RefToCriteria,Value) :-
    createObservation(ProductRef,RefToCriteria,[],[]),
    addObservation(ProductRef,RefToCriteria),
    setValue(ProductRef,_,RefToCriteria,Value).

createAndAddObservation(ProductRef,RefToObservation,Criteria,Subjects,Value) :-
    createObservation(ProductRef,RefToObservation,Criteria,Subjects),
    addObservation(ProductRef,RefToObservation),
    setValue(ProductRef,_,RefToObservation,Value).


/* ------------------------- CSV READING ------------------------- */
:- use_module(library(csv)).

%csv_read_file("notes.csv",P).
%readCSVToProduct("notes.csv", Product)
%csv_read_file("notes.csv", Rows,[separator(0';)])
readCSVToProduct(FileName, Product) :-
    csv_read_file(FileName, [CriteriaRow|Rows],[separator(0';)]),
    createProduct(Product, FileName),
    %% create the criteria
    readRow(CriteriaRow,Criteria),
    createLineCriteria(Product, Criteria,[],RefsToCriteria),
        write('Criteria : '),
        printValuesOf(RefsToCriteria),
        writeln(''),
    %% create the observations line by line
    createObservationsByLine(Product, RefsToCriteria, Rows).

readRow(Row,List) :-
    Row =.. [row|List].


%% create the criteria
%% createLineCriteria('notes.csv5', ['C1', 'C2', 'Moyenne'], [obs17], _20396)
createLineCriteria(ProductRef, [CValue|Criteria],L, [RefToCriteria|CriteriaList]) :-
    createLineCriteria(ProductRef, Criteria,L,CriteriaList),
    createAndAddCriteria(ProductRef,RefToCriteria,CValue).
createLineCriteria(_Product,[],RefsToCriteria,RefsToCriteria).

%% create the observations line by line
createObservationsByLine(Product, Criteria, [Row|Rows]) :-
    createObservationBySubject(Product, Criteria, Row),
    createObservationsByLine(Product, Criteria, Rows).
createObservationsByLine(_Product, _Criteria, []).

%% create the observations line

createObservationBySubject(Product,[SubjectCriteria|Criteria],Row)  :-
    readRow(Row,[Subject|Values]),
    write('reading line for subject :'),
    write(Subject),
    write('  --> '),
    printValueOf(SubjectCriteria),
    %write(Values),
    %create the subject
    createAndAddObservation(Product,RefToSubject,SubjectCriteria,[],Subject),

    %create the observations on this subject
    createObservationBySubject(Product,Criteria,RefToSubject,Values).
createObservationBySubject(_Product,_, []). %% Empty line

%% create the observations on a given subject
%%createObservationBySubject('notes.csv6', [obs23, obs22, obs21], obs25, ['AC1', 'AC2', 'MoyenneAC12'])
createObservationBySubject(Product,[C|Criteria],RefToSubject,[ObsValue|Row]) :-
    write(' create observation for criteria :'),
    write(C),
    write('  for :'),
    write(ObsValue),
    createAndAddObservation(Product,_RefToObs,C,[RefToSubject],ObsValue),
    createObservationBySubject(Product,Criteria,RefToSubject,Row).
createObservationBySubject(_Product,_Criteria,_RefToSubject,[]). %% Empty line


%% ------------------------- Well-formed -------------------------
%% By construction, the product is well-formed on many properties but we have to addObservation
%% the following properties :
%% 1) All the criteria have a value (if they have it is different)
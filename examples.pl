%% reconsult('examples').
%%:-reconsult('core').

%% ------------------ SPLC 2023 examples ---------
createSPLC23Example(P1,P2,Template,P3,P4) :-
    firstStep(P1),!,
    secondStep(P1,P2),!,
    thirdStep(P2,Template),!,
    fourthStep(Template,P3),
    fiveStep(Template,P4).

%% Sc.1 As a Breeder, I want to create a template named "grasses-Template" that contains all the standard information required
%% for a grasses trial which are "Species, Plot, Yield" to begin the trials promptly.
firstStep(P) :-
    writeln("========> First step : create a template named grassesTemplate that contains all the standard information required for a grasses trial which are Species Plot Yield"),
    createProduct(P,"grassesTemplate"),
    createAndAddCriteria(P,RefSpecies,"species"),
    createAndAddCriteria(P,RefPlot,"plot"),
    createAndAddCriteria(P,RefYield,"yield"),
    createRequirement(P,RefYield,[RefSpecies,RefPlot]),
    checkAProduct(P,"grassesTemplate"),
    checkACriteria(P,RefSpecies,"species"),
    checkACriteria(P,RefPlot,"plot"),
    checkACriteria(P,RefYield,"yield").

%%Sc.2 As a field worker, I want to fill my trial field book about
%%grasses following a template named "grassesTemplate" and
%%add the harvest date at filling time to support multiple harvest cycles.
secondStep(P1, P) :-
    writeln('========>  Second step : derive and fill my trial field book about grasses'),
    deriveAndDo(P1,"p2",P),
    createAndAddCriteria(P,RefHarvestDate,"harvestDate"),
    getCriteriaByNameIn(P,RefSpecies,"species"),
    createAndAddObservation(P,AG1,RefSpecies,[],"Andropogon Gerardii"),
    createAndAddObservation(P,AH1,RefSpecies,[],"Andropogon hallii"),
    getCriteriaByNameIn(P,RefPlot,"plot"),
    createAndAddObservation(P,PL1,RefPlot,[],"1"),
    createAndAddObservation(P,PL2,RefPlot,[],"2"),
    getCriteriaByNameIn(P,RefHarvestDate,"harvestDate"),
    createAndAddObservation(P,HD1,RefHarvestDate,[],"01/04"),
    createAndAddObservation(P,HD2,RefHarvestDate,[],"01/08"),
    getCriteriaByNameIn(P,RefYield,"yield"),
        createRequirement(P,RefYield,[RefSpecies,RefPlot, RefHarvestDate]),
        createAndAddObservation(P,Y1,RefYield,[AG1,PL1,HD1],"6"),
        createAndAddObservation(P,Y2,RefYield,[AH1,PL2,HD1],"8"),
        createAndAddObservation(P,Y3,RefYield,[AG1,PL1,HD2],"8"),
    checkAProduct(P,"p2"),
    checkAnObservation(P,AG1,RefSpecies,"Andropogon Gerardii",[]),
    checkAnObservation(P,AH1,RefSpecies,"Andropogon hallii",[]),
    checkAnObservation(P,Y1,RefYield,"6",[AG1,PL1,HD1]),
    checkAnObservation(P,Y2,RefYield,"8",[AH1,PL2,HD1]),
    checkAnObservation(P,Y3,RefYield,"8",[AG1,PL1,HD2]).


%Sc.3 As a Breeder, I want to create a new Template based on
%the last grasses trial field book and add an information about
%temperature collected on the plot thanks to IoT sensors to
%develop a species that can withstand high temperatures.
thirdStep(P2, Template) :-
     writeln('========>  Third step : create a new Template based on the last grasses trial field book'),
     criteria(P2,Criteria),
     extractAndDo(P2,Criteria,"grassesTemplateBis",Template),
     checkAProduct(P,"grassesTemplateBis"),
     getCriteriaByNameIn(Template,RefSpecies,"species"),
     checkACriteria(P,RefSpecies,"species").

fourthStep(Template,P) :-
        writeln('========>  Fourth step : fill the new template with the temperature information.'),
        deriveAndDo(Template,"p3",P),
        createAndAddCriteria(P,RefTemperature,"temperature"),
        getCriteriaByNameIn(P,RefharvestDate,"harvestDate"),
        getCriteriaByNameIn(P,RefPlot,"plot"),
        createRequirement(P,RefTemperature,[RefharvestDate,RefPlot]),

        getCriteriaByNameIn(P,RefSpecies,"species"),
        createAndAddObservation(P,AM1,RefSpecies,[],"Andropogon Minarum"),
        createAndAddObservation(P,SB1,RefSpecies,[],"Sorghum Bicolor"),

        getCriteriaByNameIn(P,RefPlot,"plot"),
        createAndAddObservation(P,PL4,RefPlot,[],"4"),
        createAndAddObservation(P,PL9,RefPlot,[],"9"),

        getCriteriaByNameIn(P,RefHarvestDate,"harvestDate"),
        createAndAddObservation(P,HD1,RefHarvestDate,[],"08/05"),
        createAndAddObservation(P,HD2,RefHarvestDate,[],"20/05"),

        getCriteriaByNameIn(P,RefYield,"yield"),
        createAndAddObservation(P,_Y1,RefYield,[AM1,PL4,HD1],"6"),
        createAndAddObservation(P,_Y2,RefYield,[SB1,PL9,HD1],"5"),

        createAndAddObservation(P,_T1,RefTemperature,[PL4,HD1],"17"),
        createAndAddObservation(P,_T2,RefTemperature,[PL9,HD2],"16"),
        checkAProduct(P,"p3").

fiveStep(Template,P) :-
                writeln('========>  Five step : fill the new template'),
                deriveAndDo(Template,"p4",P),!,
                createAndAddCriteria(P,RefTemperature,"temperature"),
                getCriteriaByNameIn(P,RefharvestDate,"harvestDate"),
                getCriteriaByNameIn(P,RefPlot,"plot"),
                createRequirement(P,RefTemperature,[RefharvestDate,RefPlot]),

                getCriteriaByNameIn(P,RefSpecies,"species"),
                createAndAddObservation(P,AM1,RefSpecies,[],"Andropogon Minarum"),
                createAndAddObservation(P,SB1,RefSpecies,[],"Sorghum Bicolor"),

                getCriteriaByNameIn(P,RefPlot,"plot"),
                createAndAddObservation(P,PL4,RefPlot,[],"4"),
                createAndAddObservation(P,PL9,RefPlot,[],"9"),

                getCriteriaByNameIn(P,RefHarvestDate,"harvestDate"),
                createAndAddObservation(P,HD1,RefHarvestDate,[],"08/05"),
                createAndAddObservation(P,HD2,RefHarvestDate,[],"20/05"),

                getCriteriaByNameIn(P,RefYield,"yield"),
                createAndAddObservation(P,_Y1,RefYield,[AM1,PL4,HD1],"6"),
                createAndAddObservation(P,_Y2,RefYield,[SB1,PL9,HD1],"5"),

                createAndAddObservation(P,_T1,RefTemperature,[PL4,HD1],"17"),
                createAndAddObservation(P,_T2,RefTemperature,[PL9,HD2],"16"),
                checkAProduct(P,"p4").

%Species Plot Yield Harvest date Temp
%Andropogon Minarum 4 4 08/05 17
%Sorghum Bicolor 9 5 20/05

%% ----------------------------- To check -----------------------------
checkAProduct(P,Name) :-
    product(P,Name),
    writeln('----------------- Result of check Product : '),
    printProduct(P).
checkACriteria(Product,RefObs,Name) :-
    obs(RefObs,[],[]),
    obs(Product,RefObs),
     stringOrAtom(Name,Atom) ,
    val(_,RefObs,Atom),!,
    writeln('----------------- Result of check Criteria : '),
    printObservation(RefObs).
checkAnObservation(Product,RefObs,RefCrit,Value,RefSubjects) :-
    obs(RefObs,RefCrit,RefSubjects),
    obs(Product,RefObs),
    stringOrAtom(Value,Atom),
    val(_,RefObs,Atom),
    writeln('----------------- Result of check Observation : '),
    printObservation(RefObs).
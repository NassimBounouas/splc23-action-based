FROM swipl:9.1.8
WORKDIR /app
COPY core.pl examples.pl /app
ENTRYPOINT swipl -g 'consult("core.pl").' -g 'consult("examples.pl").' -g 'createSPLC23Example(P1,P2,Template,P3,P4).'

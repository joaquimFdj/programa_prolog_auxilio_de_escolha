% criar o fato "resposta" que sera instanciado em tempo de execução segundo
% as respostas do usuario:
:- dynamic resposta/2.


% lembrar de usar, no predicado da interface "retractall(resposta(_, _))." 
% so como um padrao do prolog mesmo (vai apagar as instancias existentes ate entao de 
% "resposta()".)


% base fixa de perguntas:
pergunta("Retorno financeiro e muito importante para voce?").
pergunta("Impacto social e muito importante para voce?").
pergunta("Voce lida bem com pressao?").
pergunta("Voce gosta de fazer as coisas com calma?").
pergunta("Voce e uma pessoa ansiosa?").
pergunta("Voce trabalha bem com outras pessoas?").
pergunta("Voce tem facilide com programacao back-end?").
pergunta("Voce tem facilidade com raciocinio logico?").
pergunta("Voce gosta de trabalhar com front-end?").


fazer_perguntas() :-
    % cria lista com as perguntas na base
    findall(P, pergunta(P), Lista),
    % chama percorrer() passando essa lista
    percorrer(Lista).


% isso é uma recursão: primeiro é o caso base (lista vazia) dai tem a recursao: 
% fazer a pergunta e chamar a propria funcao passando o resto da lista, quando
% chega no caso base para.
% (a sintaxe do prolog [head|tail] separa o primeiro elemento da lista (head) e o resto dela)
percorrer([]).
percorrer([P|Resto]) :-
    perguntar(P),
    percorrer(Resto).


% funcao para fazer uma pergunta e armazenar o texto respondido no terminal pelo usuario:
perguntar(Pergunta) :-
    format("~w (responda: sim ou nao)~n", [Pergunta]),
    read(Resp),
    % instancia resposta():
    assertz(resposta(Pergunta, Resp)).

% criar o fato "resposta" que sera instanciado em tempo de execução segundo
% as respostas do usuario:
:- dynamic resposta/2.


% lembrar de usar, no predicado da interface "retractall(resposta(_, _))." 
% so como um padrao do prolog mesmo (vai apagar as instancias existentes ate entao de 
% "resposta()".)


% trilhas disponíveis (formato: trilha(Trilha, Descrição))

trilha(dev_web, "Desenvolvedor WEB - Desenvolve páginas web, com foco maior em front-end.").
trilha(dev_software, "Desenvolvedor Back End - Desenvolve aplicativos e programas, com foco maior em back-end.").
trilha(ciencia_dados, "Ciência de dados - Analisa e trata dados em larga escala.").
trilha(redes_infra, "Redes - Constrói e mantém sistemas de rede com VPNs, Firewalls, VLANs, etc.").
trilha(cyber_sec, "Cibersegurança - Garante a segurança do sistema da empresa, com criptografias e afins.").

% base fixa de perguntas (formato: pergunta(Pergunta, Numero

pergunta("Você deseja seguir algo com foco em programação", 1).
pergunta("Você gosta de trabalhar com design", 2).
pergunta("Você lida bem com pressão", 3).
pergunta("Você lida bem com responsabilidade", 4).
pergunta("Você gosta de arquitetar e administrar sistemas", 5).
pergunta("Você tem facilide com programação back-end", 6).
pergunta("Você tem facilidade com programação front-end", 7).
pergunta("Você tem experiência com criptografias diversas", 8).
pergunta("Você gosta de trabalhar com hardware", 9).
pergunta("Você tem facilidade em encontrar falhas onde outras pessoas tem dificuldade", 10).


% peso de cada habilidade para cada trilha (formato: peso(Num_Pergunta, Trilha, Peso).

peso(1, dev_web, 5).
peso(1, dev_software, 5).
peso(1, ciencia_dados, 2).
peso(1, redes_infra, 1).
peso(1, cyber_sec, 3).

peso(2, dev_web, 5).
peso(2, dev_software, 3).
peso(2, ciencia_dados, 1).
peso(2, redes_infra, 2).
peso(2, cyber_sec, 1).

peso(3, dev_web, 4).
peso(3, dev_software, 4).
peso(3, ciencia_dados, 2).
peso(3, redes_infra, 2).
peso(3, cyber_sec, 2).

peso(4, dev_web, 2).
peso(4, dev_software, 2).
peso(4, ciencia_dados, 4).
peso(4, redes_infra, 4).
peso(4, cyber_sec, 5).

peso(5, dev_web, 1).
peso(5, dev_software, 1).
peso(5, ciencia_dados, 4).
peso(5, redes_infra, 5).
peso(5, cyber_sec, 1).

peso(6, dev_web, 3).
peso(6, dev_software, 5).
peso(6, ciencia_dados, 3).
peso(6, redes_infra, 1).
peso(6, cyber_sec, 4).

peso(7, dev_web, 5).
peso(7, dev_software, 3).
peso(7, ciencia_dados, 1).
peso(7, redes_infra, 1).
peso(7, cyber_sec, 2).

peso(8, dev_web, 2).
peso(8, dev_software, 1).
peso(8, ciencia_dados, 3).
peso(8, redes_infra, 3).
peso(8, cyber_sec, 5).

peso(9, dev_web, 1).
peso(9, dev_software, 2).
peso(9, ciencia_dados, 1).
peso(9, redes_infra, 5).
peso(9, cyber_sec, 2).

peso(10, dev_web, 3).
peso(10, dev_software, 3).
peso(10, ciencia_dados, 2).
peso(10, redes_infra, 4).
peso(10, cyber_sec, 4).

fazer_perguntas() :-
    % cria lista com as perguntas na base
    findall(Numero-Pergunta, pergunta(Pergunta, Numero), Lista),
    % chama percorrer() passando essa lista
    percorrer(Lista).


% isso é uma recursão: primeiro é o caso base (lista vazia) dai tem a recursao: 
% fazer a pergunta e chamar a propria funcao passando o resto da lista, quando
% chega no caso base para.
% (a sintaxe do prolog [head|tail] separa o primeiro elemento da lista (head) e o resto dela)
percorrer([]).
percorrer([Numero-Pergunta|Resto]) :-
    perguntar(Numero, Pergunta),
    percorrer(Resto).


% funcao para fazer uma pergunta e armazenar o texto respondido no terminal pelo usuario:
perguntar(Numero, Pergunta) :-
    format("~w? (s/n)~n", [Pergunta]),
    read(Resp),
    % validação da resposta: caso seja inválida, não armazena a resposta e pergunta novamente
    (Resp == s ; Resp == n) ->  assertz(resposta(Numero, Resp));
    (writeln("Resposta inválida"), perguntar(Numero, Pergunta)).

calcular_resultado(Pontuacao) :-
    % funcao para criar uma lista com as pontuações em cada trilha com base nas respostas
    findall(Pontos-Trilha,
            calcular(Trilha, Pontos),
            Pontuacao).

calcular(Trilha, Pontos) :-
    % funcao para contar a quantidade de pontos de uma trilha
    % trilha(Trilha, _Descricao) seleciona uma trilha e findall soma os pesos da trilha
    % onde a resposta foi sim
    trilha(Trilha, _Descricao),
    findall(Peso,
            (resposta(Numero, s), peso(Numero, Trilha, Peso)),
            Pesos),
    somaLista(Pesos, Pontos).

% funcao para somar os valores da lista
somaLista([], 0).
somaLista([H|T], Soma) :-
    somaLista(T, SomaResto),
    Soma is H + SomaResto.

recomendar_trilha(Pontuacao) :-
    % funcao para ordenar as recomendações e inciar as impressões
    keysort(Pontuacao, P_Crescente),
    reverse(P_Crescente, P_Decrescente),
    imprimir_recomendacao(P_Decrescente).


imprimir_recomendacao([Pontos-Trilha|Resto]) :-
    % funcao para imprimir as recomendações, imprime a mais recomendada e a
	% justificativa, além de chamar a função para imprimir o resto das recomendações
    % caso todas as respostas sejam não, a funcao imprime uma mensagem de erro
    (Pontos == 0) ->  writeln("Como todas as respostas foram negativas, não foi possível encontrar uma recomendação");
    (trilha(Trilha, Descricao),
	format("Trilha mais recomendada: ~w ~n", [Descricao]),
    writeln("O que mais influenciou a recomendação: "),
    encontrar_justificativas(Trilha),
    format("~nOutras trilhas em ordem de mais recomendada para menos recomendada:~n"),
    imprimir_outras_trilhas(Resto)).


encontrar_justificativas(Trilha) :-
    % funcao para encontrar as perguntas com peso 4 ou 5 para a trilha mais recomendada
	% e que foram responsidadas como sim pelo usuário
    findall(Justificativa,
            (resposta(Numero, s), peso(Numero, Trilha, Peso), Peso >= 4, pergunta(Justificativa, Numero)),
            Justificativas),
    imprimir_justificativas(Justificativas).

% funcao para imprimir as perguntas que justificaram a recomendação
imprimir_justificativas([]).
imprimir_justificativas([H|T]) :-
    format("~w ~n", H),
    imprimir_justificativas(T).    

% funcao para imprimir o resto das trilhas em ordem de mais recomandada para menos recomendada
imprimir_outras_trilhas([]).
imprimir_outras_trilhas([_Pontos-Trilha|Resto]) :-
    trilha(Trilha, Descricao),
    format("~w ~n", [Descricao]),
    imprimir_outras_trilhas(Resto).


iniciar :-
    fazer_perguntas,
    calcular_resultado(Pontuacao),
    recomendar_trilha(Pontuacao).

consult :-
    calcular_resultado(Pontuacao),
    recomendar_trilha(Pontuacao).
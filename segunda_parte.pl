
% trilhas disponíveis (formato: trilha(Trilha, Descrição))

trilha(dev_web, "Desenvolve páginas web, com foco maior em front-end.").
trilha(dev_software, "Desenvolve aplicativos e programas, com foco maior em back-end.").
trilha(ciencia_dados, "Analisa e trata dados em larga escala.").
trilha(redes_infra, "Constrói e mantém sistemas de rede com VPNs, Firewalls, VLANs, etc.").
trilha(cyber_sec, "Garante a segurança do sistema da empresa, com criptografias e afins.").

% base fixa de perguntas (formato: pergunta(Pergunta, Pred))

pergunta("Você deseja seguir algo com foco em programação?", programa).
pergunta("Você gosta de trabalhar com design?", design).
pergunta("Você lida bem com pressão?", lida_pressao).
pergunta("Você lida bem com responsabilidade?", lida_responsa).
pergunta("Você gosta de arquitetar e administrar sistemas?", arq_admin).
pergunta("Você tem facilide com programação back-end?", back_end).
pergunta("Você tem facilidade com programação front-end?", front_end).
pergunta("Você tem experiência com criptografias diversas?", criptografia).
pergunta("Você gosta de trabalhar com hardware?", hardware).
pergunta("Você tem facilidade em encontrar falhas onde outras pessoas tem dificuldade?", ve_falhas).


% peso de cada habilidade para cada trilha (formato: peso(Pred, Trilha, Peso).

peso(programa, dev_web, 5).
peso(programa, dev_software, 5).
peso(programa, ciencia_dados, 2).
peso(programa, redes_infra, 1).
peso(programa, cyber_sec, 3).

peso(design, dev_web, 5).
peso(design, dev_software, 3).
peso(design, ciencia_dados, 1).
peso(design, redes_infra, 2).
peso(design, cyber_sec, 1).

peso(lida_pressao, dev_web, 4).
peso(lida_pressao, dev_software, 4).
peso(lida_pressao, ciencia_dados, 2).
peso(lida_pressao, redes_infra, 2).
peso(lida_pressao, cyber_sec, 2).

peso(lida_responsa, dev_web, 2).
peso(lida_responsa, dev_software, 2).
peso(lida_responsa, ciencia_dados, 4).
peso(lida_responsa, redes_infra, 4).
peso(lida_responsa, cyber_sec, 5).

peso(arq_admin, dev_web, 1).
peso(arq_admin, dev_software, 1).
peso(arq_admin, ciencia_dados, 4).
peso(arq_admin, redes_infra, 5).
peso(arq_admin, cyber_sec, 1).

peso(back_end, dev_web, 3).
peso(back_end, dev_software, 5).
peso(back_end, ciencia_dados, 3).
peso(back_end, redes_infra, 1).
peso(back_end, cyber_sec, 4).

peso(front_end, dev_web, 5).
peso(front_end, dev_software, 3).
peso(front_end, ciencia_dados, 1).
peso(front_end, redes_infra, 1).
peso(front_end, cyber_sec, 2).

peso(criptografia, dev_web, 2).
peso(criptografia, dev_software, 1).
peso(criptografia, ciencia_dados, 3).
peso(criptografia, redes_infra, 3).
peso(criptografia, cyber_sec, 5).

peso(hardware, dev_web, 1).
peso(hardware, dev_software, 2).
peso(hardware, ciencia_dados, 1).
peso(hardware, redes_infra, 5).
peso(hardware, cyber_sec, 2).

peso(ve_falhas, dev_web, 3).
peso(ve_falhas, dev_software, 3).
peso(ve_falhas, ciencia_dados, 2).
peso(ve_falhas, redes_infra, 4).
peso(ve_falhas, cyber_sec, 4).

% --funções--

fazer_perguntas() :-
    % cria lista com as perguntas na base
    findall((Pergunta, Pred), pergunta(Pergunta, Pred), Lista),
    % chama percorrer() passando essa lista
    percorrer(Lista).


% isso é uma recursão: primeiro é o caso base (lista vazia) dai tem a recursao: 
% fazer a pergunta e chamar a propria funcao passando o resto da lista, quando
% chega no caso base para.
% (a sintaxe do prolog [head|tail] separa o primeiro elemento da lista (head) e o resto dela)
percorrer([]).
percorrer([Pergunta, Pred|Resto]) :-
    perguntar(Pergunta, Pred),
    percorrer(Resto).


% funcao para fazer uma pergunta e armazenar o texto respondido no terminal pelo usuario:
perguntar(Pergunta, Pred) :-
    format("~w (responda: s para sim ou n para nao)~n", [Pergunta]),
    read(Resp),
    % torna o predicado associado com cada pergunta true ou false dependendo da resposta
    (   Resp == s ->  V = true ; V = false),
    Skill =.. [Pred, V],
    assertz(Skill).
% p.s. depois da execução, usar "retractall(Skill)", para resetar as respostas da execução anterior

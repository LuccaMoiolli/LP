-- Aluno: Lucca Moreira Moiolli Rodrigues
-- Matrícula: 190112379


module Root.Exercicios.UnBCare where

-- import Root.Modelo.ModeloDados

type Medicamento = String

type Quantidade = Int

type Horario = Int

type EstoqueMedicamentos = [(Medicamento, Quantidade)]

type Prescricao = (Medicamento, [Horario])

type Receituario = [Prescricao]

type PlanoMedicamento = [(Horario, [Medicamento])]

type Plantao = [(Horario, [Cuidado])]

data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento

instance Show Cuidado where
  show (Comprar m q) =
    "Comprar "
      ++ Prelude.show q
      ++ " comprimido(s) do medicamento: "
      ++ m
  show (Medicar m) = "Ministrar medicamento: " ++ m

{-
 

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

-- meu preludio

-- meu isJust
ehJust :: Maybe x -> Bool
ehJust m = case m of
    Nothing -> False
    _ -> True

-- meu fomJust
deJust :: Maybe t -> t
deJust x = case x of
    Just y -> y 

-- meu map
mapeia :: (a -> b) -> [a] -> [b]
mapeia _ [] = []
mapeia f (x:xs) = f x : mapeia f xs

-- meu any
algum :: (a -> Bool) -> [a] -> Bool
algum _ [] = False
algum f (x:xs) = f x || algum f xs

-- meu all
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos f (x:xs) = f x && todos f xs

-- meu break
quebra :: (t -> Bool) -> [t] -> ([t], [t])
quebra _ [] = ([],[])
quebra f (x:xs) 
    | f x = ([], x:xs)
    | otherwise = (x:antes, depois)
    where (antes, depois) = quebra f xs

-- meu reverse
reverso :: [t] -> [t]
reverso = reverso' []
    where
    reverso' acc [] = acc
    reverso' acc (x:xs) = reverso' (x:acc) xs 

-- meu foldl
dobrae :: (t -> u -> t) -> t -> [u] -> t
dobrae _ acc [] = acc
dobrae f acc (x:xs) = dobrae f (f acc x) xs

-- meu foldr
dobrad :: (u -> t -> t) -> t -> [u] -> t
dobrad _ acc [] = acc
dobrad f acc (x:xs) = f x (dobrad f acc xs)
-- meu length
tamanho :: [t] -> Int
tamanho = dobrae (\x _ -> x + 1) 0

-- meu min
mini :: Ord t => t -> t -> t
mini a b
    | a < b     = a
    | otherwise = b 

-- meu minimum 
menor :: Ord t => [t] -> t
menor (x:xs) = menor' x xs
    where 
        menor' m []     = m
        menor' m (x:xs) = menor' (mini m x) xs   

-- meu filter
filtro :: (t -> Bool) -> [t] -> [t]
filtro _ [] = []
filtro f (x:xs) 
    | f x = x : filtro f xs
    | otherwise = filtro f xs

-- meu sum
soma :: Num t => [t] -> t
soma = dobrae (+) 0

-- meu elem
elementoDe :: Eq t => t -> [t] -> Bool
elementoDe = algum . (==)

-- contrario do meu elem
foraDe :: Eq t => t -> [t] -> Bool
foraDe = todos . (/=)

-- remove a primeira ocorrencia
sem :: Eq t => [t] -> t -> [t]
sem (x:xs) y 
    | x == y = xs
    | otherwise = x : (xs `sem` y)

-- retorna primeira ocorrencia do valor, quebra se não achar
primeiro :: (t -> Bool) -> [t] ->  t
-- primeiro _ [] = erro
primeiro f (x:xs) 
    | f x = x
    | otherwise = primeiro f xs

-- meu lookup (-maybe)
procura :: Eq c => c -> [(c, v)] -> v
procura c = snd . primeiro ((c==) . fst)

-- meu sort
ordena :: (Ord t, Eq t) => [t] -> [t]
ordena []   = []
ordena l    = primeiro : ordena (l `sem` primeiro)
    where primeiro = menor l     

-- muito útil
mudaSeAchar :: (t -> Bool) -> (t -> t) -> [t] -> ([t], Bool)
mudaSeAchar _ _ [] = ([], False)
mudaSeAchar alvo mud (x:xs) = 
    if alvo x then (mud x : xs, True)
    else (x : resto, veredito) 
    where (resto, veredito) = mudaSeAchar alvo mud xs

-- muito útil também
tiraSeAchar :: (t -> Bool) -> [t] -> ([t], Bool)
tiraSeAchar _ [] = ([], False)
tiraSeAchar alvo (x:xs) = 
    if alvo x then (xs, True)
    else (x : resto, veredito) 
    where (resto, veredito) = tiraSeAchar alvo xs

-- se os elementos são distintos e crescentes
distintamenteOrdenado :: Ord t => [t] -> Bool
distintamenteOrdenado (f : s : rst) = f < s && distintamenteOrdenado (s : rst)
distintamenteOrdenado _ = True

-- se os elementos estão em ordem crescente
ordenado :: Ord t => [t] -> Bool
ordenado (f : s : rst) = f <= s && distintamenteOrdenado (s : rst)
ordenado _ = True


{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

-- presumindo que cada nome de medicamento só aparece uma vez no estoque
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qtd est = if comprou then estoque else (med, qtd) : estoque
    where
        (estoque, comprou) = mudaSeAchar ((med==) . fst) compra est
        compra (m, q) = (m, q + qtd)

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

-- presumindo que não se pode ter quantidades negativas de um medicamento
tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med est = if tomou then Just estoque else Nothing
    where
        (estoque, tomou) = mudaSeAchar temRemedio toma est
        temRemedio (m, q) = med == m && q > 0
        toma (m, q) = (m, q - 1)

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento med ((m, q):e) 
    | m == med = q
    | otherwise = consultarMedicamento med e

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

-- presume que todos os medicamentos são distintos, mas que podem vir fora de ordem
demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos r = ordena medQuant
    where 
        medQuant = mapeia (\(m, l) -> (m, tamanho l)) r   -- [(Medicamento, [Horario])] -> [(Medicamento, Quantidade)]


{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

receituarioValido :: Receituario -> Bool
receituarioValido r = distintamenteOrdenado (mapeia fst r) && todos (distintamenteOrdenado . snd) r

planoValido :: PlanoMedicamento -> Bool
planoValido r = distintamenteOrdenado (mapeia fst r) && todos (distintamenteOrdenado . snd) r

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

soMedicar :: Cuidado -> Bool
soMedicar c = case c of
    Comprar _ _ -> False
    Medicar _ -> True 

getMedicamento :: Cuidado -> String
getMedicamento c = case c of
    Comprar m _ -> m
    Medicar m -> m

--separa os cuidados em [[Medicamento](Comprar), [Medicamento](Medicar)]
separaCuidados :: [Cuidado] -> ([Medicamento], [Medicamento])
separaCuidados l = (mapeia getMedicamento comprar, mapeia getMedicamento medicar)
    where 
        (comprar, medicar) = (filtro (not . soMedicar) l, filtro soMedicar l)

-- se algum medicamento comprado é ministrado no mesmo horario
compraEMedica :: ([Medicamento], [Medicamento]) -> Bool
compraEMedica (comp, med) = algum (`elementoDe` comp) med


plantaoValido :: Plantao -> Bool
plantaoValido p = cond1 && cond2 && cond3
    where
        cond1 = distintamenteOrdenado (mapeia fst p)
        cond2 = todos (not . compraEMedica) (mapeia (separaCuidados . snd) p)
        cond3 = todos ordenado (mapeia (mapeia getMedicamento) medicacoes)  -- para cada horario cada medicação ministrada é listada em ordem
        medicacoes = mapeia (filtro soMedicar . snd) p                      -- Plantao -> [[Cuidado(Medicar)]]



{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

-- agrupa une os valores de chaves iguais adjacentes
agrupa :: Eq c => [(c, [v])] -> [(c, [v])]
agrupa = dobrad agrupa' []
agrupa' x [] = [x]
agrupa' (c', [v']) ((c, v) : rst) -- caso em que v' é vetor unitário
    | c' == c   = (c, v' : v) : rst
    | otherwise = (c', [v']) : (c, v) : rst

-- incremento pra funcionar com dicionarios completos, ineficiente portanto último caso
agrupa' (c', v') ((c, v) : rst)
    | c' == c   = (c, v' ++ v) : rst
    | otherwise = (c', v') : (c, v) : rst

-- troca as chaves e valores de papel e ordena
valores2chaves :: (Eq k, Eq v, Ord k, Ord v) => [(k,[v])] -> [(v,[k])]
valores2chaves = agrupa . ordena . v2c
    where 
        -- v2c troca chave por valor, mas cada valor é um vetor unitário
        v2c [] = [] 
        v2c dict = [(v, [c]) | (c, vs) <- dict, v <- vs]


-- presume que o receituário dado é válido sob as condições da questão 5
geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario = valores2chaves

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

-- presume que o plano dado é válido sob as condições da questão 5
geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano = valores2chaves

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

plantao2cuidados :: Plantao -> [Cuidado]
plantao2cuidados p = [c | (_, cs) <- p, c <- cs]

executaPlantao' :: [Cuidado] -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos    
executaPlantao' [] e = Just (ordena e)
executaPlantao' (c:cs) e = case c of
    Comprar m q -> executaPlantao' cs (comprarMedicamento m q e)
    Medicar m   -> if ehJust tomado 
        then executaPlantao' cs (deJust tomado)
        else Nothing
        where tomado = tomarMedicamento m e


executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = executaPlantao' . plantao2cuidados
    
{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

-- presume que os medicamentos devem ser ministrados nos mesmos horários descritos no plano
-- também presume que se um plantão medicar fora do plano não deixa de satisfazê-lo
satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plt pln est = todos (satisfazPlano medicadas) pln && ehJust (executaPlantao plt est)
    where 
        medicadas = [(h, m) | (h, cs) <- plt, m <- snd (separaCuidados cs)]
        satisfazPlano _ (h, []) = True
        satisfazPlano p (h, m:ms) = achou && satisfazPlano pSemM (h, ms) 
            where (pSemM, achou) = tiraSeAchar (== (h, m)) p

{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

instance Eq Cuidado where
    (==) (Medicar m1) (Medicar m2) = m1 == m2
    (==) (Comprar m1 q1) (Comprar m2 q2) = m1 == m2 && q1 == q2
    (==) _ _ = False

instance Ord Cuidado where
    (<=) a b = getMedicamento a <= getMedicamento b


-- presume que para todos os testes é possível fazer o plano
-- presume também que os horários vão de 0 a 23
plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto p _ = ordena (mapeia (\(h, l) -> (h, ordena l)) tudo)
    where 
        horas       = [0..23]
        receituario = geraReceituarioPlano p 
        demanda     = (`procura` demandaMedicamentos receituario)
        horaMed     = mapeia (\(m, hs) -> (primeiro (`foraDe` hs) horas, m)) receituario
        horaCompra  = mapeia (\(h, m) -> (h, [Comprar m (demanda m)])) horaMed
        compras     = agrupa (ordena horaCompra)
        medicadas   = mapeia (\(h, ms) -> (h, [Medicar m | m <- ms])) p
        tudo        = agrupa (compras ++ medicadas)

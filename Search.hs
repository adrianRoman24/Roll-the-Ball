{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Nil | NodeState {state :: (s),
								 action :: (Maybe a),
								 parent :: (Node s a),
								 depth :: (Int),
								 children :: ([Node s a])}
								 deriving (Eq)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState node = state node

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent Nil = Nothing
nodeParent node = Just $ parent node

nodeDepth :: Node s a -> Int
nodeDepth Nil = 0
nodeDepth node = depth node

nodeAction :: Node s a -> Maybe a
nodeAction Nil = Nothing
nodeAction node = action node

nodeChildren :: Node s a -> [Node s a]
nodeChildren Nil = []
nodeChildren node = children node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace st = newNode
					where
					newNode = NodeState st Nothing Nil 0 newChildren
					newChildren = createChildren (successors st) newNode
					createChildren succs par
							| null succs = []
							| otherwise = thisNode : (createChildren (tail succs) par)
							where
							secondChildren = createChildren (successors newState) thisNode
							thisNode = NodeState newState newAction par (1 + depth par) secondChildren
							newState = snd $ head succs
							newAction = Just $ fst $ head succs

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: (Ord s, Eq a) => Node s a -> [([Node s a], [Node s a])]
bfs node = ([node], [node]) : createBfs [state node] [node] -- q este coada (frontiera)
		where
		filterChildren visited childrenList = case childrenList of
			[] -> []
			list -> if elem (state (head list)) visited then filterChildren visited (tail list) else head list : filterChildren visited (tail list)
		createBfs visited q
				| null q = []
				| otherwise = (newChildren, newQueue) : (createBfs (visited ++ (selectStates newChildren)) newQueue)
				where
				selectStates list = case list of
					[] -> []
					x -> state (head x) : selectStates (tail x)
				newChildren = filterChildren visited $ children (head q)
				newQueue = addLast (deleteFirst q) newChildren
		deleteFirst queue = tail queue
		addLast queue list = case list of
			[] -> queue
			l -> addLast (queue ++ [head l]) (tail l)


{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: (Ord s, Eq a) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS node1 node2
	| state node1 == state node2 = (node1, node2)
	| otherwise = getRoute 1 node1 node2 ---de schimbat
		where
		getRoute currentIter n1 n2
			| nn1 == Nil && nn2 == Nil = getRoute (currentIter + 1) n1 n2
			| otherwise = (nn1, nn2)
				where
				(nn1, nn2) = searchAll list1 list2
				list1 = snd (head (reverse (take currentIter (bfs n1))))
				list2 = snd (head (reverse (take currentIter (bfs n2))))
				searchAll l1 l2
					| null l1 = (Nil, Nil)
					| otherwise = if (rez1 == Nil && rez2 == Nil) then searchAll (tail l1) l2 else (rez1, rez2)
						where
						(rez1, rez2) = search (head l1) l2
					
				search node list
					| null list = (Nil, Nil)
					| otherwise = if state node == state (head list) then (node, (head list)) else search node (tail list)
				


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: (Eq a, Eq s) => Node s a -> [(Maybe a, s)]
extractPath n1 = reverse $ map getAS $ parents n1
		where
		getAS node = (action node, state node)
		parents node
			| node == Nil = []
			| otherwise = node : parents (parent node)



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s, Eq a)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve s1 s2 = r1 ++ r3
		where
		node1 = createStateSpace s1
		node2 = createStateSpace s2
		(inters1, inters2) = bidirBFS node1 node2
		r1 = extractPath inters1 --lista1
		r2 = reverse $ tail $ extractPath inters2 --lista2
		addFinal list
			| null list = []
			| otherwise = case (fst (head list)) of
				Nothing -> (Nothing, (snd (head list))) : (addFinal (tail list))
				(Just aa) -> ((Just newAA), newSS) : (addFinal (tail list))
					where
					(newAA, newSS) = reverseAction (aa, (snd (head list)))
		r3 = addFinal r2
-- Traveling from Java to Haskell

-- Quite a long distance to travel but many times we were already mentally there:
--   lambdas - Java 8
--   immutable objects - concurrency, safety, reasoning, memory management, Guava, Vavr
--   value objects - POJOs, Lombok
--   limiting side effects - Stream API, reactive programming, refactoring
--   lazy evaluation - suppliers, Callable
--   testability - given/when/then
--

-- Some things remain the same
--   memory management/garbage collection
-- Some are fundamentally different
--  pure functional
--  lazy
--  strongly statically typed
--  type inference
--  no inheritence
--  no primitive/complex data types distinction
--  compiled to native code (but nowadays we ship docker containers anyway?)
--  interpreter (REPL) available
--  more general purpose language

-- Some say Haskell is difficult
--   Monoids, Semigroups, Rings, Monads, Applicative
--   monoid in the category of endofunctors
--   Yoneda lemma

-- But what about OOP?
--  "owijaczka"
--  "to zależy"
--  AbstractSingletonProxyFactoryBean, SimpleRemoteStatelessSessionProxyFactoryBean, @EnableMagic
--  polymorphic methods in arguments
--  boxing
--  switch/case
--  Java generics, super/extends, covariant/contravariant, PECS
--  UML
--  static source code analysis

-- Java Specification Requests -> Haskell language extensions
-- Haskell 2010 specification of base language supplemented with
{-# LANGUAGE RankNTypes, FlexibleInstances, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleContexts, GADTs, FunctionalDependencies #-}

-- Java package -> Haskell module
-- Generally *.java files in Java maps to fragments of *.hs files in Haskell.
-- Java package directories map to a single *.hs file in Haskell.
-- Java packages are grouped into libraries, Haskell modules are grouped into packages.
module Java2Haskell where

-- Java imports on the class level -> Haskell imports on the module level
import Data.Ix
import Data.Time
import Data.Maybe
import qualified GoogleDirections as G
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS

-- Java static imports on the class/interface members level -> Haskell imports on the function level
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- Java enum -> Haskell Algebraic Data Type - sum
data Fuel = Diesel | Gas | LPG
data Ticket = Ticket20Min | Ticket40Min | Ticket60Min
data Person = Child | Adult

-- Java primitive -> Haskell ADT
-- Java boxing issues -> no such thing in Haskell
-- data Bool = True | False

-- Java complex type -> Haskell ADT - product
data Coordinates = Coordinates Float Float

-- Java "beans" -> Haskell ADT - product with record syntax
data Car = Car { fuel :: Fuel, fuelConsumptionLitresPerKilimeter :: Float }

-- Java getters -> Haskell functions
-- Java Lombok annotations -> Haskell functions for free
getCarFuel :: Car -> Fuel
getCarFuel = fuel


-- Java setters -> Haskell functions, but pure ones, all values immutable, modified copy returned, original value intact
-- Java sort methods (in-routePointLocation, sorted copy returned, or both) -> Haskell persistent data types
setCarFuelConsumption :: Car -> Float -> Car
setCarFuelConsumption car consumption = car { fuelConsumptionLitresPerKilimeter = consumption }

-- Java immutable classes -> Haskell ADTs

-- Java polymorphism (having different "shapes") -> Haskell ADT - union types, low-level polymorhisms
data Transport = CarTransport Car | BikeTransport | PublicTransport

data Location = AtCoordinates Coordinates | AtAddress String


-- Java inheritance (sharing common members) -> Haskell ADT product of sum, “composition over inheritance” enforced
data Trip = Trip {
  tripPersons :: [Person],
  tripTransport :: Transport
}


-- Java Object.equals method -> Haskell pattern mathing, sometimes...
willNeedLPG :: Trip -> Bool
willNeedLPG Trip{ tripTransport=CarTransport Car{ fuel = LPG } } = True
willNeedLPG _ = False

-- Java null/Optional -> Haskell Maybe
-- null as "billion dollar mistake"
willUseCar :: Trip -> Maybe Car
willUseCar Trip{ tripTransport=(CarTransport car) } = Just car
willUseCar _ = Nothing

-- Java constants -> Haskell no argument functions
childTicketDiscount :: Float
childTicketDiscount = 0.5

-- Java static methods without side effects -> Haskell functions
-- referentially transparent, can be equasionally reasoned, inlined, no side effects, no mutations
-- Java switch/case statement -> Haskell ADTs pattern matching
-- Switch/case considered harmful in Java? http://wiki.c2.com/?CaseStatementsConsideredHarmful
-- It's anyway messy: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/switch.html
ticketDiscount :: Person -> Float
ticketDiscount Adult = 0
ticketDiscount Child = childTicketDiscount

-- Java loops -> Haskell recursion
-- Java if/then/else -> Haskell guards
ticketsNeeded :: NominalDiffTime -> [Ticket]
ticketsNeeded diffTime
  | diffTime > 40 * secondsInMinute = Ticket60Min:ticketsNeeded (diffTime - 60 * secondsInMinute)
  | diffTime > 20 * secondsInMinute = Ticket40Min:ticketsNeeded (diffTime - 40 * secondsInMinute)
  | diffTime > 0 = Ticket20Min:ticketsNeeded (diffTime - 20 * secondsInMinute)
  | otherwise = []
    where
      -- Java private class members -> Haskell where clause
      secondsInMinute = 60

-- C typedef (non existing in Java) -> Haskell type synonyms
type RouteKilometers = Float -- adds specific meaning to not specific types, not checked by the compiler

-- Java wrapper classes -> Haskell newtype
-- narrowing down possible values and functions
-- type with single constructor with single parameter
newtype Cost = Cost Float -- adds specific meaning to not specific types, checked at compile time

toCost :: Float -> Maybe Cost
toCost f
  | f >= 0 = Just (Cost f)
  | otherwise = Nothing

multiplyCost :: Cost -> Float -> Cost
multiplyCost (Cost a) m = Cost (m * a)

applyDiscount :: Cost -> Float -> Cost
applyDiscount cost discount = multiplyCost cost (1 - discount)

-- Java design patterns -> Haskell high-level abstraction type classes
instance Monoid Cost where
  mempty = Cost 0
  mappend (Cost a) (Cost b) = Cost (a + b)

-- Java: Stream.of(1,2,3).reduce(0,(a,b)->a+b); -> Haskell: mconcat
noCost :: Cost
noCost = mempty

sumCost :: Cost -> Cost -> Cost
sumCost = mappend

sumCosts :: [Cost] -> Cost
sumCosts = mconcat


-- Java interfaces -> Haskell type classes
-- Java implementating classes -> Haskell instances
-- Java polymorhisms (argument type-specific behavior) -> Haskell ad-hoc polymorhisms
class HasCost t where
  cost :: t -> Cost
  -- Java default method implementations -> Haskell default function implementations
  cost _ = noCost

-- Java open-closed principle, visitor pattern - instances defined separately to data type definition
instance HasCost Fuel where
  cost LPG = Cost 2.50
  cost Diesel = Cost 5.60
  cost Gas = Cost 5.40

instance HasCost Ticket where
  cost Ticket20Min = Cost 2.80
  cost Ticket40Min = Cost 4
  cost Ticket60Min = Cost 5

-- Java toString() -> Show typeclass, open/closed principle
instance Show Cost where
  show (Cost a) = show a ++ " PLN"

instance Show Location where
  show (AtCoordinates (Coordinates latitude longitude)) = show latitude ++ "," ++ show longitude
  show (AtAddress address) = address




-- Java muliple parameter polymorhisms, Visitor design pattern -> Haskell multi-param typeclasses
class GeneratesCost a b | a -> b where
  generatedCost :: a -> b -> Cost

instance GeneratesCost Person NominalDiffTime where
  -- Java Bifunction -> Haskell function infix notation
  generatedCost person diffTime = sumCosts (fmap cost (ticketsNeeded diffTime)) `applyDiscount` ticketDiscount person

instance GeneratesCost Car RouteKilometers where
  generatedCost (Car fuel fuelConsumptionLitresPerKilimeter) kms = cost fuel `multiplyCost` kms `multiplyCost` fuelConsumptionLitresPerKilimeter

data Leg = Leg {
  legDiffTime :: NominalDiffTime,
  legKilometers :: RouteKilometers
}

instance GeneratesCost Trip Leg where
  generatedCost trip leg = case tripTransport trip of
    CarTransport car -> generatedCost car (legKilometers leg)
    BikeTransport -> noCost
    PublicTransport -> foldMap (`generatedCost` legDiffTime leg) (tripPersons trip)

-- Java generics -> Haskell parametrized data types
data Route a b = Route a [(a, b)]

-- Java static methods with parameterized types -> Haskell parametric polymorphism
routeSections :: Route a b -> [(a, a, b)]
routeSections (Route a []) = []
routeSections (Route a0 ((a1, b):rest)) = (a0, a1, b):routeSections (Route a1 rest)

routeReturn :: Route a b -> Maybe (a, a)
routeReturn (Route a []) = Nothing
routeReturn (Route a [(an, _)]) = Just (a, an)
routeReturn (Route a (_:rest)) = routeReturn (Route a rest)

-- Java generics 'extends' -> Haskell typeclass constraints
instance (Show a, Show b) => Show (Route a b) where
  show route = sectionsString ++ returnString
    where
      sectionsString = foldMap (\(a1, a2, b) -> show a1 ++ "->" ++ show a2 ++ "\n" ++ show b ++ "\n") (routeSections route)
      returnString = maybe "" (\(a1, a2) -> show a1 ++ "->" ++ show a2 ++ "\n") (routeReturn route)

-- TODO
-- Java ternary operator ?: -> Haskell if then else
-- Laziness - supplier, every expression is a supplier, if/else/case as function
-- https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.25
-- TODO: instance HasCost BikeTransport where
-- TODO: total functions - no nulls, no errors
-- TODO: Haskell case
-- TODO: Recursive data types
-- TODO: pattern matching
-- TODO: Java "Data Transfer Objects" -> Haskell one-liners, fosters interface segregation and reduces coupling
-- TODO
-- Java buider - Haskel type classes
-- http://blog.ezyang.com/2010/05/design-patterns-in-haskel/
-- TODO
-- Java Abstract Factory -> Haskell type class
-- class TripFactory t where
--   createTrip :: t -> Day -> Movement -> Trip
-- TODO
-- Java fromString()? -> Read typeclass
-- TODO
-- Java equals() -> Eq typeclass
-- TODO
-- Java Comparable -> Haskell Ord typeclass
-- Java Integer.MIN_VALUE etc. -> Haskell Bounded typeclass
-- TODO
-- Java Enum.valueOf() -> Haskell Read typeclass
-- Java Enum.name() -> Haskell Show typeclass
-- Java Enum.ordinal() -> Haskell Enums
-- TODO
-- Eq, Read, Show, Ord, Enum, Ix, Bounded
-- TODO: list comprehensions
-- Java “single responsibility principle” -> Haskell polymorphic functions - http://degoes.net/articles/insufficiently-polymorphic
quicksort :: Ord a => [a] -> [a] -- comparing elements decoupled from constructing list, also more reusable
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

data RoutePoint = RoutePoint {
  routePointLocation :: Location,
  routePointTime :: UTCTime,
  routePointCost :: Cost
} deriving Show

data Stop = Stop {
  stopLocation :: Location,
  stopStayMinutes :: NominalDiffTime
}

-- data Route = Route {
--   start :: RoutePoint,
--   transport' :: Transport,
--   stops :: [Stop]
-- }

-- TODO
-- -> Haskell functor
yyy' ::  Trip -> Stop -> RoutePoint -> Leg -> RoutePoint
yyy' trip stop routePoint leg = let
  cost = generatedCost trip leg `mappend` routePointCost routePoint
  time = addUTCTime (legDiffTime leg + 60 * stopStayMinutes stop) (routePointTime routePoint)
  in RoutePoint (stopLocation stop) time cost

yyy ::  Trip -> Stop -> RoutePoint -> Maybe Leg -> Maybe RoutePoint
yyy trip stop routePoint = fmap (yyy' trip stop routePoint)

-- TODO
-- -> Haskell applicative functor

-- -> Haskell monad, do notation
foo :: Maybe G.GoogleDirectionsResponse -> Maybe Leg
foo mGoogleDirectionsResponse = do
  response <- mGoogleDirectionsResponse
  firstRoute <- listToMaybe (G.routes response)
  firstLeg <- listToMaybe (G.legs firstRoute)
  let diffTimeSeconds = (fromInteger . G.value . G.duration) firstLeg
  let distanceMeters = (fromInteger . G.value . G.distance) firstLeg
  return Leg{ legDiffTime=diffTimeSeconds, legKilometers=distanceMeters/1000 }


xxx :: HTTP.Manager -> String -> Trip -> Stop -> RoutePoint -> IO (Maybe RoutePoint)
xxx manager googleApiKey trip stop routePoint = do
  mGoogleDirectionsResponse <- G.getGoogleDirections manager googleApiKey (transport2Mode (tripTransport trip)) (show (routePointLocation routePoint)) (show (stopLocation stop)) ((round . utcTimeToPOSIXSeconds) (routePointTime routePoint))
  return $ yyy trip stop routePoint (foo mGoogleDirectionsResponse)
    where
      transport2Mode :: Transport -> G.Mode
      transport2Mode (CarTransport _) = G.Driving
      transport2Mode BikeTransport = G.Bicycling
      transport2Mode PublicTransport  = G.Transit

-- Java dependency injection -> Haskell partial function application

main :: IO ()
main = do
  manager <- HTTP.newManager TLS.tlsManagerSettings
  apiKey <- readFile ".apiKey"
  let xxx' = fold''' manager apiKey
  now <- getCurrentTime
  let family = [Adult, Adult, Child]
  let carTransport = CarTransport Car{ fuel = LPG, fuelConsumptionLitresPerKilimeter = 0.11 }
  let route = Route (AtAddress "Złota Podkowa Kraków") [(AtAddress "Pawia 9 Kraków", 8 * 60), (AtCoordinates (Coordinates 50.067938 19.901295), 60), (AtAddress "Lindego 1C, Kraków", 30)]
  stop <- xxx' Trip{ tripPersons=family, tripTransport=carTransport } (AtAddress "Złota Podkowa Kraków") now [
    Stop{ stopLocation = AtAddress "Pawia 9 Kraków", stopStayMinutes = 8 * 60},
    Stop{ stopLocation = AtCoordinates (Coordinates 50.067938 19.901295), stopStayMinutes = 1 * 60},
    Stop{ stopLocation = AtAddress "Lindego 1C, Kraków", stopStayMinutes = 30}]
  print stop

-- Java iteration over collection -> Haskell fold
fold''' :: HTTP.Manager -> String -> Trip -> Location -> UTCTime -> [Stop] -> IO (Maybe RoutePoint)
fold''' manager googleApiKey trip startLocation startTime = foldl appendStop startRoutePoint
  where
    startRoutePoint :: IO (Maybe RoutePoint)
    startRoutePoint = return $ Just RoutePoint{ routePointTime=startTime, routePointLocation = startLocation, routePointCost = noCost}
    appendStop :: IO (Maybe RoutePoint) -> Stop -> IO (Maybe RoutePoint)
    appendStop iomrp stop = do
      mrp <- iomrp
      case mrp of
        Nothing -> return Nothing
        Just rp -> xxx manager googleApiKey trip stop rp

-- http://cheatsheet.codeslower.com/CheatSheet.pdf

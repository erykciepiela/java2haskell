-- Traveling from Java to Haskell

-- Java Specification Requests -> Haskell language extensions
--   - Haskell 2010 specification of base language
--   - supplemented with language extensions
{-# LANGUAGE RankNTypes, FlexibleInstances, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleContexts, GADTs, FunctionalDependencies #-}

-- Java package -> Haskell module
--   - Java package is a directory, Haskell module is a single *.hs file
--   - *.java files map to fragments of *.hs files
--   - Java packages grouped into libraries, Haskell modules grouped into packages
module Main where

-- Java imports on the package level -> Haskell imports on the module level
--   - Java imports only releases from refering by qualified names, you can still refer to anything in the classpath
--   - Haskell imports are required in order to use modules, you can refer to only imported things
--   - explicit module dependencies in Haskell
import Data.Ix
import Data.Time
import Data.Maybe

-- Java qualified names -> Haskell qualified imports
--   - aliases for qualified names
import qualified GoogleDirections as G
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS

-- Java imports on the class/method level -> Haskell imports on the type/function level
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- Java enum -> Haskell Algebraic Data Type - sum
data Fuel = Diesel | Gas | LPG deriving Show -- `deriving` will be explained later
data Ticket = Ticket20Min | Ticket40Min | Ticket60Min
data Person = Adult | Child | SmallChild deriving Show

-- Java complex type -> Haskell ADT - product
data Coordinates = Coordinates Float Float

-- Java primitives -> Haskell ADT
--   - all data as ADT in Haskell e.g. `data Bool = True | False`
--   - Java boxing issues
--   - no objects, only immutable values in Haskell
--   - Java immutable value objects -> Haskell ADTs

-- Java "beans" -> Haskell ADT - product with record syntax
data Car = Car { fuel :: Fuel, fuelConsumptionLitresPerKilometer :: Float } deriving Show

-- Java getters -> Haskell functions
--   - Java Lombok annotations
--   - Haskell getter functions for free
getCarFuel :: Car -> Fuel
getCarFuel = fuel

-- Java setters -> Haskell functions
--  - pure functions, all values immutable, modified copy returned, original value left intact
--  - Haskell persistent data types
--  - how the sort methods work? in-place? sorted copy returned? both?
setCarFuelConsumption :: Car -> Float -> Car
setCarFuelConsumption car consumption = car { fuelConsumptionLitresPerKilometer = consumption }

-- Java polymorphism (having different "shapes") -> Haskell ADT sum
--   - low-level polymorhisms
--   - union types
data Transport = CarTransport Car | BikeTransport | PublicTransport deriving Show

data Location = AtCoordinates Coordinates | AtAddress String

-- Java inheritance (sharing common members) -> Haskell ADT product of sum
--   - composition over inheritance enforced
--   - no inheritance in Haskell
data Travelers = Travelers {
  persons :: [Person],
  transport :: Transport
} deriving Show

-- C typedef -> Haskell type synonyms
--   - non existing in Java
--   - adds specific meaning to not specific types
--   - not checked by the compiler
type LengthKilometers = Float
type FuelConsumptionLitresPerKilometer = Float
type FuelConsumptionLitres = Float
type Exception = String
type DurationSeconds = NominalDiffTime

fuelConsumption :: LengthKilometers -> FuelConsumptionLitresPerKilometer -> FuelConsumptionLitres
fuelConsumption km lpkm = lpkm * km

-- Java constants -> Haskell values
--   - every value is constant
childTicketDiscount :: Float
childTicketDiscount = 0.5

smallChildTicketDiscount :: Float
smallChildTicketDiscount = 1

-- Java static methods without side effects -> Haskell functions
--   - referential transparency, equasional reasoning, no side effects, no mutations
-- Java switch/case statement, polymorhisms -> Haskell ADTs pattern matching
--   - Java switch/case applicable only to some types: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/switch.html
--   - otherwise favour polymorhisms over witch/case? http://wiki.c2.com/?CaseStatementsConsideredHarmful
--   - or use reflection...
--   - Haskell pattern matching more powerful than that, many examples below
ticketDiscount :: Person -> Float
ticketDiscount Adult = 0
ticketDiscount Child = childTicketDiscount
ticketDiscount SmallChild = smallChildTicketDiscount

-- Java switch/case statement over multiple parameters -> Haskell ADTs pattern matching
--   - Java nested switch/case or polymorhisms with Visitor pattern
--   - you better cover it with parameterized unit test!
--   - expressiveness of pattern matching: wildcards
canBeTransported :: Person -> Transport -> Bool
canBeTransported Adult _ = True
canBeTransported Child _ = True
canBeTransported SmallChild (CarTransport _) = True
canBeTransported SmallChild PublicTransport = True
canBeTransported SmallChild _ = False

-- Java if/then/else if/else -> Haskell guards
isNonNegative :: Float -> Bool
isNonNegative f
  | f >= 0 = True
  | otherwise = False

-- Java loops -> Haskell recursion
--   - no loops in Haskell
--   - generally Java flow control statements map to Haskell functions
ticketsNeeded :: DurationSeconds -> [Ticket]
ticketsNeeded duration
  | duration > 40 * secondsInMinute = Ticket60Min : ticketsNeeded (duration - 60 * secondsInMinute)
  | duration > 20 * secondsInMinute = Ticket40Min : ticketsNeeded (duration - 40 * secondsInMinute)
  | duration > 0 = Ticket20Min : ticketsNeeded (duration - 20 * secondsInMinute)
  | otherwise = []
    where
      -- Java private class members -> Haskell where section
      secondsInMinute = 60

-- Java wrapper classes -> Haskell newtype
--   - narrowing down possible values and applicable functions
--   - type with single constructor with single parameter
--   - adds specific meaning to not generic types
--   - checked by the compiler
newtype Cost = Cost Float

addCosts :: Cost -> Cost -> Cost
addCosts (Cost a) (Cost b) = Cost (a + b)

noCost :: Cost
noCost = Cost 0

-- Java exceptions -> Haskell Either
--   - `data Either a b = Left a | Right b`
--   - Right for constructing return value, Left for constructing exception (convention)
-- Java ternary operator ?: -> Haskell if/then/else
--   - then/else parts lazily evaluated as all expressions in Haskell
toCost :: Float -> Either Exception Cost
toCost f = if isNonNegative f then Right (Cost f) else Left "Cost cannot be negative"

multiplyCost :: Cost -> Float -> Either Exception Cost
multiplyCost (Cost a) m = toCost (m * a)

applyDiscount :: Cost -> Float -> Either Exception Cost
applyDiscount cost discount = multiplyCost cost (1 - discount)

-- Java polymorhisms (argument type-specific behavior) -> Haskell typeclasses with ad-hoc polymorhisms
--   - Java interfaces -> Haskell type classes
--   - Java implementating classes -> Haskell instances
--   - Java toString() -> Show typeclass
--   - instances defined separately to data type definition
--   - open/closed principle
instance Show Cost where
  show (Cost a) = show a ++ " PLN"

instance Show Location where
  show (AtCoordinates (Coordinates latitude longitude)) = show latitude ++ "," ++ show longitude
  show (AtAddress address) = address

class HasCost t where
  cost :: t -> Either Exception Cost
  -- Java default method implementations -> Haskell default function implementations
  cost _ = Right noCost

instance HasCost Fuel where
  cost LPG = toCost 2.50
  cost Diesel = toCost 5.60
  cost Gas = toCost 5.40

instance HasCost Ticket where
  cost Ticket20Min = toCost 2.80
  cost Ticket40Min = toCost 4
  cost Ticket60Min = toCost 5

-- Most of Java design patterns -> Haskell functions or typeclasses with ad-hoc polymorhisms
--   - Java Stream.reduce(a0, (a, b) -> a + b) -> Haskell Monoid, mconcat
--   - typeclasses more expressive than interface, Monoid not expressible as interface
instance Monoid Cost where
  mempty = noCost
  mappend = addCosts

sumCosts :: [Cost] -> Cost
sumCosts = mconcat

-- Java impure computations -> Haskell Monads
--   - in Haskell impure computation is expressed in types as `a -> m b` as opposed to pure computation as `a -> b`
--   - e.g. `a -> Maybe b`, `a -> Either e b`, `a -> [b]`
--   - `impure` doesn't necessarily mean `with side effects`
--   - Monads allow for composition of impure computations: `(>>=) :: m a -> (a -> m b) -> m b`
fuelCost :: Fuel -> FuelConsumptionLitresPerKilometer -> LengthKilometers -> Either Exception Cost
fuelCost fuel fuelConsumptionLitresPerKilometer kms = do
  fuelLitreCost <- cost fuel
  fuelKmCost <- multiplyCost fuelLitreCost fuelConsumptionLitresPerKilometer
  multiplyCost fuelKmCost kms

-- Java producers -> Haskell Functors
--   - Java generics variance PECS mnemonic - "producer - extends, consumer - super" - producer - (covariant) functor, consumer - (contravariant) cofunctor
--   - producer examples: collections (e.g. Haskell list []), functions returning given type
--   - Functors allow for mapping produced values `fmap :: (a -> b) -> f a -> f b`
ticketCosts :: DurationSeconds -> [Either Exception Cost]
ticketCosts duration = fmap cost (ticketsNeeded duration)

-- Java bounded type parameters in interface definition -> classtype constraints in instance definition
instance Monoid b => Monoid (Either a b) where
  mempty = Right mempty
  mappend (Left a) _ = Left a
  mappend _ (Left a) = Left a
  mappend (Right b1) (Right b2) = Right (mappend b1 b2)

ticketsCost :: DurationSeconds -> Either Exception Cost
ticketsCost duration = mconcat (ticketCosts duration)

discountTicketsCost :: Person -> DurationSeconds -> Either Exception Cost
discountTicketsCost person duration  = do
  cost <- ticketsCost duration
  applyDiscount cost (ticketDiscount person)

-- Java muliple parameter polymorhisms via Visitor design pattern -> Haskell multi-param typeclasses
--   - not supported directly in Java, possible on top of Java with Visitor pattern
--   - supported by Haskell typeclasses
--   - Haskell functional dependencies
class GeneratesCost a b | a -> b where
  generatedCost :: a -> b -> Either Exception Cost

instance GeneratesCost Person DurationSeconds where
  generatedCost = discountTicketsCost

instance GeneratesCost Car LengthKilometers where
  generatedCost (Car fuel fuelConsumptionLitresPerKilometer) = fuelCost fuel fuelConsumptionLitresPerKilometer

data Leg = Leg {
  legSeconds :: DurationSeconds,
  legKilometers :: LengthKilometers
}

-- Java switch/case/polymorhisms -> Haskell pattern matching
--   - Haskell case block
--   - Java polymorhisms approach would introduce coupling of Transport with Leg because we would need method `Transport.generateCost(Leg): Cost`
instance GeneratesCost Travelers Leg where
  generatedCost travelers leg = case transport travelers of
    CarTransport car -> generatedCost car (legKilometers leg)
    BikeTransport -> Right noCost
    PublicTransport -> foldMap (`generatedCost` legSeconds leg) (persons travelers)

-- Java type parameters in class definition -> Haskell parametrized data types
data RouteStop a = RouteStop {
  routeStopLocation :: Location,
  routeStopWhat :: a
}

data Route a = Route {
  routeBaseLocation :: Location,
  routeStops :: [RouteStop a]
}

-- Java static methods with type parameters -> Haskell parametric polymorphism
routeStopsCount :: Route a -> Int
routeStopsCount route = length (routeStops route)

-- Java ? -> Haskell point-free style
--   - composition of functions
--   - useful in lambdas - fast creation of anonymous functions in place
routeStopsCount' :: Route a -> Int
routeStopsCount' = length . routeStops

-- Java Object.equals() -> Haskell Eq typeclass
instance Eq a => Eq (RouteStop a) where
  stop1 == stop2 = routeStopWhat stop1 == routeStopWhat stop2

-- Java Comparable -> Haskell Ord typeclass
instance Ord a => Ord (RouteStop a) where
  stop1 <= stop2 = routeStopWhat stop1 <= routeStopWhat stop2

-- Java bounded type parameters in methods -> Haskell typeclass constraints in functions
-- Java ? -> Haskell pattern matching argument capture (`@`)
sortRoute :: Ord a => Route a -> Route a
sortRoute route@Route{ routeStops=stops } = route{ routeStops=quicksort stops }

-- Java “single responsibility principle” -> Haskell polymorphic functions - http://degoes.net/articles/insufficiently-polymorphic
--   - comparing elements decoupled from constructing list, higher reusability, less coupling
-- Java ? -> Haskell list comprehensions
quicksort :: Ord a => [a] -> [a]
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- Java nested Function, BiFuction... in method parameters -> Haskell Higher order functions
--   - Java: BiFunction<BiFunction<B, Location, Function<Location, B>>, BiFunction<B, A, B>, BiFunction<B, Route<B>, B>
traverseRoute :: (b -> Location -> Location -> b) -> (b -> a -> b) -> b -> Route a -> b
traverseRoute _ _ b (Route _ []) = b
traverseRoute traverseMovement traverseStay b route@(Route baseLoc _) = doFoldRoute traverseMovement traverseStay b route baseLoc
  where
    doFoldRoute traverseMovement traverseStay b (Route baseLoc []) lastLoc = traverseMovement b lastLoc baseLoc
    doFoldRoute traverseMovement traverseStay b (Route baseLoc (RouteStop loc a:rest)) lastLoc =
      -- Java final values -> Haskell let/in expressions
      let
        b1 = traverseMovement b lastLoc loc
        b2 = traverseStay b1 a
      in doFoldRoute traverseMovement traverseStay b2 (Route baseLoc rest) loc

-- Java lambdas -> Haskell lambdas
instance Show a => Show (Route a) where
  show = traverseRoute (\s loc1 loc2 -> s ++ show loc1 ++ " -> " ++ show loc2 ++ ", ") (\s a -> s ++ show a ++ ",") ""

-- Java "Data Transfer Objects" -> Haskell one-liners
--   - fosters interface segregation and reduces coupling
-- Java default implementations of Object methods -> Haskell derivable instances of typeclasses
--   - derivable Eq, Read, Show, Ord, Enum, Ix, Bounded
data RouteSummary = RouteSummary { routeSummaryTime :: UTCTime, routeSummaryCost :: Cost } deriving Show

data Journey = Journey {
  travelers :: Travelers,
  startTime :: UTCTime,
  route :: Route DurationSeconds
} deriving Show

updateRouteSummary :: Travelers -> Leg -> RouteSummary -> Either Exception RouteSummary
updateRouteSummary travelers leg routeSummary@RouteSummary{ routeSummaryCost=routeSummaryCost, routeSummaryTime=routeSummaryTime } = case generatedCost travelers leg of
  Right legCost -> Right RouteSummary{ routeSummaryCost = mappend legCost routeSummaryCost, routeSummaryTime = addUTCTime (legSeconds leg) routeSummaryTime }
  Left exc -> Left exc

-- Java I/O -> Haskell IO Monad
--   - a -> IO b ~= (RealWorld, a) -> (b, RealWorld)
--   - a -> IO b ~= compute b basing on a and performing some I/O
updateRouteSummaryWithGoogleDirections :: HTTP.Manager -> G.ApiKey -> Travelers -> Location -> Location -> RouteSummary -> IO (Either Exception RouteSummary)
updateRouteSummaryWithGoogleDirections manager googleApiKey travelers loc1 loc2 routeSummary = do
  mGoogleDirectionsResponse <- G.getGoogleDirections manager googleApiKey (transport2Mode (transport travelers)) (show loc1) (show loc2) ((round . utcTimeToPOSIXSeconds) (routeSummaryTime routeSummary))
  return $ case extractLeg mGoogleDirectionsResponse of
    Nothing -> Left "Cannot extract data from GoogleDirections response"
    Just leg -> updateRouteSummary travelers leg routeSummary
    where
      transport2Mode :: Transport -> G.Mode
      transport2Mode (CarTransport _) = G.Driving
      transport2Mode BikeTransport = G.Bicycling
      transport2Mode PublicTransport  = G.Transit
      -- Maybe Monad
      extractLeg :: Maybe G.GoogleDirectionsResponse -> Maybe Leg
      extractLeg mGoogleDirectionsResponse = do
        response <- mGoogleDirectionsResponse
        firstRoute <- listToMaybe (G.routes response)
        firstLeg <- listToMaybe (G.legs firstRoute)
        let diffTimeSeconds = (fromInteger . G.value . G.duration) firstLeg
        let distanceMeters = (fromInteger . G.value . G.distance) firstLeg
        return Leg{ legSeconds=diffTimeSeconds, legKilometers=distanceMeters/1000 }

computeJourneySummaryWithGoogleDirections :: HTTP.Manager -> G.ApiKey -> Journey -> IO (Either Exception RouteSummary)
computeJourneySummaryWithGoogleDirections manager googleApiKey journey = traverseRoute traverseMovement traverseStop startRouteSummary (route journey)
  where
    startRouteSummary :: IO (Either Exception RouteSummary)
    startRouteSummary = return $ Right RouteSummary{ routeSummaryTime=startTime journey, routeSummaryCost = noCost }
    traverseMovement :: IO (Either Exception RouteSummary) -> Location -> Location -> IO (Either Exception RouteSummary)
    traverseMovement ioers loc1 loc2 = do
      ers <- ioers
      case ers of
        Right rs -> updateRouteSummaryWithGoogleDirections manager googleApiKey (travelers journey) loc1 loc2 rs
        l -> return l
    traverseStop :: IO (Either Exception RouteSummary) -> DurationSeconds -> IO (Either Exception RouteSummary)
    traverseStop ioers durationSeconds = do
      ers <- ioers
      return $ case ers of
        Right rs@RouteSummary{ routeSummaryTime=time } -> Right rs{ routeSummaryTime=addUTCTime durationSeconds time }
        l -> l

-- Java `public static void main()` -> Haskell module main function
--   - Haskell program is a value of type `IO ()` ~ it does some I/O and returns unit
main :: IO ()
main = do
  -- Java I/O -> Haskell IO Monad
  manager <- HTTP.newManager TLS.tlsManagerSettings
  googleApiKey <- readFile ".apiKey"
  now <- getCurrentTime
  -- Java dependency injection -> Haskell partial function application
  let computeJourneySummary = computeJourneySummaryWithGoogleDirections manager googleApiKey
  -- Java JSON bindings -> Haskell ADTs
  --   - Show typeclass to serialize
  --   - Read typeclass to deserialize
  let journey = Journey {
    travelers = Travelers {
      persons = [Adult, Adult],
      transport = CarTransport Car {
        fuel = LPG,
        fuelConsumptionLitresPerKilometer = 0.11
      }
    },
    startTime = now,
    route = Route (AtAddress "Złota Podkowa, Kraków") [
      RouteStop (AtAddress "Pawia 9, Kraków") (8 * 60 * 60),
      RouteStop (AtCoordinates (Coordinates 50.067938 19.901295)) (60 * 60),
      RouteStop (AtAddress "Lindego 1C, Kraków") (30 * 60)
    ]
  }
  putStrLn $ "Input:\n" ++ show journey
  eRouteSummary <- computeJourneySummary journey
  putStrLn $ either (\e -> "Exception:\n" ++ show e) (\routeSummary -> "Success:\n" ++ show routeSummary) eRouteSummary

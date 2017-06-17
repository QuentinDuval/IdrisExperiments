module HexagonalSNCF


-- https://github.com/emilybache/KataTrainReservation


--------------------------------------------------------------------------------
-- Domain types
--------------------------------------------------------------------------------

TrainId : Type
TrainId = String

CoachId : Type
CoachId = String

SeatId : Type
SeatId = Int

DateTime : Type
DateTime = Int

record ReservationRequest where
  constructor MkReservationRequest
  seatCount : Nat
  dateTime : DateTime

record CoachTypology where
  constructor MkCoachTypology
  coachId : CoachId
  totalSeatCount : Nat
  availableSeats : List SeatId

record TrainTypology where
  constructor MkTrainTypology
  trainId : TrainId
  coaches : List CoachTypology

record Reservation where -- TODO: use phantom type for the confirmation
  constructor MkReservation
  trainId : TrainId
  coachId : CoachId
  seatNbs : List SeatId

Show Reservation where
  show r = "{" ++ show (trainId r) ++ ", "
               ++ show (coachId r )++ ","
               ++ show (seatNbs r) ++ "}"

data ReservationResult
  = Confirmed Reservation
  | NoTrainAvailable

Show ReservationResult where
  show (Confirmed r) = "Confirmed: " ++ show r
  show NoTrainAvailable = "No Trains Available"

--------------------------------------------------------------------------------
-- Definition of the DSL for reservation
--------------------------------------------------------------------------------

data ReservationExpr : Type -> Type where
  -- TODO: add state... to force a workflow (and add abort + confirm + pay)
  SearchTrain : DateTime -> ReservationExpr (List TrainId)
  GetTypology : TrainId -> ReservationExpr TrainTypology
  Reserve : Reservation -> ReservationExpr (Maybe Reservation)
  Log : String -> ReservationExpr ()
  Pure : ta -> ReservationExpr ta
  Bind : ReservationExpr ta -> (ta -> ReservationExpr tb) -> ReservationExpr tb

(>>=): ReservationExpr ta -> (ta -> ReservationExpr tb) -> ReservationExpr tb
(>>=) = Bind

Functor ReservationExpr where
  map fn expr = expr >>= Pure . fn

Applicative ReservationExpr where
  pure = Pure
  fExpr <*> aExpr = fExpr >>= \f => map f aExpr


--------------------------------------------------------------------------------
-- Interpreter: this is the transformation from the abstract problem
-- to the real world (allows to plug the SPI without Dependency Injection)
--------------------------------------------------------------------------------

evalReservation : ReservationExpr ty -> IO ty
evalReservation (Log msg) = putStrLn msg
evalReservation (Pure val) = pure val
evalReservation (Bind val next) = evalReservation val >>= evalReservation . next
evalReservation (SearchTrain dateTime) = pure ["T1", "T2"]
evalReservation (GetTypology trainId) = do
  putStrLn ("GetTypology: " ++ trainId)
  if trainId == "T1"
    then pure $ MkTrainTypology "T1" [MkCoachTypology "A" 100 [5..100]]
    else pure $ MkTrainTypology "T2" [MkCoachTypology "A" 100 [5..100]]

evalReservation (Reserve command) = do
  putStrLn ("Reserve: " ++ show command)
  pure $ Just command -- TODO: introduce errors


--------------------------------------------------------------------------------
-- The code (should follow the rule of the DSL)
-- Decoupling is pretty good:
-- * Invariants of the Business Rules are in the DSL
-- * Implementation of the SPI are in the interpreter
-- * Current implementation that satisfies the rules is exression of the DSL
--------------------------------------------------------------------------------

record OccupancyRatio where
  constructor MkOccupancyRatio
  occupied : Nat
  seatCount : Nat

Semigroup OccupancyRatio where
  (<+>) a b = MkOccupancyRatio (occupied a + occupied b) (seatCount a + seatCount b)

Monoid OccupancyRatio where
  neutral = MkOccupancyRatio 0 0

occupancyPercent : OccupancyRatio -> Double
occupancyPercent r =
  if occupied r >= seatCount r
    then 1.0
    else cast (occupied r) / cast (seatCount r)

belowThreshold : Double -> OccupancyRatio -> Bool
belowThreshold threshold ratio = occupancyPercent ratio <= threshold

addOccupied : Nat -> OccupancyRatio -> OccupancyRatio
addOccupied seatRequest r = record { occupied $= (+ seatRequest) } r

--------------------------------------------------------------------------------
-- RULES:
-- TODO: sum the total seats to check that will not go over 70% of the train
-- TODO: check the typology of the coaches to put the same family in one coach
-- TODO: ideally, we should not go over 70% in one coach
--------------------------------------------------------------------------------

TrainMaxOccupancy : Double
TrainMaxOccupancy = 0.7

CoachMaxOccupancy : Double
CoachMaxOccupancy = 0.8

coachOccupancy : CoachTypology -> OccupancyRatio
coachOccupancy coach =
  let totalSeats = totalSeatCount coach
      takenSeats = length (availableSeats coach)
  in case isLTE takenSeats totalSeats of
        Yes prf => MkOccupancyRatio totalSeats (totalSeats - takenSeats)
        No contra => MkOccupancyRatio totalSeats totalSeats

trainOccupancy : TrainTypology -> OccupancyRatio
trainOccupancy train = concatMap coachOccupancy (coaches train)

toCoachTypologies : List TrainTypology -> List (TrainId, CoachTypology)
toCoachTypologies trains = concat (map trainTypologies trains)
  where
    trainTypologies train = map (\coach => (trainId train, coach)) (coaches train)

coachToReservation : Nat -> (TrainId, CoachTypology) -> Reservation
coachToReservation seatRequest (trainId, coach) =
  MkReservation trainId (coachId coach) (take seatRequest (availableSeats coach))

reservationsByDecreasingPreference : Nat -> List TrainTypology -> List Reservation
reservationsByDecreasingPreference seatRequest trains =
  let freeTrains = filter (belowThreshold TrainMaxOccupancy . addOccupied seatRequest . trainOccupancy) trains
      allCoaches = toCoachTypologies freeTrains
      bestCoaches = filter (belowThreshold CoachMaxOccupancy . addOccupied seatRequest . coachOccupancy . snd) allCoaches
      nextCoaches = filter (belowThreshold 1.0 . addOccupied seatRequest . coachOccupancy . snd) allCoaches
  in map (coachToReservation seatRequest) (bestCoaches ++ nextCoaches) -- TODO: avoid duplicates (use sort)

reserve : ReservationRequest -> ReservationExpr ReservationResult
reserve request = do
    trainIds <- SearchTrain (dateTime request)
    typologies <- sequence (map GetTypology trainIds)
    let reservations = reservationsByDecreasingPreference (seatCount request) typologies
    Log (show reservations)
    confirmByPref reservations
  where
    confirmByPref [] = Pure NoTrainAvailable
    confirmByPref (r::rs) = do
      validated <- Reserve r
      case validated of
        Nothing => confirmByPref rs
        Just ok => Pure (Confirmed ok)


--------------------------------------------------------------------------------
-- Run Tests
--------------------------------------------------------------------------------

run_tests : IO ()
run_tests = do
  let request = MkReservationRequest 10 10
  result <- evalReservation (reserve request)
  printLn result

--

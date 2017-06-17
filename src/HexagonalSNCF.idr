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

record ReservationCommand where
  constructor MkReservationCommand
  trainId : TrainId
  coachId : CoachId

data ReservationError
  = TechnicalError -- TODO: could be made such that the evaluator handles them
  | CoachIsFull

record ConfirmedReservation where
  constructor MkConfirmedReservation
  trainId : TrainId
  coachId : CoachId
  seatNbs : List Int

Reservation : Type
Reservation = Either ReservationError ConfirmedReservation


--------------------------------------------------------------------------------
-- Definition of the DSL for reservation
--------------------------------------------------------------------------------

data ReservationExpr : Type -> Type where
  -- TODO: add state... to force a workflow (and add abort + confirm + pay)
  SearchTrain : DateTime -> ReservationExpr (List TrainId)
  GetTypology : TrainId -> ReservationExpr TrainTypology
  Reserve : ReservationCommand -> ReservationExpr Reservation
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
evalReservation = ?hole


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
    then 100.0
    else cast (occupied r) / cast (seatCount r)

belowThreshold : Double -> OccupancyRatio -> Bool
belowThreshold threshold ratio = occupancyPercent ratio <= threshold

addOccupied : Nat -> OccupancyRatio -> OccupancyRatio
addOccupied seatRequest r = record { occupied $= (+ seatRequest) } r

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

    -- TODO: two loops, one to search for the best match (70% less occupancy), second ignores that (or sort...)

bestTypology : Nat -> List TrainTypology -> Maybe ReservationCommand
bestTypology seatRequest trains =
  let freeTrains = filter (belowThreshold TrainMaxOccupancy . addOccupied seatRequest . trainOccupancy) trains
  in ?bestTYpology

-- TODO: sum the total seats to check that will not go over 70% of the train
-- TODO: check the typology of the coaches to put the same family in one coach
-- TODO: ideally, we should not go over 70% in one coach

reserve : ReservationRequest -> ReservationExpr Reservation -- TODO: should be different errors (mapping is nice!)
reserve request = do
  trainIds <- SearchTrain (dateTime request)
  typologies <- sequence (map GetTypology trainIds)
  let command = bestTypology (seatCount request) typologies
  -- TODO: might not be any command...
  case command of
    Nothing       => Pure (Left CoachIsFull)
    Just command  => do
      r <- Reserve command
      -- TODO: handle errors (race conditions... ask for retry or abort)
      Pure r





--

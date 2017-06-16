module HexagonalSNCF


-- https://github.com/emilybache/KataTrainReservation


--------------------------------------------------------------------------------
-- Domain types
--------------------------------------------------------------------------------

TrainId : Type
TrainId = String

CoachId : Type
CoachId = String

DateTime : Type
DateTime = Int

record ReservationRequest where
  constructor MkReservationRequest
  seatCount : Nat
  dateTime : DateTime

record ReservationCommand where
  constructor MkReservationCommand
  trainId : TrainId
  coachId : CoachId

record CoachTypology where
  constructor MkCoachTypology
  coachId : CoachId
  availableSeats : List Int

record TrainTypology where
  constructor MkTrainTypology
  trainId : TrainId
  coaches : List CoachTypology

data Reservation -- Use either or error?
  = ConfirmedReservation String
  | FailedReservation String


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

implementation Functor ReservationExpr where
  map fn expr = expr >>= Pure . fn

implementation Applicative ReservationExpr where
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

bestTypology : Nat -> List TrainTypology -> ReservationCommand
bestTypology seatCount typologies = ?bestTypology

-- TODO: sum the total seats to check that will not go over 70% of the train
-- TODO: check the typology of the coaches to put the same family in one coach
-- TODO: ideally, we should not go over 70% in one coach

reserve : ReservationRequest -> ReservationExpr Reservation
reserve request = do
  trainIds <- SearchTrain (dateTime request)
  typologies <- sequence (map GetTypology trainIds)
  let command = bestTypology (seatCount request) typologies
  r <- Reserve command
  -- TODO: handle errors (race conditions... ask for retry or abort)
  Pure r





--

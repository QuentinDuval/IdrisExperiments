module HexagonalSNCF

TrainId : Type
TrainId = String

CoachId : Type
CoachId = String

TrainTypology : Type
TrainTypology = List String

ReservationRequest : Type
ReservationRequest = String

data Reservation -- Use either or error?
  = ConfirmedReservation String
  | FailedReservation String

||| Definition of the DSL for reservation
data ReservationExpr : Type -> Type where
  -- TODO: add state... to force a workflow (and add abort + confirm + pay)
  SearchTrain : ReservationRequest -> ReservationExpr (List TrainId)
  GetTypology : TrainId -> ReservationExpr TrainTypology
  Reserve : TrainId -> CoachId -> ReservationExpr Reservation
  Pure : ta -> ReservationExpr ta
  Bind : ReservationExpr ta -> (ta -> ReservationExpr tb) -> ReservationExpr tb

(>>=): ReservationExpr ta -> (ta -> ReservationExpr tb) -> ReservationExpr tb
(>>=) = Bind

implementation Functor ReservationExpr where
  map fn expr = expr >>= Pure . fn

implementation Applicative ReservationExpr where
  pure = Pure
  fExpr <*> aExpr = fExpr >>= \f => map f aExpr


||| Interpreter: this is the transformation from the abstract problem
||| to the real world (allows to plug the SPI without Dependency Injection)
evalReservation : ReservationExpr ty -> IO ty
evalReservation = ?hole

||| The code (should follow the rule of the DSL)
||| Decoupling is pretty good:
||| - Invariants of the Business Rules are in the DSL
||| - Implementation of the SPI are in the interpreter
||| - Current implementation that satisfies the rules is exression of the DSL
reserve : ReservationRequest -> ReservationExpr Reservation
reserve request = do
  trainIds <- SearchTrain request
  typologies <- sequence (map GetTypology trainIds)
  -- Exploit the typology there
  r <- Reserve "ID of the train" "Coach ID"
  Pure r






--

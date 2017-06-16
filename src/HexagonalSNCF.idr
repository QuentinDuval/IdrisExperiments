module HexagonalSNCF

TrainId : Type
TrainId = String

CoachId : Type
CoachId = String

TrainTypology : Type
TrainTypology = List String

data Reservation -- Use either or error?
  = ConfirmedReservation String
  | FailedReservation String

-- TODO: add state...

data ReservationExpr : Type -> Type where
  GetTypology : TrainId -> ReservationExpr TrainTypology
  Reserve : TrainId -> CoachId -> ReservationExpr Reservation
  Pure : ta -> ReservationExpr ta
  (>>=): ReservationExpr ta -> (ta -> ReservationExpr tb) -> ReservationExpr tb

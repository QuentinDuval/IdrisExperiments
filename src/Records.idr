module Records

record Account where
  constructor MkAccount
  accound_id : String
  address : String

record Customer where
  constructor MkCustomer
  name : String
  address : String
  account : Account

john : Customer
john = MkCustomer "John" "NYC" (MkAccount "123" "NYC")

custom_billing_account : Customer -> Bool
custom_billing_account customer =
  Customer.address customer /= Account.address (account customer)

update_billing_address : Customer -> String -> Customer
update_billing_address customer address =
  record { account->address = address } customer

concat_billing_address : Customer -> String -> Customer
concat_billing_address customer complement =
  record { account->address $= (++ complement) } customer

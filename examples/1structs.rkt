#lang gorack

(package structs
  (type MyStruct
    (struct
      (x int)
      ([x y] int)
      (FieldName string)
      (AnotherField int (tag (json another_field #:omitempty)))
      (zip_code string (tag (json zip_code))))))

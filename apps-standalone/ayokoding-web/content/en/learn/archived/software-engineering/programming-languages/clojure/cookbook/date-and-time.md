---
title: "Date and Time"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/date_and_time).

</aside>

## Working with Local Time, Local Date, and Local Date Time

### Code

```clojure
(ns date-and-time.core
  (:import java.time.format.DateTimeFormatter
           java.time.LocalDate
           java.time.LocalDateTime
           java.time.LocalTime))

;; ---
;; Working with LocalTime, LocalDate, LocalDateTime
;; ---

;; get current time and date

(def now (LocalDateTime/now))
(def today (LocalDate/now))
(def current-time (LocalTime/now))

now
;; => #object[java.time.LocalDateTime 0x1d4bb3cb "2023-06-16T17:06:16.866955"]
(str now)
;; => "2023-06-16T17:06:16.866955"
today
;; => #object[java.time.LocalDate 0x6ed8521a "2023-06-16"]
(str today)
;; => "2023-06-16"
current-time
;; => #object[java.time.LocalTime 0x4811ab65 "17:06:24.039461"]
(str current-time)
;; => "17:06:24.039461"

;; Playing with date and time format

(-> now (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")))
;; => "2023-06-16 17:06:16"
(-> today (.format DateTimeFormatter/ISO_LOCAL_DATE))
;; => "2023-06-16"
(-> current-time (.format DateTimeFormatter/ISO_LOCAL_TIME))
;; => "17:06:24.039461"

(def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))
(def formatted-date-time (.format formatter now))

formatted-date-time
;; => "2023-06-16 17:06:16"

(def parsed-as-local-date-time (LocalDateTime/parse formatted-date-time formatter))
(def parsed-as-local-date (LocalDate/parse formatted-date-time formatter))
(def parsed-as-local-time (LocalTime/parse formatted-date-time formatter))

(class parsed-as-local-date-time)
;; => java.time.LocalDateTime
(str parsed-as-local-date-time)
;; => "2023-06-16T17:06:16"
(class parsed-as-local-date)
;; => java.time.LocalDate
(str parsed-as-local-date)
;; => "2023-06-16"
(class parsed-as-local-time)
;; => java.time.LocalTime
(str parsed-as-local-time)
;; => "17:06:16"
```

### Explanation

This Clojure code is utilizing the java.time package for working with dates and times. Here's a line-by-line breakdown:

1. **Namespace declaration**: `(ns date-and-time.core)` - This is declaring the namespace as `date-and-time.core`. In Clojure, the namespace is a container that holds a set of named functions, variables, etc.
2. **Imports**: It imports various classes from the `java.time` and `java.time.format` packages to work with date and time.
3. **Variable declarations**:
   - `(def now (LocalDateTime/now))` - This defines a variable `now` that holds the current date-time.
   - `(def today (LocalDate/now))` - This defines a variable `today` that holds the current date.
   - `(def current-time (LocalTime/now))` - This defines a variable `current-time` that holds the current time.
4. **Printing values**: Next, it prints the `now`, `today`, and `current-time` values first as objects and then as strings.
5. **Formatting date and time**:
   - Using the `format` method and `DateTimeFormatter/ofPattern` function, it formats the date and time into specific patterns.
   - `(-> now (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")))` - This line is using the thread-first macro (`>`) to apply the format function to `now`. The pattern "yyyy-MM-dd HH:mm:ss" represents the four-digit year, two-digit month, two-digit day, two-digit hour, two-digit minute, and two-digit second.
   - Similarly, `today` and `current-time` are formatted using `DateTimeFormatter/ISO_LOCAL_DATE` and `DateTimeFormatter/ISO_LOCAL_TIME` respectively.
6. **Custom formatter**:
   - `(def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))` - This line creates a custom date-time formatter.
   - `(def formatted-date-time (.format formatter now))` - Then, it formats `now` using the custom formatter.
7. **Parsing date and time**:
   - It creates three new variables: `parsed-as-local-date-time`, `parsed-as-local-date`, and `parsed-as-local-time`.
   - Each of these variables uses the corresponding parse method to convert `formatted-date-time` back into date-time, date, and time objects.
8. **Inspecting the parsed variables**: It finally prints the class type and string representation of the parsed variables.

## Time Manipulation and Comparison

### Code

```clojure
(ns date-and-time.core
  (:import java.time.format.DateTimeFormatter
           java.time.LocalDate
           java.time.LocalDateTime
           java.time.LocalTime))

;; ---
;; Date Time manipulation and comparison
;; ---

(def now (LocalDateTime/now))
(def today (LocalDate/now))
(def current-time (LocalTime/now))

(def tomorrow (-> today (.plusDays 1)))
(def future-date-time (-> now (.plusHours 2) (.plusMinutes 30)))
(def future-date (-> today (.plusDays 2)))
(def future-time (-> current-time (.plusHours 2) (.plusMinutes 30)))

(str now)
;; => "2023-06-16T17:06:16.866955"
(str tomorrow)
;; => "2023-06-17"
(str future-date-time)
;; => "2023-06-16T19:36:16.866955"
(str future-date)
;; => "2023-06-18"
(str future-time)
;; => "19:36:24.039461"

(def yesterday (-> today (.minusDays 1)))
(def past-date-time (-> now (.minusHours 2) (.minusMinutes 30)))
(def past-date (-> today (.minusDays 2)))
(def past-time (-> current-time (.minusHours 2) (.minusMinutes 30)))

(str now)
;; => "2023-06-16T17:06:16.866955"
(str yesterday)
;; => "2023-06-15"
(str past-date-time)
;; => "2023-06-16T14:36:16.866955"
(str past-date)
;; => "2023-06-14"
(str past-time)
;; => "14:36:24.039461"

;; Comparing two times

(-> today (.compareTo tomorrow))
;; => -1
(-> today (.compareTo yesterday))
;; => 1
(-> tomorrow (.compareTo yesterday))
;; => 2

(-> future-date (.compareTo past-date))
;; => 4
(-> future-date-time (.compareTo past-date-time))
;; => 1
(-> future-time (.compareTo past-time))
;; => 1
```

### Explanation

This Clojure code works with date and time manipulations, comparisons, and transformations. It uses the `java.time` package for these purposes. Here's an explanation of the code:

1. The `ns` expression defines the namespace `date-and-time.core`. This is essentially the name of the module or script.
2. `:import` keyword is used to import specific classes from the `java.time` package: `DateTimeFormatter`, `LocalDate`, `LocalDateTime`, `LocalTime`.
3. `def` is used to create global vars or constants. The `LocalDate/now`, `LocalDateTime/now`, `LocalTime/now` functions fetch the current date, date-time, and time respectively.
4. The following vars are defined using Clojure's threading macro `>` which helps in chaining function calls:
   - `tomorrow`: This adds 1 day to `today`.
   - `future-date-time`: This adds 2 hours and 30 minutes to `now`.
   - `future-date`: This adds 2 days to `today`.
   - `future-time`: This adds 2 hours and 30 minutes to `current-time`.
   - `yesterday`: This subtracts 1 day from `today`.
   - `past-date-time`: This subtracts 2 hours and 30 minutes from `now`.
   - `past-date`: This subtracts 2 days from `today`.
   - `past-time`: This subtracts 2 hours and 30 minutes from `current-time`.
5. `str` is a function that converts its argument to a string. It's used here to represent date/time objects as strings.
6. `(-> today (.compareTo tomorrow))`: This compares `today` to `tomorrow`. If `today` is less than `tomorrow`, it returns -1, equal would return 0, and if `today` is greater than `tomorrow` it would return a positive number. The same logic applies to the other date comparisons.
7. `(-> future-date (.compareTo past-date))`: This compares `future-date` with `past-date`. It will return a positive number if `future-date` is after `past-date`, zero if they are the same, and a negative number if `future-date` is before `past-date`.
8. Similar comparisons are made for `future-date-time` vs `past-date-time` and `future-time` vs `past-time`. These will yield 1 if the future timestamp is greater than the past one (which is expected).

Note that Clojure doesn't have its own date and time library, hence the use of Java's built-in date and time functions.

## Duration Between 2 times

### Code

```clojure
(ns date-and-time.core
  (:import java.time.Duration
           java.time.format.DateTimeFormatter
           java.time.LocalDate
           java.time.LocalDateTime
           java.time.LocalTime
           java.time.Period
           java.time.ZonedDateTime
           java.time.ZoneId
           java.time.ZoneOffset))

;; ---
;; Duration between two temporal
;; ---

(def duration-datetime (Duration/between
                        (LocalDateTime/now)
                        (-> (LocalDateTime/now) (.plusDays 1) (.plusHours 12))))
(str duration-datetime)
;; => "PT36H0.000017S"
(.toSeconds duration-datetime)
;; => 129600
(.toHours duration-datetime)
;; => 36
(.toDays duration-datetime)
;; => 1

(def duration-date (Period/between
                    (LocalDate/now)
                    (-> (LocalDate/now) (.plusDays 33))))
(str duration-date)
;; => "P1M3D"
(.getDays duration-date)
;; => 3
(.getMonths duration-date)
;; => 1
(.toTotalMonths duration-date)
;; => 1

(def duration-time (Duration/between
                    (LocalTime/now)
                    (-> (LocalTime/now) (.plusHours 36))))
(str duration-time)
;; => "PT12H0.000013S"
(.toSeconds duration-time)
;; => 43200
(.toHours duration-time)
;; => 12
(.toDays duration-time)
;; => 0
```

### Explanation

Let's break down each section.

1. `(ns date-and-time.core ...)` - This is the namespace declaration for the Clojure code. The `ns` macro is used to declare the namespace of this file. This namespace is named `date-and-time.core`. The `:import` keyword is followed by several Java classes related to date and time handling, which are imported for use in this file.
2. `Duration/between` - This function creates a duration that represents the amount of time between two temporal objects. In the first instance, it's used with `LocalDateTime/now` (the current date and time) and a time that is 36 hours (1 day and 12 hours) in the future.
3. `(str duration-datetime)` - Converts the duration into a string. The output "PT36H0.000017S" represents a period of 36 hours and a tiny fraction of a second (likely due to the time elapsed between calculating the duration and formatting it).
4. `(.toSeconds duration-datetime)` - Converts the duration into total seconds. The result, 129600, is equivalent to 36 hours.
5. `(.toHours duration-datetime)` - Converts the duration into total hours.
6. `(.toDays duration-datetime)` - Converts the duration into total days.
7. `Period/between` - Like `Duration/between`, but used for dates instead of time. It measures the difference between two `LocalDate` objects. In this case, it's used with `LocalDate/now` (the current date) and a date 33 days in the future.
8. `(.getDays duration-date)`, `(.getMonths duration-date)`, `(.toTotalMonths duration-date)` - These lines extract the days and months component of the period and the total months. The result is a period of 1 month and 3 days.
9. The final set of expressions creates another duration, but this time between two `LocalTime` instances, measures the difference between the current time and 36 hours in the future. Then it converts this duration to a string, seconds, hours, and days.

## Time Zone: Get Current Time

### Code

```clojure
(ns date-and-time.core
  (:import java.time.format.DateTimeFormatter
           java.time.LocalDate
           java.time.LocalDateTime
           java.time.LocalTime
           java.time.ZonedDateTime
           java.time.ZoneId
           java.time.ZoneOffset))

;; ---
;; Working with timezone - get current-time
;; ---

;; Get current time with timezone

(def now-with-zone (ZonedDateTime/now))
(class now-with-zone)
;; => java.time.ZonedDateTime
(str now-with-zone)
;; => "2023-06-16T18:12:34.375243+07:00[Asia/Jakarta]"

;; Get current time with timezone in a place

(def now-in-berlin (-> now-with-zone (.withZoneSameInstant (ZoneId/of "Europe/Berlin"))))
(class now-in-berlin)
;; => java.time.ZonedDateTime
(str now-in-berlin)
;; => "2023-06-16T13:12:34.375243+02:00[Europe/Berlin]"

(class (ZoneId/getAvailableZoneIds))
;; => java.util.HashSet
(count (ZoneId/getAvailableZoneIds))
;; => 603
(filter #(.contains % "Jakarta") (ZoneId/getAvailableZoneIds))
;; => ("Asia/Jakarta")
```

### Explanation

This Clojure code is about working with date, time, and timezone in Clojure by using the Java 8 date-time API. Here's the line-by-line explanation:

1. `(ns date-and-time.core ...)` - This defines a new namespace named `date-and-time.core`. This is a typical way to start a Clojure file.
2. `(:import ...)` - The import clause is used to bring in classes from the Java library. The classes imported here are all parts of the Java 8 date-time API.
3. `(def now-with-zone (ZonedDateTime/now))` - This defines a var `now-with-zone` which holds the current date and time from the system clock in the default time-zone.
4. `(class now-with-zone)` - It's getting the class of `now-with-zone` which is `java.time.ZonedDateTime`.
5. `(str now-with-zone)` - It's converting `now-with-zone` to a string. The result looks like "2023-06-16T18:12:34.375243+07:00[Asia/Jakarta]".
6. `(def now-in-berlin (-> now-with-zone (.withZoneSameInstant (ZoneId/of "Europe/Berlin"))))` - This defines a var `now-in-berlin` which is the same instant as `now-with-zone` but in the timezone of Berlin.
7. `(class now-in-berlin)` - It's getting the class of `now-in-berlin` which is again `java.time.ZonedDateTime`.
8. `(str now-in-berlin)` - It's converting `now-in-berlin` to a string. The result looks like "2023-06-16T13:12:34.375243+02:00[Europe/Berlin]".
9. `(class (ZoneId/getAvailableZoneIds))` - It's getting the class of the result of `getAvailableZoneIds` method on `ZoneId`, which is `java.util.HashSet`.
10. `(count (ZoneId/getAvailableZoneIds))` - This counts the total number of available zone IDs, in this case, 603.
11. `(filter #(.contains % "Jakarta") (ZoneId/getAvailableZoneIds))` - This filters the available zone IDs to find ones that contain the string "Jakarta". The result is a list containing "Asia/Jakarta".

## Time Zone: Time Manipulation

### Code

```clojure
(ns date-and-time.core
  (:import java.time.format.DateTimeFormatter
           java.time.LocalDate
           java.time.LocalDateTime
           java.time.LocalTime
           java.time.ZonedDateTime
           java.time.ZoneId
           java.time.ZoneOffset))

;; ---
;; Working with timezone - manipulation
;; ---

;; Convert to GMT+ something

(def now-in-zone-offset (-> now-with-zone (.withZoneSameInstant (ZoneOffset/ofHours 5))))
(class now-in-zone-offset)
;; => java.time.ZonedDateTime
(str now-in-zone-offset)
;; => "2023-06-16T16:12:34.375243+05:00"

;; Convert to Local

(class (LocalDateTime/from now-in-berlin))
;; => java.time.LocalDateTime
(str (LocalDateTime/from now-in-berlin))
;; => "2023-06-16T13:12:34.375243"
(class (LocalDate/from now-in-berlin))
;; => java.time.LocalDate
(str (LocalDate/from now-in-berlin))
;; => "2023-06-16"
(class (LocalTime/from now-in-berlin))
;; => java.time.LocalTime
(str (LocalTime/from now-in-berlin))
;; => "13:12:34.375243"

(.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss z") now-with-zone)
;; => "2023-06-16 18:12:34 WIB"
(.format DateTimeFormatter/ISO_LOCAL_DATE now-with-zone)
;; => "2023-06-16"
(.format DateTimeFormatter/ISO_LOCAL_TIME now-with-zone)
;; => "18:12:34.375243"
(try (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss z") now)
     (catch Exception e (.getMessage e)))
;; => "Unable to extract ZoneId from temporal 2023-06-16T13:12:34.375243"
```

### Explanation

This Clojure code operates on the java.time package, and it's about working with dates, times, and time zones. Here's a breakdown of what each part does:

1. `(ns date-and-time.core ...)` - This line defines the namespace for the code. The namespace is called `date-and-time.core`. In the namespace declaration, several classes from the java.time package are imported for use in the code.
2. `(def now-in-zone-offset ...)` - This line defines a var called `now-in-zone-offset`. It takes a `now-with-zone` value (which should be a ZonedDateTime) and uses the `withZoneSameInstant` method to create a new ZonedDateTime that is in the GMT+5 timezone. The "->" is the thread-first macro that threads `now-with-zone` as the first argument to `.withZoneSameInstant`.
3. `(class now-in-zone-offset)` - This line returns the class of the `now-in-zone-offset` object, which is `java.time.ZonedDateTime`.
4. `(str now-in-zone-offset)` - This line returns a string representation of the `now-in-zone-offset` object.
5. `(class (LocalDateTime/from now-in-berlin))` - This line gets the LocalDateTime object from the ZonedDateTime `now-in-berlin` and returns its class, which is `java.time.LocalDateTime`.
6. `(str (LocalDateTime/from now-in-berlin))` - This line gets the LocalDateTime object from the ZonedDateTime `now-in-berlin` and returns its string representation.
7. Similarly, it does the same operation for LocalDate and LocalTime objects and prints the classes and string representations.
8. `(.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss z") now-with-zone)` - This line formats the `now-with-zone` ZonedDateTime object to a string using a specified date and time format, which includes the time zone.
9. `(.format DateTimeFormatter/ISO_LOCAL_DATE now-with-zone)` - This line formats the `now-with-zone` ZonedDateTime object to a string using the ISO_LOCAL_DATE format, which is yyyy-MM-dd.
10. `(.format DateTimeFormatter/ISO_LOCAL_TIME now-with-zone)` - This line formats the `now-with-zone` ZonedDateTime object to a string using the ISO_LOCAL_TIME format, which is HH:mm:ss.SSSSSS.
11. `(try ... (catch Exception e ...))` - This block attempts to format a LocalDateTime object (assumed by the `now` var) using a DateTimeFormatter that includes a time zone. However, LocalDateTime doesn't include any time zone information. This leads to an exception, which is then caught and the message "Unable to extract ZoneId from temporal 2023-06-16T13:12:34.375243" is printed out.

## Further Readings

- Java time library documentation: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
